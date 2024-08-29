### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
## Connect to Database
library(rpostgis)
library(RPostgreSQL)
library(RPostgres)

# Create an object that has connection to remote Database whose aDDress is 132.... anD name of Db is safegraph
conObesity <- RPostgreSQL::dbConnect("PostgreSQL", host = "132.216.183.3",
                                     dbname = "safegraph", user = "chizhang",
                                     password = "*****")


# Write query to get Data - Data are too big, so DownloaD the first 100,000 only. I will later show how to DownloaD quebec Data only.  
#q <- "select * from ca_od_long where province = 'QC';"
q <- "select * from ca_od_long where province = 'QC' and Y = 2021 and m BETWEEN 1 AND 5;"
#q <- "select * from ca_oD_long limit 30000:400000;"

# SenD query to the Database
routing <- dbGetQuery(conObesity, q) #"routing" is the data retrieved from the database

#length(unique(routing$area))
#length(unique(routing$home_area))

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
## Get DA & CT census data for MTL area
## Create accout and get API for spatial census data https://censusmapper.ca/users/sign_in
library(devtools)
library(dplyr)
library(tmap)
library(sf)
remotes::install_github("mountainmath/cancensus")

library(cancensus)

options(cancensus.api_key = "CensusMapper_91294cf61b6144bca0fcac3333a180b6")
options(cancensus.cache_path = "./")



# Then download census data to merge with your origin-destination data 

DA <- get_census(dataset='CA21', 
                 regions=list(CMA = "24462"),
                 level='DA', use_cache = FALSE, geo_format = "sf", quiet = TRUE, 
                 api_key = Sys.getenv("CM_API_KEY")) %>%
  st_transform(crs = 2959)


CT <- get_census(dataset='CA21', 
                 regions=list(CMA = "24462"),
                 level='CT', use_cache = FALSE, geo_format = "sf", quiet = TRUE, 
                 api_key = Sys.getenv("CM_API_KEY")) %>%
  st_transform(crs = 2959)
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
## Map identifier of DA to identifier of CT (to aggregate DA~6574 areas into CT~1004 areas)

# library(dplyr)
# mydata <- read.csv("2021_98260004.csv") 
# data <- mydata %>% select(PRDGUID_PRIDUGD, DADGUID_ADIDUGD, CMADGUID_RMRIDUGD, CTDGUID_SRIDUGD)
# write.csv(data, "my_dataset.csv")
# write.csv(data, "~/Desktop/my_dataset.csv", row.names = FALSE)

data <- read.csv("my_dataset.csv") 
# selects areas in Montreal only. 
# (Select all rows in CMADGUID_RMRIDUGD  whose number end with "462")
selected_Mtl <- data[grepl("462$", data$CMADGUID_RMRIDUGD), ]

# selects only ID for each Dissemination areas (DA)
selected_Mtl$DADGUID_ADIDUGD <- substr(selected_Mtl$DADGUID_ADIDUGD,
                                       nchar(selected_Mtl$DADGUID_ADIDUGD) - 7,
                                       nchar(selected_Mtl$DADGUID_ADIDUGD))

# selects only ID for Census Tracts (CT)
selected_Mtl$CTDGUID_SRIDUGD <- substr(selected_Mtl$CTDGUID_SRIDUGD,
                                       nchar(as.character(selected_Mtl$CTDGUID_SRIDUGD)) - 9,
                                       nchar(as.character(selected_Mtl$CTDGUID_SRIDUGD)))

mtl_selected_unique <- selected_Mtl %>% distinct(CTDGUID_SRIDUGD, .keep_all = TRUE)
length(mtl_selected_unique$CTDGUID_SRIDUGD) # ==nrow(CT)
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
## Merge "mtl_selected_unique"(CT code) with our "routing" data -> Joined data
### Two area codes(DA) in "routing" data frame,one is called AREA, and another called AREA_HOME.
### Both codes have to have correspoining CT codes, so that we can aggregate CT into DA.
### Then, aggregate all stop(sum stops of same area) counts based on CT codes and will have much smaller number of areas (CT)

# aggregate CT into DA 
# Df <- merge(routing, D, by.x = "area", by.y = "DADGUID_ADIDUGD", all.x = TRUE)
library(dplyr)

routing$area <- as.character(routing$area)
routing$home_area <- as.character(routing$home_area)
# Join on 'area'
joined_Data <- routing %>%
  left_join(mtl_selected_unique, by = c("area" = "DADGUID_ADIDUGD")) %>%
  rename(CTDGUID_SRIDUGD_area = CTDGUID_SRIDUGD)

# Join on 'home_area'
final_Data <- joined_Data %>%
  left_join(mtl_selected_unique, by = c("home_area" = "DADGUID_ADIDUGD")) %>%
  rename(CTDGUID_SRIDUGD_home_area = CTDGUID_SRIDUGD)

final_Data <- final_Data %>%
  select(serial_id, source, area, home_area, stops, province, y, m,
         CTDGUID_SRIDUGD_area, CTDGUID_SRIDUGD_home_area)

final_Data <- final_Data %>% replace(is.na(.), 0)
length(unique(final_Data$CTDGUID_SRIDUGD_area))

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
## Aggregate all stop(sum stops of same area) counts based on CT codes and will have much smaller number of areas (CT)
length(unique(CT$GeoUID)) #1004
length(unique(final_Data$CTDGUID_SRIDUGD_area)) #1002?

final_Data_CT <- final_Data %>%
                 group_by(CTDGUID_SRIDUGD_area, CTDGUID_SRIDUGD_home_area) %>%
                 summarise(stops = sum(stops),.groups = 'drop')

# Ensure that 'area' is treated as factor or character
final_Data_CT$CTDGUID_SRIDUGD_area <- as.character(final_Data_CT$CTDGUID_SRIDUGD_area)
final_Data_CT$CTDGUID_SRIDUGD_home_area <- as.character(final_Data_CT$CTDGUID_SRIDUGD_home_area)
CT$GeoUID <- as.character(CT$GeoUID)

# Get the geometry information based on CT code
CT_joined_Data_area <- final_Data_CT %>%
  left_join(CT %>% select(GeoUID, geometry), by = c("CTDGUID_SRIDUGD_area" = "GeoUID"))  %>%
  rename(geometry_area = geometry) #Why some are MULTIPOLYGON EMPTY for geometry column?(not matched in CT)

CT_joined_Data <- CT_joined_Data_area %>%
  left_join(CT %>% select(GeoUID, geometry), by = c("CTDGUID_SRIDUGD_home_area" = "GeoUID"))  %>%
  rename(geometry_home_area = geometry) #Why some are MULTIPOLYGON EMPTY for geometry column?(not matched in CT)

CT_clean_data <- CT_joined_Data %>% filter(!st_is_empty(geometry_area)) %>% filter(!st_is_empty(geometry_home_area))

# plot CT areas
ggplot(CT_clean_data$geometry_area) + geom_sf() 
ggplot(CT_clean_data$geometry_home_area) + geom_sf() 

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
## get network graph
library(igraph)
library(dplyr)
### for m=1-5
edges <- CT_clean_data %>%
  select(CTDGUID_SRIDUGD_area, CTDGUID_SRIDUGD_home_area, stops)
# nodes <- CT_clean_data %>%
#   select(CTDGUID_SRIDUGD_area, CTDGUID_SRIDUGD_home_area, geometry_area, geometry_home_area) %>%
#   distinct(area,  .keep_all = TRUE)
nodes <- CT %>% select(GeoUID)

network <- graph_from_data_frame(edges, nodes, directed = TRUE)
# V(network)$geometry_home_area
# E(network)

plot(network, edge.arrow.size=.1,vertex.label=NA, vertex.size=2, edge.curved=.1)
net <- simplify(network, remove.multiple = F, remove.loops = T) 
plot(net, edge.arrow.size=.1,vertex.label=NA, vertex.size=2, edge.curved=.1)

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###

## Cluster nodes into communities to reduce visual complexity and highlight group structures.
# Community detection
communities <- cluster_louvain(g_filtered)

# Plot with community structure
plot(communities, g_filtered, 
     vertex.size = V(g_filtered)$size, 
     vertex.label = V(g_filtered)$name, 
     edge.width = E(g_filtered)$weight / max(E(g_filtered)$weight) * 5, 
     edge.color = adjustcolor("grey", alpha.f = 0.5), 
     layout = layout)
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
## So many edges & nodes!! Not interpretable!! 
## Try mtl Borough instead!(So total 34 nodes and aggregate stops to these 34 areas instead of CT).

## Get boroughs and related cities of MTL
# download from https://open.canada.ca/data/en/dataset/9797a946-9da8-41ec-8815-f6b276dec7e9/resource/cfb1f359-cc0d-4b8d-84f4-e80ae2b9769e
mtl<-st_read("limites-administratives-agglomeration.shp")
# plot(mtl)
# class(mtl)
# st_crs(mtl)
mtl <- st_transform(mtl, 2959) # Mtl boroughs
# st_crs(mtl)
# 
# st_crs(DA)
# 
# ggplot(mtl) +
#   geom_sf(fill = "white", color = "black") + 
#   geom_sf(data= DA, aes(fill = "white", color = "black"), alpha = 0.1)
DA_mtl <- DA %>% st_join(mtl, largest = T) # boroughs information for DA

# class(DA)
# class(mtl)
# 
# ggplot(DA_mtl) + geom_sf(aes(fill = NUM))

length(unique(DA_mtl$CT_UID))

DA_mtl %>% select(NOM, GeoUID) %>% st_drop_geometry()  %>% filter(!is.na(NOM))

# boroughs information(mtl) for each DA(~3000) in mtl
DA_MTL_Key <- unique(DA_mtl[, c('NOM', 'GeoUID', "CT_UID", "Population")]) %>% st_drop_geometry() %>% filter(!is.na(NOM)) 
#DA_MTL_Key <- unique(DA_mtl[, c('NOM', 'GeoUID')]) %>% st_drop_geometry() %>% filter(!is.na(NOM)) 

# Populations for each boroughs area in mtl(total 34)
DA_MTL_Nom_pop_Key <- unique(DA_MTL_Key[, c('NOM', "Population")]) %>% st_drop_geometry() %>% filter(!is.na(NOM)) %>% group_by(NOM) %>%   
  summarise(Population = sum(Population), .groups = 'drop')
length(unique(DA_MTL_Key$GeoUID))

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
# Add boroughs information(mtl) for each routing data edge

library(dplyr)
library(ggplot2)

routing$area <- as.character(routing$area)
routing$home_area <- as.character(routing$home_area)

joined_Data_b <- routing %>%
  left_join(DA_MTL_Key, by = c("area" = "GeoUID")) %>%
  rename("CT_UID_area" = "CT_UID") %>%
  rename("NOM_area" = "NOM") 

# Join on 'home_area'
final_Data_b <- joined_Data_b %>%
  left_join(DA_MTL_Key, by = c("home_area" = "GeoUID")) %>%
  rename("CT_UID_home_area" = "CT_UID") %>%
  rename("NOM_home_area" = "NOM") 

final_Data_b <- final_Data_b %>%
  select(serial_id, source, area, home_area, stops, province, y, m,
         CT_UID_area, CT_UID_home_area, NOM_area, NOM_home_area)

# Aggregate all stop(sum stops of same area) counts based on 34 boroughs -> Aggregate stops for same edge
DA_clean_data <- final_Data_b %>% filter(!is.na(NOM_area)) %>% filter(!is.na(NOM_home_area))
DA_clean_data_march <- DA_clean_data %>%
  group_by(NOM_area, NOM_home_area) %>%   # unique edge
  summarise(stops = sum(stops), .groups = 'drop') 
max(DA_clean_data_march$stops)

# Get the weighted stops for each unique edge(*(Population_area)/(total population))
DA_clean_data_march <- DA_clean_data_march %>%
  left_join(DA_MTL_Nom_pop_Key, by = c("NOM_area" = "NOM")) %>%
  rename("Population_area" = "Population") 
DA_clean_data_march <- DA_clean_data_march %>%
  select(NOM_area, NOM_home_area, stops, Population_area)
# Weighted by population_area
DA_clean_data_march$stops_W <- DA_clean_data_march$stops/sum(DA_clean_data_march$Population_area)*DA_clean_data_march$Population_area
summary(DA_clean_data_march$stops_W)

# Get the within-node traffic (loop)
# with node attributes: stops, stops_W, Population_area
DA_clean_data_march_loop <- DA_clean_data_march %>% filter(NOM_area == NOM_home_area) 
#DA_clean_data_march_loop <- DA_clean_data_march_loop %>% filter(NOM_area != "Pointe-Claire")

# new_row <- data.frame( NOM_area = "L'Île-Dorval",
#                        NOM_home_area = "L'Île-Dorval",
#                        stops = 0,
#                        Population_area=0,
#                        stops_W=0) # L'Île-Dorval has 0
# DA_clean_data_march_loop <- rbind(DA_clean_data_march_loop, new_row)

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
## Create G(N, E)
library(igraph)
edges <- unique(DA_clean_data_march[, c("NOM_area", "NOM_home_area", "stops", "stops_W")]) 
nodes <- DA_clean_data_march_loop %>% select(NOM_area, stops)
summary(DA_clean_data_march$stops_W)

# Create the histogram of edges
ggplot(edges, aes(x = stops)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Stops 2019", x = "Stops", y = "Frequency") +
  theme_minimal()

ggplot(edges, aes(x = stops_W)) +
  geom_histogram(binwidth = 3, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Stops_W 2019 march", x = "Weighted Stops", y = "Frequency") +
  theme_minimal()

network <- graph_from_data_frame(edges, nodes, directed = TRUE)
#V(network)$stops
#E(network)

net <- simplify(network, remove.multiple = F, remove.loops = T) 
plot(net, edge.arrow.size=.1,vertex.label=NA, vertex.size=2, edge.curved=.1,layout=layout_randomly)