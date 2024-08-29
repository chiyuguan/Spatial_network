## For graph components:
## Connect to Data
library(rpostgis)
library(RPostgreSQL)
library(RPostgres)

# Create an object that has connection to remote Database whose aDDress is 132.... anD name of Db is safegraph
conObesity <- RPostgreSQL::dbConnect("PostgreSQL", host = "132.216.183.3",
                                     dbname = "safegraph", user = "chizhang",
                                     password = "*****")


# Write query to get Data - Data are too big, so DownloaD the first 100,000 only. I will later show how to DownloaD quebec Data only.  
#q <- "select * from ca_od_long where province = 'QC';"
q <- "select * from ca_od_long where province = 'QC' and Y = 2021 and m BETWEEN 7 AND 11;"
#q <- "select * from ca_od_long where province = 'QC' and Y = 2021 and m = 11;"
#q <- "select * from ca_oD_long limit 30000:400000;"

# SenD query to the Database
routing <- dbGetQuery(conObesity, q)

# Convert 'source' column to Date
# routing$source <- as.Date(routing$source)
# 
# # Filter rows with dates from 2021-11-01 to 2021-11-07
# start_date <- as.Date('2021-11-01')
# end_date <- as.Date('2021-11-07')
# routing <- subset(routing, source >= start_date & source <= end_date)

length(unique(routing$area))
length(unique(routing$home_area))

## Get DA & CT of mtl
library("devtools")
library(dplyr)
library(tmap)
library(sf)
library(cancensus)

options(cancensus.api_key = "CensusMapper_91294cf61b6144bca0fcac3333a180b6")
options(cancensus.cache_path = "./")

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

mtl <- st_transform(mtl, 2959)
## Get boroughs and related cities of MTL
# mtl<-st_read("limites-administratives-agglomeration.shp")
# mtl <- st_transform(mtl, 2959)
DA_mtl <- DA %>% 
  st_join(mtl, largest = T)

DA_mtl %>% select(NOM, GeoUID) %>% st_drop_geometry()  %>% filter(!is.na(NOM))
DA_MTL_Key <- unique(DA_mtl[, c('NOM', 'GeoUID', "CT_UID", "Population")]) %>% st_drop_geometry() %>% filter(!is.na(NOM)) 
#DA_MTL_Key <- unique(DA_mtl[, c('NOM', 'GeoUID')]) %>% st_drop_geometry() %>% filter(!is.na(NOM)) 
DA_MTL_Nom_pop_Key <- unique(DA_MTL_Key[, c('NOM', "Population")]) %>% st_drop_geometry() %>% filter(!is.na(NOM)) %>% group_by(NOM) %>%   
  summarise(Population = sum(Population), .groups = 'drop')
length(unique(DA_MTL_Key$GeoUID))

library(dplyr)

routing$area <- as.character(routing$area)
routing$home_area <- as.character(routing$home_area)

joined_Data_b <- routing %>%
  left_join(DA_MTL_Key, by = c("area" = "GeoUID")) %>%
  rename("CT_UID_area" = "CT_UID") %>%
  rename("NOM_area" = "NOM") 

final_Data_b <- joined_Data_b %>%
  left_join(DA_MTL_Key, by = c("home_area" = "GeoUID")) %>%
  rename("CT_UID_home_area" = "CT_UID") %>%
  rename("NOM_home_area" = "NOM") 

final_Data_b <- final_Data_b %>%
  select(row.names, source, area, home_area, stops, province, y, m,
         CT_UID_area, CT_UID_home_area, NOM_area, NOM_home_area)

DA_clean_data <- final_Data_b %>% filter(!is.na(NOM_area)) %>% filter(!is.na(NOM_home_area))

DA_clean_data_m1_5 <- DA_clean_data %>%
  group_by(NOM_area, NOM_home_area) %>%   
  summarise(stops = sum(stops), .groups = 'drop') #weight/CT_UID_area

DA_clean_data_m1_5 <- DA_clean_data_m1_5 %>%
  left_join(DA_MTL_Nom_pop_Key, by = c("NOM_area" = "NOM")) %>%
  rename("Population_area" = "Population") 
DA_clean_data_m1_5 <- DA_clean_data_m1_5 %>%
  select(NOM_area, NOM_home_area, stops, Population_area)
# Weighted by population_area
DA_clean_data_m1_5$stops_W <- DA_clean_data_m1_5$stops/sum(DA_clean_data_m1_5$Population_area)*DA_clean_data_m1_5$Population_area
summary(DA_clean_data_m1_5$stops_W)
write.csv(DA_clean_data_m1_5, "DA_clean_data_m1_5.csv")

DA_clean_data_m1_5_loop <- DA_clean_data_m1_5 %>%
  filter(NOM_area == NOM_home_area) %>%
  rename("NOM"="NOM_area")  %>% 
  select(NOM, stops)

library(igraph)
library(dplyr)
edges <- unique(DA_clean_data_m1_5[, c("NOM_area", "NOM_home_area", "stops", "stops_W")]) 
nodes <- mtl %>% select(NOM) %>% filter(NOM != "L'Île-Dorval")

network <- graph_from_data_frame(edges, nodes, directed = F)
adjacency_matrix <- as.matrix(as_adjacency_matrix(network, attr="stops"))

# Replace row names with CODEID
rownames(adjacency_matrix) <- mtl$CODEID[match(rownames(adjacency_matrix), mtl$NOM)]

# Replace column names with CODEID
colnames(adjacency_matrix) <- mtl$CODEID[match(colnames(adjacency_matrix), mtl$NOM)]

# Assuming your matrix is named mat

# Order the row and column names
ordered_row_names <- rownames(adjacency_matrix)[order(as.numeric(rownames(adjacency_matrix)))]
ordered_col_names <- colnames(adjacency_matrix)[order(as.numeric(colnames(adjacency_matrix)))]

# Reorder the matrix
adjacency_matrix <- adjacency_matrix[ordered_row_names, ordered_col_names]


adjacency_matrix

