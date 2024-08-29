### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
library(rpostgis)
library(RPostgreSQL)
library(RPostgres)

# Create an object that has connection to remote Database whose aDDress is 132.... anD name of Db is safegraph
conObesity <- RPostgreSQL::dbConnect("PostgreSQL", host = "132.216.183.3",
                                     dbname = "safegraph", user = "chizhang",
                                     password = "******")


# Write query to get Data - Data are too big, so DownloaD the first 100,000 only. I will later show how to DownloaD quebec Data only.  
#q <- "select * from ca_od_long where province = 'QC';"
q <- "select * from ca_od_long where province = 'QC' and Y = 2019 and m = 3;"
#q <- "select * from ca_oD_long limit 30000:400000;"

# SenD query to the Database
routing <- dbGetQuery(conObesity, q)

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

routing$area <- as.character(routing$area)
routing$home_area <- as.character(routing$home_area)

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
## Get boroughs and related cities of MTL
mtl<-st_read("limites-administratives-agglomeration.shp")
mtl <- st_transform(mtl, 2959)

# ggplot(mtl) +
#   geom_sf(fill = "white", color = "black") + 
#   geom_sf(data= DA, aes(fill = "white", color = "black"), alpha = 0.1)

DA_mtl <- DA %>% 
  st_join(mtl, largest = T)

# class(DA)
# class(mtl)
# 
# ggplot(DA_mtl) + geom_sf(aes(fill = NUM))

length(unique(DA_mtl$CT_UID))

DA_mtl %>% select(NOM, GeoUID) %>% st_drop_geometry()  %>% filter(!is.na(NOM))
DA_MTL_Key <- unique(DA_mtl[, c('NOM', 'GeoUID', "CT_UID", "Population")]) %>% st_drop_geometry() %>% filter(!is.na(NOM)) 
#DA_MTL_Key <- unique(DA_mtl[, c('NOM', 'GeoUID')]) %>% st_drop_geometry() %>% filter(!is.na(NOM)) 
DA_MTL_Nom_pop_Key <- unique(DA_MTL_Key[, c('NOM', "Population")]) %>% st_drop_geometry() %>% filter(!is.na(NOM)) %>% group_by(NOM) %>%   
  summarise(Population = sum(Population), .groups = 'drop')
length(unique(DA_MTL_Key$GeoUID))

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
library(dplyr)
#I. plot for mtl
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

DA_clean_data <- final_Data_b %>% filter(!is.na(NOM_area)) %>% filter(!is.na(NOM_home_area))
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
#II. plot added South&North
joined_Data_b <- routing %>%
  left_join(DA, by = c("area" = "GeoUID")) %>%
  rename("CT_UID_area" = "CT_UID") 

# Join on 'home_area'
final_Data_b <- joined_Data_b %>%
  left_join(DA, by = c("home_area" = "GeoUID")) %>%
  rename("CT_UID_home_area" = "CT_UID") 

final_Data_b <- final_Data_b %>%
  select(serial_id, source, area, home_area, stops, province, y, m,
         CT_UID_area, CT_UID_home_area)

DA_clean_data <- final_Data_b %>% filter(!is.na(area)) %>% filter(!is.na(home_area))

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
DA_clean_data <- DA_clean_data %>%
  group_by(area, home_area) %>%
  summarise(stops = sum(stops),.groups = 'drop')

edges <- unique(DA_clean_data[, c("area", "home_area", "stops")]) 

# plot histogram of stops from the routing data frame 
# (DA-level stop count, so II.DA_clean_data, not borough level that you already provided before), 
# for absolute stop count only (not popualtion normalized)
ggplot(edges, aes(x = stops)) +
          geom_histogram(binwidth = 40, fill = "blue", color = "black", alpha = 0.7) +
         labs(title = "Histogram of absolute routing Stops", x = "Stops", y = "Frequency") +
           theme_minimal()

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
## for the matrix area*home_area (~3000 * ~3000), entries are corresponding edge weight(stops)
# sum(row) plot, total stops for each area
DA_clean_data_unique_area <- DA_clean_data %>% group_by(area) %>% summarise(stops = sum(stops),.groups = 'drop')
DA_clean_data_unique_area <- DA_clean_data_unique_area  %>%
                                 left_join(DA, by = c("area" = "GeoUID")) %>%
                                 select(area, stops,geometry,Population) %>% 
                                 filter(!is.na(Population))

st_geometry(DA_clean_data_unique_area) <- DA_clean_data_unique_area$geometry

ggplot(DA_clean_data_unique_area) + geom_sf(aes(fill = stops)) +
  ggtitle("2019 march", subtitle = "area stops")
st_geometry(DA_clean_data_unique_area) <- DA_clean_data_unique_area$geometry
ggplot(DA_clean_data_unique_area) + geom_sf(aes(fill = stops/Population)) +
  ggtitle("2019 march", subtitle = "area stops divided by area Population")

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
# sum(col) plot, total stops for each home_area
DA_clean_data_unique_home_area <- DA_clean_data %>% group_by(home_area) %>% summarise(stops = sum(stops),.groups = 'drop')
DA_clean_data_unique_home_area <- DA_clean_data_unique_home_area  %>%
  left_join(DA, by = c("home_area" = "GeoUID")) %>%
  select(home_area, stops,geometry,Population) %>% 
  filter(!is.na(Population))

st_geometry(DA_clean_data_unique_home_area) <- DA_clean_data_unique_home_area$geometry

ggplot(DA_clean_data_unique_home_area) + geom_sf(aes(fill = stops)) +
  ggtitle("2019 march", subtitle = "home area stops")

ggplot(DA_clean_data_unique_home_area) + geom_sf(aes(fill = stops/Population)) +
  ggtitle("2019 march", subtitle = "home area stops divided by home area Population")

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
# mean(col) plot, mean stops for each home_area
# (since our data may count the same person traveling a lot multiple times so sum may be problemic)
DA_clean_data_unique_home_area_mean <- DA_clean_data %>% group_by(home_area) %>% summarise(stops = mean(stops),.groups = 'drop')
DA_clean_data_unique_home_area_mean <- DA_clean_data_unique_home_area_mean  %>%
  left_join(DA, by = c("home_area" = "GeoUID")) %>%
  select(home_area, stops,geometry,Population) 

st_geometry(DA_clean_data_unique_home_area_mean) <- DA_clean_data_unique_home_area_mean$geometry

ggplot(DA_clean_data_unique_home_area_mean) + geom_sf(aes(fill = stops)) +
  ggtitle("2019 march", subtitle = "home area mean stops")

ggplot(DA_clean_data_unique_home_area_mean) + geom_sf(aes(fill = stops/Population)) +
  ggtitle("2019 march", subtitle = "home area mean stops/Population")
# Average count of stops in each home_area divided by population


