map_nodes <- function(edges,nodes) {
  
  nodes <- nodes  %>%
    left_join(mtl_geo_key, by = c("id" = "NOM")) 
  nodes_sf <- st_as_sf(nodes)
  nodes$geometry <- st_centroid(nodes$geometry)
  
  edges <- edges %>%
    left_join(mtl_geo_key, by = c("from" = "NOM")) %>%
    rename(from_geo = geometry) %>%
    left_join(mtl_geo_key, by = c("to" = "NOM")) %>%
    rename(to_geo = geometry) 
  
  edges_sf <- st_as_sf(edges)
  
  edges_sf$geometry <- mapply(function(from, to) {
    from_coords <- st_coordinates(st_centroid(from))
    to_coords <- st_coordinates(st_centroid(to))
    st_linestring(rbind(from_coords, to_coords))
  }, edges$from_geo, edges$to_geo, SIMPLIFY = FALSE)
  
  #edges_sf <- edges  %>% st_as_sf(coords = c(from_cor, to_cor), crs = 2959)
  edges_sf <- st_as_sfc(edges_sf$geometry, crs = 2959)
  edges_sf <- st_as_sf(edges_sf, crs = 2959)
  
  ggplot() +
    geom_sf(data = mtl) +
    geom_sf(data = edges_sf, color = "blue") +
    geom_sf(data = nodes$geometry) +
    geom_text(data = nodes, aes(x = st_coordinates(geometry)[,1], 
                                y = st_coordinates(geometry)[,2], 
                                label = label), size = 3, vjust = -1) +
    ggtitle("2019 Network Flow Plot", subtitle = "nodes(loop)") +
    #geom_text(data = nodes, aes(label = label), size = size, hjust = 1.5) +
    scale_size_continuous(range = c(0.5, 3)) +
    scale_color_identity() +
    theme_map() +
    theme(legend.position = "none")
}


map_edges <- function(edges) {
  mtl_sf <- st_as_sf(mtl)
  
  # Extract centroids for each state to use as node coordinates
  mtl_geo_key <- unique(mtl[, c('NOM', "geometry")])
  mtl_unique_center <- st_centroid(mtl_geo_key$geometry)
  
  edges <- edges %>%
    left_join(mtl_geo_key, by = c("from" = "NOM")) %>%
    rename(from_geo = geometry) %>%
    left_join(mtl_geo_key, by = c("to" = "NOM")) %>%
    rename(to_geo = geometry) 
  
  edges_sf <- st_as_sf(edges)
  
  edges_sf$geometry <- mapply(function(from, to) {
    from_coords <- st_coordinates(st_centroid(from))
    to_coords <- st_coordinates(st_centroid(to))
    st_linestring(rbind(from_coords, to_coords))
  }, edges$from_geo, edges$to_geo, SIMPLIFY = FALSE)
  
  #edges_sf <- edges  %>% st_as_sf(coords = c(from_cor, to_cor), crs = 2959)
  edges_sf <- st_as_sfc(edges_sf$geometry, crs = 2959)
  edges_sf <- st_as_sf(edges_sf, crs = 2959)
  
  edges$geometry <- mapply(function(from, to) {
    from_coords <- st_coordinates(st_centroid(from))
    to_coords <- st_coordinates(st_centroid(to))
    st_linestring(rbind(from_coords, to_coords))
  }, edges$from_geo, edges$to_geo, SIMPLIFY = FALSE)
  
  # Function to calculate midpoint of a LINESTRING
  calculate_midpoint <- function(linestring) {
    coords <- st_coordinates(linestring)
    midpoint <- coords[ceiling(nrow(coords) / 2), ]
    return(midpoint)
  }
  
  edges$geometry <- st_as_sfc(edges$geometry, crs = 2959)
  edges_convert_sf <- st_as_sf(edges, sf_column_name = "geometry", crs = 2959)
  # Create a new data frame with midpoints and stops for edges
  edge_midpoints <- edges_convert_sf %>%
    rowwise() %>%
    mutate(midpoint = list(calculate_midpoint(geometry)),
           mid_x = midpoint[1],
           mid_y = midpoint[2]) %>%
    select(stops, mid_x, mid_y)
  
  ggplot() +
    geom_sf(data = mtl) +
    geom_sf(data = edges_sf, color = "blue") +
    geom_sf(data = mtl_unique_center) +
    geom_label_repel(data = edge_midpoints, aes(x = mid_x, y = mid_y, label = stops), 
                     size = 2, color = "red", fill = "white", box.padding = 0.3, point.padding = 0.5)  +
    ggtitle("Network Flow Plot", subtitle = "Edge Weights Representing Stops") +
    #geom_text(data = nodes, aes(label = label), size = size, hjust = 1.5) +
    scale_size_continuous(range = c(0.5, 3)) +
    scale_color_identity() +
    theme_minimal() +
    theme(legend.position = "none")
}

# edges <- select(-from_geo, -to_geo)
# edges_sf$geometry <- as.character(edges_sf$geometry )
# st_geometry(edges_sf) <- edges_sf$geometry
# ggplot() +
#   geom_sf(data = mtl) +
#   geom_sf(data = edges_sf, aes(size = stops)) +
#   geom_sf(data = mtl_unique_center) +
#   ggtitle("Network Flow Plot", subtitle = "Edge Weights Representing Stops") +
#   #geom_text(data = nodes, aes(label = label), size = size, hjust = 1.5) +
#   scale_size_continuous(range = c(0.5, 3)) +
#   scale_color_identity() +
#   theme_minimal() +
#   theme(legend.position = "none")

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###

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
q <- "select * from ca_od_long where province = 'QC' and Y = 2021 and m BETWEEN 1 AND 5;"
#q <- "select * from ca_oD_long limit 30000:400000;"

# SenD query to the Database
routing <- dbGetQuery(conObesity, q)

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

## Get boroughs and related cities of MTL
mtl<-st_read("limites-administratives-agglomeration.shp")
mtl <- st_transform(mtl, 2959)
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
  select(serial_id, source, area, home_area, stops, province, y, m,
         CT_UID_area, CT_UID_home_area, NOM_area, NOM_home_area)

DA_clean_data <- final_Data_b %>% filter(!is.na(NOM_area)) %>% filter(!is.na(NOM_home_area))
DA_clean_data_m1_5 <- DA_clean_data %>%
  group_by(NOM_area, NOM_home_area) %>%   
  summarise(stops = sum(stops), .groups = 'drop') 
max(DA_clean_data_m1_5$stops)

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### For m=1
DA_clean_data_M1 <- DA_clean_data[DA_clean_data$m==1,]
# Group by 'area' and 'home_area' and summarize 'stops'
DA_clean_data_m1 <- DA_clean_data_M1 %>%
  group_by(NOM_area, NOM_home_area) %>%
  summarise(stops = sum(stops),.groups = 'drop')
DA_clean_data_m1 <- DA_clean_data_m1 %>%
  left_join(DA_MTL_Nom_pop_Key, by = c("NOM_area" = "NOM")) %>%
  rename("Population_area" = "Population") 
DA_clean_data_m1 <- DA_clean_data_m1 %>%
  select(NOM_area, NOM_home_area, stops, Population_area)
# Weighted by population_area
DA_clean_data_m1$stops_W <- DA_clean_data_m1$stops/sum(DA_clean_data_m1$Population_area)*DA_clean_data_m1$Population_area
summary(DA_clean_data_m1$stops_W)

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
library(igraph)
library(dplyr)

edges <- unique(DA_clean_data_m1[, c("NOM_area", "NOM_home_area", "stops", "stops_W")]) 
nodes <- mtl %>% select(NOM)

network <- graph_from_data_frame(edges, nodes, directed = TRUE)

net <- simplify(network, remove.multiple = F, remove.loops = T) 

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
#Reduce Edge Density
# Filter edges based on a weight threshold
edge_threshold <- 0.1  # Set a threshold for edge weight(First 25%)
filtered_edges <- E(net)[stops_W > edge_threshold] # only plot stops > 5

# Create a subgraph with only the filtered edges
net_filtered <- subgraph.edges(net, filtered_edges, delete.vertices = FALSE)

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
# Plot the filtered graph
set.seed(123)
# Highlight nodes with high degrees or other centrality measures.
# Calculate degree centrality
degree_centrality <- degree(net_filtered)
V(net_filtered)$size <- degree_centrality / max(degree_centrality) * 10 + 5  # Scale node size
V(net_filtered)$color <- "lightblue"

#Represent the weight of edges with varying widths and colors.
plot(net_filtered, 
     vertex.label = NA, 
     edge.width = E(net_filtered)$stops / max(E(net_filtered)$stops) * 5,  # Scale edge width
     edge.color = adjustcolor("grey", alpha.f = 0.5),# Set edge color with transparency
     edge.arrow.size=.2, 
     vertex.size=V(net_filtered)$size, vertex.color = V(net_filtered)$color,
     edge.curved=.1, 
     layout = layout_with_fr(net_filtered))  # Fruchterman-Reingold layout)  
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
# Using visNetwork for interactive plotting
library(visNetwork)

# Convert igraph to visNetwork
vis_data <- toVisNetworkData(net_filtered)
vis_data <- toVisNetworkData(net)

# Create interactive network
visNetwork(vis_data$nodes, vis_data$edges) %>%
  visEdges(scaling = list(min = 2, max = 10)) %>%
  visNodes(scaling = list(min = 10, max = 30)) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
# plot(net_filtered, vertex.label = V(net_filtered)$name, edge.arrow.size=.2, vertex.size=2, edge.curved=.1)
# plot(net_filtered, vertex.label = NA, edge.arrow.size=.2, vertex.size=2, edge.curved=.1)

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### For m=2
DA_clean_data_M2 <- DA_clean_data[DA_clean_data$m==2,]
# Group by 'area' and 'home_area' and summarize 'stops'
DA_clean_data_m2 <- DA_clean_data_M2 %>%
  group_by(NOM_area, NOM_home_area) %>%
  summarise(stops = sum(stops),.groups = 'drop')
DA_clean_data_m2 <- DA_clean_data_m2 %>%
  left_join(DA_MTL_Nom_pop_Key, by = c("NOM_area" = "NOM")) %>%
  rename("Population_area" = "Population") 
DA_clean_data_m2 <- DA_clean_data_m2 %>%
  select(NOM_area, NOM_home_area, stops, Population_area)
# Weighted by population_area
DA_clean_data_m2$stops_W <- DA_clean_data_m2$stops/sum(DA_clean_data_m2$Population_area)*DA_clean_data_m2$Population_area
summary(DA_clean_data_m2$stops_W)

edges <- unique(DA_clean_data_m2[, c("NOM_area", "NOM_home_area", "stops", "stops_W")]) 
nodes <- mtl %>% select(NOM)

network <- graph_from_data_frame(edges, nodes, directed = TRUE)

net <- simplify(network, remove.multiple = F, remove.loops = T) 

#Reduce Edge Density
# Filter edges based on a weight threshold
edge_threshold <- 0.1  # Set a threshold for edge weight(First 25%)
filtered_edges <- E(net)[stops_W > edge_threshold] # only plot stops > 5

# Create a subgraph with only the filtered edges
net_filtered <- subgraph.edges(net, filtered_edges, delete.vertices = FALSE)

# Plot the filtered graph
# Convert igraph to visNetwork
vis_data <- toVisNetworkData(net_filtered)

# Create interactive network
visNetwork(vis_data$nodes, vis_data$edges) %>%
  visEdges(scaling = list(min = 2, max = 10)) %>%
  visNodes(scaling = list(min = 10, max = 30)) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### For m=3
DA_clean_data_M3 <- DA_clean_data[DA_clean_data$m==3,]
# Group by 'area' and 'home_area' and summarize 'stops'
DA_clean_data_m3 <- DA_clean_data_M3 %>%
  group_by(NOM_area, NOM_home_area) %>%
  summarise(stops = sum(stops),.groups = 'drop')
DA_clean_data_m3 <- DA_clean_data_m3 %>%
  left_join(DA_MTL_Nom_pop_Key, by = c("NOM_area" = "NOM")) %>%
  rename("Population_area" = "Population") 
DA_clean_data_m3 <- DA_clean_data_m3 %>%
  select(NOM_area, NOM_home_area, stops, Population_area)
# Weighted by population_area
DA_clean_data_m3$stops_W <- DA_clean_data_m3$stops/sum(DA_clean_data_m3$Population_area)*DA_clean_data_m3$Population_area
summary(DA_clean_data_m3$stops_W)

edges <- unique(DA_clean_data_m3[, c("NOM_area", "NOM_home_area", "stops", "stops_W")]) 
nodes <- mtl %>% select(NOM)

network <- graph_from_data_frame(edges, nodes, directed = TRUE)

net <- simplify(network, remove.multiple = F, remove.loops = T) 

#Reduce Edge Density
# Filter edges based on a weight threshold
edge_threshold <- 0.1  # Set a threshold for edge weight(First 25%)
filtered_edges <- E(net)[stops_W > edge_threshold] # only plot stops > 5

# Create a subgraph with only the filtered edges
net_filtered <- subgraph.edges(net, filtered_edges, delete.vertices = FALSE)

# Plot the filtered graph
# Using visNetwork for interactive plotting
# Convert igraph to visNetwork
vis_data <- toVisNetworkData(net_filtered)
#vis_data <- toVisNetworkData(network)

# Create interactive network
visNetwork(vis_data$nodes, vis_data$edges) %>%
  visEdges(scaling = list(min = 2, max = 10)) %>%
  visNodes(scaling = list(min = 10, max = 30)) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### For m=4
DA_clean_data_M4 <- DA_clean_data[DA_clean_data$m==4,]
# Group by 'area' and 'home_area' and summarize 'stops'
DA_clean_data_m4 <- DA_clean_data_M4 %>%
  group_by(NOM_area, NOM_home_area) %>%
  summarise(stops = sum(stops),.groups = 'drop')
DA_clean_data_m4 <- DA_clean_data_m4 %>%
  left_join(DA_MTL_Nom_pop_Key, by = c("NOM_area" = "NOM")) %>%
  rename("Population_area" = "Population") 
DA_clean_data_m4 <- DA_clean_data_m4 %>%
  select(NOM_area, NOM_home_area, stops, Population_area)
# Weighted by population_area
DA_clean_data_m4$stops_W <- DA_clean_data_m4$stops/sum(DA_clean_data_m4$Population_area)*DA_clean_data_m4$Population_area
summary(DA_clean_data_m4$stops_W)

edges <- unique(DA_clean_data_m4[, c("NOM_area", "NOM_home_area", "stops", "stops_W")]) 
nodes <- mtl %>% select(NOM)

network <- graph_from_data_frame(edges, nodes, directed = TRUE)

net <- simplify(network, remove.multiple = F, remove.loops = T) 

#Reduce Edge Density
# Filter edges based on a weight threshold
edge_threshold <- 0.1  # Set a threshold for edge weight(First 25%)
filtered_edges <- E(net)[stops_W > edge_threshold] # only plot stops > 5

# Create a subgraph with only the filtered edges
net_filtered <- subgraph.edges(net, filtered_edges, delete.vertices = FALSE)

# Plot the filtered graph
# Using visNetwork for interactive plotting
# Convert igraph to visNetwork
vis_data <- toVisNetworkData(net_filtered)

# Create interactive network
visNetwork(vis_data$nodes, vis_data$edges) %>%
  visEdges(scaling = list(min = 2, max = 10)) %>%
  visNodes(scaling = list(min = 10, max = 30)) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### For m=5
DA_clean_data_M5 <- DA_clean_data[DA_clean_data$m==5,]
# Group by 'area' and 'home_area' and summarize 'stops'
DA_clean_data_m5 <- DA_clean_data_M5 %>%
  group_by(NOM_area, NOM_home_area) %>%
  summarise(stops = sum(stops),.groups = 'drop')
DA_clean_data_m5 <- DA_clean_data_m5 %>%
  left_join(DA_MTL_Nom_pop_Key, by = c("NOM_area" = "NOM")) %>%
  rename("Population_area" = "Population") 
DA_clean_data_m5 <- DA_clean_data_m5 %>%
  select(NOM_area, NOM_home_area, stops, Population_area)
# Weighted by population_area
DA_clean_data_m5$stops_W <- DA_clean_data_m5$stops/sum(DA_clean_data_m5$Population_area)*DA_clean_data_m5$Population_area
summary(DA_clean_data_m5$stops_W)

edges <- unique(DA_clean_data_m5[, c("NOM_area", "NOM_home_area", "stops", "stops_W")]) 
nodes <- mtl %>% select(NOM)

network <- graph_from_data_frame(edges, nodes, directed = TRUE)

net <- simplify(network, remove.multiple = F, remove.loops = T) 

#Reduce Edge Density
# Filter edges based on a weight threshold
edge_threshold <- 0.1  # Set a threshold for edge weight(First 50%)
filtered_edges <- E(net)[stops_W > edge_threshold] # only plot stops > 5

# Create a subgraph with only the filtered edges
net_filtered <- subgraph.edges(net, filtered_edges, delete.vertices = FALSE)

# Plot the filtered graph
# Using visNetwork for interactive plotting
# Convert igraph to visNetwork
vis_data <- toVisNetworkData(net_filtered)

# Create interactive network
visNetwork(vis_data$nodes, vis_data$edges) %>%
  visEdges(scaling = list(min = 2, max = 10)) %>%
  visNodes(scaling = list(min = 10, max = 30)) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### For m=1-5
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

edges <- unique(DA_clean_data_m1_5[, c("NOM_area", "NOM_home_area", "stops", "stops_W")]) 
nodes <- mtl %>% select(NOM)

network <- graph_from_data_frame(edges, nodes, directed = TRUE)

net <- simplify(network, remove.multiple = F, remove.loops = T) 

#Reduce Edge Density
# Filter edges based on a weight threshold
edge_threshold <- 0.2  # Set a threshold for edge weight(First 25%)
filtered_edges <- E(net)[stops_W > edge_threshold] # only plot stops > 5

# Create a subgraph with only the filtered edges
net_filtered <- subgraph.edges(net, filtered_edges, delete.vertices = FALSE)

# Plot the filtered graph
# Using visNetwork for interactive plotting
# Convert igraph to visNetwork
vis_data <- toVisNetworkData(net_filtered)

# Create interactive network
visNetwork(vis_data$nodes, vis_data$edges) %>%
  visEdges(scaling = list(min = 2, max = 10)) %>%
  visNodes(scaling = list(min = 10, max = 30)) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
## Add loop to Node_attributes
### for month 1-5
DA_clean_data_m1_5_loop <- DA_clean_data_m1_5 %>% filter(NOM_area == NOM_home_area) 
# new_row <- data.frame( NOM_area = "L'Île-Dorval",
#                        NOM_home_area = "L'Île-Dorval",
#                        stops = 0,
#                        Population_area=0,
#                        stops_W=0) # L'Île-Dorval has 0
# DA_clean_data_m1_5_loop <- rbind(DA_clean_data_m1_5_loop, new_row)

edges <- unique(DA_clean_data_m1_5[, c("NOM_area", "NOM_home_area", "stops", "stops_W")]) 
nodes <- DA_clean_data_m1_5_loop %>% select(NOM_area, stops)
summary(DA_clean_data_m1_5$stops_W)

network <- graph_from_data_frame(edges, nodes, directed = TRUE)
#V(network)$stops
#E(network)

net <- simplify(network, remove.multiple = F, remove.loops = T) 

#Reduce Edge Density
# Filter edges based on a weight threshold
edge_threshold <- 0.2  # Set a threshold for edge weight(First 25%)
filtered_edges <- E(net)[stops_W > edge_threshold] # only plot stops > 5

# Create a subgraph with only the filtered edges
net_filtered <- subgraph.edges(net, filtered_edges, delete.vertices = FALSE)

# Plot the filtered graph
# Using visNetwork for interactive plotting
# Convert igraph to visNetwork
vis_data <- toVisNetworkData(net_filtered)
nodes <- vis_data$nodes
edges <- vis_data$edges

# Create interactive network
# Add stops to the label
nodes$label <- paste(nodes$label, nodes$stops, sep = ": Stops=")
length(nodes$stops)
visNetwork(nodes, edges) %>%
  visEdges(scaling = list(min = 2, max = 10)) %>%
  visNodes(scaling = list(min = 10, max = 30)) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

map_nodes(edges,nodes)
#map_edges(edges)

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### For m=1 Add loop to Node_attributes
DA_clean_data_m1_loop <- DA_clean_data_m1 %>% filter(NOM_area == NOM_home_area) 
# new_row <- data.frame( NOM_area = "L'Île-Dorval",
#                        NOM_home_area = "L'Île-Dorval",
#                        stops = 0,
#                        Population_area=0,
#                        stops_W=0) # L'Île-Dorval has 0
# DA_clean_data_m1_loop <- rbind(DA_clean_data_m1_loop, new_row)

edges <- unique(DA_clean_data_m1[, c("NOM_area", "NOM_home_area", "stops", "stops_W")]) 
nodes <- DA_clean_data_m1_loop %>% select(NOM_area, stops)
summary(DA_clean_data_m1$stops_W)

network <- graph_from_data_frame(edges, nodes, directed = TRUE)
#V(network)$stops
#E(network)

net <- simplify(network, remove.multiple = F, remove.loops = T) 

#Reduce Edge Density
# Filter edges based on a weight threshold
edge_threshold <- 0.1  # Set a threshold for edge weight(First 25%)
filtered_edges <- E(net)[stops_W > edge_threshold] # only plot stops > 5

# Create a subgraph with only the filtered edges
net_filtered <- subgraph.edges(net, filtered_edges, delete.vertices = FALSE)

# Plot the filtered graph
# Using visNetwork for interactive plotting
# Convert igraph to visNetwork
vis_data <- toVisNetworkData(net_filtered)
nodes <- vis_data$nodes
edges <- vis_data$edges
# Create interactive network

# Add stops to the label
nodes$label <- paste(nodes$label, nodes$stops, sep = ": Stops=")
length(nodes$stops)
visNetwork(nodes, edges) %>%
  visEdges(scaling = list(min = 2, max = 10)) %>%
  visNodes(scaling = list(min = 10, max = 30)) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

map_nodes(edges,nodes)
#map_edges(edges)

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### For m=2 Add loop to Node_attributes
DA_clean_data_m2_loop <- DA_clean_data_m2 %>% filter(NOM_area == NOM_home_area) 

edges <- unique(DA_clean_data_m2[, c("NOM_area", "NOM_home_area", "stops", "stops_W")]) 
nodes <- DA_clean_data_m2_loop %>% select(NOM_area, stops)
summary(DA_clean_data_m2$stops_W)

network <- graph_from_data_frame(edges, nodes, directed = TRUE)
#V(network)$stops
#E(network)

net <- simplify(network, remove.multiple = F, remove.loops = T) 

#Reduce Edge Density
# Filter edges based on a weight threshold
edge_threshold <- 0.1  # Set a threshold for edge weight(First 25%)
filtered_edges <- E(net)[stops_W > edge_threshold] # only plot stops > 5

# Create a subgraph with only the filtered edges
net_filtered <- subgraph.edges(net, filtered_edges, delete.vertices = FALSE)

# Plot the filtered graph
# Convert igraph to visNetwork
vis_data <- toVisNetworkData(net_filtered)
nodes <- vis_data$nodes
edges <- vis_data$edges

# Add stops to the label
nodes$label <- paste(nodes$label, nodes$stops, sep = ": Stops=")
length(nodes$stops)
visNetwork(nodes, edges) %>%
  visEdges(scaling = list(min = 2, max = 10)) %>%
  visNodes(scaling = list(min = 10, max = 30)) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

map_nodes(edges,nodes)
#map_edges(edges)

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### For m=3 Add loop to Node_attributes
DA_clean_data_m3_loop <- DA_clean_data_m3 %>% filter(NOM_area == NOM_home_area) 

edges <- unique(DA_clean_data_m3[, c("NOM_area", "NOM_home_area", "stops", "stops_W")]) 
nodes <- DA_clean_data_m3_loop %>% select(NOM_area, stops)
summary(DA_clean_data_m3$stops_W)

network <- graph_from_data_frame(edges, nodes, directed = TRUE)
#V(network)$stops
#E(network)

net <- simplify(network, remove.multiple = F, remove.loops = T) 

#Reduce Edge Density
# Filter edges based on a weight threshold
edge_threshold <- 0.48  # Set a threshold for edge weight(First 25%)
filtered_edges <- E(net)[stops_W > edge_threshold] # only plot stops > 5

# Create a subgraph with only the filtered edges
net_filtered <- subgraph.edges(net, filtered_edges, delete.vertices = FALSE)

# Plot the filtered graph
# Convert igraph to visNetwork
vis_data <- toVisNetworkData(net_filtered)
nodes <- vis_data$nodes
edges <- vis_data$edges
# Create interactive network

# Add stops to the label
nodes$label <- paste(nodes$label, nodes$stops, sep = ": Stops=")
length(nodes$stops)
visNetwork(nodes, edges) %>%
  visEdges(scaling = list(min = 2, max = 10)) %>%
  visNodes(scaling = list(min = 10, max = 30)) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

map_nodes(edges,nodes)
#map_edges(edges)

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### For m=4 Add loop to Node_attributes
DA_clean_data_m4_loop <- DA_clean_data_m4 %>% filter(NOM_area == NOM_home_area) 

edges <- unique(DA_clean_data_m4[, c("NOM_area", "NOM_home_area", "stops", "stops_W")]) 
nodes <- DA_clean_data_m4_loop %>% select(NOM_area, stops)
summary(DA_clean_data_m4$stops_W)

network <- graph_from_data_frame(edges, nodes, directed = TRUE)
#V(network)$stops
#E(network)

net <- simplify(network, remove.multiple = F, remove.loops = T) 

#Reduce Edge Density
# Filter edges based on a weight threshold
edge_threshold <- 0.1  # Set a threshold for edge weight(First 25%)
filtered_edges <- E(net)[stops_W > edge_threshold] # only plot stops > 5

# Create a subgraph with only the filtered edges
net_filtered <- subgraph.edges(net, filtered_edges, delete.vertices = FALSE)

# Plot the filtered graph
# Convert igraph to visNetwork
vis_data <- toVisNetworkData(net_filtered)
nodes <- vis_data$nodes
edges <- vis_data$edges

# Add stops to the label
nodes$label <- paste(nodes$label, nodes$stops, sep = ": Stops=")
length(nodes$stops)
visNetwork(nodes, edges) %>%
  visEdges(scaling = list(min = 2, max = 10)) %>%
  visNodes(scaling = list(min = 10, max = 30)) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

map_nodes(edges,nodes)
#map_edges(edges)

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### For m=5 Add loop to Node_attributes
DA_clean_data_m5_loop <- DA_clean_data_m5 %>% filter(NOM_area == NOM_home_area) 

edges <- unique(DA_clean_data_m5[, c("NOM_area", "NOM_home_area", "stops", "stops_W")]) 
nodes <- DA_clean_data_m5_loop %>% select(NOM_area, stops)
summary(DA_clean_data_m5$stops_W)

network <- graph_from_data_frame(edges, nodes, directed = TRUE)
#V(network)$stops
#E(network)

net <- simplify(network, remove.multiple = F, remove.loops = T) 

#Reduce Edge Density
# Filter edges based on a weight threshold
edge_threshold <- 0.1  # Set a threshold for edge weight(First 25%)
filtered_edges <- E(net)[stops_W > edge_threshold] # only plot stops > 5

# Create a subgraph with only the filtered edges
net_filtered <- subgraph.edges(net, filtered_edges, delete.vertices = FALSE)

# Plot the filtered graph
# Convert igraph to visNetwork
vis_data <- toVisNetworkData(net_filtered)
nodes <- vis_data$nodes
edges <- vis_data$edges

# Add stops to the label
nodes$label <- paste(nodes$label, nodes$stops, sep = ": Stops=")
length(nodes$stops)
visNetwork(nodes, edges) %>%
  visEdges(scaling = list(min = 2, max = 10)) %>%
  visNodes(scaling = list(min = 10, max = 30)) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

map_nodes(edges,nodes)
#map_edges(edges)