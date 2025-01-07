## Connect to Data
library(rpostgis)
library(RPostgreSQL)
library(RPostgres)

# Create an object that has connection to remote Database whose aDDress is 132.... anD name of Db is safegraph
conObesity <- RPostgreSQL::dbConnect("PostgreSQL", host = "132.216.183.3",
                                     dbname = "safegraph", user = "*",
                                     password = "*")

# Write query to get Data - Data are too big, so DownloaD the first 100,000 only. I will later show how to DownloaD quebec Data only.  
#q <- "select * from ca_od_long where province = 'QC';"
q <- "select * from ca_od_long where province = 'ON' and Y = 2020 and m BETWEEN 3 AND 5;;"
#q <- "select * from ca_od_long where province = 'QC' and Y = 2022 ;"
#q <- "select * from ca_oD_long limit 30000:400000;"

# SenD query to the Database
routing <- dbGetQuery(conObesity, q)
routing_selected_Trt <- routing[grepl("535$", routing$row.names), ]

length(unique(routing_selected_Trt$area))
length(unique(routing_selected_Trt$home_area))


# FSA data 
library(spdep)
library(sf)
library(dplyr)

fsa <- st_read("data/lfsa000b21a_e/lfsa000b21a_e.shp")
head(fsa)

fsa <- st_transform(fsa, 2952)

# Filter rows where CFSAUID starts with 'M'
fsa <- fsa[grep("^M", fsa$CFSAUID), ]# Assuming your data frame is named df
fsa$CFSAUID[fsa$CFSAUID == "M5H"] <- "M5G"
fsa$CFSAUID[fsa$CFSAUID == "M5C"] <- "M5B"
fsa <- fsa %>%
  group_by(CFSAUID) %>%
  summarize(
    LANDAREA = sum(LANDAREA),  # Sum the land areas or other relevant attributes
    geometry = st_union(geometry)  # Merge (union) the geometries
  )


## Get DA & CT of mtl
library("devtools")
library(dplyr)
library(tmap)
library(sf)
library(cancensus)

options(cancensus.api_key = "CensusMapper_91294cf61b6144bca0fcac3333a180b6")
options(cancensus.cache_path = "./")

DA <- get_census(dataset='CA21', 
                 regions=list(CMA = "35 535"),
                 level='DA', use_cache = FALSE, geo_format = "sf", quiet = TRUE, 
                 api_key = Sys.getenv("CM_API_KEY")) %>%
  st_transform(crs = 2952)
DA$area <-st_area(DA) # area for each DA
fsa$area_fsa <-st_area(fsa)
overlapDA_FSA <- st_intersection(DA, fsa) # map the corresponding DAs to each fsa
overlapDA_FSA$areaOverlap <- st_area(overlapDA_FSA) # area for the overlap part of DA on FSA
# overlapDA_FSA gives for each FSA, corresponding DA and proportional area in that FSA
overlapDA_FSA$prop_DAOverlap <- overlapDA_FSA$areaOverlap/overlapDA_FSA$area_fsa ## can group by FSA to get weughted pop for each fsa
overlapDA_FSA$prop_pop <- round(overlapDA_FSA$Population*overlapDA_FSA$prop_DAOverlap,0)
overlapDA_FSA$prop_pop <- as.numeric(overlapDA_FSA$prop_pop)
overlapDA_FSA_map <- overlapDA_FSA %>% 
                     select('GeoUID', "CFSAUID", "prop_pop", "prop_DAOverlap") 

A <- overlapDA_FSA_map %>% 
    group_by(GeoUID) %>% 
     summarise(Sum = sum(prop_DAOverlap), .groups = 'drop') 

B <- overlapDA_FSA_map %>% 
  group_by(CFSAUID) %>% 
  summarise(Sum = sum(prop_DAOverlap), .groups = 'drop') # Expect to be 1

library(ggplot2)
ggplot() +
  geom_sf(data=overlapDA_FSA, fill = NA) +
  geom_sf(data=fsa, color = "red", fill = NA) +
  theme_void() +
  labs(title = "Spatial Data Plot", caption = "Polygon and Multipolygon Geometries")

plot(st_geometry(DA))

DA_FSA_Key <- overlapDA_FSA[, c('GeoUID', "CFSAUID", "prop_pop", "prop_DAOverlap")] %>% 
              st_drop_geometry() %>%
              filter(!is.na(CFSAUID)) 
 
DA_FSA_Nom_pop_Key <- DA_FSA_Key[, c('CFSAUID', "prop_pop")] %>%
  group_by(CFSAUID) %>%   
  summarise(Prop_pop = sum(prop_pop), .groups = 'drop')

length(unique(DA_FSA_Key$GeoUID))

routing$area <- as.character(routing$area)
routing$home_area <- as.character(routing$home_area)

joined_Data_b <- routing %>%
  left_join(DA_FSA_Key, by = c("area" = "GeoUID")) %>%
  rename("CFSAUID_area" = "CFSAUID") 

final_Data_b <- joined_Data_b %>%
  left_join(DA_FSA_Key, by = c("home_area" = "GeoUID")) %>%
  rename("CFSAUID_home_area" = "CFSAUID") 

final_Data_b <- final_Data_b %>%
  select(row.names, source, area, home_area, stops, province, y, m,
         CFSAUID_area, CFSAUID_home_area)

#mobility of areas only for fsa
DA_clean_data <- final_Data_b %>% filter(!is.na(CFSAUID_area)) %>% filter(!is.na(CFSAUID_home_area))

fsa_clean_data_m_all <- DA_clean_data %>%
  group_by(CFSAUID_area, CFSAUID_home_area) %>%   
  summarise(stops = sum(stops), .groups = 'drop') #weight/CFSAUID_area

d <- DA_clean_data %>% group_by(area) %>% summarise(sum_stop = sum(stops))
d
hist(log(d$sum_stop))

fsa_clean_data_m_all <- fsa_clean_data_m_all %>%
  left_join(DA_FSA_Nom_pop_Key, by = c("CFSAUID_home_area" = "CFSAUID")) %>%
  rename("Population_home_area" = "Prop_pop") 

fsa_clean_data_m_all <- fsa_clean_data_m_all %>%
  select(CFSAUID_area, CFSAUID_home_area, stops, Population_home_area)

# Weighted by population_area
fsa_clean_data_m_all$stops_W <- fsa_clean_data_m_all$stops/sum(fsa_clean_data_m_all$Population_home_area)*fsa_clean_data_m_all$Population_home_area
summary(fsa_clean_data_m_all$stops_W)
write.csv(fsa_clean_data_m_all, "fsa_clean_data_m_all.csv")

fsa_clean_data_m_all_loop <- fsa_clean_data_m_all %>%
  filter(CFSAUID_area == CFSAUID_home_area) %>%
  rename("CFSAUID"="CFSAUID_area")  %>% 
  select(CFSAUID, stops, stops_W)

library(igraph)
library(dplyr)
edges <- fsa_clean_data_m_all[, c("CFSAUID_area", "CFSAUID_home_area", "stops", "stops_W","Population_home_area")]
sum(edges$stops==0)
sum(edges$Population_home_area==0)
nodes <- fsa_clean_data_m_all_loop 
#hist(edges$CFSAUID_area,log(edges$stops)) # hist for stops
# match M5H with M5G
# No many neborhoods, cant borrow prior information form neighbor, then match this outliers with others

network <- graph_from_data_frame(edges, nodes, directed = F)
adjacency_matrix <- as.matrix(as_adjacency_matrix(network, attr="stops"))
#0/1, 10%
adjacency_matrix <- as.matrix(as_adjacency_matrix(net_filtered, attr="stops"))
adjacency_matrix[adjacency_matrix > 0] <- 1
adjacency_matrix

network_D <- graph_from_data_frame(edges, nodes, directed = T)
adj_matrix_Directed <- as.matrix(as_adjacency_matrix(network_D, attr="stops"))

adj_matrix_Directed 

col_sums <- log(colSums(adj_matrix_Directed)) # Stops arrived at this FSA

# Plot histogram
barplot(col_sums, 
     main = "Total Stops departure from each FSA area", 
     xlab = "FSA", 
     ylab = "Log Stops", 
     col = "grey", 
     border = "white")

hist(col_sums, 
     main = "Log Distribution of Total Stops Destined for Each FSA Area", 
     sub = "March to May 2020", 
     xlab = "Logarithmic Scale of Total Stops",
     border = "black",  # Define borders for clarity
     cex.main = 1.2,    # Increase title size
     cex.lab = 1.1,     # Increase axis label size
     cex.sub = 0.9)     # Decrease subtitle size

fsa_clean_data_m_all_loop$Total_stops <- colSums(adj_matrix_Directed)
plot_case$Total_stops <- log(colSums(adj_matrix_Directed))
ggplot(data = plot_case) +
  geom_sf(aes(fill = Total_stops, geometry = geometry), color="black")  +
  scale_fill_gradient(low = "grey10", high = "grey90") +
  labs(title = "Log of Total Stops Destined for Each FSA Area",
       subtitle = "March to May 2020",
       fill = "Log of Stops") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10))
  ) +
  theme_void()+
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 10, b = 10)), # Adds top and bottom margin to the title
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(t = 5, b = 10)),              # Adds margin to the subtitle
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.margin = margin(20, 20, 20, 20)  # Adds extra space around the entire plot
  )

diag(adj_matrix_Directed) <- 0
adj_matrix_Directed_wo_loop <- adj_matrix_Directed
row_sums_wo_loop <- log(rowSums(adj_matrix_Directed_wo_loop))

# Plot histogram
barplot(row_sums_wo_loop, 
        main = "Total Stops departure from each FSA area to other FSA areas", 
        xlab = "FSA", 
        ylab = "Log (Stops between areas)", 
        col = "grey", 
        border = "white")

## graph
library(igraph)
library(visNetwork)
net <- simplify(network, remove.multiple = F, remove.loops = T) 

#Reduce Edge Density
# Filter edges based on a weight threshold
#edge_threshold <- 0.06  # Set a threshold for edge weight(First 10%)
edge_threshold <- 0.14
filtered_edges <- E(net)[stops_W > edge_threshold] # only plot stops >
#filtered_edges <- E(net)[E(net)$stops_W > edge_threshold]

# Create a subgraph with only the filtered edges
net_filtered <- subgraph.edges(net, filtered_edges, delete.vertices = FALSE)

# Plot the filtered graph

set.seed(123)
# Highlight nodes with high degrees or other centrality measures.
# Calculate degree centrality
degree_centrality <- degree(net_filtered)
V(net_filtered)$size <- degree_centrality / max(degree_centrality) * 10 + 5  # Scale node size
V(net_filtered)$color <- "lightblue"

# Using visNetwork for interactive plotting

# Convert igraph to visNetwork
vis_data <- toVisNetworkData(net_filtered)
nodes <- vis_data$nodes
edges <- vis_data$edges
# Create interactive network

# Add stops to the label
#nodes$label <- paste(nodes$label, nodes$stops, sep = ": Stops=")
nodes$label <- paste(nodes$stops)
length(nodes$stops)
# visNetwork(nodes, edges) %>%
#   visEdges(scaling = list(min = 2, max = 10)) %>%
#   visNodes(scaling = list(min = 10, max = 30)) %>%
#   visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

map_nodes <- function(edges,nodes) {
 
  # Extract centroids for each state to use as node coordinates
  fsa_geo_key <- unique(fsa[, c('CFSAUID', "geometry")])
  
  nodes <- nodes  %>%
    left_join(fsa_geo_key, by = c("id" = "CFSAUID")) 
  nodes_sf <- st_as_sf(nodes)
  nodes$geometry <- st_centroid(nodes$geometry)
  
  edges <- edges %>%
    left_join(fsa_geo_key, by = c("from" = "CFSAUID")) %>%
    rename(from_geo = geometry) %>%
    left_join(fsa_geo_key, by = c("to" = "CFSAUID")) %>%
    rename(to_geo = geometry) 
  
  edges_sf <- st_as_sf(edges)
  
  edges_sf$geometry <- mapply(function(from, to) {
    from_coords <- st_coordinates(st_centroid(from))
    to_coords <- st_coordinates(st_centroid(to))
    st_linestring(rbind(from_coords, to_coords))
  }, edges$from_geo, edges$to_geo, SIMPLIFY = FALSE)
  
  #edges_sf <- edges  %>% st_as_sf(coords = c(from_cor, to_cor), crs = 2959)
  edges_sf <- st_as_sfc(edges_sf$geometry, crs = 2952)
  edges_sf <- st_as_sf(edges_sf, crs = 2952)
  
  ggplot() +
    geom_sf(data = fsa) +
    geom_sf(data = edges_sf, color = "blue") +
    geom_sf(data = nodes$geometry) +
    geom_text(data = nodes, aes(x = st_coordinates(geometry)[,1], 
                                y = st_coordinates(geometry)[,2], 
                                label = label), size = 3, vjust = -1) +
    ggtitle("Top 5 % Network Flow Plot", subtitle = "March to May 2020") +
    labs(caption = "Note: Edges outside Toronto were excluded, treating Toronto as a separate graph from the rest of Canada.") + 
    #geom_text(data = nodes, aes(label = label), size = size, hjust = 1.5) +
    scale_size_continuous(range = c(0.5, 3)) +
    scale_color_identity() +
    theme_void() +
    theme(legend.position = "none")+
    theme(
      legend.position = "none",
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 10, b = 10)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(t = 5, b = 10)),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      plot.caption = element_text(size = 9, hjust = 0.5, vjust = 0, margin = margin(t = 10)),
      plot.margin = margin(20, 20, 40, 20)  # Adjust bottom margin for caption space
    )
}

map_nodes(edges,nodes)

binary_matrix_trafic <- ifelse(adjacency_matrix > 0, 1, 0)

# continuity

library(spdep)
nb <- spdep::poly2nb(fsa, queen = TRUE)
head(nb) # For nb[[i]], it will list the indices of the polygons that are adjacent to the i-th polygon.
# plot(st_geometry(mtl), border = "lightgray")
# plot.nb(nb, st_geometry(mtl), add = TRUE)
# nb

# Convert neighborhood list to spatial weights matrix
weights_matrix <- spdep::nb2mat(nb, style = "B", zero.policy = TRUE)

colnames(weights_matrix) <- rownames(weights_matrix)

# Order the row and column names
ordered_row_names <- rownames(weights_matrix)[order(as.numeric(rownames(weights_matrix)))]
ordered_col_names <- ordered_row_names

# Reorder the matrix
weights_matrix <- weights_matrix[ordered_row_names, ordered_col_names]


weights_matrix
