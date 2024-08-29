# Purpose: This script provides the dataset for the spatial model.

library(sf)
library(dplyr)
pos_28 <- which(mtl$CODEID == 28)
pos_34 <- which(mtl$CODEID == 34)

# Step 3: Switch the entries
mtl$CODEID[pos_28] <- 34
mtl$CODEID[pos_34] <- 28

mtl <- mtl[order(mtl$CODEID), ]

last_week_2021_new_cases_total$NOM <- row.names(last_week_2021_new_cases_total)

# Convert one sf object to a data frame
new_E <- as.data.frame(new_E)

# Perform the merge
new_E <- new_E %>% 
  mutate(Expected_count_for_each_boro = ifelse(Expected_count_for_each_boro == 0, 1, Expected_count_for_each_boro))
merged_df <- merge(new_E, Predictor_var, by = "NOM")
merged_df <- merge(merged_df, last_week_2021_new_cases_total, by = "NOM") #traffic based
#merged_df <- merge(merged_df, last_week_2021_new_cases_total_34, by = "NOM") #Contiguity based
merged_df <- merge(merged_df, mtl[, 1:2], by = "NOM")
merged_df <- merge(merged_df,DA_clean_data_m1_5_loop, by = "NOM")

#missing_in_df1 <- setdiff(last_week_2021_new_cases_total$NOM, merged_df$NOM)
# Convert the merged data frame back to an sf object
#merged_sf <- st_as_sf(merged_df, sf_column_name = "geometry")

# Display the merged sf object
#print(merged_sf)

data <- merged_df

# Reorder the data frame by the CODEID column
my_data <- data[order(data$CODEID), ]

my_data

# Components:
#   y: Counts of disease event in 34 different regions. (last_week_2021_new_cases_total)
#   E: Expected counts (Expected_count_for_each_boro).
#   x: Predictor variables.(Predictor_var)
