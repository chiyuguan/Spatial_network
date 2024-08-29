library("devtools")
library(dplyr)
library(tmap)
library(sf)
library(cancensus)
## Get boroughs and related cities of MTL
mtl<-st_read("limites-administratives-agglomeration.shp")
plot(mtl)
class(mtl)
st_crs(mtl)
mtl <- st_transform(mtl, 2959)
st_crs(mtl)

disease_data_mtl_Boro <- read.csv("data_mtl_boroughs.csv")

# Identify columns that contain 'new_cases'
new_cases_columns <- grep("new_cases", colnames(disease_data_mtl_Boro), value = TRUE)

# Convert the 'date' column to Date type
disease_data_mtl_Boro$borough <- as.Date(disease_data_mtl_Boro$borough, format="%Y-%m-%d")

# Filter the data for the last week of 2021
last_week_2021 <- disease_data_mtl_Boro %>% filter(borough >= as.Date("2020-03-30") & borough <= as.Date("2020-04-06"))
#last_week_2021 <- disease_data_mtl_Boro %>% filter(borough >= as.Date("2021-1-1") & borough <= as.Date("2021-12-31"))

# Select columns that contain "new_cases" in their names
new_cases_columns <- grep("4", names(last_week_2021), value = TRUE)

# Select the new_cases columns along with borough and date
last_week_2021_new_cases <- last_week_2021 %>% select(borough, all_of(new_cases_columns))

# total new cases for 2021-12-20 to 2021-12-31
last_week_2021_new_cases_total <- last_week_2021_new_cases[-1]

# Convert all columns to numeric
last_week_2021_new_cases_total[] <- lapply(last_week_2021_new_cases_total, function(x) as.numeric(as.character(x)))

last_week_2021_new_cases_total <- colSums(last_week_2021_new_cases_total, na.rm = TRUE)

last_week_2021_new_cases_total <- as.data.frame(last_week_2021_new_cases_total )

# Assuming your dataframe is named df
row.names(last_week_2021_new_cases_total) <- gsub(".4", "", row.names(last_week_2021_new_cases_total))
row.names(last_week_2021_new_cases_total) <- gsub("\\.", "-", row.names(last_week_2021_new_cases_total))
row.names(last_week_2021_new_cases_total) <- gsub("Baie-D-Urfé", "Baie-D'Urfé", row.names(last_week_2021_new_cases_total))
row.names(last_week_2021_new_cases_total) <- gsub("L-Île-Bizard-Sainte-Geneviève", "L'Île-Bizard-Sainte-Geneviève", row.names(last_week_2021_new_cases_total))
row.names(last_week_2021_new_cases_total) <- gsub("Plateau-Mont-Royal", "Le Plateau-Mont-Royal", row.names(last_week_2021_new_cases_total))
row.names(last_week_2021_new_cases_total) <- gsub("Rosemont-La-Petite-Patrie", "Rosemont-La Petite-Patrie", row.names(last_week_2021_new_cases_total))
row.names(last_week_2021_new_cases_total) <- gsub("Sud-Ouest", "Le Sud-Ouest", row.names(last_week_2021_new_cases_total))

last_week_2021_new_cases_total_34 <- last_week_2021_new_cases_total
new_row <- data.frame(last_week_2021_new_cases_total = 1) # L'Île-Dorval has 0
row.names(new_row) <- "L'Île-Dorval"
last_week_2021_new_cases_total_34 <- rbind(last_week_2021_new_cases_total_34, new_row)

sum(last_week_2021_new_cases_total$last_week_2021_new_cases_total)
# total 25794 case counts in all boros in mtl
