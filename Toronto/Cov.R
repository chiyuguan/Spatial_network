
library(sf)
#library(rgdal)
library(tidyverse)
library(cancensus)
library(ggmap)
library(stringr)
library(kableExtra)
library(htmlTable)
library(ggsankeyfier)
library(grid)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(RPostgres)
library(rpostgis)
library(readxl)

rm(list=ls())

# Canceuss 
#options(cancensus.cache_path = "cache/")
#set_cancensus_cache_path("cache/", install = TRUE)
options(cancensus.cache_path = "cache/.cancensus_cache")

bool_loadCensus = TRUE

# Plot global params 
theme_set(theme_classic())
theme_set(
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank(), 
    text = element_text(size=12), 
    plot.title = element_text(size=15),   
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  )
)


# Load census vector names for each year 
dfCensVec <- readRDS("data/censMappedVars.rds")


# Variable name description 
dfVarDesc<- data.frame(Description = c(
  "Visible Minority", 
  "Indigenous identity", 
  "Max High sch, 15",
  "Max High sch, 25-64",
  "No High sch, 15",
  "No High sch, 25-64",
  "Moved past 5 yrs", 
  "Moved past 1 yr", 
  "Low-income cutoff", 
  "Recent immigrants",
  "Age below 15", 
  "Age over 65", 
  ">30% of income for housing", 
  "Tenant, as opposed to owner"), 
  varName = c(
    "i_visMin",
    "i_aboriginal",
    "i_educ_15",
    "i_educ_25_64",
    "i_educ_noHS_15",
    "i_educ_noHS_25_64",
    "i_move_5yrs",
    "i_move_1yrs",
    "i_LICO",
    "i_rec_imm",
    "i_young",
    "i_old",
    "i_tenant_30income",
    "i_tenant"
  ), 
  varShortName =c(
    "Visible minorities",
    "Aboriginal",
    "High school",
    "Education_25",
    "No High school",
    "No High school 25",
    "Recent movers",
    "Recent move_1yr",
    "Low income",
    "Recent immigrants",
    "Children",
    "Seniors",
    "Housing expense",
    "Tenants")
)


#Download census data based on the identified vector names, and create proporiton measures for each dissemination area. Rehspaed for facet map originally, but protting through looping. 

funcGetCensVector <- function(df, dfVarDesc){
  # Calculate proportions
  df <- funcCensusIndicator(df)
  # Make it long if used for facet plot  
  dfMapLong <- df %>% 
    select(starts_with("i_")) %>% 
    pivot_longer(cols = starts_with("i_"), names_to = "x",values_to = "y")
  # Join sf with variable description
  dfMapLong <- dfMapLong %>% 
    left_join(dfVarDesc, by = c("x"="varName"))
  return(dfMapLong)
}



# function to create variable name 
funcCensusIndicator <- function(df){
  df %>% 
    mutate(i_visMin = (visibleMin_count/visibleMin_denom), #yes
           i_aboriginal = indigi_count/indigi_denom, #no
           i_educ_15 = educ_HS_count_15yr/educ_HS_denom_15yr, #no
           i_educ_25_64 = educ_HS_count_25_64yr/educ_HS_denom_25_64yr, #no
           i_educ_noHS_15 = educ_no_HS_count_15yr/educ_HS_denom_15yr,  #no
           i_educ_noHS_25_64 = (educ_no_HS_count_25_64yr/educ_HS_denom_25_64yr),  #yes
           i_move_5yrs = (move_5yr_count/move_5yr_denom), #yes
           i_move_1yrs = move_1yr_count/move_1yr_denom, #no
           i_LICO = (LICO_prevalence*0.01), #make it to proportion  #yes
           i_rec_imm = (recentImmig_count/recentImmig_denom), #yes
           i_young = (age_1_14_count/age_denom), #yes
           i_old =  (age_64_over/age_denom), #yes
           i_tenant_30income = housingExpend_30_count/property_denom, #no
           i_tenant = (tenant_count/(property_denom))#yes
    )
  # %>% 
  #   select(i_visMin, i_educ_noHS_25_64, i_LICO, NOM)
}

df<-trt_fsa_P_weight
Population_W <- trt_fsa_P_weight$DAPW
funcCensusIndicator_W <- function(df,Population_W){
  df %>% 
    mutate(i_visMin = Population_W*(visibleMin_count/visibleMin_denom), #yes
           i_aboriginal = Population_W*indigi_count/indigi_denom, #no
           i_educ_15 = Population_W*educ_HS_count_15yr/educ_HS_denom_15yr, #no
           i_educ_25_64 = Population_W*educ_HS_count_25_64yr/educ_HS_denom_25_64yr, #no
           i_educ_noHS_15 = Population_W*educ_no_HS_count_15yr/educ_HS_denom_15yr,  #no
           i_educ_noHS_25_64 = Population_W*(educ_no_HS_count_25_64yr/educ_HS_denom_25_64yr),  #yes
           i_move_5yrs = Population_W*(move_5yr_count/move_5yr_denom), #yes
           i_move_1yrs = Population_W*move_1yr_count/move_1yr_denom, #no
           i_LICO = Population_W*(LICO_prevalence*0.01), #make it to proportion  #yes
           i_rec_imm = Population_W*(recentImmig_count/recentImmig_denom), #yes
           i_young = Population_W*(age_1_14_count/age_denom), #yes
           i_old =  Population_W*(age_64_over/age_denom), #yes
           i_tenant_30income = Population_W*housingExpend_30_count/property_denom, #no
           i_tenant = Population_W*(tenant_count/(property_denom))#yes
)   # First, create all calculated columns with mutate
  
  # Then, perform grouping and summarizing
  df <- df %>%
    group_by(CFSAUID) %>%
    summarize(across(2:25, sum, na.rm = TRUE))
  
}


### 2021 data 
options(cancensus.api_key = "CensusMapper_91294cf61b6144bca0fcac3333a180b6")
options(cancensus.cache_path = "./")

# Following codes will extract the census vectors, along with polygons 
b <- dfCensVec %>% pull(X2021)
b <- setNames(b, rownames(dfCensVec)) # Make it named vector so that variable names will be passed to extracted census vars  


trt_2021 <- get_census(dataset="CA21", 
                       regions=list(CMA='35535'), 
                       vectors=b,
                       level= "DA", 
                       use_cache = TRUE, 
                       geo_format = "sf", 
                       quiet = TRUE, 
                       api_key = Sys.getenv("CM_API_KEY")) %>% st_transform(crs = 2952)

plot(trt_2021)

trt_fsa_DA <- trt_2021  %>%
  left_join(DA_FSA_Key, by = c("GeoUID" = "GeoUID")) %>%
  filter(!is.na(CFSAUID)) %>%
  select(3, 15:42) 
trt_fsa_DA <- funcCensusIndicator(trt_fsa_DA)  %>%
  select(1, 23:43) 

trt_fsa_DA <- trt_fsa_DA %>%
  mutate(across(2:4, ~ . * trt_fsa_DA[[7]])) %>%
  mutate(across(2:4, round, 0)) # since one DA corresponding to multiple fsa(5642 , unique 3807)


trt_fsa_P_weight <- trt_fsa_DA %>%
  left_join(DA_FSA_Nom_pop_Key, by = c("CFSAUID" = "CFSAUID")) %>%
  rename("P" = "Prop_pop") 
trt_fsa_P_weight$DA_Weight <- trt_fsa_P_weight$prop_pop/trt_fsa_P_weight$P

ggplot(trt_fsa_P_weight) + 
  geom_sf(aes(fill=DA_Weight)) + 
  geom_sf(data=fsa, color = "white", fill = NA)  +
  scale_fill_gradient(low = "grey10", high = "grey90") +
  ggtitle("Weighted (By FSA level) Population for each Overlaped DA area")+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

trt_fsa_P_weight <- trt_fsa_P_weight %>%
  mutate(across(8:21, ~ . * trt_fsa_P_weight[[24]])) # weighted(overlap/fsa) ppl for each DA
trt_fsa <- trt_fsa_P_weight %>% 
  group_by(CFSAUID) %>%
  summarize(across(c(2:4,7:20,23), sum, na.rm = TRUE))

# for DA 
# trt_fsa_P_weight %>% 
#   funcCensusIndicator  %>% 
#   select(starts_with("i_")) %>% 
#   st_drop_geometry() %>% 
#   cor(use = "complete.obs") %>% 
#   corrplot::corrplot.mixed(number.cex = 0.5, tl.cex = 0.5)


trt_fsa <- trt_fsa  %>% 
  left_join(fsa_clean_data_m_all_loop, by = c("CFSAUID" = "CFSAUID"))  %>% 
  rename("Stops" = "Total_stops") 

Predictor_var <- trt_fsa  %>% 
  select(i_young, i_educ_15, i_LICO, Stops, i_move_5yrs,i_rec_imm, i_visMin) %>% 
  rename("Youth" = "i_young") %>% 
  rename("High school 15" = "i_educ_15") %>%
  rename("Low Income" = "i_LICO") %>%
  rename("Recent movers 5" = "i_move_5yrs") %>%
  rename("Recent immigrants" = "i_rec_imm") %>%
  rename("Visible minorities" = "i_visMin")

Predictor_var %>% 
  st_drop_geometry() %>% 
  cor(use = "complete.obs") %>% 
  corrplot::corrplot.mixed(number.cex = 1, tl.cex = 0.5)

Predictor_var$E <- plot_case$E

trt_fsa %>% 
  select(starts_with("i_")) %>% 
  st_drop_geometry() %>% 
  cor(use = "complete.obs") %>% 
  corrplot::corrplot.mixed(number.cex = 0.5, tl.cex = 0.5)

  # trt_fsa_P_weight_1 <- st_drop_geometry(trt_fsa_P_weight)
  # trt_fsa_P_weight_1[, 2:25] <- lapply(trt_fsa_P_weight_1[, 2:25], drop_units)
  # # Add geometry back
  #  trt_fsa_P_weight <- st_as_sf(trt_fsa_P_weight_1, geometry = st_geometry( trt_fsa_P_weight))
  
  # Map of variables
  funcGetCensusVector_W <- function(dfVarDesc){
    # Calculate proportions
    df <- trt_fsa
    # Make it long if used for facet plot  
    dfMapLong <- df %>% 
      select(starts_with("i_")) %>% 
      pivot_longer(cols = starts_with("i_"), names_to = "x",values_to = "y")
    # Join sf with variable description
    dfMapLong <- dfMapLong %>% 
      left_join(dfVarDesc, by = c("x"="varName"))
    return(dfMapLong)
  }
  
  # Generate plot with descriptive name 
  dfMapLong_trt <- funcGetCensusVector_W(dfVarDesc)
  pList_trt <- list()
  
  for(i in dfVarDesc$Description){
    pList_trt[[i]] <-  dfMapLong_trt %>%
      filter(Description == i) %>% 
      ggplot() +
      geom_sf(inherit.aes = FALSE, aes(fill = y), size =0.5, color= "black") + 
      ggtitle(i) + 
      theme(legend.position = "bottom") + 
      theme(legend.text=element_text(size=9))
  }
  
  
  # And plot 
  pList_trt
  


