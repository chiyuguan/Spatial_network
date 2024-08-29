## a_{ik} is population (number of residents in each Borough) in area i in strata k
## (k=1,2,3 corresponding young, mid, old).
mtl_Boro_young_mid_old_Total <- mtl_Boro %>%
  group_by(NOM) %>% 
  summarise(across(c(age_1_14_count,
                     age_15_64_count,
                     age_64_over), sum))

## r_k is the Montreal-wide rate (all boroughs combined) as the incidence rate per each of 3 age strata
## (2021/12/5 to 2022/3/12)
incidence_rate_age_rk <- read.csv("PL_AGE_SEXE_v5.csv") 

# Set the first row as column names
colnames(incidence_rate_age_rk) <- incidence_rate_age_rk[1,]

# Remove the first row from the data frame
incidence_rate_age_rk <- incidence_rate_age_rk[-1,]

# Set the first column as row names
rownames(incidence_rate_age_rk) <- incidence_rate_age_rk[,1]

# Remove the first column from the data frame
incidence_rate_age_rk <- incidence_rate_age_rk[,-1]

library(dplyr)
# Subset and sum for each age range
incidence_rate_age_rk <- incidence_rate_age_rk %>%
  rownames_to_column(var = "age_group")

incidence_rate_age_rk$`Cas total CUMUL` <- as.numeric(as.character(incidence_rate_age_rk$`Cas total CUMUL`))

# Sum the values for each age range
cas_r1_0_19 <- round((incidence_rate_age_rk %>%
  filter(age_group %in% c("0-9 ans", "10-19 ans")) %>%
  select(`Cas total CUMUL`) %>%
  sum())/sum(mtl_Boro_Total$Population),2)

cas_r2_20_69 <- round((incidence_rate_age_rk %>%
  filter(age_group %in% c("20-29 ans", "30-39 ans", "40-49 ans", "50-59 ans", "60-69 ans")) %>%
  select(`Cas total CUMUL`) %>%
  sum())/sum(mtl_Boro_Total$Population), 2)

cas_r3_70_plus <- round((incidence_rate_age_rk %>%
  filter(age_group %in% c("70-79 ans", "80-89 ans", "90 ans et plus")) %>%
  select(`Cas total CUMUL`) %>%
  sum())/sum(mtl_Boro_Total$Population), 2) #"Inconnu" cont for 0.01% #/Total population

new_E <- mtl_Boro_young_mid_old_Total %>%
  mutate(Expected_count_for_each_boro = age_1_14_count * cas_r1_0_19 + age_15_64_count * cas_r2_20_69 + age_64_over * cas_r3_70_plus)


