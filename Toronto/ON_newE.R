##map fsa to disease data start with M
cases <- read.csv("data/COVID19 cases.csv")
head(cases)

cases$FSA[cases$CFSAUID == "M5H"] <- "M5G"
cases$FSA[cases$CFSAUID == "M5C"] <- "M5B"

fsa_cases <- cases %>%
  left_join(DA_FSA_Nom_pop_Key, by = c("FSA" = "CFSAUID"))
head(fsa_cases)

# Group by FSA and Age.Group, then count the number of cases (rows)
age_group_counts <- fsa_cases %>%
  group_by(FSA, Age.Group, Prop_pop) %>%
  summarise(case_count = n(), .groups = "drop") %>% # n() counts the number of rows (cases)
  filter(Age.Group != "" & FSA != "" & Prop_pop != 0) # remove 8228 missing FSA(1% missing)

# age_group_counts <- age_group_counts %>%
#   left_join(trt_fsa, by = c("FSA" = "CFSAUID"))

head(age_group_counts) 

###############################################################################################
###############################################################################################
########### plot cases for each age group over time
###############################################################################################
###############################################################################################
time_age_case <- fsa_cases %>%
  group_by(Age.Group, Prop_pop, Reported.Date) %>%
  summarise(case_count = n(), .groups = "drop") %>% # n() counts the number of rows (cases)
  filter(Age.Group != "" & Prop_pop != 0) # remove 8228 missing FSA(1% missing)

# Ensure Reported.Date is in Date format
time_age_case <- time_age_case %>%
  mutate(Reported.Date = as.Date(Reported.Date))  # Convert to Date if not already

time_age_case_filtered_young <- subset(time_age_case, Age.Group == "19 and younger")

# Now extract the year and group by it
time_age_case_filtered_young <- time_age_case_filtered_young %>%
  mutate(age_group_category = case_when(
    Age.Group == "19 and younger" ~ "young",
    Age.Group %in% c("20 to 29 Years", "30 to 39 Years", "40 to 49 Years", "50 to 59 Years") ~ "mid",
    Age.Group %in% c("60 to 69 Years", "70 to 79 Years", "80 to 89 Years", "90 and older") ~ "old"
  )) %>%
  mutate(YearMonth = format(Reported.Date, "%Y-%m")) %>%  # Extract month
  group_by(YearMonth, age_group_category) %>%
  summarize(total_cases = sum(case_count))  # Sum cases per year

# Extract the year and filter for relevant age groups
time_age_case_mid <- time_age_case %>%
  mutate(Year = format(Reported.Date, "%Y"),   # Extract Year
         Month = format(Reported.Date, "%m"),
         YearMonth = paste(Year, Month, sep = "-")) %>%
  filter(Age.Group %in% c("20 to 29 Years", 
                          "30 to 39 Years", 
                          "40 to 49 Years", 
                          "50 to 59 Years"))  # Filter for relevant age groups

# Summarize total cases by year and age group
time_age_case_mid <- time_age_case_mid %>%
  mutate(age_group_category = case_when(
    Age.Group == "19 and younger" ~ "young",
    Age.Group %in% c("20 to 29 Years", "30 to 39 Years", "40 to 49 Years", "50 to 59 Years") ~ "mid",
    Age.Group %in% c("60 to 69 Years", "70 to 79 Years", "80 to 89 Years", "90 and older") ~ "old"
  )) %>%
  group_by(YearMonth, age_group_category) %>%
  summarize(total_cases = sum(case_count), .groups = "drop")  # Sum cases per group

# Extract the year and filter for relevant age groups
time_age_case_old <- time_age_case %>%
  mutate(Year = format(Reported.Date, "%Y"),   # Extract Year
         Month = format(Reported.Date, "%m"),
         YearMonth = paste(Year, Month, sep = "-")) %>%
  filter(Age.Group %in% c("60 to 69 Years", "70 to 79 Years", "80 to 89 Years", "90 and older"))  # Filter for relevant age groups

# Summarize total cases by year and age group
time_age_case_old <- time_age_case_old %>%
  mutate(age_group_category = case_when(
    Age.Group == "19 and younger" ~ "young",
    Age.Group %in% c("20 to 29 Years", "30 to 39 Years", "40 to 49 Years", "50 to 59 Years") ~ "mid",
    Age.Group %in% c("60 to 69 Years", "70 to 79 Years", "80 to 89 Years", "90 and older") ~ "old"
  )) %>%
  group_by(YearMonth, age_group_category) %>%
  summarize(total_cases = sum(case_count), .groups = "drop")  # Sum cases per group

# Load necessary library
library(ggplot2)

# Convert YearMonth to Date format (use the first day of the month)
time_age_case_filtered_young <- time_age_case_filtered_young %>%
  mutate(YearMonth = as.Date(paste0(YearMonth, "-01"), format = "%Y-%m-%d"))

time_age_case_mid <- time_age_case_mid %>%
  mutate(YearMonth = as.Date(paste0(YearMonth, "-01"), format = "%Y-%m-%d"))

time_age_case_old <- time_age_case_old %>%
  mutate(YearMonth = as.Date(paste0(YearMonth, "-01"), format = "%Y-%m-%d"))

# Plot the data using ggplot2
ggplot(time_age_case_filtered_young, aes(x = YearMonth, y = total_cases)) +
  geom_line(color = "black")  +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "5 month") +
  labs(title = "Number of Cases Over Time for young", 
       x = NULL, y = "Number of cases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

ggplot(time_age_case_mid, aes(x = YearMonth, y = total_cases)) +
  geom_line(color = "black")  +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "5 month") +
  labs(title = "Number of Cases Over Time for middle ages", 
       x = NULL, y = "Number of cases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

ggplot(time_age_case_old, aes(x = YearMonth, y = total_cases)) +
  geom_line(color = "black")  +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "5 month") +
  labs(title = "Number of Cases Over Time for old", 
       x = NULL, y = "Number of cases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

combined_data <- bind_rows(time_age_case_filtered_young, time_age_case_mid, time_age_case_old)
filtered_data <- combined_data %>%
  filter(format(YearMonth, "%Y-%m") %in% c("2020-03", "2020-04", "2020-05"))

# Plot the overlay plot using ggplot2
ggplot(combined_data, aes(x = YearMonth, y = (total_cases), color = age_group_category, group = age_group_category)) +
  geom_line(size = 1) +
  labs(title = "Number of Cases Over Time by Age Group",
       x = "Year-Month", y = "Number of Cases", color = "Age Group",
       caption = "Age groups are categorized as: 'young' for ages 19 and younger, 'mid' for ages 20–59, and 'old' for ages 60 and above."
       
  )  +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "transparent", color = NA),  # Transparent panel background
    plot.background = element_rect(fill = "transparent", color = NA),  # Transparent overall plot background
    panel.grid = element_blank()                                       # Removes grid lines
  ) 

###############################################################################################
###############################################################################################
# Create custom age categories
grouped_data <- age_group_counts %>%
  mutate(age_group_category = case_when(
    Age.Group == "19 and younger" ~ "young",
    Age.Group %in% c("20 to 29 Years", "30 to 39 Years", "40 to 49 Years", "50 to 59 Years") ~ "mid",
    Age.Group %in% c("60 to 69 Years", "70 to 79 Years", "80 to 89 Years", "90 and older") ~ "old"
  )) %>%
  
  # Group by FSA and age category, then summarise the number of cases for each category
  group_by(FSA, age_group_category, Prop_pop) %>%
  summarise(case_count = sum(case_count), .groups = "drop")  # Sum the case counts per group

total_young <- sum(trt_fsa$age_1_14_count)
total_mid <- sum(trt_fsa$age_15_64_count)
total_old <- sum(trt_fsa$age_64_over)

r_young <- grouped_data %>%
  filter(age_group_category == "young") %>%
  summarise(total_cases_young = sum(case_count)) %>% # total population 
  mutate(r_young = total_cases_young / total_young)

r_mid <- grouped_data %>%
  filter(age_group_category == "mid") %>%
  summarise(total_cases_mid = sum(case_count)) %>%
  mutate(r_mid = total_cases_mid / total_mid)

r_old <- grouped_data %>%
  filter(age_group_category == "old") %>%
  summarise(total_cases_old = sum(case_count)) %>%
  mutate(r_old = total_cases_old / total_old)

# COVID-19 cases form Jan 2020 to Feb 2024

# Get E
library(dplyr)

weighted_young <- grouped_data %>%
  filter(age_group_category == "young") %>% # Filter only young group
  left_join(trt_fsa, by = c("FSA" = "CFSAUID")) %>%
  select(FSA, Prop_pop, case_count, age_group_category, age_1_14_count)
weighted_young$E <- round(weighted_young$age_1_14_count*r_young$r_young, 0)

weighted_mid <- grouped_data %>%
  filter(age_group_category == "mid") %>% # Filter only mid group
  left_join(trt_fsa, by = c("FSA" = "CFSAUID")) %>%
  select(FSA, Prop_pop, case_count, age_group_category, age_15_64_count) 
weighted_mid$E <- round(weighted_mid$age_15_64_count*r_mid$r_mid,0)

weighted_old <- grouped_data %>%
  filter(age_group_category == "old") %>% # Filter only old group
  left_join(trt_fsa, by = c("FSA" = "CFSAUID")) %>%
  select(FSA, Prop_pop, case_count, age_group_category, age_64_over)  
weighted_old$E <- round(weighted_old$age_64_over*r_old$r_old,0)

E <- weighted_young$E + weighted_mid$E + weighted_old$E

plot_case <- grouped_data%>%
  group_by(FSA, Prop_pop) %>%
  summarise(case_count = sum(case_count), .groups = "drop") %>%
  left_join(fsa, by = c("FSA" = "CFSAUID")) %>%
  select(FSA, case_count, Prop_pop, geometry)
plot_case$E <- E

library(ggplot2)
# Create the map
ggplot(data = plot_case) +
  geom_sf(aes(fill = case_count, geometry = geometry)) +
  scale_fill_gradient(low = "grey10", high = "grey90") +
  labs(
    title = "Observed COVID-19 cases in Toronto",
    subtitle = "From January 2020 to February 2024",
    fill = "Cases Observed"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10))
  )


library("units")
plot_case$E <- drop_units(plot_case$E)
ggplot(data = plot_case) +
  geom_sf(aes(fill = E, geometry = geometry)) +
  scale_fill_gradient(low = "grey10", high = "grey90") +
  labs(
    title = "Expected COVID-19 cases in Toronto",
    subtitle = "From Jan 2020 to Feb 2024",
    fill = "Expected cases"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10))
  )

plot_case$SMR <- plot_case$case_count/plot_case$E
ggplot(data = plot_case) +
  geom_sf(aes(fill = SMR, geometry = geometry)) +
  scale_fill_gradient(low = "grey10", high = "grey90")   +
  labs(title = "SMR in Toronto",
       subtitle = "From Jan 2020 to Feb 2024",
       fill = "SMR") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10))
  )

CI_low <- plot_case$SMR*exp(-1.96/sqrt(plot_case$case_count))
CI_high <- plot_case$SMR*exp(1.96/sqrt(plot_case$case_count))

ggplot(plot_case, aes(x = case_count, y = SMR)) +
  geom_point(color = "blue") +
  geom_line(color = "blue") +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2, color = "red") +
  labs(title = "SMR with Confidence Intervals", x = "Case Count", y = "SMR") +
  theme_minimal()

plot_case$CI_width <- CI_high-CI_low
ggplot(data = plot_case) +
  geom_sf(aes(fill = CI_width, geometry = geometry), color="black")  +
  scale_fill_gradient(low = "grey10", high = "grey90") +
  labs(title = "SMR with Confidence Intervals in Toronto",
       subtitle = "From Jan 2020 to Feb 2024",
       fill = "CI_width") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10))
  )

# time seriers for all age & all cases
r <- sum(plot_case$case_count)/sum(plot_case$Prop_pop)
plot_case$oldE <- plot_case$Prop_pop*r
ggplot(data = plot_case) +
  geom_sf(aes(fill = oldE, geometry = geometry)) +
  scale_fill_viridis_c()  +
  labs(title = "E form Jan 2020 to Feb 2024 in Toronto",
       fill = "E of cases")

###############################################################################################
###############################################################################################
########### 2020 3-5
###############################################################################################
###############################################################################################
cases_filtered <- cases %>%
  filter(Reported.Date >= as.Date("2020-03-01") & Reported.Date <= as.Date("2020-05-01"))

fsa_cases <- cases_filtered %>%
  left_join(DA_FSA_Nom_pop_Key, by = c("FSA" = "CFSAUID"))
head(fsa_cases)

# Group by FSA and Age.Group, then count the number of cases (rows)
age_group_counts <- fsa_cases %>%
  group_by(FSA, Age.Group, Prop_pop) %>%
  summarise(case_count = n(), .groups = "drop") %>% # n() counts the number of rows (cases)
  filter(Age.Group != "" & FSA != "" & Prop_pop != 0) # remove 8228 missing FSA(1% missing)

grouped_data <- age_group_counts %>%
  mutate(age_group_category = case_when(
    Age.Group == "19 and younger" ~ "young",
    Age.Group %in% c("20 to 29 Years", "30 to 39 Years", "40 to 49 Years", "50 to 59 Years") ~ "mid",
    Age.Group %in% c("60 to 69 Years", "70 to 79 Years", "80 to 89 Years", "90 and older") ~ "old"
  )) %>%
  
  # Group by FSA and age category, then summarise the number of cases for each category
  group_by(FSA, age_group_category, Prop_pop) %>%
  summarise(case_count = sum(case_count), .groups = "drop")  # Sum the case counts per group

total_young <- sum(trt_fsa$age_1_14_count)
total_mid <- sum(trt_fsa$age_15_64_count)
total_old <- sum(trt_fsa$age_64_over)

r_young <- grouped_data %>%
  filter(age_group_category == "young") %>%
  summarise(total_cases_young = sum(case_count)) %>% # total population 
  mutate(r_young = total_cases_young / total_young)

r_mid <- grouped_data %>%
  filter(age_group_category == "mid") %>%
  summarise(total_cases_mid = sum(case_count)) %>%
  mutate(r_mid = total_cases_mid / total_mid)

r_old <- grouped_data %>%
  filter(age_group_category == "old") %>%
  summarise(total_cases_old = sum(case_count)) %>%
  mutate(r_old = total_cases_old / total_old)

# COVID-19 cases form Jan 2020 to Feb 2024

# Get E
library(dplyr)
library(tidyr)

# Create a data frame with all combinations of FSA and age group categories
all_fsa_age_groups <- grouped_data %>%
  select(FSA) %>%
  distinct() %>%
  expand(FSA, age_group_category = c("young", "mid", "old"))

# Left join with the original data to add missing age group categories
expanded_data <- all_fsa_age_groups %>%
  left_join(grouped_data, by = c("FSA", "age_group_category")) %>%
  mutate(case_count = ifelse(is.na(case_count), 0, case_count))

#  Combine with the original data
grouped_data <- expanded_data %>% arrange(FSA, age_group_category)

weighted_young <- grouped_data %>%
  filter(age_group_category == "young") %>% # Filter only young group
  left_join(trt_fsa, by = c("FSA" = "CFSAUID")) %>%
  select(FSA, Prop_pop, case_count, age_group_category, age_1_14_count)
weighted_young$E <- round(weighted_young$age_1_14_count*r_young$r_young, 0)

weighted_mid <- grouped_data %>%
  filter(age_group_category == "mid") %>% # Filter only mid group
  left_join(trt_fsa, by = c("FSA" = "CFSAUID")) %>%
  select(FSA, Prop_pop, case_count, age_group_category, age_15_64_count) 
weighted_mid$E <- round(weighted_mid$age_15_64_count*r_mid$r_mid,0)

weighted_old <- grouped_data %>%
  filter(age_group_category == "old") %>% # Filter only old group
  left_join(trt_fsa, by = c("FSA" = "CFSAUID")) %>%
  select(FSA, Prop_pop, case_count, age_group_category, age_64_over)  
weighted_old$E <- round(weighted_old$age_64_over*r_old$r_old,0)

E <- weighted_young$E + weighted_mid$E + weighted_old$E

plot_case <- grouped_data%>%
  group_by(FSA, Prop_pop) %>%
  summarise(case_count = sum(case_count), .groups = "drop") %>%
  filter(!is.na(Prop_pop)) %>%
  left_join(fsa, by = c("FSA" = "CFSAUID")) %>%
  select(FSA, case_count, Prop_pop, geometry)
plot_case$E <- E

library(ggplot2)
ggplot(data = plot_case) +
  geom_sf(aes(fill = case_count, geometry = geometry)) +
  scale_fill_gradient(low = "grey10", high = "grey90") +
  labs(
    title = "Observed COVID-19 cases in Toronto",
    subtitle = "form March 2020 to May 2020",
    fill = "Cases Observed"
  ) +
  theme_void()+
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 10, b = 10)), # Adds top and bottom margin to the title
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(t = 5, b = 10)),              # Adds margin to the subtitle
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.margin = margin(20, 20, 20, 20)  # Adds extra space around the entire plot
  )


library("units")
plot_case$E <- drop_units(plot_case$E)
ggplot(data = plot_case) +
  geom_sf(aes(fill = E, geometry = geometry)) +
  scale_fill_gradient(low = "grey10", high = "grey90") +
  labs(
    title = "Expected COVID-19 cases in Toronto",
    subtitle = "form March 2020 to May 2020",
    fill = "Expected cases"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10))
  )+
  theme_void()+
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 10, b = 10)), # Adds top and bottom margin to the title
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(t = 5, b = 10)),              # Adds margin to the subtitle
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.margin = margin(20, 20, 20, 20)  # Adds extra space around the entire plot
  )

plot_case$SMR <- plot_case$case_count/plot_case$E
ggplot(data = plot_case) +
  geom_sf(aes(fill = SMR, geometry = geometry)) +
  scale_fill_gradient(low = "grey10", high = "grey90")   +
  labs(title = "SMR in Toronto",
       subtitle = "form March 2020 to May 2020",
       fill = "SMR") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10))
  )+
  theme_void()+
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 10, b = 10)), # Adds top and bottom margin to the title
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(t = 5, b = 10)),              # Adds margin to the subtitle
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.margin = margin(20, 20, 20, 20)  # Adds extra space around the entire plot
  )

CI_low <- plot_case$SMR*exp(-1.96/sqrt(plot_case$case_count))
CI_high <- plot_case$SMR*exp(1.96/sqrt(plot_case$case_count))

ggplot(plot_case, aes(x = case_count, y = SMR)) +
  geom_point(color = "blue") +
  geom_line(color = "blue") +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2, color = "red") +
  labs(title = "SMR with Confidence Intervals", x = "Case Count", y = "SMR") +
  theme_minimal()

plot_case$CI_width <- CI_high-CI_low
ggplot(data = plot_case) +
  geom_sf(aes(fill = CI_width, geometry = geometry), color="black")  +
  scale_fill_gradient(low = "grey10", high = "grey90") +
  labs(title = "SMR with Confidence Intervals in Toronto",
       subtitle = "form March 2020 to May 2020",
       fill = "CI_width") +
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

plot_case$VM <- S2$`Visible minorities`
ggplot(data = plot_case) +
  geom_sf(aes(fill = VM, geometry = geometry), color="black")  +
  scale_fill_gradient(low = "grey10", high = "grey90") +
  labs(title = "Variation in Visible Minorities Across Areas",
      # subtitle = "form March 2020 to May 2020",
       fill = "Proportion") +
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

plot_case$imm <- S2$`Recent immigrants`
ggplot(data = plot_case) +
  geom_sf(aes(fill = imm, geometry = geometry), color="black")  +
  scale_fill_gradient(low = "grey10", high = "grey90") +
  labs(title = "Variation in Recent immigrants Across Areas",
       # subtitle = "form March 2020 to May 2020",
       fill = "Proportion") +
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

plot_case$Nohigh <- S2$`No High sch, 25-64`
ggplot(data = plot_case) +
  geom_sf(aes(fill = Nohigh, geometry = geometry), color="black")  +
  scale_fill_gradient(low = "grey10", high = "grey90") +
  labs(title = "Variation in No High sch, 25-64 Across Areas",
       # subtitle = "form March 2020 to May 2020",
       fill = "Proportion") +
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

plot_case$logstops <- S2$`Log of Stops`
ggplot(data = plot_case) +
  geom_sf(aes(fill = logstops, geometry = geometry), color="black")  +
  scale_fill_gradient(low = "grey10", high = "grey90") +
  labs(title = "Variation in Log of Stops Across Areas",
       # subtitle = "form March 2020 to May 2020",
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

plot_case$lico <- S2$`Low-income cutoff`
ggplot(data = plot_case) +
  geom_sf(aes(fill = lico, geometry = geometry), color="black")  +
  scale_fill_gradient(low = "grey10", high = "grey90") +
  labs(title = "Variation in Low-income cutoff Across Areas",
       # subtitle = "form March 2020 to May 2020",
       fill = "Proportion") +
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

plot_case$Total_stops <- colSums(adj_matrix_Directed)
#plot_case <- plot_case %>% left_join(fsa_clean_data_m_all_loop, by = c("FSA" = "CFSAUID")) # add for loop
# ggplot(data = plot_case) +
#   geom_sf(aes(fill = Total_stops, geometry = geometry)) +
#   scale_fill_gradient(low = "grey10", high = "grey90") +
#   labs(title = "Stops (weighted by Population) within each FSA area in Toronto",
#        subtitle = "March to May 2020",
#        fill = "stops")+
#   theme(
#     plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#     plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10))
#   )+
#   theme_void()+
#   theme(
#     plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 10, b = 10)), # Adds top and bottom margin to the title
#     plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(t = 5, b = 10)),              # Adds margin to the subtitle
#     legend.title = element_text(size = 10, face = "bold"),
#     legend.text = element_text(size = 9),
#     plot.margin = margin(20, 20, 20, 20)  # Adds extra space around the entire plot
#   )


barplot(fsa_clean_data_m_all_loop$stops_W, 
        main = "Weighted Stops within each FSA area in Toronto", 
        xlab = "FSA", 
        ylab = "Weighted Stops loop", 
        col = "grey", 
        border = "white")

## case/Pop
plot_case$Propotion <- plot_case$case_count/plot_case$Prop_pop
ggplot(data = plot_case) +
  geom_sf(aes(fill = Propotion, geometry = geometry)) +
  scale_fill_viridis_c()  +
  labs(title = "Proportion",
       fill = "Proportion")
