
# load
library(readxl)
library(tidyverse)
url <- "https://www.countyhealthrankings.org/sites/default/files/media/document/2025%20County%20Health%20Rankings%20Data%20-%20v3.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
download.file(url, destfile = temp_file, mode = "wb")
excel_sheets(temp_file)
health_data <- read_excel(temp_file, sheet = "Select Measure Data")
glimpse(health_data)
county_health_mental <- county_health |>
  select(
    "State FIPS Code", "County FIPS Code", "5-digit FIPS Code", 
    "State Abbreviation", "Name", 
    "Poor Mental Health Days raw value", "Poor Mental Health Days numerator", 
    "Poor Mental Health Days denominator", "Poor Mental Health Days CI low", 
    "Poor Mental Health Days CI high", 
    "Poor or Fair Health raw value", "Poor or Fair Health numerator", 
    "Poor or Fair Health denominator", "Poor or Fair Health CI low", 
    "Poor or Fair Health CI high", 
    "Mental Health Providers raw value", "Mental Health Providers numerator", 
    "Mental Health Providers denominator", "Mental Health Providers CI low", 
    "Mental Health Providers CI high", 
    "Ratio of population to mental health providers.", 
    "Frequent Mental Distress raw value", "Frequent Mental Distress numerator", 
    "Frequent Mental Distress denominator", "Frequent Mental Distress CI low", 
    "Frequent Mental Distress CI high", 
    "Suicides raw value", "Suicides numerator", "Suicides denominator", 
    "Suicides CI low", "Suicides CI high", "Crude suicide rate", 
    "Suicides (AIAN)", "Suicides CI low (AIAN)", "Suicides CI high (AIAN)", 
    "Suicides (Asian)", "Suicides CI low (Asian)", "Suicides CI high (Asian)", 
    "Suicides (Black)", "Suicides CI low (Black)", "Suicides CI high (Black)", 
    "Suicides (Hispanic)", "Suicides CI low (Hispanic)", "Suicides CI high (Hispanic)", 
    "Suicides (White)", "Suicides CI low (White)", "Suicides CI high (White)", 
    "Suicides (NHOPI)", "Suicides CI low (NHOPI)", "Suicides CI high (NHOPI)", 
    "Feelings of Loneliness raw value", "Feelings of Loneliness numerator", 
    "Feelings of Loneliness denominator", "Feelings of Loneliness CI low", 
    "Feelings of Loneliness CI high", 
    "Population raw value", "Population numerator", "Population denominator", 
    "Population CI low", "Population CI high", 
    "Lack of Social and Emotional Support raw value", 
    "Lack of Social and Emotional Support numerator", 
    "Lack of Social and Emotional Support denominator", 
    "Lack of Social and Emotional Support CI low", 
    "Lack of Social and Emotional Support CI high")

# US counties plots
# scatterplot 1 - mental health providers per 100k (standardized variable for population)
county_health_mental |>
  mutate(
    `Poor_Mental_Health_Days_raw_value` = as.numeric(gsub(",", "", `Poor_Mental_Health_Days_raw_value`)),
    `Ratio_of_population_to_mental_health_providers.` = as.numeric(gsub(",", "", `Ratio_of_population_to_mental_health_providers.`)),
    providers_per_100k = 100000 / `Ratio_of_population_to_mental_health_providers.`) |>
  filter(
    !is.na(`Poor_Mental_Health_Days_raw_value`),
    !is.na(providers_per_100k),
    providers_per_100k > 0,
    providers_per_100k < 500) |>
  ggplot(aes(
    x = providers_per_100k,
    y = `Poor_Mental_Health_Days_raw_value`)) +
  geom_point(alpha = 0.5, color = "darkorange") +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  labs(
    title = "Mental Health Providers per 100K vs. Poor Mental Health Days",
    x = "Mental Health Providers per 100,000 people",
    y = "Average Poor Mental Health Days (past 30 days)") +
  theme_minimal()

# scatterplot 2 - frequent mental distress & mental health providers
county_health_mental |>
  mutate(
    `Frequent_Mental_Distress_raw_value` = as.numeric(gsub(",", "", `Frequent_Mental_Distress_raw_value`)),
    `Mental_Health_Providers_raw_value` = as.numeric(gsub(",", "", `Mental_Health_Providers_raw_value`))) |>
  filter(
    !is.na(`Frequent_Mental_Distress_raw_value`),
    !is.na(`Mental_Health_Providers_raw_value`),
    `Mental_Health_Providers_raw_value` > 0,
    `Mental_Health_Providers_raw_value` < 1000) |>
  ggplot(aes(
    x = `Mental_Health_Providers_raw_value`,
    y = `Frequent_Mental_Distress_raw_value`)) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm", se = TRUE, color = "darkblue") +
  labs(
    title = "Mental Health Providers vs. Frequent Mental Distress",
    x = "Total Mental Health Providers (raw count)",
    y = "Frequent Mental Distress (%)") +
  theme_minimal()

# scatterplot 3 - frequent mental distress & poor or fair health
county_health_mental |>
  mutate(
    `Poor_or_Fair_Health_raw_value` = as.numeric(gsub(",", "", `Poor_or_Fair_Health_raw_value`)),
    `Frequent_Mental_Distress_raw_value` = as.numeric(gsub(",", "", `Frequent_Mental_Distress_raw_value`))) |>
  filter(
    !is.na(`Poor_or_Fair_Health_raw_value`),
    !is.na(`Frequent_Mental_Distress_raw_value`)) |>
  ggplot(aes(
    x = `Frequent_Mental_Distress_raw_value`,
    y = `Poor_or_Fair_Health_raw_value`)) +
  geom_point(alpha = 0.5, color = "pink") +
  geom_smooth(method = "lm", se = TRUE, color = "darkblue") +
  labs(
    title = "Poor or Fair Health vs. Frequent Mental Distress",
    x = "Poor or Fair Health",
    y = "Frequent Mental Distress (%)") +
  theme_minimal()

# linear regression model
model <- lm(`Poor_Mental_Health_Days_raw_value` ~ providers_per_100k, data = county_health_mental |>
              mutate(
                `Poor_Mental_Health_Days_raw_value` = as.numeric(gsub(",", "", `Poor_Mental_Health_Days_raw_value`)),
                `Ratio_of_population_to_mental_health_providers.` = as.numeric(gsub(",", "", `Ratio_of_population_to_mental_health_providers.`)),
                providers_per_100k = 100000 / `Ratio_of_population_to_mental_health_providers.`) |>
              filter(
                !is.na(`Poor_Mental_Health_Days_raw_value`),
                !is.na(providers_per_100k),
                providers_per_100k > 0,
                providers_per_100k < 500))
summary(model)

# AK boroughs/census areas plots
# scatterplot 1 - AK frequent mental distress & poor or fair health
county_health_mental |>
  filter(`State_Abbreviation` == "AK") |>
  mutate(
    `Poor_or_Fair_Health_raw_value` = as.numeric(gsub(",", "", `Poor_or_Fair_Health_raw_value`)),
    `Frequent_Mental_Distress_raw_value` = as.numeric(gsub(",", "", `Frequent_Mental_Distress_raw_value`))) |>
  filter(
    !is.na(`Poor_or_Fair_Health_raw_value`),
    !is.na(`Frequent_Mental_Distress_raw_value`)) |>
  ggplot(aes(
    x = `Frequent_Mental_Distress_raw_value`,
    y = `Poor_or_Fair_Health_raw_value`)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkblue") +
  labs(
    title = "Alaska: Poor or Fair Health vs. Frequent Mental Distress",
    x = "Poor or Fair Health",
    y = "Frequent Mental Distress (%)") +
  theme_minimal()

# scatterplot 2 - AK lack of social and emotional support & poor mental health days (color coded by region type)
library(ggplot2)
library(ggrepel)

ak_df <- county_health_mental[county_health_mental$State_Abbreviation == "AK", ]
unique(ak_df$Name)

ak_df$Region_Type <- ifelse(
  ak_df$Name %in% c(
    "Anchorage Municipality",
    "Fairbanks North Star Borough",
    "Juneau City and Borough",
    "Matanuska-Susitna Borough",
    "Kenai Peninsula Borough",
    "Ketchikan Gateway Borough",
    "Sitka City and Borough"),
  "Urban",
  "Rural")

ggplot(ak_df, aes(
  x = as.numeric(Lack_of_Social_and_Emotional_Support_raw_value),
  y = as.numeric(Poor_Mental_Health_Days_raw_value),
  color = Region_Type)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text_repel(aes(label = Name), size = 2.5, max.overlaps = 20) +
  geom_smooth(method = "lm", se = TRUE, color = "yellow") +
  scale_color_manual(values = c("Urban" = "blue", "Rural" = "#097969")) +
  labs(
    title = "Alaska: Social & Emotional Support vs. Poor Mental Health Days",
    subtitle = "Urban vs. Rural boroughs/census areas",
    x = "Lack of Social & Emotional Support (%)",
    y = "Poor Mental Health Days",
    color = "Region Type") +
  theme_minimal(base_size = 13)
