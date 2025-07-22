library(randomForest)
library(tidyverse)
library(caret)

# Load and clean the dataset
raw_data <- read_csv("county_health_mental.csv", show_col_types = FALSE) |>
  slice(-1)  # Drop metadata row

# Select and prepare relevant columns
mental_health_full <- raw_data |>
  select(
    fips         = x5_digit_fips_code,
    state_abbr   = state_abbreviation,
    county_name  = name,
    poor_mental_days = poor_mental_health_days_raw_value,
    suicide_rate = suicides_raw_value,
    social_support = lack_of_social_and_emotional_support_raw_value,
    provider_ratio = ratio_of_population_to_mental_health_providers,
    loneliness = feelings_of_loneliness_raw_value,
    crude_suicide = crude_suicide_rate,
    population = population_raw_value
  ) |>
  filter(
    county_name != "United States"
  ) |>
  mutate(
    fips = str_pad(fips, width = 5, side = "left", pad = "0"),
    across(
      c(poor_mental_days, suicide_rate, social_support,
        provider_ratio, loneliness, crude_suicide, population),
      as.numeric
    )
  ) |>
  drop_na()

# Create model dataset
rf_data <- mental_health_full |>
  select(
    poor_mental_days, social_support, loneliness,
    suicide_rate, crude_suicide, provider_ratio, population
  )

# Split into training and testing sets
set.seed(42)
train_indices <- createDataPartition(rf_data$poor_mental_days, p = 0.8, list = FALSE)
train_data <- rf_data[train_indices, ]
test_data  <- rf_data[-train_indices, ]

# Train Random Forest model
rf_model <- randomForest(
  poor_mental_days ~ .,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

# Evaluate model
predictions <- predict(rf_model, test_data)
mse <- mean((predictions - test_data$poor_mental_days)^2)
rmse <- sqrt(mse)

cat("📉 Mean Squared Error (MSE):", round(mse, 3), "\n")
cat("📏 Root Mean Squared Error (RMSE):", round(rmse, 3), "\n")

# Variable importance
cat("📊 Variable Importance:\n")
print(importance(rf_model))

# Create a modified importance data frame with %IncRMSE
var_imp <- importance(rf_model) |>
  as.data.frame() |>
  rownames_to_column("Variable") |>
  mutate(`%IncRMSE` = `%IncMSE` / 2) |>
  arrange(desc(`%IncRMSE`))

# Plot using %IncRMSE
ggplot(var_imp, aes(x = reorder(Variable, `%IncRMSE`), y = `%IncRMSE`)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(
    x = "Predictor",
    y = "% Increase in RMSE"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10))
  )
    