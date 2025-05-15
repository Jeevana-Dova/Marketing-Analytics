# Load Required Libraries
library(data.table)   # Fast data import
library(dplyr)        # Data wrangling
library(ggplot2)      # Visualization
library(rstatix)      # (Optional) Effect sizes
library(ggpubr)       # (Optional) Publication-ready plots

# Load the Raw Data
df <- fread("C:/Users/admin/Desktop/Marketing Anaytics Project/airbnb.csv")

# Initial Data Inspection
str(df)                    # Structure of the dataset
summary(df)                # Summary statistics for each column
colSums(is.na(df))         # Count of missing values in each column

# Handle Missing Values and Clean Price
df_clean <- df %>%
  # Remove rows missing critical information
  filter(
    !is.na(availability_365),
    !is.na(neighbourhood_cleansed),
    !is.na(room_type)
  ) %>%
  # Clean the 'price' column by removing currency symbols and converting to numeric
  mutate(
    price = as.numeric(gsub("[^0-9.]", "", price)),
    price = ifelse(price <= 0, NA, price)  # Remove non-sensible zero or negative prices
  ) %>%
  # Impute missing prices with the median price by room_type
  group_by(room_type) %>%
  mutate(price = ifelse(is.na(price), median(price, na.rm = TRUE), price)) %>%
  ungroup()

# Calculate Occupancy Rate
df_clean <- df_clean %>%
  mutate(occupancy_rate = (365 - availability_365) / 365)

# Handle Price Outliers
# Cap price at the 1st and 99th percentile to reduce the effect of extreme outliers
df_clean <- df_clean %>%
  mutate(
    price = case_when(
      price > quantile(price, 0.99, na.rm = TRUE) ~ quantile(price, 0.99, na.rm = TRUE),
      price < quantile(price, 0.01, na.rm = TRUE) ~ quantile(price, 0.01, na.rm = TRUE),
      TRUE ~ price
    )
  )

#Filter Neighborhoods with Enough Listings
# Keep neighborhoods that have at least 30 listings, and reset factor levels
df_filtered <- df_clean %>%
  group_by(neighbourhood_cleansed) %>%
  filter(n() >= 30) %>%
  ungroup() %>%
  mutate(
    neighbourhood_cleansed = droplevels(as.factor(neighbourhood_cleansed)),
    room_type = as.factor(room_type)
  )

# Remove Duplicate Rows
df_filtered <- df_filtered[!duplicated(df_filtered), ]

# Add Log-Transformed Price Column
# This transformation helps normalize the distribution for regression analysis
df_filtered$log_price <- log1p(df_filtered$price)

# Final Summary Checks
glimpse(df_filtered)                         # Overview of the cleaned data
summary(df_filtered$occupancy_rate)          # Stats on occupancy rate
summary(df_filtered[c("occupancy_rate", "price")])  # Quick look at key vars
table(df_filtered$neighbourhood_cleansed)    # Count of listings per neighborhood

# Save Cleaned Dataset to CSV
fwrite(df_filtered, "C:/Users/admin/Desktop/Marketing Anaytics Project/paris_airbnb_cleaned.csv")


# -----------------------------
# QUESTION 1:
# Does increasing price lower bookings?
# Model: Linear Regression + Beta Regression (occupancy_rate ~ log_price)
# -----------------------------

# Filter for beta regression where occupancy rate is strictly between 0 and 1
df_price_model <- df_filtered %>%
  filter(occupancy_rate > 0 & occupancy_rate < 1)

# Linear Regression
model_price_lm <- lm(occupancy_rate ~ log_price, data = df_filtered)
summary(model_price_lm)

# Beta Regression
library("betareg")
model_price_beta <- betareg(occupancy_rate ~ log_price, data = df_price_model)
summary(model_price_beta)

# Scatter Plot: Log Price vs Occupancy Rate
ggplot(df_filtered, aes(x = log_price, y = occupancy_rate)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  labs(
    title = "Effect of Log Price on Occupancy Rate",
    x = "Log(Price)", y = "Occupancy Rate"
  ) +
  theme_minimal()

# Compute unique quantile breaks
breaks <- unique(quantile(df_filtered$price, probs = seq(0, 1, 0.2), na.rm = TRUE))

# Only run cut() if we have enough unique bins (at least 2)
if(length(breaks) > 2) {
  df_filtered %>%
    mutate(price_bin = cut(price, breaks = breaks, include.lowest = TRUE)) %>%
    ggplot(aes(x = price_bin, y = occupancy_rate)) +
    geom_boxplot(fill = "lightblue") +
    labs(
      title = "Occupancy Rate by Price Quintile",
      x = "Price Quintile", y = "Occupancy Rate"
    ) +
    theme_minimal()
} else {
  cat("Not enough unique price breaks to create bins.")
}

# -----------------------------
# QUESTION 2:
# Does closer distance to attractions (like the Eiffel Tower) lead to more bookings?
# Model: Negative Binomial Regression (reviews_per_month ~ distance_to_eiffel_km)
# -----------------------------

# Load required libraries
library(geosphere)
library(MASS)
library(pscl)

# Compute distance to Eiffel Tower (latitude = 48.8584, longitude = 2.2945)
eiffel_coords <- c(2.2945, 48.8584)

df_filtered$distance_to_eiffel_km <- distHaversine(
  matrix(c(df_filtered$longitude, df_filtered$latitude), ncol = 2),
  eiffel_coords
) / 1000  # Convert meters to kilometers

# Filter non-missing review counts
df_distance_model <- df_filtered %>% filter(!is.na(reviews_per_month))

# Negative Binomial Regression
model_distance_nb <- glm.nb(reviews_per_month ~ distance_to_eiffel_km, data = df_distance_model)
summary(model_distance_nb)

# Pseudo R² for model performance
pR2(model_distance_nb)

# Fit model
library(MASS)
model_nb <- glm.nb(reviews_per_month ~ distance_to_eiffel_km, data = df_distance_model)
summary(model_nb)

# Add predicted values to plot
df_distance_model$predicted_reviews <- predict(model_nb, type = "response")

# Plot with fitted line
ggplot(df_distance_model, aes(x = distance_to_eiffel_km, y = reviews_per_month)) +
  geom_point(alpha = 0.3, color = "darkgreen") +
  geom_line(aes(y = predicted_reviews), color = "red", linewidth = 1) +
  labs(
    title = "Negative Binomial Fit: Distance vs Reviews",
    x = "Distance to Eiffel Tower (km)", y = "Predicted Reviews per Month"
  ) +
  theme_minimal()



# Histogram: Distribution of Listings by Distance
ggplot(df_distance_model, aes(x = distance_to_eiffel_km)) +
  geom_histogram(fill = "skyblue", bins = 30, color = "black") +
  labs(
    title = "Distribution of Listings by Distance from Eiffel Tower",
    x = "Distance to Eiffel Tower (km)", y = "Count of Listings"
  ) +
  theme_minimal()

# -----------------------------
# QUESTION 3:
# Does too much competition (many listings by same host) reduce occupancy?
# Model: Linear Regression + Beta Regression (occupancy_rate ~ host listing count)
# -----------------------------

# Filter data for valid occupancy values
df_market_model <- df_filtered %>%
  filter(occupancy_rate > 0 & occupancy_rate < 1)

# Linear Regression
model_market_lm <- lm(occupancy_rate ~ calculated_host_listings_count, data = df_filtered)
summary(model_market_lm)

# Beta Regression
model_market_beta <- betareg(occupancy_rate ~ calculated_host_listings_count, data = df_market_model)
summary(model_market_beta)

# Scatter Plot: Host Listings vs Occupancy
ggplot(df_filtered, aes(x = calculated_host_listings_count, y = occupancy_rate)) +
  geom_point(alpha = 0.3, color = "purple") +
  geom_smooth(method = "lm", color = "orange") +
  labs(
    title = "Effect of Host Saturation on Occupancy Rate",
    x = "Number of Listings by Host", y = "Occupancy Rate"
  ) +
  theme_minimal()

# Boxplot: Single vs. Multi-listing Hosts
df_filtered %>%
  mutate(host_type = ifelse(calculated_host_listings_count == 1, "Single Listing", "Multiple Listings")) %>%
  ggplot(aes(x = host_type, y = occupancy_rate, fill = host_type)) +
  geom_boxplot() +
  labs(
    title = "Occupancy Rate by Host Type",
    x = "Host Type", y = "Occupancy Rate"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("lightgreen", "lightcoral")) +
  theme(legend.position = "none")

