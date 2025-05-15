#install packages
install.packages("rstatix")  # For effect sizes (eta-squared)
install.packages("ggpubr")   # For enhanced visualizations
install.packages("dplyr")  # For data manipulation
install.packages("ggplot2")  # For visualization
install.packages("data.table")
install.packages("car")

# Load libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(rstatix)   # For effect sizes
library(ggpubr)    # For publication-ready plots

# Data Preprocessing
# Step 1: Load Data
df <- fread("C:/Users/dovaj/OneDrive/Documents/Marketing Analytics/list.csv")

# Step 2: Initial Inspection
str(df)
summary(df)
colSums(is.na(df))

# Step 3: Handle Missing Values
df_clean <- df %>%
  filter(
    !is.na(availability_365),
    !is.na(neighbourhood_cleansed),
    !is.na(room_type)
  ) %>%
  mutate(
    price = as.numeric(gsub("[€$,]", "", price)),
    price = ifelse(price <= 0, NA, price)
  ) %>%
  group_by(room_type) %>%
  mutate(price = ifelse(is.na(price), median(price, na.rm = TRUE), price)) %>%
  ungroup()

# Step 4: Calculate Occupancy Rate
df_clean <- df_clean %>%
  mutate(occupancy_rate = (365 - availability_365) / 365) 

# Step 5: Handle Outliers
df_clean <- df_clean %>%
  mutate(
    price = case_when(
      price > quantile(price, 0.99) ~ quantile(price, 0.99),
      price < quantile(price, 0.01) ~ quantile(price, 0.01),
      TRUE ~ price
    )
  )

# Step 6: Neighborhood Filtering + Drop Factor Levels
df_filtered <- df_clean %>%
  group_by(neighbourhood_cleansed) %>%
  filter(n() >= 30) %>%
  ungroup() %>%
  mutate(
    neighbourhood_cleansed = droplevels(as.factor(neighbourhood_cleansed)),
    room_type = as.factor(room_type)
  )
  mutate(neighbourhood_cleansed = droplevels(neighbourhood_cleansed))

# Step 7: Remove Duplicates
df_filtered <- df_filtered[!duplicated(df_filtered), ]

df_filtered$log_price <- log1p(df_filtered$price)

# Step 8: Final Check
glimpse(df_filtered)  
summary(df_filtered$occupancy_rate) 
summary(df_filtered[c("occupancy_rate", "price")])
table(df_filtered$neighbourhood_cleansed)

# Step 9: Save Cleaned Data
fwrite(df_filtered, "paris_airbnb_cleaned.csv")

# ANOVA TEST
# One-way ANOVA: Neighborhood effect
anova_neighborhood <- aov(occupancy_rate ~ neighbourhood_cleansed, data = df_filtered)
summary(anova_neighborhood)

# Two-way ANOVA: Neighborhood + Room Type
anova_combined <- aov(occupancy_rate ~ neighbourhood_cleansed * room_type, data = df_filtered)
summary(anova_combined)

# Homogeneity of variances
car::leveneTest(occupancy_rate ~ neighbourhood_cleansed, data = df_filtered)

# Effect size (Eta-squared)
eta_squared(anova_neighborhood)
eta_squared(anova_combined)

# Kruskal Test
kruskal.test(occupancy_rate ~ neighbourhood_cleansed, data = df_filtered)

# Visualize ANOVA Results
# Boxplot: Occupancy Rate by Neighborhood (Top 20 neighborhoods for readability)
top_neighborhoods <- df_filtered %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(n = n()) %>%
  top_n(20, n) %>%
  pull(neighbourhood_cleansed)

df_filtered %>%
  filter(neighbourhood_cleansed %in% top_neighborhoods) %>%
  ggplot(aes(
    x = reorder(neighbourhood_cleansed, occupancy_rate, median),
    y = occupancy_rate,
    fill = neighbourhood_cleansed
  )) +
  geom_boxplot(alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Occupancy Rate by Neighborhood (Top 20)",
    x = "Neighborhood",
    y = "Occupancy Rate"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "gray90")
  )

# Interaction Plot
df_filtered %>%
  group_by(neighbourhood_cleansed, room_type) %>%
  summarise(mean_occupancy = mean(occupancy_rate, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(
    x = neighbourhood_cleansed,
    y = mean_occupancy,
    color = room_type,
    group = room_type
  )) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Interaction: Neighborhood vs. Room Type",
    x = "Neighborhood",
    y = "Mean Occupancy Rate"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Tukey HSD Plot
tukey <- TukeyHSD(anova_neighborhood)

# Convert to a dataframe and clean column names
tukey_df <- as.data.frame(tukey$neighbourhood_cleansed) %>%
  tibble::rownames_to_column("comparison") %>%
  dplyr::rename(
    diff = diff,
    lwr = lwr,
    upr = upr,
    p.adj = `p adj`
  ) %>%
  dplyr::arrange(desc(abs(diff))) %>%
  dplyr::slice_head(n = 10) %>%
  dplyr::mutate(significance = ifelse(p.adj < 0.05, "Significant", "Non-significant"))

# Plot using the cleaned dataframe
ggplot(tukey_df, aes(
  x = reorder(comparison, diff),
  y = diff,
  fill = significance
)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Tukey HSD: Pairwise Differences in Occupancy Rates",
    x = "Neighborhood Comparison",
    y = "Difference in Occupancy Rate",
    fill = "Significance"
  ) +
  scale_fill_manual(values = c("Non-significant" = "gray", "Significant" = "red")) +
  theme_minimal()

# Q-Q Plot (For Residuals)
# Step 1: Create a dataframe of residuals
residuals_df <- data.frame(residuals = residuals(anova_neighborhood))

# Step 2: Plot Q-Q with explicit data
qqplot <- ggplot(residuals_df, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  labs(title = "Q-Q Plot for ANOVA Residuals") +
  theme_minimal()

# Step 3: Print the plot
print(qqplot)

