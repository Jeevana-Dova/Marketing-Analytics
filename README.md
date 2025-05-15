# Airbnb Occupancy Analysis – Paris
This project explores Airbnb occupancy trends in Paris, focusing on how **neighborhood** and **room type** affect booking rates. It uses statistical tests, regression modeling, and visualizations, all implemented in R.

## Overview
We investigated:
- Do occupancy rates differ by neighborhood?
- Does room type performance vary across locations?
- How do price, location, and competition affect occupancy?

## Dataset Source
The dataset used in this project was downloaded from [Inside Airbnb](https://insideairbnb.com/), a publicly available data platform that provides detailed listings scraped from Airbnb’s website.

Specifically, we used:
- **City**: Paris, France
- **File**: `listings.csv` (detailed listing data including availability, price, location, room type, etc.)

Inside Airbnb is a valuable open dataset for analyzing trends in short-term rentals across global cities.

## Project Files
- `Statistical_Analysis.R`: Complete R script with data cleaning, ANOVA tests, Kruskal-Wallis, Tukey HSD, and plots.

- `Project_Regression.R`: This script answers key research questions using regression models:
  - How does **price** affect **occupancy rate**?
  - Does **proximity to attractions** (like the Eiffel Tower) increase bookings?
  - Does **competition** (host saturation) reduce performance?

- `Project Presenation.pptx`: Final slides presenting key findings, graphs, PCA, and recommendations.

## Key Techniques
- **One-Way & Two-Way ANOVA**
- **Levene’s Test & Kruskal-Wallis**
- **Tukey’s HSD for Post-hoc**
- **Beta Regression & Negative Binomial Regression**
- **Principal Component Analysis (PCA)**
- **Boxplots, Interaction Plots, Heatmaps, Q-Q Plots**

## Insights
- Neighborhood significantly affects occupancy, but the effect size is small (η² ≈ 2.2%).
- Room types like **private rooms** and **entire homes** show stable demand.
- **Shared rooms** perform well in select areas like Bourse and Élysée.
- Price and host competition negatively affect occupancy.
- Proximity to attractions (like the Eiffel Tower) increases booking rates.

## Recommendations
- Convert low-performing hotel rooms to private/shared options.
- Target high-demand, low-supply areas (e.g., Élysée).
- Use pricing elasticity and review frequency to adjust listing strategy.

---

## Requirements
- R 4.x
- Libraries: `ggplot2`, `dplyr`, `ggpubr`, `rstatix`, `car`, `data.table`

```r
install.packages(c("ggplot2", "dplyr", "ggpubr", "rstatix", "car", "data.table"))
