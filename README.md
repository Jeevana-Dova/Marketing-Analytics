# Airbnb Occupancy Analysis – Paris

This project explores Airbnb occupancy trends in Paris, focusing on how **neighborhood** and **room type** affect booking rates. It uses statistical tests, regression modeling, and visualizations, all implemented in R.

## Overview

We investigated:
- Do occupancy rates differ by neighborhood?
- Does room type performance vary across locations?
- How do price, location, and competition affect occupancy?

**Data Source:** Inside Airbnb (public scraped data)

## Project Files
- `project.R`: Complete R script with data cleaning, ANOVA tests, Kruskal-Wallis, Tukey HSD, regression analysis, and all plots.
- `Project_ANOVA_Test.docx`: Summary report covering statistical tests, interpretation, and effect size analysis.
- `BIA_672_Final_Presenation.pptx`: Final slides presenting key findings, graphs, PCA, and recommendations.

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
