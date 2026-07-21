# Load packages
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load data
data <- read_dta(file.choose())

# Check variable names
names(data)

# Keep only variables needed for Figure 1
fig1_raw <- data %>%
  select(
    T,
    prop_wood0, prop_wood1,
    prop_eleccook0, prop_eleccook1,
    prop_elec0, prop_elec1,
    prop_candles0, prop_candles1
  )

# View clean Figure 1 data
View(fig1_raw)

# Check how many observations are in project/non-project areas
table(fig1_raw$T, useNA = "ifany")

#average fractions by electricity project status
fig1_summary <- fig1_raw %>%
  group_by(T) %>%
  summarise(
    
    wood_cooking_1996 = mean(prop_wood0, na.rm = TRUE),
    wood_cooking_2001 = mean(prop_wood1, na.rm = TRUE),
    
    electric_cooking_1996 = mean(prop_eleccook0, na.rm = TRUE),
    electric_cooking_2001 = mean(prop_eleccook1, na.rm = TRUE),
    
    electric_lighting_1996 = mean(prop_elec0, na.rm = TRUE),
    electric_lighting_2001 = mean(prop_elec1, na.rm = TRUE),
    
    candles_1996 = mean(prop_candles0, na.rm = TRUE),
    candles_2001 = mean(prop_candles1, na.rm = TRUE)
  )

# Reshape data from wide to long format for ggplot
fig1_long <- fig1_summary %>%
  pivot_longer(
    cols = -T,
    names_to = c("category", "year"),
    names_pattern = "(.*)_(1996|2001)",
    values_to = "fraction"
  ) %>%
  mutate(
    project_status = ifelse(
      T == 1,
      "Panel A. Electricity project areas",
      "Panel B. Areas with no electricity projects"
    ),
    
    category = recode(
      category,
      "wood_cooking" = "Wood cooking",
      "electric_cooking" = "Electric cooking",
      "electric_lighting" = "Electric lighting",
      "candles" = "Candles"
    ),
    
    category = factor(
      category,
      levels = c(
        "Wood cooking",
        "Electric cooking",
        "Electric lighting",
        "Candles"
      )
    ),
    
    year = factor(year, levels = c("1996", "2001")),
    
    project_status = factor(
      project_status,
      levels = c(
        "Panel A. Electricity project areas",
        "Panel B. Areas with no electricity projects"
      )
    )
  )

# View long-format plotting data
View(fig1_long)

# Make Figure 1
fig1_plot <- ggplot(fig1_long, aes(x = category, y = fraction, fill = year)) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65,
    color = "black"
  ) +
  facet_wrap(~ project_status, nrow = 1) +
  scale_y_continuous(
    limits = c(0, 0.9),
    breaks = seq(0, 0.9, 0.1)
  ) +
  labs(
    title = "Changing Home Production Techniques by Electricity Project Status",
    x = "",
    y = "Fraction of households",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "top"
  )

# Show plot
fig1_plot

# Save plot
ggsave(
  filename = "fig1_replication.png",
  plot = fig1_plot,
  width = 11,
  height = 5,
  dpi = 300
)

