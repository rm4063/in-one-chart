require(tidyverse)
require(readr)
require(ggplot2)

setwd('~/Desktop')


fire_df <- data.frame(
  YEAR = 2015:2024,
  FEDERAL = c(2131, 1976, 2918, 3143, 1590, 2274, 4389, 3549, 3166, NA),
  CALIFORNIA = c(608, 534, 947, 890, 448, 1230, 896, 601, 915, 699)
)

fire_df <- fire_df %>%
  mutate(FEDERAL = ifelse(YEAR == 2024, NA, FEDERAL),  
         CALIFORNIA = replace_na(CALIFORNIA, 0))      

long_df <- fire_df %>%
  pivot_longer(
    cols = c("FEDERAL", "CALIFORNIA"), 
    names_to = "Source", 
    values_to = "Amount"
  ) %>%
  filter(!is.na(Amount)) %>%  
  mutate(Source = factor(Source, levels = c("FEDERAL", "CALIFORNIA")))

totals <- long_df %>%
  group_by(Source) %>%
  summarize(Total = sum(Amount, na.rm = TRUE))
federal_total <- scales::comma(totals$Total[totals$Source == "FEDERAL"])
california_total <- scales::comma(totals$Total[totals$Source == "CALIFORNIA"])

ggplot(long_df, aes(x = YEAR, y = Amount, fill = Source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7, alpha = 0.8) +
  
  geom_bar(data = subset(long_df, YEAR == 2020 & Source == "CALIFORNIA"),
           aes(x = YEAR, y = Amount),
           stat = "identity",
           position = position_dodge(width = 0.7),
           width = 0.7,
           fill = "#94c6a3",
           color = "darkgreen",
           linewidth = 1) +
  
  annotate("curve",
           x = 2020.3, y = 1700,
           xend = 2020, yend = 1600,
           curvature = 0.2,
           arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("text",
           x = 2020.5, y = 1800,
           label = "At a maximum of \n$1,230M in state spending in last 10 years",
           hjust = 0,
           size = 3.5,
           fontface = "bold") +
  

  annotate("text",
           x = 2021.5, y = 4700,
           label = "Max Federal spending\nof $4,389M in 2021",
           hjust = 0,
           size = 3.5) +
  
  geom_text(
    aes(label = scales::comma(Amount)),
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 3
  ) +
  
  labs(
    title = "California Fire Suppression Cost: Fighting Wildfires in the Last Decade",
    subtitle = paste0("Total Federal Spending: $", federal_total, "M, State Spending: $", california_total, "M"),
    caption = "Note: 2024 Federal data not yet available\nSources: **National Interagency Fire Center (Federal data), **California Department of Forestry and Fire Protection (State data)",
    x = "Year",  
    y = "Expenditure (US$ Millions)"
  ) +
  scale_fill_manual(values = c("FEDERAL" = "#7cb4c4", "CALIFORNIA" = "#94c6a3")) + 
  theme_minimal() +
  
  scale_y_continuous(
    breaks = seq(0, max(long_df$Amount, na.rm = TRUE) + 500, by = 500),
    labels = scales::comma_format(),
    expand = expansion(mult = c(0, 0.1))
  ) +  
  
  scale_x_continuous(
    breaks = 2015:2024,
    labels = 2015:2024
  ) + 
  
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.2),
    plot.margin = margin(20, 10, 20, 10),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.caption = element_text(hjust = 0, size = 8, color = "grey40", lineheight = 1.2)
  )
