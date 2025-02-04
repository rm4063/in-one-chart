require('tidyverse')
require("readr")
require("ggplot2")

ggplot()

setwd('~/Desktop')

fire_df = read_csv('fire_fund.csv')


fire_df = fire_df %>%
  mutate(total_funds = FEDERAL + CALIFORNIA) %>%
  mutate(percentage_california = (CALIFORNIA / total_funds) * 100,
         percentage_federal = (FEDERAL / total_funds) * 100)

long_df = fire_df %>%
  pivot_longer(cols = c(percentage_federal, percentage_california), 
               names_to = "Source", 
               values_to = "Percentage")

long_df$Source = factor(long_df$Source, levels = c("percentage_federal", "percentage_california"))

ggplot(long_df) +
  aes(x = YEAR, y = Percentage, fill = Source) +
  geom_bar(stat = "identity", 
           alpha = ifelse(long_df$Source == "percentage_federal", 0.2, 1)) +  
  geom_text(
    data = long_df %>% filter(Source == "percentage_california"),  
    aes(label = scales::percent(Percentage / 100, accuracy = 0.1)),
    position = position_stack(vjust = 0.5),  
    angle = 90  
  ) +
  geom_text(
    data = long_df %>% filter(Source == "percentage_federal"),  
    aes(label = scales::comma(FEDERAL)),
    position = position_stack(vjust = 1.1), 
    size = 3, # 
    color = "black"
  ) +
  labs(
    title = "California Fire Suppression Expenditure as Percentage in the last 10 years",
    subtitle = "Maximum Funds were spend for the state for 2020 California Wildfires",
    x = "Year",
    y = "Federal Amounts in US$ Millions"
  ) +
  scale_fill_manual(values = c("percentage_california" = "#4682B4")) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1))