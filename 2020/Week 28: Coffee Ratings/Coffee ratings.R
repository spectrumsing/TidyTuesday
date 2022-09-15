# Load libraries
library(tidyverse)
library(ggplot2)
library(ggdist)


# Read data
data <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

# Data preparation
df_coffee <- 
  data %>% 
  filter(total_cup_points != 0) %>% 
  group_by(country_of_origin) %>% 
  filter(n() > 25) %>% 
  group_by(country_of_origin) %>% 
  mutate(
    med = median(total_cup_points),
    n = n(),
    country_of_origin = case_when(country_of_origin == "United States (Hawaii)" ~ "United States",
                                  country_of_origin == "Tanzania, United Republic Of" ~ "Tanzania",
                                  TRUE ~ country_of_origin)
    ) %>% 
  ungroup() %>% 
  arrange(med) %>% 
  mutate(country_of_origin = factor(country_of_origin, levels = unique(country_of_origin)))


# Load font
windowsFonts(
  inter_title = windowsFont("Inter Semi Bold"), 
  inter_axis = windowsFont("Inter medium"),
  inter_body = windowsFont("Inter")
  )

# Plot
ggplot(
  df_coffee, 
  aes(x = total_cup_points, y = country_of_origin)
  ) +
  stat_dots(
    quantiles = 100,
    dotsize = 0.6,
    stackratio = 0.9,
    position = "dodge",
    justification = -0.04
    ) +
  stat_pointinterval(
    aes(interval_color = stat(level)),
    .width = c(0.25, 0.5, 0.95),
    interval_size = 10,
    point_size = 2.5,
    shape = 21,
    point_color = "#39312f",
    point_fill = "#FFFFFF",
    stroke = 1.25,
    position = "dodge",
    show.legend = F
    ) +
  scale_color_manual(
    values = c("#aa9486", "#d67237", "#5b1a18"),
    aesthetics = "interval_color"
    ) +
  geom_text(
    aes(x = med, y = country_of_origin, label = round(med, 1)),
        size = 4.5,
        vjust = 1.7,
        family = "inter_axis"
    ) +
  labs(
    title = "Coffee Quality Institute Grades by Country",
    subtitle = "points, 2010-2018",
    caption = "Source: Coffee Quality Institute Database by James LeDoux (@jldbc)"
    ) +
  theme(
    plot.background = element_rect(fill = "#edede9"),
    panel.background = element_rect(fill = "#edede9"),
    plot.margin = margin(10, 30, 10, 30),
    panel.grid = element_blank(),
    plot.title = element_text(family = "inter_title", 
                              size = 26, 
                              face = "bold", 
                              margin = margin(t = 10, b = 7)),
    plot.subtitle = element_text(family = "inter_axis",
                                 size = 18, 
                                 margin = margin(b = -10)),
    plot.caption = element_text(family = "inter_body",
                                size = 12,
                                color = "#aa9486",
                                margin = margin(t = 5, b = 5), 
                                hjust = 0.35),
    axis.title = element_blank(),
    axis.text = element_text(family = "inter_axis",
                             size = 16),
    axis.text.x = element_text(margin = margin(t = -10, b = 20)),
    axis.text.y = element_text(margin = margin(l = 10)),
    axis.ticks = element_blank()
    )

ggsave("cupping score.png", width = 12, height = 17)