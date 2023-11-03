# 04-11-2023	A bad map	Let's get this over with

suppressPackageStartupMessages({library(tidyverse); library(sf); library(tigris); library(janitor); library(cowplot); library(rleuven)})

us <- counties(cb = T, resolution = '20m') |> 
  filter(STATEFP < 60,
         !STUSPS %in% c('AK','HI')) |> 
  mutate(`FIPS Code` = as.numeric(GEOID))

ggplot() +
  geom_sf(data = us, aes(fill = `FIPS Code`), color = 'black', linewidth = .1) + 
  scale_fill_gradient(high = 'green', low = 'red') +
  theme_minimal(base_family = 'Comic Sans MS') +
  labs(title = 'which State has \n the highest FIPS codes?',
       subtitle = 'a bad Map') +
  theme(
    plot.background = element_rect(fill = "lightyellow"),
    plot.title = element_text(size = 30, hjust = 0.25),
    plot.subtitle = element_text(size = 20, hjust = 0.75, color = "mediumvioletred", family = "serif"),
    plot.caption = element_text(size = 10, face = "italic", angle = 25),
    panel.background = element_rect(fill = 'lightblue', colour = 'darkred', size = 4),
    panel.border = element_rect(fill = NA, color = "green", size = 2),
    panel.grid.major.x = element_line(color = "purple", linetype = 2),
    panel.grid.minor.x = element_line(color = "orange", linetype = 3),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_text(face = "bold.italic", color = "blue"),
    axis.title.y = element_text(family = "mono", face = "bold", size = 20, hjust = 0.25),
    axis.text = element_text(face = "italic", size = 15, color = "blue"),
    axis.text.x.bottom = element_text(angle = 180, color = "blue"), 
    strip.background = element_rect(fill = "magenta"),
    strip.text.y = element_text(color = "white"),
    strip.placement = "outside",
    legend.background = element_rect(fill = "orangered4"),
    legend.key = element_rect(fill = "orange"),
    legend.justification = "left",
    legend.title = element_text(family = "serif", color = "white"),
    legend.text = element_text(family = "mono", face = "italic", color = "limegreen")
  ) 

# theme: https://gist.github.com/emilyriederer/2bf4f67d7e198f8359b61706c82e42ee

ggsave('plot/day04.jpg', height = 8, width = 8)
