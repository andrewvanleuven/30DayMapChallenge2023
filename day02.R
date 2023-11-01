#	02-11-2023	Lines	A map with lines

suppressPackageStartupMessages({library(tidyverse); library(sf); library(ggpubr); library(tigris); library(janitor); library(rvest); library(mapboxapi); library(rleuven)})

# https://hub.arcgis.com/datasets/esri::usa-freeway-system-over-1500k

usa <- states(T,'20m') |> 
  shift_geometry() |> 
  filter(STATEFP < 60, !STUSPS %in% c('HI','AK'))

fwy <- st_read('data/us_fwy.geojson') |> 
  clean_names() |> 
  mutate(five = ifelse(number %in% seq(5, 100, by = 10) & class == 'I'| number %in% c('35W','35E'), 1, 0)) |> 
  shift_geometry() |> 
  st_intersection(usa)

# I had to go in and manually draw a few segments that were missing from the source data
# library(mapview); library(mapedit)
# incomplete <- mapview(fwy |> filter(five == 1)) %>%
#   editMap() 
# st_write(incomplete, 'data/fwy_segments.geojson')

fwy_segments <- st_read('data/fwy_segments.geojson') |> select(geometry)

ggplot() + 
  geom_sf(data = usa, fill = '#ffe8c4', alpha = .7, color = 'black', linewidth = .2) + 
  geom_sf(data = fwy, aes(color = factor(five), alpha = factor(five), linewidth = factor(five))) + 
  geom_sf(data = fwy_segments, color = '#bb0000', linewidth = .75) + 
  theme_void(base_family = 'CMU Sans Serif') +
  scale_color_manual(values = c('grey30','#bb0000')) +
  scale_linewidth_manual(values = c(.3,.75)) +
  scale_alpha_manual(values = c(.2,1)) +
  guides(color=guide_legend(ncol=2,override.aes = list(size = 3, alpha = 1))) +
  theme(legend.position = 'none')

ggsave('plot/day02.png', dpi = 600, height = 7.5)

