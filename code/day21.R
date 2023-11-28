# 21-11-2023	Raster	Pixels, please

library(tidyverse); library(sf); library(tidyterra); library(tigris); library(scales); library(janitor); library(rleuven); library(terra)

df <- rast('data/day21.TIF')
dfs <- rast('data/sullivan.TIF')

indy <- states(T,'5m') |> 
  filter(STUSPS == 'IN') |> 
  st_transform(3857)

sullivan <- counties('IN',T,'5m') |> 
  filter(NAME == 'Sullivan') |> 
  st_transform(3857)

sully_rds <- roads('IN', 'Sullivan') |> 
  st_transform(3857)

temp <- as.data.frame(df, xy = T)
sully_temp <- as.data.frame(dfs, xy = T)

# ggplot() +
#   geom_sf(data = sullivan, color = 'black', fill = 'cornsilk') +
#   geom_raster(data = sully_temp, aes(x = x, y = y), fill = 'forestgreen', alpha = .6) +
#   geom_sf(data = sully_rds, color = 'black', alpha = .75, size = .2) +
#   theme_void()
# ggsave('plot/day21a.jpg', dpi = 600, height = 10, width = 10)

ggplot() +
  geom_sf(data = indy, color = 'black', fill = 'cornsilk') +
  geom_raster(data = temp, aes(x = x, y = y), fill = 'forestgreen') +
  geom_sf(data = sullivan, color = 'white', alpha = 0, linewidth = 1.25) +
  geom_sf(data = sullivan, color = 'black', alpha = 0, linewidth = .5) +
  theme_void() -> rast_plot

ggsave('plot/day21.jpg', rast_plot, dpi = 600, height = 10, width = 10)
beepr::beep()

