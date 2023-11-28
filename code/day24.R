# 24-11-2023	Black & white	Only two colors allowed

library(tidyverse); library(sf); library(tidycensus); library(tigris); library(scales); library(janitor); library(rleuven); library(mapedit); library(mapview); library(magrittr)

oreo <- mapedit::drawFeatures()

ks <- counties('KS',T,'5m') |> 
  st_transform(6925)

oreo_ks <- ks |> 
  st_intersection(oreo |> st_transform(6925)) |> 
  mutate(filling = 1) |> 
  select(GEOID,filling) |> 
  st_drop_geometry()

oreo_ks_full <- ks |> 
  select(GEOID) |> 
  left_join(oreo_ks) |> 
  replace_na(list(filling = 0))

faux_oreo <- oreo_ks_full |> 
  slice(2:3) |> 
  st_centroid()

ks_st <- st_dissolve(ks)

ggplot() +
  geom_sf(data = faux_oreo, aes(fill = factor(filling)), 
          shape = 21, size = 4, color = 'black') +
  geom_sf(data = oreo_ks_full |> filter(filling == 0), fill = 'black', color = 'white') +
  geom_sf(data = oreo_ks_full |> filter(filling == 1), fill = 'white', color = 'black') +
  geom_sf(data = ks_st, color = 'black', alpha = 0) +
  ggtitle('Kansas as an Oreo') +
  theme_void(base_family = 'Oreos', base_size = 18) + 
  scale_fill_manual(values = c('black', 'white'), labels = c('Cookie', 'Creme'), name = '') +
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 30),
        legend.title = element_text(hjust = .5, face = 'bold'),
        legend.position = 'bottom')
ggsave('plot/day24.jpg', dpi = 600, height = 6, width = 8)
