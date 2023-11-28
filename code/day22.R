# 22-11-2023	North is not always up	⬆️

library(tidyverse); library(sf); library(tidycensus); library(tigris); library(scales); library(janitor); library(rleuven); library(ggspatial)

# rivers <- st_read('/Users/andrew/Desktop/rv16my07/rv16my07.shp')
# arky <- rivers |> 
#   filter(PNAME == 'ARKANSAS R')
# st_write(arky, 'data/day22.geojson')
crs <- '+proj=omerc +lonc=90 +lat_0=40 +gamma=-120 +alpha=0'

arky <- st_read('data/day22.geojson') |> 
  st_transform(crs)

bbox <- st_bbox(arky) |> st_as_sfc() |> st_as_sf()

ark_st <- states(cb = T, resolution = '20m') |> 
  filter(STUSPS %in% c('AR','OK','KS','CO','MO')) |> 
  st_transform(crs) |> 
  st_intersection(bbox)

mapview::mapview(ark_st)

ark_cities <- places(state = c('AR','OK','KS','CO'), cb = T) |> 
  mutate(city_name = paste0(NAME, ', ', STUSPS)) |> 
  select(city_name,geometry) |> 
  filter(city_name %in% c(
    'Pine Bluff, AR',
    'Little Rock, AR',
    'Fort Smith, AR',
    'Muskogee, OK',
    'Tulsa, OK',
    'Ponca City, OK',
    'Wichita, KS',
    'Hutchinson, KS',
    'Dodge City, KS',
    'Garden City, KS',
    'Lamar, CO',
    'La Junta, CO',
    'Pueblo, CO',
    'Cañon City, CO'
  )) |> 
  st_transform(crs) |> 
  st_centroid() |> 
  st_centroid_xy() |> 
  mutate(labx = -10070000)

# i <- st_intersection(ark_st |> filter(STUSPS != 'MO'), ark_st |> filter(STUSPS != 'MO'))
# i2 <- i[i$STUSPS != i$STUSPS.1,]
# plot(i2$geometry)

# mapview::mapview(bbox)

ggplot() +
  geom_sf(data = st_buffer(bbox,300000), color = NA, alpha = 0) +
  geom_sf(data = ark_st, color = 'white', alpha = 0) +
  geom_sf(data = arky, color = '#04d9ff') +
  geom_sf(data = ark_cities, color = 'yellow', size = 2) +
  geom_text(data = ark_cities, size = 1.9, hjust = 0, color = 'white', family = 'Monoid',
            aes(label = city_name, x = labx, y = y)) +
  theme_void(base_family = 'Monoid') +
  labs(title = 'Major Towns and Cities\nAlong the Arkansas River',
       caption = 'Source: NWS Hydrologic Automated Basin Boundary System') +
  annotation_north_arrow(which_north = 'true', location ='bl',
                         style = north_arrow_fancy_orienteering(line_col = 'white',
                                                                fill = c('black', 'white'),
                                                                text_col = 'white',
                                                                text_family = 'Monoid')) +
  theme(panel.background = element_rect(fill = 'black', color = NA),
        plot.background = element_rect(fill = 'black', color = NA),
        plot.caption = element_text(color = 'white', size = 5),
        plot.title = element_text(color = 'white', face = "bold", lineheight=1.2,
                                  size = 20, hjust = 0.5))

ggsave('plot/day22.jpg', height = 8, width = 8, dpi = 600)

