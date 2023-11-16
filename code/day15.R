# 15-11-2023	OpenStreetMap	The greatest of the datasets. Remember to give credit.

lapply(c('tidyverse', 'ggplot2', 'osmdata', 'sf', 'mapview'), 
      library, character.only = T)
# Code from https://www.mzes.uni-mannheim.de/ -----------------------------
rotate_sf <- function(data, x_add = 0, y_add = 0) {
  
  shear_matrix <- function (x) { 
    matrix(c(2, 1.2, 0, 1), 2, 2) 
  }
  
  rotate_matrix <- function(x) { 
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
  }
  
  data %>% 
    dplyr::mutate(
      geometry = 
        .$geometry * shear_matrix() * rotate_matrix(pi / 20) + c(x_add, y_add)
    )
}

# Get OSM data ------------------------------------------------------------
okst <- getbb('Oklahoma State University') %>% 
  opq(timeout = 25*100) %>%
  add_osm_feature(
    key = 'amenity', 
    value = 'university') %>% 
  osmdata_sf() %$%
  osm_polygons %>% 
  filter(name == 'Oklahoma State University') %>% # filter on city level
  select(geometry) %>%
  st_transform(6552) 

roads1 <- getbb('Oklahoma State University') %>% 
  opq(timeout = 25*100) %>%
  add_osm_feature(key = 'highway') %>% 
  osmdata_sf() %$% 
  osm_lines %>% 
  st_transform(6552) %>% 
  st_intersection(okst) %>% 
  select(highway,geometry) |> 
  st_collection_extract(type = 'LINESTRING') |> 
  filter(!is.na(highway)) |> 
  select(geometry)

roads2 <-
  getbb('Oklahoma State University') %>% 
  opq(timeout = 25*100) %>%
  add_osm_feature(key = 'highway') %>% 
  osmdata_sf() %$% 
  osm_multipolygons %>% 
  st_transform(6552) %>% 
  st_intersection(okst) %>% 
  select(geometry)

buildings1 <- getbb('Oklahoma State University') %>% 
  opq(timeout = 25*100) %>%
  add_osm_feature(key = 'building') %>% 
  osmdata_sf() %$% 
  osm_polygons %>%
  st_transform(6552) %>% 
  st_intersection(okst) |> 
  filter(building %in% c('apartments','barn','church','civic','dormitory',
                         'mosque','office','parking', #'warehouse','greenhouse',
                         'roof','school','stadium','university', 'yes')) |> 
  select(geometry)

buildings2 <- getbb('Oklahoma State University') %>% 
  opq(timeout = 25*100) %>%
  add_osm_feature(key = 'building') %>% 
  osmdata_sf() %$% 
  osm_multipolygons %>%
  st_transform(6552) %>% 
  st_intersection(okst) |> 
  filter(building %in% c('apartments','barn','church','civic','dormitory',
                         'mosque','office','parking', #'warehouse','greenhouse',
                         'roof','school','stadium','university', 'yes')) |> 
  select(geometry)

buildings <- st_union(buildings1,buildings2)

parking <- getbb('Oklahoma State University') %>% 
  opq(timeout = 25*100) %>%
  add_osm_feature(key = 'amenity', value = c('parking')) %>% 
  osmdata_sf() %$% 
  osm_polygons %>%
  st_transform(6552) %>% 
  st_intersection(okst)

green1 <- getbb('Oklahoma State University') %>% 
  opq(timeout = 25*100) %>%
  add_osm_feature(key = 'landuse', 
                  value = c('grass','farmland','forest',
                            'meadow','recreation_ground')) %>% 
  osmdata_sf() %$% 
  osm_polygons %>%
  st_transform(6552) %>% 
  st_intersection(okst) |> 
  select(geometry)

green2 <- getbb('Oklahoma State University') %>% 
  opq(timeout = 25*100) %>%
  add_osm_feature(key = 'leisure', 
                  value = c('common','commons','garden','park',
                            'playground')) %>% 
  osmdata_sf() %$% 
  osm_polygons %>%
  st_transform(6552) %>% 
  st_intersection(okst) |> 
  select(geometry)

green3 <- getbb('Oklahoma State University') %>% 
  opq(timeout = 25*100) %>%
  add_osm_feature(key = 'natural', value = 'tree') %>% 
  osmdata_sf() %$% 
  osm_points %>%
  st_transform(6552) %>% 
  st_intersection(okst) |> 
  st_centroid() |> 
  st_buffer(5) |> 
  select(geometry)

green <- st_union(green1,green2,green3)
rm(green1,green2,green3)

sports <- getbb('Oklahoma State University') %>% 
  opq(timeout = 25*100) %>%
  add_osm_feature(key = 'leisure', 
                  value = c('sports_centre','pitch','track','swimming_pool',
                            'fitness_centre')) %>% 
  osmdata_sf() %$% 
  osm_polygons %>%
  filter(osm_id != 799114790,
         osm_id != 977585161,
         osm_id != 916688496) |> 
  st_transform(6552) %>% 
  st_intersection(okst)

mapview(green)

# 2-D Map -----------------------------------------------------------------
ggplot() +
  geom_sf(data = okst, fill = 'gold', alpha = .1) +
  geom_sf(data = green, fill = 'lightgreen', linewidth = .2, alpha = .8) +
  geom_sf(data = sports, fill = 'forestgreen', linewidth = .2, alpha = .8) +
  geom_sf(data = buildings, color = NA, fill = 'black', alpha = 1) +
  geom_sf(data = parking, color = NA, fill = 'grey', alpha = 1) +
  geom_sf(data = roads2, fill = 'grey', color = NA) +
  geom_sf(data = roads1, linewidth = .2, alpha = .8) +
  theme_void()

# 3-D map -----------------------------------------------------------------
unit <- 3000
ggplot() +
  geom_sf(data = rotate_sf(okst),fill = '#fe5c00',color = 'black', alpha = .1) +
  geom_sf(data = rotate_sf(roads1), linewidth = .25, alpha = .9, color='#fe5c00') +
  geom_sf(data = rotate_sf(okst),alpha = 0, linewidth = .7) +
  geom_sf(data = rotate_sf(okst, y_add = unit),fill = 'grey',alpha = .2) +
  geom_sf(data = rotate_sf(buildings, y_add = unit),color = NA, fill = 'black') +
  geom_sf(data = rotate_sf(roads2, y_add = unit), fill = 'grey35', color = NA) +
  geom_sf(data = rotate_sf(parking, y_add = unit),color = NA, fill = 'grey35') +
  geom_sf(data = rotate_sf(okst, y_add = unit),alpha = 0, linewidth = .7) +
  geom_sf(data = rotate_sf(okst, y_add = unit*2),fill = 'lightgreen',alpha = .1) +
  geom_sf(data = rotate_sf(green, y_add = unit*2),color = NA, fill = 'lightgreen') +
  geom_sf(data = rotate_sf(sports, y_add = unit*2),color = NA, fill = 'forestgreen') +
  geom_sf(data = rotate_sf(okst, y_add = unit*2),alpha = 0, linewidth = .7) +
  theme_void(base_family = 'Roboto') +
  labs(caption = 'Source: OpenStreetMap     \n      ')
ggsave('plot/day15.jpg', dpi = 600, height = 8, width = 16)
