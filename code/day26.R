# 26-11-2023	Minimal	Less is more

library(tidyverse); library(sf); library(tidycensus); library(tigris); library(magrittr); library(janitor); library(rleuven); library(osmdata); library(mapview); library(mapedit)

b2 <- 'Madison, Wisconsin'

# dt_bbox <- drawFeatures()
# dt_crs <- crsuggest::suggest_crs(dt_bbox) |> slice(1) |> pull(crs_code) |> as.numeric()
# dt_bbox <- dt_bbox |> st_transform(dt_crs) |> select(geometry)
# st_write(dt_bbox,'data/day26.geojson')
dt_bbox <- st_read('data/day26.geojson')
dt_crs <- crsuggest::suggest_crs(dt_bbox) |> slice(1) |> pull(crs_code) |> as.numeric()
mapview(dt_bbox)  

dt_bldgs <- getbb(b2) %>% 
  opq(timeout = 25*100) %>%
  add_osm_feature(key = 'building') %>% 
  osmdata_sf() %$% 
  osm_polygons %>%
  st_transform(dt_crs) %>% 
  st_intersection(dt_bbox) 
beepr::beep()

mapview(dt_bldgs)  

ggplot() +
  geom_sf(data = dt_bldgs, fill = 'white', color = '#C5050C') +
  theme_void(base_family = 'Red Hat Display') +
  labs(title = 'Downtown Madison, Wisconsin',
       caption = 'Source: OpenStreetMap, 2023') +
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 30, color = 'white',
                                  margin=margin(15,0,10,0)),
        plot.caption = element_text(color = 'white',margin=margin(10,0,10,20)),
        panel.background = element_rect(fill = '#C5050C', color = NA),
        plot.background = element_rect(fill = '#C5050C', color = NA))

ggsave('plot/day26.jpg', dpi = 600, height = 8, width = 8)
