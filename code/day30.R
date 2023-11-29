# 30-11-2023	"My favorite.."	You choose!

library(tidyverse); library(sf); library(tidycensus); library(tigris); library(magrittr); library(janitor); library(rleuven); library(osmdata); library(mapview); library(mapedit)

# rect_bbox <- drawFeatures()
# rect_crs <- crsuggest::suggest_crs(rect_bbox) |> slice(1) |> pull(crs_code) |> as.numeric()
# rect_bbox <- rect_bbox |> st_transform(rect_crs) |> select(geometry)
# st_write(rect_bbox,'data/day30.geojson')
rect_bbox <- st_read('data/day30.geojson')
rect_crs <- crsuggest::suggest_crs(rect_bbox) |> slice(1) |> pull(crs_code) |> as.numeric()
mapview(rect_bbox)  

rect_counties <- counties('PA',T,'5m') |> 
  st_transform(rect_crs) |> 
  st_intersection(rect_bbox)

rect_ctys <- pull(rect_counties,NAME)

dt_water <- map_df(.x = rect_ctys,
                .f = function(x){area_water('PA',x)}) |> 
  st_transform(rect_crs) |> 
  st_intersection(rect_bbox)

dt <- places('PA',T) |> 
  st_transform(rect_crs) |> 
  st_intersection(rect_bbox)

dt_rds <- map_df(.x = rect_ctys,
              .f = function(x){roads('PA',x)}) |> 
  st_transform(rect_crs) |> 
  st_intersection(rect_bbox) |> 
  st_collection_extract(type = 'LINESTRING') |> 
  filter(not_na(RTTYP))

mapview(rect_bbox)

ggplot() +
  geom_sf(data = rect_bbox, color = 'black', fill = '#FDFFFC') +# alpha = 0) +
  geom_sf(data = st_dissolve(dt), color = 'orange', fill = '#F5E7A4', alpha = .7, linewidth = .1) +
  geom_sf(data = dt_water, fill = '#80D2E9', color = NA) +
  geom_sf(data = dt_rds, aes(color = RTTYP, linewidth = RTTYP)) +
  geom_sf(data = rect_bbox, color = 'black', alpha = 0) +
  scale_color_manual(values = c('#4883C5','#202020','#AE516B','#A66684','#EFA839')) +
  scale_linewidth_manual(values = c(0.8,0.1,0.4,0.25,0.5)) +
  theme_void(base_family = 'Frutiger LT 55 Roman') + 
  coord_sf(crs = 4326) +
  labs(title = 'Towns Along the Monongahela',
       subtitle = 'Comprising Portions of Greene, Westmoreland, Allegheny,\nWashington, and Fayette Counties',
       caption = 'Source: Census TIGER Line Files, 2023       \n      ') +
  theme(plot.title = element_text(hjust = .5, face = 'bold', margin=margin(15,0,10,0), size = 30,
                                  family = 'Frutiger LT 45 Light'),
        plot.subtitle = element_text(hjust = .5, margin=margin(0,0,-20,0)),
        plot.caption = element_text(margin=margin(-10,0,0,0)),
        legend.position = 'none')

ggsave('plot/day30.jpg', dpi = 600, height = 11, width = 7.5)

  
       