# 13-11-2023	Choropleth	Classic thematic map: a choropleth

library(tidyverse); library(sf); library(tidycensus); library(tigris); library(scales); library(janitor); library(rleuven)

cty_sf <- counties(cb = T, resolution = '5m') |>
  shift_geometry() |> 
  select(GEOID, STUSPS, NAME) |> 
  filter(GEOID < 60,
         STUSPS != 'AK',
         STUSPS != 'HI')
st_sf <- st_dissolve(cty_sf,STUSPS)

rma_sf <- cty_sf |> select(GEOID) |> 
  left_join(read_csv('data/day13.csv'), by = 'GEOID')

ggplot() + 
  geom_sf(data = rma_sf, aes(fill = policies_acre*1000), color = 'black', linewidth = .05) + 
  geom_sf(data = st_sf, alpha = 0, color = 'black', linewidth = .15) + 
  scale_fill_viridis_c(option = 'A', direction = -1, 
                       labels = c('0','25','50','75','100+'),
                       breaks = c(0,25,50,75,100),
                       limits = c(0,100), oob = scales::squish,
                       guide = guide_colourbar(title = "Number of Crop Insurance Policies Sold\n(Per 1,000 Acres of Cropland), 2015–18", 
                                               title.position = "top", title.hjust = 0.5, ticks = TRUE, label = TRUE,
                                               barheight = 0.35, barwidth = 15)) + 
  theme_void(base_family = 'Goldman Sans Condensed') + 
  theme(legend.title = element_text(hjust = 0.5, vjust = 0.5, face = 'bold'), 
        legend.position = c(.2,.1),
        legend.direction = 'horizontal') +
  labs(caption = "Source: USDA Risk Management Agency, 2023  ")

ggsave('plot/day13.jpg', dpi = 600, height = 6, width = 9)
