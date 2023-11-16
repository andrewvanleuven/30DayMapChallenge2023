# 14-11-2023	Europe	The westernmost peninsulas of Eurasia

suppressPackageStartupMessages({library(tidyverse); library(sf); library(rvest); library(eurostat); library(janitor)})

wiki_buildings <- read_html('https://en.wikipedia.org/wiki/List_of_tallest_buildings_by_country') |> 
  html_table()

wiki_buildings_table <- countries[[2]] |> clean_names() |> 
  select(country = 1, 2:4, 6, 7)

map_nuts_0 <- eurostat::get_eurostat_geospatial(
  resolution = "20",
  nuts_level = "0",
  year = 2021) |> 
  st_transform(3035)

eu_xw <- read_csv('data/nuts.csv') |> 
  select(NUTS_ID = 2, country = 1)

eu_sf <- map_nuts_0 |> 
  left_join(eu_xw) |> 
  left_join(wiki_buildings_table) |> 
  separate(built, into = c('built','N')) |> 
  select(-N) |> 
  mutate(height_m = as.numeric(height_m),
         built = as.numeric(built),
         floors = as.numeric(floors))

# bb <- mapedit::drawFeatures()
cropped_eu <- st_intersection(eu_sf, (st_transform(bb,3035)))
mapview::mapview(cropped_eu)

ggplot() + 
  geom_sf(data = cropped_eu, aes(fill = height_m), color = 'white') +
  scale_fill_viridis_c(option = 'D', direction = 1, 
                       # labels = c('0','25','50','75','100+'),
                       # breaks = c(0,25,50,75,100),
                       # limits = c(0,100), oob = scales::squish,
                       guide = guide_colourbar(title = "Height (m) of Tallest Building", 
                                               title.position = "top", title.hjust = 0.5, 
                                               ticks = TRUE, label = TRUE,
                                               barheight = 0.35, barwidth = 15)) + 
  theme_void(base_family = 'Goldman Sans Condensed') + 
  theme(legend.title = element_text(hjust = 0.5, vjust = 0.5, face = 'bold'), 
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        panel.background = element_rect(fill = "#d9f5ff", color = NA),
        plot.background = element_rect(fill = "#d9f5ff", color = NA)) +
  labs(caption = "Source: Wikipedia                    \n   ")

ggsave('plot/day14.jpg', dpi = 600, height = 7, width = 10)


