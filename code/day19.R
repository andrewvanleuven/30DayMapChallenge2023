# 19-11-2023	5-minute map	Spend no more than 5 minutes to prepare this map

library(tidyverse); library(sf); library(tidycensus); library(tigris)

car <- get_acs(geography = 'county', 
               variables = c(cars = 'B25046_001', #Aggregate number of vehicles available:
                             hh = 'B11001_001'), 
               year = 2021,
               state = 'PR',
               output = 'wide',
               geometry = T) |> 
  mutate(vehicles_per_household = carsE/hhE) |> 
  st_transform(3991)

pr <- states(cb = T) |> 
  filter(GEOID == '72') |> 
  st_transform(3991)

ggplot() +
  geom_sf(data = car, aes(fill = vehicles_per_household), linewidth = .1, color = 'white') +
  geom_sf(data = pr, alpha = 0, linewidth = .25, color = 'black') +
  scale_fill_viridis_c(option = 'mako', direction = -1, 
                       name = 'Vehicles per Household') +
  labs(title = 'Aggregate Number of Vehicles Available Per Household',
       subtitle = 'Puerto Rico, 5-Year Average by County (2017-2021)',
       caption = 'Source: U.S. Census 5-year ACS Estimates       ') +
  theme_void(base_family = 'JetBrains Mono') +
  theme(plot.caption = element_text(margin=margin(50,0,0,0)), 
        plot.subtitle = element_text(hjust = 0.5,margin=margin(0,0,30,0)), 
        plot.title = element_text(face = "bold", hjust = 0.5,margin=margin(0,0,10,0)), 
        legend.title = element_text(face = "bold", hjust = 0.5), 
        legend.position = 'bottom',
        legend.direction = "horizontal") +
  guides(fill = guide_colourbar(barheight = 0.35, barwidth = 15, title.position = "top"))

ggsave('plot/day19.jpg', height = 5, width = 8)
