# 20-11-2023	Outdoors	Map of mountains, trails, or something completely different

library(tidyverse); library(sf); library(tidycensus); library(tigris); library(scales); library(janitor); library(rleuven); library(gganimate); library(gifski); library(kriging)

parks <- st_read('/Users/andrew/Library/CloudStorage/GoogleDrive-vanleuven.andrew@gmail.com/My Drive/code_data/qol/data/USA_Parks/v108/park_dtl.gdb')

nj <- states(T,'5m') |> 
  filter(STUSPS == 'NJ') |> 
  st_transform(6527)

nj_parks <- parks |> 
  st_transform(6527) |> 
  st_intersection(nj)

nj_parks$type <- case_when(nj_parks$FEATTYPE %in% c('County park',
                                                    'Local park') ~ 'Local or County',
                           nj_parks$FEATTYPE %in% c('Regional park',
                                                    'State park or forest') ~ 'Regional or State',
                           nj_parks$FEATTYPE %in% c('National park or forest') ~ 'National Park/Forest') |> str_to_upper()

# mapview::mapview(nj_parks)

nj_counties <- counties('NJ', T) |> 
  st_transform(6527) 

nj_hwy <- primary_secondary_roads('NJ') |> 
  st_transform(6527) 

grn_colors <- RColorBrewer::brewer.pal(6, 'Greens')[2:6]

ggplot() +
  geom_sf(data = nj, linewidth = .2, color = 'black', 
          fill = 'brown', alpha = .125) +
  geom_sf(data = nj_parks, aes(fill = factor(type)), color = NA, show.legend = 'point') +
  geom_sf(data = nj_hwy |> filter(RTTYP %in% c('I')), 
          color = 'black', linewidth = .1, alpha = .6) +
  scale_fill_manual(values = rev(grn_colors),
                    name = 'TYPE OF PARK') +
  theme_void(base_family = 'National Park Variable', base_size = 30) +
  labs(caption = 'SOURCE: ESRI/TOMTOM (2010)',
       title = 'PARKS IN NEW JERSEY') +
  theme(plot.caption = element_text(hjust = .1, margin=margin(10,0,10,0)),
        # plot.subtitle = element_text(hjust = 0.5,margin=margin(0,0,15,0)),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 48,
                                  margin=margin(15,0,10,0)),
        legend.title = element_text(face = "bold", hjust = 0.5,margin=margin(30,0,10,0)),
        legend.spacing.y = unit(.4, 'cm')) +
  guides(fill = guide_legend(byrow = T, override.aes = list(shape = 21, size = 5))) 

# ggsave('plot/day20.jpg', dpi = 600, height = 8, width = 8)
 