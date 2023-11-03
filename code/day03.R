# 01-11-2023	03-11-2023	Polygons	A map with polygons

suppressPackageStartupMessages({library(tidyverse); library(sf); library(tigris); library(janitor); library(cowplot); library(rleuven)})

# https://opendataphilly.org/datasets/land-use/
phl_lu <- st_read('https://opendata.arcgis.com/datasets/e433504739bd41049de5d8f4a22d34ba_0.geojson')

phl <- places(state = 'PA', cb = T) |> 
  filter(NAME == 'Philadelphia') |> 
  st_transform(6526)

rowhouses <- phl_lu |> filter(C_DIG3 %in% c(121,124))

phl_rds <- roads(42,101) |> 
  st_transform(6526)

p <- ggplot() + 
  geom_sf(data = phl, color = NA, fill = '#010B12') + 
  geom_sf(data = phl_rds |> filter(RTTYP != 'M'), color = 'white', size = .1, alpha = .4) +
  geom_sf(data = rowhouses, color = NA, fill = '#39FF13') +
  labs(caption = "Source: OpenDataPhilly    \n ") +
  theme_void(base_family = 'IBM Plex Mono') +
  theme(panel.background = element_rect(fill = "gray85", color = NA),
        plot.background = element_rect(fill = "gray85", color = NA)) 

logo_file <- "data/rowhouses.png"
# Generated using Bing AI image creator

note1 <- expression('Parcels in Philadelphia, PA with a land-use')
note2 <- expression('classification of "Residential Rowhouse"')


j <- ggdraw(p) + 
  draw_image(logo_file, x = .9, y = .7, hjust = 1, vjust = 1, width = .2) +
  draw_label("Philadelphia's\nRowhouses", size = 22, fontfamily = 'IBM Plex Mono',
             fontface = 'bold', x = .9, y = .35, hjust = 1, vjust = 1) +
  draw_label(note1, size = 11, fontfamily = 'IBM Plex Mono', x = .9, y = .1, hjust = 1, vjust = 1) +
  draw_label(note2, size = 11, fontfamily = 'IBM Plex Mono', x = .9, y = .075, hjust = 1, vjust = 1) 
  
ggsave('plot/day03.jpg', j, dpi = 600)

