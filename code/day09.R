# 09-11-2023	Hexagons	6 sides, 6 angles, and 6 vertices

suppressPackageStartupMessages({library(tidyverse); library(sf); library(tigris); library(janitor); library(cowplot); library(rleuven); library(SpatialKDE)})
m_to_mi <- function(m){m*1609}

il <- tracts(state = 'IL', cb = T) |> 
  st_transform(6456) |> 
  select(tract_fips = GEOID)

ill <- states(cb = T) |> 
  filter(STUSPS == 'IL') |> 
  st_transform(6456)

illc <- counties('IL',cb = T) |> 
  # filter(STUSPS == 'IL') |> 
  st_transform(6456)

df <- qs::qread('data/day09.qs') |> 
  filter(st == 'IL') |> 
  st_transform(6456)

il_lenders <- st_intersection(df,il)


# Density Estimation ------------------------------------------------------
cell_size <- m_to_mi(5)
band_width <- m_to_mi(10)

lender_density <- il_lenders |>
  kde(band_width = band_width, kernel = "epanechnikov", 
      grid = ill |> create_grid_hexagonal(cell_size = cell_size, side_offset = band_width)) %>%
  st_intersection(ill,.) |>
  mutate(z = round((kde_value - mean(kde_value)) / sd(kde_value),2))

krnl <- ggplot() +
  geom_sf(data = lender_density, aes(fill = kde_value), linewidth = .05, color = "black") +
  geom_sf(data = ill, alpha = 0, color = 'black', size = .2) +
  scale_fill_viridis_c(option = 'rocket', direction = -1, 
                       name = 'Lender Density') +
  theme_void(base_family = 'CMU Sans Serif') +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.title = element_text(face = "bold", hjust = 0.5),
        legend.position = 'bottom') +
  guides(fill = guide_colourbar(barheight = 0.35, barwidth = 15, title.position = "top"))

beepr::beep()

# Tract Count -------------------------------------------------------------

trct_df <- tracts('IL', cb = T) |> 
  select(trct_fips = GEOID, st = STUSPS) |> 
  inner_join(read_csv('/Users/andrew/Documents/GitHub/lendR/data/tract_distance_based.csv')) |> 
  st_transform(6456)

trct_df <- il |>
  left_join(trct_df) 

count <- ggplot() +
  geom_sf(data = trct_df, aes(fill = (trct_lenders_05)), size = .05, color = NA) +
  geom_sf(data = illc, alpha = 0, color = 'black', size = .25) +
  # geom_sf(data = lender_df) +
  scale_fill_viridis_c(option = 'rocket', direction = -1, name = 'Tract Lender Count', labels = c('0','5','50','500'), trans = 'pseudo_log',
                       breaks = c(0,5,50,500)) +
  theme_void(base_family = 'CMU Sans Serif') +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.title = element_text(face = "bold", hjust = 0.5),
        legend.position = 'bottom') +
  guides(fill = guide_colourbar(barheight = 0.35, barwidth = 15, title.position = "top"))


library(ggpubr)

ggarrange(count,krnl)
ggsave('plot/day09a.jpg', width = 10)
