# 17-11-2023	Flow	Flow of transport, people

suppressPackageStartupMessages({library(ggpubr); library(sf); library(nngeo); library(tidyverse); library(janitor); library(tigris)})
options(scipen = 999)

# df <- read_csv('/Users/andrew/Desktop/misc/FAF5.5.1_State/FAF5.5.1_State.csv') |> 
#   select(1:11) |> 
#   filter(sctg2 == 2,
#          dms_origst == 19,
#          tons_2017 > 0,
#          dms_mode %in% c(1:3))
# write_csv(df,'data/faf5_ia_cereal.csv')

df <- read_csv('data/faf5_ia_cereal.csv') |> 
  mutate(mode = case_when(dms_mode == 1 ~ 'Truck',
                          dms_mode == 2 ~ 'Rail',
                          dms_mode == 3 ~ 'Water'))

usa <- states(cb = T) |> 
  shift_geometry() |>
  filter(STATEFP < 60, !STUSPS %in% c('HI','AK')) 

us_dots <- usa |> 
  select(state = STUSPS, st = STATEFP, geometry) |> 
  st_centroid() 

st_aggregate <- df |> 
  summarize(tons_2017 = sum(tons_2017), 
            .by = c(dms_destst,mode)) |> 
  mutate(dms_destst = str_pad(dms_destst,width = 2, pad = '0'))

us48 <- arrange(us_dots,st) |> pull(state)

st_freight_line <- function(the_data,st1 = 'IA', st2, fmode){
  st_fips <- st_drop_geometry(us_dots) |> 
    filter(state == st2) |> 
    pull(st)
  tons <- the_data |> 
    filter(dms_destst == st_fips,
           mode == fmode) |> 
    pull(tons_2017)
  tons <- ifelse(is_empty(tons),0,tons)
  nngeo::st_connect(us_dots |> filter(state == 'IA'),
                    us_dots |> filter(state == st2)) |> 
    st_as_sf() |> 
    mutate(dest_state = st2,
           tonnage = tons,
           mode = fmode) |> 
    select(dest_state,tonnage,mode,geometry = x)
}

freight_lines_truck <- map_df(.x = us48,
                             .f = function(x){st_freight_line(the_data = st_aggregate,
                                                              st2 = x, 
                                                              fmode = 'Truck')})
freight_lines_rail <- map_df(.x = us48,
                             .f = function(x){st_freight_line(the_data = st_aggregate,
                                                              st2 = x, 
                                                              fmode = 'Rail')})

freight_lines <- bind_rows(freight_lines_truck, freight_lines_rail) |> 
  filter(dest_state != 'IA')

a <- ggplot() +
  geom_sf(data = usa, alpha = 0) +
  geom_sf(data = freight_lines |> filter(mode == 'Truck', tonnage > 0), size = .1,
          aes(linewidth = tonnage), color = 'dodgerblue', alpha = .6) +
  theme_void(base_family = 'Harding Text Web') + 
  labs(title = 'Truck') +
  scale_linewidth(labels = scales::comma, name = 'Freight\nTonnage') +
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 30),
        legend.title = element_text(hjust = .5, face = 'bold'))

b <- ggplot() +
  geom_sf(data = usa, alpha = 0) +
  geom_sf(data = freight_lines |> filter(mode == 'Rail', tonnage > 0), size = .1,
          aes(linewidth = tonnage), color = 'firebrick', alpha = .6) +
  theme_void(base_family = 'Harding Text Web') + 
  labs(title = 'Rail') +
  scale_linewidth(labels = scales::comma, name = 'Freight\nTonnage') +
  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 30),
        legend.title = element_text(hjust = .5, face = 'bold'))

c <- ggarrange(a, NULL, b, NULL, nrow = 1,
               ncol = 4, widths = c(1, 0.1, 1, 0.05))
annotate_figure(c, 
                top = text_grob('Outward Flow of Freight From Iowa in 2017',
                                   family = 'Harding Text Web', vjust = 1,
                                   color = 'Black', face = 'bold', size = 35),
                bottom = text_grob('Source: BTS Freight Analysis Framework      ', 
                                   color = 'black', family = 'Harding Text Web',
                                   hjust = 1, x = 1, vjust = 0, size = 10))

ggsave('plot/day17.jpg', height = 6, width = 12, dpi = 600)
beepr::beep()
