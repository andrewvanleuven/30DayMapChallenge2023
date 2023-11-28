# 27-11-2023	Dot	Dot density, a single dot in space or something different

library(tidyverse); library(sf); library(tidycensus); library(tigris); library(scales); library(janitor); library(rleuven)


# Identify variables for mapping

okla_base <- counties(40,T,'5m')

okla_race <- get_decennial(
  geography = 'block',
  variables = c(Native = 'P1_005N'),
  state = 'OK', 
  geometry = TRUE,
  year = 2020)

sum(okla_race$value)

tictoc::tic()
okla_dots <- as_dot_density(okla_race, 
                            value = 'value', 
                            values_per_dot = 10, 
                            group = 'variable')
tictoc::toc()
beepr::beep()

ok_hwy <- primary_secondary_roads(40) |> 
  filter(RTTYP == 'I')


ggplot() +
  geom_sf(data = okla_base, fill = 'grey95', color = 'black', linewidth = .2) +
  geom_sf(data = okla_base |> st_dissolve(), alpha = 0, color = 'black', linewidth = .3) +
  geom_sf(data = ok_hwy, color = 'black', linewidth = .3) +
  geom_sf(data = okla_dots |> filter(variable == 'Native'),
          aes(color = variable), size = .35, stroke = 0, alpha = .7) +
  theme_void(base_family = 'Copperplate') + 
  labs(title = 'Native American Population of Oklahoma',
       subtitle = 'Census Respondents Identifying as \"American Indian or Alaska Native\"\n1 Dot = 10 people',
       caption = 'Source: U.S. 2020 Decennial Census         ') +
  coord_sf(crs = 6552) +
  scale_color_manual(values = c('dodgerblue', 'black'), name = '',
                     labels = c('American Indian and/or Alaska Native')) +
  guides(color=guide_legend(nrow = 2,override.aes = list(size = 3, alpha = 1))) +
  theme(plot.title = element_text(face = 'bold', hjust = .5, size = 25, margin=margin(15,0,10,0)),
        plot.subtitle = element_text(hjust = .5, margin=margin(0,0,10,0), lineheight = 1.2),
        plot.caption = element_text(margin=margin(10,10,10,10)),
        legend.position = 'none')

ggsave('plot/day27.jpg', dpi = 600, height = 8, width = 12)

