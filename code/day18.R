# 18-11-2023	Atmosphere	Conditions of the atmosphere can be either weather or climate

library(tidyverse); library(sf); library(tidycensus); library(tigris); library(scales); library(janitor); library(rleuven); library(gganimate); library(gifski); library(kriging)

ok_mesonet_22 <- qs::qread('data/day18.qs') |> st_transform(4326) |> 
  filter(not_na(degrees_f),
         degrees_f > -10)
okla <- states(cb = T, resolution = '5m') |> filter(GEOID == 40)
okla_xy <- st_coordinates(okla) |> as.data.frame() |> select(x = X, y = Y)
ok_polygon <- list(data.frame(okla_xy$x, okla_xy$y))
dates_range <- seq.Date(as.Date('2022-01-01'),as.Date('2022-12-31'), by = 'day')

mesonet_krig <- function(thedate = '2022-05-04',thedata = ok_mesonet_22){
  pull <- thedata |> filter(date == thedate)
  date <- pull |> slice(1) |> pull(date)
  latlon <- as.data.frame(st_coordinates(pull))
  pull_xy <- cbind(pull,latlon)
  kriged <- kriging(pull_xy$X, pull_xy$Y, pull_xy$degrees_f, polygons = ok_polygon, pixels= 400)
  as.data.frame(kriged[['map']]) |> 
    st_as_sf(coords = c('x','y'), crs = 4326, remove = F) |>
    st_transform(6552) |> 
    mutate(date = as.Date(date))
}

tictoc::tic()
for (i in 1:365) {
  d <- dates_range[i]
  j <- mesonet_krig(d,ok_mesonet_22)
  ggplot() +
    geom_sf(data = j, aes(color = pred)) +
    scale_color_viridis_c(option = 'turbo', 
                          limits = c(min(ok_mesonet_22$degrees_f), max(ok_mesonet_22$degrees_f)),
                          name = 'Degrees Fahrenheit') +
    labs(title = 'Air Temperatures in Oklahoma',
         subtitle = format(as.Date(d),"%B %d, %Y"),
         caption = 'Source: Mesonet, 2023') +
    theme_void(base_family = 'Helvetica') +
    theme(plot.caption = element_text(margin=margin(10,0,10,20)),
          plot.subtitle = element_text(hjust = 0.5,margin=margin(0,0,15,0)),
          plot.title = element_text(face = "bold", hjust = 0.5,margin=margin(15,0,10,0)),
          legend.title = element_text(face = "bold", hjust = 0.5,margin=margin(30,0,0,0)),
          legend.position = 'bottom',
          legend.direction = "horizontal") +
    guides(color = guide_colourbar(barheight = 0.35, barwidth = 15, title.position = "top"))
  ggsave(sprintf('plot/day18/%s.jpg',d), height = 6, width = 8)
}
tictoc::toc()