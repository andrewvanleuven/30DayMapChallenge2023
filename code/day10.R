# 10-11-2023	North America	Northern part of the American continent

suppressPackageStartupMessages({library(tidyverse); library(sf); library(rvest); library(janitor); library(rnaturalearth)})

stadiums <- read_html('https://en.wikipedia.org/wiki/List_of_North_American_stadiums_by_capacity') |> 
  html_table() 

links <- read_html('https://en.wikipedia.org/wiki/List_of_North_American_stadiums_by_capacity') |> 
  html_nodes('td:nth-child(2) a') |> 
  html_attr('href') |> 
  as_tibble() |>
  rename(url = 1) |>
  mutate(url = paste0('https://en.wikipedia.org',url),
         rank = row_number()) 

df1 <- stadiums[[2]] |> 
  clean_names() |> 
  slice(-(1:2)) |> 
  filter(rank == min(rank), .by = country) |> 
  select(rank, stadium, capacity,country) |>
  left_join(links) |> 
  separate(capacity, into = c('a','b'), sep = ',') |> 
  mutate(b = str_sub(b, end = 3),
         capacity = paste0(a,b) |> as.numeric())|> 
  select(stadium,url,capacity,country)

links2 <- read_html('https://en.wikipedia.org/wiki/List_of_North_American_stadiums_by_capacity') |> 
  html_nodes('td:nth-child(2) a') |> 
  html_attr('href') |> 
  as_tibble() |>
  rename(url = 1) |>
  mutate(url = paste0('https://en.wikipedia.org',url),
         rank = row_number()-226) |> 
  filter(between(rank,1,42))

df2 <- stadiums[[5]] |> 
  clean_names() |> 
  slice(-(1:2)) |> 
  filter(rank == min(rank), .by = country) |> 
  select(rank, stadium, capacity,country) |>
  left_join(links2) |> 
  select(stadium,url,capacity,country) |> 
  mutate(capacity = str_remove_all(capacity,',') |> as.numeric())

df <- rbind(df1,df2) |> 
  filter(capacity == max(capacity), .by = country)

stadium_location <- function(i = 1,data = df){
  stadium_name <- data |> 
    slice(i) |> 
    pull(stadium)
  stadium_capacity <- data |> 
    slice(i) |> 
    pull(capacity)
  stadium_url <- data |> 
    slice(i) |> 
    pull(url)
  stadium_cntry <- data |> 
    slice(i) |> 
    pull(country)
  xy_text <- read_html(stadium_url) |> 
    html_nodes('#coordinates span') |> 
    html_text()
  xy <- xy_text[10] |> str_split(';') |> as.data.frame()
  x <- xy[1,1]
  y <- xy[2,1]
  tibble(name = stadium_name, capacity = stadium_capacity, country = stadium_cntry, x=x, y=y)
}

stadium_locations <- map_df(.x = 1:nrow(df),
                            .f = function(x){stadium_location(x)})

stadium_pts <- stadium_locations |> 
  st_as_sf(coords = c("y","x"), crs = 4326, remove = F) |> 
  st_transform(3857)

na_sf <- ne_countries(continent = "North America", returnclass = "sf") |> 
  st_transform(3857) |> 
  filter(admin != 'Greenland')

new <- mapedit::editMap(na_sf)

ggplot() +
  geom_sf(data = na_sf, fill = 'gold', alpha = .2, color = 'black', linewidth = .1) +
  geom_sf(data = stadium_pts, shape = 21, fill = 'black', 
          aes(size = capacity),
          alpha = .8, color = 'white') +
  theme_void(base_family = 'Avenir Next') +
  scale_size(labels = scales::comma, name = 'Capacity') +
  labs(caption = 'Source: Wikipedia') +
  theme(legend.title = element_text(face = "bold", hjust = 0.5))

ggsave('plot/day10.jpg', width = 10, dpi = 600)
