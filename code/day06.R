# 06-11-2023	Asia	Largest of the continents

suppressPackageStartupMessages({library(tidyverse); library(sf); library(rvest); library(measurements)})

schools <- read_html('https://ko.wikipedia.org/wiki/%EB%8C%80%ED%95%9C%EB%AF%BC%EA%B5%AD%EC%9D%98_%EB%8C%80%ED%95%99_%EB%AA%A9%EB%A1%9D') |> 
  html_nodes('div+ p a') |> 
  html_attr('href') |> 
  as_tibble() |>
  rename(univ = 1) |>
  mutate(univ = paste0('https://ko.wikipedia.org',univ)) |> 
  pull(univ)

kor_univ <- function(url){
  univ_name <- read_html(url) |> 
    html_nodes('.org span') |> 
    html_text()
  univ_kname <- read_html(url) |> 
    html_nodes('.mw-page-title-main') |> 
    html_text()
  xy_ktext <- read_html(url) |> 
    html_nodes('.geo-dms span') |> 
    html_text()
  xy_text <- str_replace_all(xy_ktext, "[가-힣]+", "") |> 
    str_remove_all('°') |> 
    str_remove_all('′') |> 
    str_remove_all('″') 
  x <- conv_unit(xy_text[1] |> str_trim(), from = "deg_min_sec", to = "dec_deg")
  y <- conv_unit(xy_text[2] |> str_trim(), from = "deg_min_sec", to = "dec_deg")
  tibble(name = univ_name, kname = univ_kname, x=x, y=y)
}

kor_univs <- map_df(.x = schools,
                    .f = function(x){kor_univ(x)})

qs::qsave(kor_univs, 'data/day06.qs')

kor_sf <- st_read('data/KOR_adm_shp/KOR_adm1.shp') |> 
  st_transform(5178)

kor_univs <- qs::qread('data/day06.qs') |> 
  filter(!is.na(x), !is.na(y)) |> 
  st_as_sf(coords = c("y","x"), crs = 4326, remove = F) |> 
  st_transform(5178)

ggplot() + 
  geom_sf(data = kor_sf, fill = 'black', alpha = .08, color = 'black') +
  geom_sf(data = kor_univs, color = 'blue', size = .4, alpha = .7) +
  theme_void() +
  labs(title = '대한민국의 대학 목록',
       subtitle = 'Universities in South Korea') +
  theme(plot.title = element_text(face = 'bold', hjust = .5, family = "Arial Unicode MS"),
        plot.subtitle = element_text(hjust = .5))

ggsave('plot/day06.jpg', dpi = 600)
