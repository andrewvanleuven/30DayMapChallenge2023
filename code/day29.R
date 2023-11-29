# 29-11-2023	Population	A classic theme for a map

library(tidyverse); library(sf); library(tidycensus); library(tigris); library(scales); library(janitor); library(rleuven); library(ggpubr)

popul <- get_decennial(
  geography = 'place',
  variables = c(pop = 'P1_001N'),
  year = 2020) |> 
  select(GEOID,value)

joined <- places(cb = T) |> 
  shift_geometry() |> 
  st_centroid() |> 
  inner_join(popul, by = 'GEOID') |> 
  mutate(NAME = ifelse(GEOID == 2146027, 'Lexington', NAME))

us48 <- states(T,'20m') |> 
  shift_geometry() |> 
  filter(!STUSPS %in% c('AK','HI','PR'))

# common <- st_drop_geometry(joined) |> 
#   count(NAME) |> 
#   filter(n >= 10)
# 
# common_stats <- st_drop_geometry(joined) |> 
#   filter(NAME %in% pull(common,NAME)) |> 
#   select(GEOID,NAME,STUSPS,pop = value) |> 
#   mutate(over1k = ifelse(pop >= 1000, 1, 0)) |> 
#   summarize(count = n(),
#             over1k = (sum(over1k)/count) |> round(2),
#             avg = mean(pop) |> round(),
#             sd = sd(pop) |> round(),
#             .by = NAME) |> 
#   mutate(avg_rk = dense_rank(desc(avg)),
#          sd_rk = dense_rank(sd),
#          rk = ((avg_rk+sd_rk)/2) |> round()) |> 
#   arrange(rk)

ggplot() +
  geom_sf(data = us48, fill = 'black', alpha = .2, color = 'white') +
  geom_sf(data = joined |> filter(NAME == 'Franklin'), aes(size = value), lwd = .1,
          fill = 'cornflowerblue', shape = 21, color = 'black', alpha = .7) +
  theme_void(base_family = 'Gill Sans') +
  labs(title = 'Franklins of the United States',
       caption = 'Source: 2020 U.S. Decennial Census     \n     ',
       subtitle = 'Location & Population of Places in the U.S. Named \"Franklin\"') +
  scale_size(name = 'Population', labels = comma) +
  theme(plot.subtitle = element_text(hjust = 0.5),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 28),
        legend.title = element_text(face = "bold", hjust = 0.5),
        legend.position = 'bottom') +
  guides(size = guide_legend(title.position = "top"))

ggsave('plot/day29.jpg', height = 8, width = 10)

town_plot <- function(town_name){
  ggplot() +
    geom_sf(data = us48, fill = 'black', alpha = .2, color = 'white') +
    geom_sf(data = joined |> filter(NAME == town_name), aes(size = value), lwd = .1,
            fill = 'cornflowerblue', shape = 21, color = 'black', alpha = .7) +
    theme_void(base_family = 'Gill Sans') +
    scale_size(name = sprintf('Population of every %s',town_name), labels = comma) +
    theme(plot.subtitle = element_text(hjust = 0.5), 
          plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
          legend.title = element_text(face = "bold", hjust = 0.5),
          legend.position = 'bottom',
          legend.margin=margin(b = 1, unit='cm')) +
    guides(size = guide_legend(title.position = "top"))
}

a <- town_plot('Arlington')
b <- town_plot('Ashland')
c <- town_plot('Burlington')
d <- town_plot('Chester')
e <- town_plot('Clinton')
f <- town_plot('Columbia')
g <- town_plot('Florence')
h <- town_plot('Franklin')
i <- town_plot('Hudson')
j <- town_plot('Jamestown')
k <- town_plot('Lexington')
l <- town_plot('Middletown')
m <- town_plot('Monroe')
n <- town_plot('Monticello')
o <- town_plot('Newport')
p <- town_plot('Springfield')

grd <- ggarrange(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p, nrow = 4, ncol = 4)

ggsave('plot/day29a.jpg', grd, height = 16, width = 24)
