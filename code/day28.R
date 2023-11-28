# 28-11-2023	Is this a chart or a map?	In thematic maps, you can't always tell. Try to stretch the limits

library(tidyverse); library(sf); library(tidycensus); library(tigris); library(scales); library(janitor); library(rleuven); library(geofacet)

# xw <- states_df |> 
#   select(1,state = 3)
# 
# cty_pop <- read_csv('/Users/andrew/Desktop/CAINC30/CAINC30__ALL_AREAS_1969_2022.csv') |> 
#   clean_names() |> 
#   filter(line_code == 100) |> 
#   mutate(st_fips = str_sub(geo_fips, end = 2)) |> 
#   select(st_fips,geo_name,x2001:x2021) |> 
#   pivot_longer(cols = x2001:x2021, names_to = 'year', values_to = 'pop') |> 
#   filter(not_na(geo_name)) |> 
#   mutate(year = str_remove_all(year,'x') |> as.numeric(),
#          pop = as.numeric(pop)) |> 
#   summarize(st_pop = sum(pop, na.rm = T), .by = c(year,st_fips)) |> 
#   left_join(xw) |> 
#   filter(not_na(state))
# 
# cty_gdp <- read_csv('/Users/andrew/Desktop/CAGDP9/CAGDP9__ALL_AREAS_2001_2021.csv') |> 
#   clean_names() |> 
#   filter(line_code %in% c(91,92)) |> 
#   mutate(st_fips = str_sub(geo_fips, end = 2)) |> 
#   select(st_fips, geo_name, description, x2001:x2021) |> 
#   pivot_longer(cols = x2001:x2021, names_to = 'year', values_to = 'gdp') |> 
#   filter(not_na(geo_name)) |> 
#   mutate(year = str_remove_all(year,'x') |> as.numeric(),
#          gdp = as.numeric(gdp),
#          description = case_when(str_detect(description,'goods') ~ 'Goods-Producing',
#                                  str_detect(description,'services') ~ 'Services-Producing')) |> 
#   summarize(st_gdp = sum(gdp, na.rm = T), .by = c(year,st_fips,description)) |> 
#   left_join(xw) |> 
#   filter(not_na(state)) |> 
#   select(-st_fips) |> 
#   left_join(cty_pop) |> 
#   mutate(gdp_pk = (st_gdp/st_pop) |> round(2))
# write_csv(cty_gdp,'data/day28.csv')
  
df <- read_csv('data/day28.csv') |> 
  mutate(yr = as.Date(paste0(year,'-01-01')))

ggplot(df) +
  aes(x = yr, y = gdp_pk, color = description) +
  geom_line() +
  scale_color_manual(values = c('tomato','dodgerblue3'), name = 'Industry Type',
                     labels = c('Private Goods-Producing','Private Services-Producing')) +
  scale_y_continuous(labels = dollar) +
  scale_x_date(NULL, breaks = scales::breaks_width("5 years"), 
               labels = scales::label_date("'%y")) +
  labs(x = 'Year', y = 'GDP Per-Capita',
       title = 'State Gross Product Trends',
       subtitle = 'Goods- vs. Services-Producing Industries, 2001–21',
       caption = 'Source: Bureau of Economic Analysis') +
  facet_geo(~ state) +
  theme_bw(base_family = 'Futura') +
  theme(axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15, face = 'bold',margin=margin(10,10,10,10)),
        plot.title = element_text(face = 'bold', hjust = .5, size = 25, margin=margin(15,0,10,0)),
        legend.title = element_text(face = 'bold', hjust = .5),
        plot.subtitle = element_text(hjust = .5, margin=margin(0,0,10,0), lineheight = 1.2),
        plot.caption = element_text(margin=margin(10,10,10,10))) +
  guides(color=guide_legend(override.aes = list(linewidth = 1.75, alpha = 1))) 

ggsave('plot/day28.jpg', height = 8, width = 13)
   
