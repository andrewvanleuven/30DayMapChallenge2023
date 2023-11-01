# 01-11-2023	Points	A map with points

suppressPackageStartupMessages({library(tidyverse); library(sf); library(ggpubr); library(tigris); library(janitor); library(rvest); library(mapboxapi); library(rleuven)})
is_even <- function(x) {return(x %% 2 == 0)}


# Scrape & Build Hometown Data --------------------------------------------
schools <- read_html('https://www.espn.com/college-football/teams/_/group/4') |> 
  html_nodes('.nowrap:nth-child(3) .AnchorLink') |> 
  html_attr('href') |> 
  as_tibble() |> 
  slice(1:14) |> 
  as_tibble() |>
  rename(team = 1) |>
  mutate(team = paste0('https://www.espn.com',team)) |> 
  pull(team)

pull_players <- function(team_url){
  player_birthplaces <- read_html(team_url) |> 
    html_table() %>%
    map_dfr(., bind_rows) |> 
    select(birthplace = 7)
  player_school <- read_html(team_url) |> 
    html_node('.fw-bold') |> 
    as.character() |> 
    str_extract(">([^<]+)<") |> 
    str_replace(">", "") |> 
    str_replace("<", "")
  player_birthplaces |> 
    mutate(school = player_school)
}
   
player_birthplaces <- map_df(.x = schools,
                             .f = function(x){pull_players(x)})  

birthplaces <- player_birthplaces |> 
  separate(birthplace, sep = ',', into = c('city', 'st_abbr')) |> 
  mutate(st_abbr = str_remove_all(st_abbr,' ')) |> 
  left_join(states_df |> select(3,2)) |> 
  mutate(place = paste(city, st_name, 'U.S.A.', sep = ', ')) |> 
  pull(place)

coords <- map_df(.x = birthplaces,
                 .f = function(x){mb_geocode(x) |>
                     as.data.frame() |>
                     select(Value = 1) |>
                     mutate(id = row_number(),
                            coo = ifelse(is_even(id),'x','y'),
                            id = rep(1:(n()/2), each = 2)) |>
                     pivot_wider(names_from = coo, values_from = Value) |>
                     select(x,y)})

birthpl_xy <- cbind(player_birthplaces,coords)

qs::qsave(birthpl_xy, 'data/day01.qs')
birthpl_xy <- qs::qread('data/day01.qs')

# Load in Spatial Data ----------------------------------------------------
df <- birthpl_xy |> 
  st_as_sf(coords = c("y","x"), crs = 4326, remove = F) |> 
  shift_geometry()

usa <- states(T,'20m') |> 
  shift_geometry()

# Mapping -----------------------------------------------------------------
colors <- c('Baylor'          = '#003015',
            'BYU'             = '#002E5D',
            'Cincinnati'      = '#E00122',
            'Houston'         = '#C8102E',
            'Iowa State'      = '#F1BE48',
            'Kansas'          = '#0051BA',
            'Kansas State'    = '#512888',
            'Oklahoma'        = '#841617',
            'Oklahoma State'  = '#FF7300',
            'TCU'             = '#4D1979',
            'Texas'           = '#BF5700',
            'Texas Tech'      = '#CC0000',
            'UCF'             = '#BA9B37',
            'West Virginia'   = '#002855')

(a <- ggplot() + 
  geom_sf(data = usa, alpha = 0, color = 'black') + 
  geom_sf(data = df, aes(color = school), size = .75, alpha = .75) + 
  theme_void(base_family = 'CMU Sans Serif') +
  scale_color_manual(values = colors, name = 'Team') +
  guides(color=guide_legend(ncol=2,override.aes = list(size = 3, alpha = 1))) +
  theme(legend.title = element_text(face = "bold", hjust = 0.5, size = 9)))

b <- get_legend(a)

ggarrange(a + theme(legend.position = 'none'),b, widths = c(1,.3))
ggsave('plot/day01.jpg', dpi = 600, width = 10, height = 5)

# library(mapview)
# mapview(df, zcol = "school", 
#         col.regions = c('#003015','#002E5D','#E00122','#C8102E','#F1BE48','#0051BA','#512888',
#                         '#841617','#FF7300','#4D1979','#BF5700','#CC0000','#BA9B37','#002855'))
# 


