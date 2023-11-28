# 23-11-2023	3D	The third dimension

library(tidyverse); library(sf); library(tidycensus); library(tigris); library(scales); library(janitor); library(rleuven); library(spikemap); library(cartography)

options(tigris_use_cache = TRUE)

# df <- haven::read_dta('/Users/andrew/Desktop/Upjohn WholeData/upjohn/wholedata2000.dta')
# 
# rugs <- df |> 
#   filter(naics == 314110,
#          area == 'County') |> 
#   mutate(GEOID = paste0(str_pad(fipstate, width = 2,pad = '0'),
#                         str_pad(fipscty, width = 3,pad = '0'))) |> 
#   select(GEOID, est, emp)
# write_csv(rugs,'data/day23.csv')

rugs <- read_csv('data/day23.csv')

cty_pop <- get_decennial(geography = "county", 
                     variables = "P001001",
                     year = 2000) |> 
  select(GEOID, pop = value)

cty_rugs <- counties(cb = T, resolution = '20m') |> 
  shift_geometry() |> 
  filter(!STUSPS %in% c('AK','HI','PR')) |> 
  select(GEOID, STUSPS, geometry) |> 
  left_join(cty_pop, by = 'GEOID') |> 
  left_join(rugs, by = 'GEOID') |> 
  replace_na(list(emp = 0, est = 0)) |> 
  mutate(emp_per_100k = ((emp/pop)*100000) |> round(2))


sm <- matrix(c(3, 1.2, 0, 1), 2, 2)

rug_tilt <- mutate(cty_rugs, geometry = geometry * sm) %>%
  st_set_crs(st_crs(cty_rugs))

us48_tilt <- st_dissolve(rug_tilt,STUSPS)


quartzFonts(avenir = c('Avenir Book', 'Avenir Black', 
                       'Avenir Book Oblique', 'Avenir Black Oblique'))
par(family = 'avenir')

png(filename="plot/day23.png", height = 800, width = 1000)
plot(st_geometry(rug_tilt), col="white", border = "black",
     lwd = 0.1, bg = NA)
plot(st_geometry(us48_tilt), col = alpha('white', 0), border = "black",
     lwd = 0.2, bg = NA, add = T)
spikemap(x = rug_tilt, var = "emp_per_100k",
         inches = 30, fixmax = 500000,
         col = "orange", border = "#bb0000",  lwd = .7,
         # legend.pos = 'bottomright',
         legend.title.txt = "Employees per 100K",
         legend.values.rnd = -3)
mtext("Carpet & Rug Mills\nin the United States",
      side = 3, adj = 0.01, padj = 0, line = -5,
      cex = 2.5, font = 3, col = "black")
dev.off()

