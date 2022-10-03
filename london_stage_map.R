

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(sf)
library(osmdata)
library(ggplot2)

x <- c(-0.120414, -0.116617, -0.1220367, -0.072976, -0.1314996)
y <- c(51.512733, 51.516394, 51.513151, 51.513394, 51.508514)
  
df_coords <- data.frame(x,y)

#df_sf <- sf::st_as_sf(df_coords, coords = c("x","y"))

bbx = c(-0.1319, 51.5033,
        -0.0706, 51.5204)

streets <- opq(bbox = bbx) %>%
  add_osm_feature(key = 'highway') %>%
  osmdata_sf()

# the thames
thames <- opq(bbox = bbx) %>%
  add_osm_feature(key = "water") %>% 
  osmdata_sf()

# plot
ggplot() +
  geom_sf(data = thames$osm_multipolygons, fill = 'grey80', color = NA) +
  geom_sf(data = streets$osm_lines, col = 'grey70') +
  geom_point(data = df_coords, aes(x = x, y = y), color = 'grey40') + 
  coord_sf(xlim = c(-.1319, -.0706), ylim = c(51.5033, 51.5204)) +
  theme_void() +
  theme(legend.position = "") 

ggsave('base_map.png', width = 8, height = 4, dpi = 300)
