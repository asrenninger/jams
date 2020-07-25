########################################
## Cleaning
########################################

source("R/package.R")
source("R/help.R")

## 

jams <- vroom("data/jams.csv") %>% glimpse()

## 

roads <- 
  getbb("Louisville, KY") %>%
  opq() %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf() %>%
  magrittr::use_series("osm_lines") %>%
  select(osm_id, highway, oneway, lanes, maxspeed) %>%
  filter(!str_detect(highway, "footway|track|steps|raceway|cycleway|pedestrian")) %>%
  st_as_sf()

bounds <-
  getbb("Louisville, KY") %>%
  opq() %>%
  add_osm_feature(key = "name", value = "Jefferson County") %>%
  osmdata_sf() %>%
  magrittr::use_series("osm_polygons")
  
##

tmap_mode("view")

tm_shape(roads) +
  tm_lines()

tm_shape(bounds) +
  tm_polygons()

## 

streets <- roads(state = "KY", county = "Jefferson", class = 'sf') %>%
  st_difference()

bounds <- tracts(state = "KY", county = "Jefferson", class = 'sf') %>%
  st_union() %>%
  st_combine()

plot(bounds)

##

intersection <- st_intersection(streets)

##

intersections <- 
  intersection %>% 
  mutate(type = st_geometry_type(.)) %>% 
  filter(type == "POINT") %>%
  st_coordinates() %>%
  as_tibble() %>%
  group_by(X, Y) %>%
  slice(1) %>%
  st_as_sf(coords = c("X", "Y"), remove = FALSE)

##

