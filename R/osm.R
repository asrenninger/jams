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
  filter(!str_detect(highway, "footway|track|steps|raceway|cycleway|pedestrian|path|construction|crossing|building_passage|abandoned")) %>%
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

bounds <- tracts(state = "KY", county = "Jefferson", class = 'sf') %>%
  st_union() %>%
  st_combine() %>%
  st_transform(2205)

waters <- area_water(state = "KY", county = "Jefferson", class = 'sf') %>%
  st_union() %>%
  st_combine() %>%
  st_transform(2205)

background <- 
  bounds %>% 
  st_difference(water) %>% 
  ms_simplify(0.005)

plot(background)

##

intersection <- 
  roads %>%
  st_transform(2205) %>% 
  st_intersection(bounds)

difference <- st_difference(intersection)

##

tm_shape(ramps %>% 
           st_buffer(10)) +
  tm_polygons(col = "osm_id")

##

major <- filter(intersection, str_detect(highway, "motorway|trunk"))
route <- filter(intersection, str_detect(highway, "primary|secondary|tertiary"))
ramps <- filter(intersection, str_detect(highway, "motorway_link|primary_link|secondary_link|tertiary_link|trunk_link"))

leftover <- glue_collapse(c(unique(major$highway), unique(route$highway), unique(ramps$highway)), "|")  

minor <- filter(intersection, !str_detect(highway,leftover))

##

tagged <- st_join(points, ramps, st_is_within_distance, dist = 10)

jams_ramps <- 
  tagged %>%
  drop_na(osm_id)

misses <- 
  tagged %>%
  filter(!jam_id %in% jams_ramps$jam_id) %>%
  select(-osm_id, -highway, -oneway, -lanes, -maxspeed)

tagged <- st_join(misses, major, st_is_within_distance, dist = 20)

jams_major <- 
  tagged %>%
  drop_na(osm_id)

misses <- 
  tagged %>%
  filter(!jam_id %in% jams_major$jam_id) %>%
  select(-osm_id, -highway, -oneway, -lanes, -maxspeed)

tagged <- st_join(misses, route, st_is_within_distance, dist = 15)

jams_route <- 
  tagged %>%
  drop_na(osm_id)

misses <- 
  tagged %>%
  filter(!jam_id %in% jams_route$jam_id) %>%
  select(-osm_id, -highway, -oneway, -lanes, -maxspeed)

tagged <- st_join(misses, minor, st_is_within_distance, dist = 10)

jams_minor <- 
  tagged %>%
  drop_na(osm_id)

misses <- 
  tagged %>%
  filter(!jam_id %in% jams_minor$jam_id) %>%
  select(-osm_id, -highway, -oneway, -lanes, -maxspeed)

tagged <- 
  rbind(jams_major, jams_minor, jams_ramps, jams_route) %>%
  group_by(jam_id) %>%
  slice(1)

dim(misses)
dim(tagged)
dim(points)

##
