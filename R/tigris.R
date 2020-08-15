########################################
## Cleaning
########################################

source("R/package.R")
source("R/help.R")

## 

jams <- vroom("data/jams.csv") %>% glimpse()

## 

options(tigris_use_cache=TRUE)

##

streets <- roads(state = "KY", county = "Jefferson", class = 'sf', year = 2018) %>%
  st_difference() %>%
  st_transform(2205)

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

points <- 
  jams %>%
  st_as_sf(coords = c("longitude", "latitude"), remove = FALSE, crs = 4326) %>% 
  st_transform(2205) %>%
  st_intersection(bounds)

##

major <- 
  streets %>%
  filter(str_detect(MTFCC,"S1100")) %>%
  st_buffer(30) %>%
  st_difference()

route <-
  streets %>%
  filter(str_detect(MTFCC,"S1200")) %>%
  st_buffer(30) %>%
  st_set_precision(1) %>%
  st_difference()

ramps <-
  streets %>%
  filter(MTFCC == "S1630") %>%
  st_buffer(15)

minor <- 
  streets %>%
  filter(!str_detect(MTFCC,"S1100|S1200|S1630")) %>%
  st_buffer(10) %>%
  st_difference()

##

tagged <- st_join(points, ramps)

jams_ramps <- 
  tagged %>%
  drop_na(LINEARID)
  
misses <- 
  tagged %>%
  filter(!jam_id %in% jams_ramps$jam_id) %>%
  select_if(str_detect(names(.), names(points)))

tagged <- st_join(misses, major)

jams_major <- 
  tagged %>%
  drop_na(LINEARID)

misses <- 
  tagged %>%
  filter(!jam_id %in% jams_major$jam_id) %>%
  select_if(str_detect(names(.), names(points)))

tagged <- st_join(misses, route)

jams_route <- 
  tagged %>%
  drop_na(LINEARID)

misses <- 
  tagged %>%
  filter(!jam_id %in% jams_route$jam_id) %>%
  select_if(str_detect(names(.), names(points)))

tagged <- st_join(misses, minor)

jams_minor <- 
  tagged %>%
  drop_na(LINEARID)

misses <- 
  tagged %>%
  filter(!jam_id %in% jams_minor$jam_id) %>%
  select_if(str_detect(names(.), names(points)))

tagged <- rbind(jams_major, jams_minor, jams_ramps, jams_route)

dim(misses)
dim(tagged)
dim(points)

##
