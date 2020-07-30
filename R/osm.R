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

options(tigris_use_cache=TRUE)

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
  st_difference(waters) %>% 
  ms_simplify(0.005)

plot(background)

##

intersection <- 
  roads %>%
  st_transform(2205) %>% 
  st_intersection(bounds)

points <- 
  jams %>%
  st_as_sf(coords = c("longitude", "latitude"), remove = FALSE, crs = 4326) %>% 
  st_transform(2205) %>%
  st_intersection(bounds)

rm(jams)
rm(waters)

##

major <- intersection %>%
  filter(!str_detect(highway, "motorway_link|primary_link|secondary_link|tertiary_link|trunk_link")) %>%
  filter(str_detect(highway, "motorway|trunk")) %>%
  st_buffer(15)

route <- intersection %>% 
  filter(str_detect(highway, "primary|secondary|tertiary")) %>%
  st_buffer(15)

ramps <- intersection %>%
  filter(str_detect(highway, "motorway_link|primary_link|secondary_link|tertiary_link|trunk_link")) %>%
  st_buffer(10)

leftover <- glue_collapse(c(unique(major$highway), unique(route$highway), unique(ramps$highway)), "|")  

minor <- intersection %>%
  filter(!str_detect(highway, leftover)) %>%
  st_buffer(10)

##

tm_shape(major) +
  tm_fill(col = "highway") +
  tm_shape(ramps) +
  tm_polygons()

##

test <- sample_n(points, 20000)

tagged <- st_join(points, ramps)

jams_ramps <- 
  tagged %>%
  drop_na(osm_id)

misses <- 
  tagged %>%
  filter(!jam_id %in% jams_ramps$jam_id) %>%
  select(-osm_id, -highway, -oneway, -lanes, -maxspeed)

tagged <- st_join(misses, major)

jams_major <- 
  tagged %>%
  drop_na(osm_id)

misses <- 
  tagged %>%
  filter(!jam_id %in% jams_major$jam_id) %>%
  select(-osm_id, -highway, -oneway, -lanes, -maxspeed)

tagged <- st_join(misses, route)

jams_route <- 
  tagged %>%
  drop_na(osm_id)

misses <- 
  tagged %>%
  filter(!jam_id %in% jams_route$jam_id) %>%
  select(-osm_id, -highway, -oneway, -lanes, -maxspeed)

tagged <- st_join(misses, minor)

jams_minor <- 
  tagged %>%
  drop_na(osm_id)

misses <- 
  tagged %>%
  filter(!jam_id %in% jams_minor$jam_id) %>%
  select(-osm_id, -highway, -oneway, -lanes, -maxspeed)

tagged <- rbind(jams_major, jams_minor, jams_ramps, jams_route)

sum(tagged$jam_id_1 %in% points$jam_id_1)

length(unique(points$jam_id))
length(unique(points$latitude))

dim(misses)
dim(tagged)
dim(points)

##

located <- 
  tagged %>%
  st_drop_geometry() %>%
  group_by(osm_id) %>%
  summarise(n = n())

##

tmap_mode("view")

test <- 
  intersection %>%
  left_join(located) %>%
  replace_na(list(n = 0)) %>%
  mutate(length = st_length(geometry) / 1000,
         raw = log(n),
         per = as.numeric(log((n) / length))) %>%
  arrange(n) %>%
  st_as_sf()

map <-
  tm_shape(test %>%
             rename(`Log Jams (per km)` = per)) +
  tm_lines(col = "Log Jams (per km)", 
           legend.title = "Log Jams (per km)",
           style = "fisher",
           palette = pal,
           midpoint = NA,
           legend.hist = TRUE) +
  tm_layout(legend.outside = TRUE,
            main.title = "Traffic Congestion According to Waze") 

tmap_save(map, "per.png", height = 10, width = 14, dpi = 300, units = "in")

map <- 
  tm_shape(test %>% 
             rename(`Log Jams` = raw)) +
  tm_lines(col = "Log Jams", 
           style = "fisher",
           palette = pal,
           midpoint = NA,
           legend.hist = TRUE,
           legend.title = "Log Jams") +
  tm_layout(legend.outside = TRUE,
            main.title = "Traffic Congestion According to Waze") 

tmap_save(map, "raw.png", height = 10, width = 14, dpi = 300, units = "in")
