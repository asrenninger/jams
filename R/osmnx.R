########################################
## Cleaning
########################################

source("R/package.R")
source("R/help.R")

##

jams <- vroom("data/jams.csv") %>% glimpse()

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
  st_difference(water) %>% 
  ms_simplify(0.005)

plot(background)

##

streets <- 
  read_sf("data/attributes/edges.shp") %>%
  st_transform(2205) %>%
  mutate(highway = str_remove_all(highway, pattern = "\\["),
         highway = str_remove_all(highway, pattern = "\\]"),
         highway = str_remove_all(highway, pattern = "\\'"),
         highway = str_remove_all(highway, pattern = ", primary|, unclassified|tertiary, |secondary, |primary, ")) %>%
  mutate(highway = case_when(highway == "road" ~ "residential",
                             highway == "living_street" ~ "residential",
                             highway == "unclassified" ~ "residential",
                             TRUE ~ highway)) %>%
  filter(highway != "crossing")


##

tmap_mode("view")

tm_shape(streets) +
  tm_lines(col = 'highway')

##

ramps <-
  streets %>%
  filter(str_detect(highway, "link")) %>%
  st_buffer(15)

major <- 
  streets %>%
  filter(!str_detect(highway, "link")) %>%
  filter(str_detect(highway,"motorway|trunk")) %>%
  st_buffer(30) %>%
  st_set_precision(1) %>%
  st_difference()

route <-
  streets %>%
  filter(!str_detect(highway, "link")) %>%
  filter(str_detect(highway,"primary|secondary|tertiary"))%>%
  st_buffer(30) %>%
  st_set_precision(20) %>%
  st_difference()

minor <- 
  streets %>%
  filter(str_detect(highway,"residential")) %>%
  st_buffer(10) %>%
  mutate(type = st_geometry_type(geometry)) %>%
  filter(type == "POLYGON") %>%
  select(-type)

##

points <- 
  jams %>%
  rename(class = name,
         distnace = length) %>%
  st_as_sf(coords = c("longitude", "latitude"), remove = FALSE, crs = 4326) %>% 
  st_transform(2205) %>%
  st_intersection(bounds)

##

tagged <- st_join(points, ramps)

jams_ramps <- 
  tagged %>%
  drop_na(osmid)

misses <- 
  tagged %>%
  filter(!id %in% jams_ramps$id) %>%
  select(names(points))

tagged <- st_join(misses, major)

jams_major <- 
  tagged %>%
  drop_na(osmid)

misses <- 
  tagged %>%
  filter(!id %in% jams_major$id) %>%
  select(names(points))

tagged <- st_join(misses, route)

jams_route <- 
  tagged %>%
  drop_na(osmid)

misses <- 
  tagged %>%
  filter(!id %in% jams_route$id) %>%
  select(names(points))

tagged <- st_join(misses, minor)

jams_minor <- 
  tagged %>%
  drop_na(osmid)

misses <- 
  tagged %>%
  filter(!id %in% jams_minor$id) %>%
  select(names(points))

tagged <- rbind(jams_major, jams_minor, jams_ramps, jams_route)

sum(!tagged$id %in% points$id)

dim(misses)
dim(tagged)
dim(points)

vroom_write(st_drop_geometry(tagged), "tagged_pre.csv")

tagged <- 
  tagged %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup()

vroom_write(st_drop_geometry(tagged), "tagged_post.csv")

tm_shape(streets) +
  tm_lines(col = 'highway') +
  tm_shape(toy %>%
             filter(!id %in% tagged$id)) +
  tm_dots()

##

spatio <- 
  tagged %>%
  st_drop_geometry() %>%
  group_by(osmid) %>%
  summarise(n = n(),
            speed = mean(speed),
            delay = mean(delay))

##

test <- 
  streets %>%
  left_join(spatio) %>%
  replace_na(list(n = 0)) %>%
  mutate(`Log Jams` = log(n),
         `Log Jams (per km)` = as.numeric(log((n) / length))) %>%
  arrange(n) %>%
  st_as_sf()

map <-
  tm_shape(test) +
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
  tm_shape(test) +
  tm_lines(col = "Log Jams", 
           style = "fisher",
           palette = pal,
           midpoint = NA,
           legend.hist = TRUE,
           legend.title = "Log Jams") +
  tm_layout(legend.outside = TRUE,
            main.title = "Traffic Congestion According to Waze") 

tmap_save(map, "raw.png", height = 10, width = 14, dpi = 300, units = "in")

##

tmap_options(bg.color = "white", legend.text.color = "black", legend.title.color = 'black')

##

tween <- 
  tm_shape(streets %>%
             mutate(betweennes = log(betweennes)) %>%
             rename(`log betweenness` = betweennes)) +
  tm_lines(col = "log betweenness", 
           style = "fisher",
           palette = pal,
           midpoint = NA,
           #legend.hist = TRUE,
           legend.title = "betweenness") +
  tm_layout(main.title = "through-movement") 

close <- 
  tm_shape(streets %>%
             rename(closeness = closeness_)) +
  tm_lines(col = "closeness", 
           style = "fisher",
           palette = pal,
           midpoint = NA,
           #legend.hist = TRUE,
           legend.title = "closeness") +
  tm_layout(main.title = "to-movement") 

degri <- 
  tm_shape(streets %>%
             rename(degree = degree_cen)) +
  tm_lines(col = "degree", 
           style = "fisher",
           palette = pal,
           midpoint = NA,
           #legend.hist = TRUE,
           legend.title = "degree") +
  tm_layout(main.title = "intersections") 

arranged <- tmap_arrange(tween, close, degri, nrow = 1)

tmap_save(arranged, "measures.png", height = 8, width = 20, dpi = 300, units = "in")

##

temporal <- 
  tagged %>%
  st_drop_geometry() %>%
  mutate(date = with_tz(pub_utc_date, "EST")) %>%
  mutate(date = floor_date(date, 'hour')) %>%
  group_by(osmid, date) %>%
  summarise(n = n(),
            speed = mean(speed),
            delay = mean(delay)) %>%
  ungroup()

##

weather <- 
  read_csv("data/weather.txt") %>% 
  clean_names() %>%
  rename(date = local_time) %>%
  mutate(forecast = str_replace_all(icon, "-", "_")) %>%
  select(date, forecast, wind_speed, cloud_cover, visibility)

##

covariates <- 
  streets %>% 
  transmute(osmid = osmid,
            grade = grade_abs,
            limit = speed_kph,
            tween = log(betweennes),
            close = closeness_,
            degri = degree_cen,
            eccen = eccentrici,
            length = length, 
            bearing = bearing,
            highway = highway,
            ease = ease,
            oneway = factor(oneway)) %>%
  glimpse()

##

commutes <- 
  read_sf("data/imputed_homes_nometa.shp", crs = 4326) %>%
  st_transform(2205)

##

overline <- stplanr::overline2(st_cast(commutes, "LINESTRING"), attr = "dissolve")

cleaning <- 
  overline %>%
  st_join(streets) %>%
  st_drop_geometry() %>%
  group_by(osmid) %>%
  summarise(n = sum(dissolve)) %>%
  mutate(commutes = n) %>%
  select(-n)

##

regression <-
  spatio %>% 
  left_join(covariates) %>%
  left_join(cleaning) %>%
  select(-geometry)

##

lm(log(n) ~ .,
   data = regression %>%
     mutate(commutes = log(commutes)) %>%
     select(-osmid, -delay, -speed)) %>%
  summary()

##

write_csv(cleaning, "commutes_streets.csv")

##

weekly <- 
  tagged %>%
  st_drop_geometry() %>%
  mutate(date = with_tz(pub_utc_date, "EST")) %>%
  mutate(date = floor_date(date, 'hour')) %>%
  group_by(osmid, date) %>%
  summarise(n = n(),
            speed = mean(speed),
            delay = sum(delay)) %>%
  ungroup() %>%
  mutate(hour = hour(date),
         day = wday(date, label = TRUE)) %>%
  #filter(!str_detect(day, "Sat|Sun")) %>% 
  group_by(osmid, day, hour) %>%
  summarise(n = mean(n),
            speed = mean(speed),
            delay = mean(delay)) %>%
  ungroup()

glimpse(weekly)

##

series <- 
  temporal %>%
  select(osmid, date, n, speed, delay) %>%
  left_join(weather) %>%
  left_join(covariates)







