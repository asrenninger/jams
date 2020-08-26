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
            oneway = factor(oneway),
            ease = ease) %>%
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

shape <- select(covariates, osmid, geometry)

##

lin <- lm(log(n) ~ degri + eccen + length + bearing + grade + ease + highway + oneway + commutes + I(commutes^2), 
          data = regression %>%
            filter(n > 10) %>%
            mutate(commutes = log(commutes)) %>%
            select(-osmid, -delay, -speed))

summary(lin)

lin %>% broom::tidy() %>% mutate(`p-value` = round(p.value, 8)) %>% select(-p.value) %>% flextable::flextable()

regression <- 
  regression %>%
  filter(n > 10) %>%
  drop_na() %>%
  mutate(residuals = lin$residuals) %>%
  left_join(shape) %>%
  st_as_sf()
  
ggplot(data = regression, aes(log(n), residuals)) +
  geom_hex(bins = 100, show.legend = FALSE) +
  scale_fill_gradientn(colours = pal, 
                       guide = 'none') + 
  theme_ver() +
  ggsave('rezzies.png', height = 6, width = 8, dpi = 300)

##

write_csv(cleaning, "commutes_streets.csv")

##

weekly <- 
  tagged %>%
  st_drop_geometry() %>%
  mutate(date = with_tz(pub_utc_date, "EST")) %>%
  mutate(date = floor_date(date, 'hour')) %>%
  mutate(freeway = factor(if_else(road_type == 3,1,0))) %>%
  group_by(osmid, freeway, date) %>%
  summarise(n = n(),
            speed = mean(speed),
            delay = sum(delay)) %>%
  ungroup() %>%
  mutate(hour = hour(date),
         day = wday(date, label = TRUE)) %>%
  filter(!str_detect(day, "Sat|Sun")) %>% 
  group_by(osmid, freeway, day, hour) %>%
  summarise(n = mean(n),
            speed = mean(speed),
            delay = mean(delay)) %>%
  mutate(peak = peaker(day, hour)) %>%
  ungroup()

glimpse(weekly)

##

series <- 
  weekly %>%
  left_join(covariates) %>%
  left_join(cleaning) %>%
  select(osmid, n, speed, delay, highway, oneway, freeway, day, hour, peak, everything()) %>%
  mutate(eccen = scale(eccen)[, 1],
         degri = scale(degri)[, 1], 
         grade = scale(grade)[, 1],
         bearing = scale(bearing)[, 1],
         commutes = scale(commutes)[, 1],
         log_n = log(n))

##

library(lme4)
library(broom)

##

lme <-  lmer(log_n ~ day:hour + day:peak + commutes +
                freeway + oneway + highway + eccen + degri + grade + bearing +
                (1 + hour||osmid),
              data = series)

fit <-
  augment(lme) %>% 
  mutate(mae = abs(.fitted - log_n),
         mape = abs(.fitted - log_n) / log_n)

mean(fit$mae)
mean(fit$mape)

##

ggplot(fit, aes(x = log_n, y = .fitted, colour = .resid)) +
  geom_hex(bins = 100) +
  geom_abline(linetype = 2, colour = '#7d7d7d', size = 1) +
  scale_fill_gradientn(colours = pal, 
                       guide = 'none') +
  annotate(geom = "segment", x = (log(1) + log(2) / 2), y = 6, xend = log(1), yend = 5, 
           arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "segment", x = (log(1) + log(2) / 2), y = 6, xend = log(2), yend = 5, 
           arrow = arrow(length = unit(2, "mm"))) + 
  annotate(geom = "text", x = 2, y = 6.2, 
           label = 'problems at segments with 1 or 2 jams', fontface = 'bold', 
           colour = '#000000') +
  xlab("log(jams)") +
  ylab("fitted values") +
  scale_y_continuous(breaks = c(-1, 0 , 1, 2, 3, 4, 5, 6)) +
  scale_x_continuous(breaks = c(-1, 0 , 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  coord_fixed() +
  theme_ver() + 
  ggsave("errors.png", height = 6, width = 8, dpi = 300)

##

series <- 
  temporal %>%
  select(osmid, date, n, speed, delay) %>%
  left_join(weather) %>%
  left_join(covariates)

##

library(photobiology)

##

links <- 
  streets %>%
  st_drop_geometry() %>%
  transmute(from = u,
            to = v)

verts <-
  bind_rows(transmute(links, id = from),
            transmute(links, id = to)) %>%
  distinct()

graph <- graph_from_data_frame(links, vertices = verts, directed = FALSE)

spect <- spectrum(graph)

spectibl <- 
  tibble(value = spect[[3]][, 1]) %>%
  mutate(spectral = rescale(value, to = c(400, 700))) %>%
  mutate(hex = w_length2rgb(spectral)) %>%
  arrange(spectral)

options(scipen = 999)

ggplot(data = spectibl) +
  geom_bar(aes(x = spectral, y = 100, fill = hex), stat = 'identity') +
  scale_fill_manual(values = unique(spectibl$hex), guide = 'none') +
  theme_void() + 
  ggsave("spectrum.png", height = 6, width = 8, dpi = 300)
