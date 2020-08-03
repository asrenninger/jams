source("R/help.r")
source("R/package.r")

##

library(lehdr)

##

main <- grab_lodes(state = "KY", year = 2015, 
                    segment = "S000", job_type = "JT01", 
                    lodes_type = "od", agg_geo = "bg", state_part = "main")

auxi <- grab_lodes(state = "KY", year = 2015, 
                    segment = "S000", job_type = "JT01", 
                    lodes_type = "od", agg_geo = "bg", state_part = "aux")

comb <- 
  rbind(main, auxi) %>% 
  filter(S000 > 10)

##

rm(main)
rm(auxi)

##

library(tigris)

##

cbgs <- 
  rbind(
    block_groups(state = "KY", cb = TRUE, class = 'sf') %>%
      st_transform(2205),
    block_groups(state = "IN", cb = TRUE, class = 'sf') %>%
      st_transform(2205))

projected <- 
  cbgs %>% 
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(cbgs) %>%
  st_as_sf() %>%
  select(GEOID, X, Y)

##

comb$id <- paste(comb$w_bg, comb$h_bg, sep = " to ")

##

louisville <- block_groups(state = "KY", county = "Jefferson", cb = TRUE, class = 'sf')

##

comb %>%
  filter(w_bg %in% louisville$GEOID) %>%
  gather(location, GEOID, h_bg:w_bg) %>%
  left_join(projected) %>%
  select(-geometry) %>%
  st_as_sf(coords = c("X", "Y"), crs = 2205) %>%
  group_by(id) %>%
  summarise(jobs = mean(S000)) %>%
  st_cast("MULTIPOINT") %>%
  st_cast("LINESTRING") %>%
  mutate(length = st_length(geometry)) %>%
  st_write("lodes.shp")

##

devtools::install_github("ropensci/stplanr")
library(stplanr)

##

od_data <- 
  comb %>%
  filter(w_bg %in% louisville$GEOID) %>%
  transmute(geo_code1 = h_bg,
            geo_code2 = w_bg,
            jobs = S000) %>%
  filter(geo_code1 != geo_code2)

cbgs_sf <- 
  projected %>%
  st_transform(4326) %>%
  transmute(geo_code = GEOID) %>%
  st_centroid()
  
##

travel_network <- od2line(flow = od_data, zones = st_transform(cbgs_sf, 4326))
w <- flow$all / max(flow$all) *10
plot(travel_network, lwd = w)

##

cbgs_sf <-
  cbgs_sf %>%
  st_coordinates() %>%
  as_tibble() %>% 
  bind_cols(cbgs_sf) %>% 
  select(-geometry)

od_data %>% 
  mutate(id = paste(geo_code1, geo_code2, sep = " to ")) %>%
  mutate(geo_code = geo_code1) %>%
  left_join(cbgs_sf) %>%
  rename(X_h = X, 
         Y_h = Y) %>%
  mutate(geo_code = geo_code2) %>%
  left_join(cbgs_sf) %>%
  rename(X_w = X, 
         Y_w = Y) %>%
  rename(geocode_h = geo_code1,
         geocode_w = geo_code2) %>%
  select(id, geocode_h, geocode_w, X_h, Y_h, X_w, Y_w, jobs) %>%
  write_csv("chunk.csv")

##

travel_streets <- read_sf("data/imputed.shp")

##

cbgs_sf <- 
  projected %>%
  st_transform(4326) %>%
  transmute(geo_code = GEOID)

##

o <- 
  od_data %>% 
  mutate(id = paste(geo_code1, geo_code2, sep = " to ")) %>%
  mutate(geo_code = geo_code1) %>%
  left_join(cbgs_sf) %>%
  st_as_sf()

d <- 
  od_data %>% 
  mutate(id = paste(geo_code1, geo_code2, sep = " to ")) %>%
  mutate(geo_code = geo_code2) %>%
  left_join(cbgs_sf) %>%
  st_as_sf()

origin_dots <- 
  reduce(
    map(1:nrow(od_data), 
        ~st_sample(o$geometry[.x], size = round(o$jobs[.x] / 2), type = 'random') %>%
          st_cast('POINT') %>%
          st_coordinates() %>%
          as_tibble() %>%
          set_names(c("X_h", "Y_h")) %>%
          mutate(id = o$id[.x])),
    rbind)

destin_dots <- 
  reduce(
    map(1:nrow(od_data), 
        ~st_sample(d$geometry[.x], size = round(d$jobs[.x] / 2), type = 'random') %>%
          st_cast('POINT') %>%
          st_coordinates() %>%
          as_tibble() %>%
          set_names(c("X_w", "Y_w")) %>%
          mutate(id = d$id[.x])),
    rbind)

nrow(destin_dots) == nrow(origin_dots)

##

origin_dots %>% 
  st_as_sf(coords = c("X_h", "Y_h"), remove = FALSE) %>% 
  select(id) %>% 
  plot() 

destin_dots %>% 
  st_as_sf(coords = c("X_w", "Y_w"), remove = FALSE) %>% 
  select(id) %>% 
  plot() 

dots <- 
  bind_cols(origin_dots %>% 
              mutate(id_o = id) %>%
              select(-id),
            destin_dots %>%
              mutate(id_d = id) %>%
              select(-id)) %>%
  select(id_o, id_d, X_h, Y_h, X_w, Y_w)

sum(dots$id_o == dots$id_d)

dots %>%
  mutate(id = id_o) %>%
  select(-id_o, -id_d) %>%
  select(id, everything()) %>%
  write_csv("dots.csv")

##s