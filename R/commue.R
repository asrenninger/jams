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
  transmute(geo_code = GEOID) %>%
  st_centroid()

##

travel_network <- od2line(flow = od_data, zones = st_transform(cbgs_sf, 4326))
w <- flow$all / max(flow$all) *10
plot(travel_network, lwd = w)

##

library(dodgr)

routes <- line2route(travel_network, route_fun = route_dodgr)

##

routes <- tibble()

coords <- 
  cbgs %>% 
  st_transform(4326) %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(cbgs) %>%
  st_as_sf() %>%
  rename(geo_code = GEOID) %>%
  select(geo_code, X, Y)

for (i in 1:nrow(od_data)) {
  
  start  <- 
    od_data %>%
    slice(i)) %>%
    transmute(geo_code = geo_code1) %>%
    left_join(coords) %>%
    select(X, Y)
    
  finish <- 
    od_data %>%
    slice(i) %>%
    transmute(geo_code = geo_code2) %>%
    left_join(coords) %>%
    select(X, Y)
  
  route <- route_dodgr(from = start, to = finish) %>% 
    st_as_sf() %>%
    as_tibble()
  
  routes <- bind_rows(routes, mutate(route, type = i))

}  

