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
    slice(i) %>%
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

##

roads <- read_sf("data/network/edges.shp")

##

graph <- sf_to_tidygraph(roads)

plot(graph,
     vertex.size = 0.5,
     vertex.label = '',
     alpha = 0.5)

##

network <- 
  graph %>%
  activate(edges) %>%
  filter(from != to) %>%
  mutate(length = st_length(geometry))

##

ggplot() +
  geom_sf(data = network %>% activate(edges) %>% as_tibble() %>% st_as_sf() %>% st_transform(st_crs(louisville)) %>% st_intersection(louisville), size = 0.1) + 
  geom_sf(data = network %>% activate(nodes) %>% as_tibble() %>% st_as_sf() %>% st_transform(st_crs(louisville)) %>% st_intersection(louisville), size = 0.1) +
  theme_void()

##

network <- 
  network %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree()) %>%
  mutate(betweenness = centrality_betweenness(weights = length)) %>%
  activate(edges) %>%
  mutate(betweenness = centrality_edge_betweenness(weights = length))

##

distances <- distances(graph = network, weights = network %>% activate(edges) %>% pull(length))

##

intersections <- 
  network %>% 
  activate(nodes) %>%
  as_tibble() %>%
  st_as_sf() %>%
  st_transform(2205) %>%
  st_coordinates() %>%
  as_tibble()

##

library(RANN)

##

waypoints <- 
  comb %>%
  filter(w_bg %in% louisville$GEOID) %>%
  filter(w_bg != h_bg) %>%
  gather(location, GEOID, h_bg:w_bg) %>%
  left_join(projected) %>%
  mutate(jobs = S000) %>%
  select(GEOID, id, location, X, Y, jobs)

nn <- nn2(intersections, select(waypoints, X, Y), k = 1, searchtype = "radius", radius = 10000)

length(nn$nn.idx[, 1])

nearest <-
  waypoints %>%
  mutate(NODEID = nn$nn.idx[, 1])

nearest <-
  network %>% 
  activate(nodes) %>%
  as_tibble() %>%
  right_join(nearest) %>%
  select(NODEID, id, location, X, Y, jobs, degree, betweenness, geometry)

##

chunk <-  pivot_wider(nearest, id_cols = id, names_from = location, values_from = NODEID)

##

routes <- tbl_graph()

##

for (i in 1:nrow(chunk)) {
  
  from_node <- chunk[i, 2]
  to_node <- chunk[i, 3]
  
  path <- 
    shortest_paths(graph = network,
                   from = from_node,
                   to = to_node,
                   output = 'both',
                   weights = network %>% activate(edges) %>% pull(length))
  
  path_graph <- 
    network %>%
    subgraph.edges(eids = path$epath %>% unlist()) %>%
    as_tbl_graph()
  
  path_graph <-
    path_graph %>%
    activate(edges) %>%
    mutate(length = path_graph %>%
             activate(edges) %>%
             as_tibble() %>%
             summarise(length = sum(length)) %>%
             pull(length),
           id = rep(chunk[i, 1], n()))
  
  routes <- bind_graphs(routes, path_graph)
  
}

path_graph %>% 
  activate(edges) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  select(length) %>% 
  plot()
