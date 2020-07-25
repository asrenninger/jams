########################################
## Animating
########################################

## Packages

library(lubridate)
library(glue)
library(gganimate)

## To make it easy, we animate only the month of june
jams <- vroom("data/jams.csv") 

sample <-
  jams %>%
  mutate(day = wday(pub_utc_date, label = TRUE),
         month = month(pub_utc_date, label = TRUE),
         num = day(pub_utc_date),
         year = year(pub_utc_date),
         hour = hour(pub_utc_date)) %>%
  mutate(string = glue("{day}, {month} {num}, {year}")) %>%
  mutate(time = format(pub_utc_date, '%A, %B %d')) %>%
  mutate(rounded = round_date(pub_utc_date, "hour")) 

## This adds a placeholder for hours without jams
test <- 
  tibble(rounded = seq.POSIXt(from = min(sample$rounded), to = max(sample$rounded), by = "hour"),
         longitude = -85.40492,
         latitude = 38.37850,
         delay = 0.0) %>%
  bind_rows(sample) %>%
  select(rounded, longitude, latitude, delay)

## Create a background
library(tigris)
library(sf)

## We need roads and the county
roads <- roads("KY", "Jefferson", class = 'sf')
tracts <- tracts("KY", "Jefferson", class = 'sf')

water <- 
  area_water("KY", "Jefferson", class = 'sf') %>%
  st_union() %>%
  st_combine()

## Then some new packages work with them
options(scipen = 9999)

library(tidyverse)
library(scales)
library(magrittr)
library(classInt)
library(janitor)

## This divides up categories of road
roads_local <- 
  roads %>% 
  filter(str_detect(MTFCC, "S1200|S1400")) %>%
  filter(str_detect(RTTYP, "M|C"))

roads_global <- 
  roads %>% 
  filter(str_detect(RTTYP, "U|S")) %>% 
  select(RTTYP)

## Then reduces them to their geometries with no other attributes

background_roads_g <-
  roads_global %>%
  st_union() %>%
  st_combine()

background_roads_l <-
  roads_local %>%
  st_union() %>%
  st_combine()

## This will be our backdrop
background <-
  tracts %>%
  mutate(dissolve = 1) %>%
  group_by(dissolve) %>%
  summarise() %>%
  st_difference(water)

## Turn this into a basemap
base <- 
  ggplot() +
  geom_sf(data = background, 
          aes(), fill = '#8a8a8a', colour = NA, size = 0, alpha = 0.5) +
  geom_sf(data = background_roads_l, 
          aes(), colour = '#E3E3E3', size = 0.1, alpha = 0.5) +
  geom_sf(data = background_roads_g, 
          aes(), colour = '#E3E3E3', size = 0.5, alpha = 0.75) +
  theme_map()

##
library(scales)

## With both of these elements, we can animate
animate <-
  base +
  geom_point(data = test, #%>%
               #filter(rounded < as_date('2018-06-8 00:00:00')), 
             #aes(longitude, latitude, size = delay, color = discreter(delay, 9)), alpha = 0.5, 
             aes(longitude, latitude, size = delay, color = delay / 60), alpha = 0.5, 
             show.legend = TRUE) +
  #scale_color_manual(values = pal,
  #                   labels = labeller(test$delay, 4),
  #                   name = "delay (minutes)",
  #                   guide = guide_discrete) +
  scale_color_gradientn(colors = pal,
                        name = "delay (minutes)",
                        guide = guide_continuous,
                        limits = c(0, 100), oob = squish) +
  scale_size_continuous(guide = FALSE) +
  xlim(-85.94412, -85.40490) +
  ylim(37.99721, 38.37852) +
  labs(title = "{frame_time}", subtitle = "JAMS OVER TIME") +
  transition_time(rounded) + 
  ease_aes(x = 'sine-out', y = 'sine-out') + 
  shadow_wake(0.05, size = 3, alpha = TRUE, wrap = FALSE, 
              falloff = 'sine-in')

## Then save it
anim_save("jams.gif", animation = animate, 
          height = 780, width = 810, nframes = 200, fps = 5,
          start_pause = 2, end_pause = 2)

anim_save("jams_trimmed.gif", animation = animate, 
          height = 600, width = 620, nframes = 100, fps = 5,
          start_pause = 2, end_pause = 2)

