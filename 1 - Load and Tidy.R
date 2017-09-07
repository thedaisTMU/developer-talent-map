#StackOverflow Developer Talent Map for Canadian Cities and Provinces
##Script for static preprocessing 

##To do
#Hover-over label: add all metrics and style
#Brookfield colors, fonts and style
#Shiny UI - make metric single select, and dev role multiselect, or miniUI; try floating transparent overlay or miniUI

library(rnaturalearth)
library(leaflet)
library(tidyverse)

#Download and subset map from www.naturalearthdata.com
region <- ne_states(country = 'canada', returnclass = c('sf'))
region <- region[region$OBJECTID_1 != 5682,] #Filter out Canada as a whole

#Process provincial data
provinces <- read_csv("devroles-province.csv") %>%
  filter(dev_role == "android developers") #Test with single dev role for now

provincelookup <- read_csv("provincelookup.csv")
provinces <- left_join(provinces, provincelookup)
region <- sp::merge(region, provinces)

#Process city data
cities <- read_csv("devroles-city.csv") %>%
  filter(dev_role == "android developers") #Test with single dev role for now
citylookup <- read_csv("citylookup.csv")
cities <- left_join(cities, citylookup)

#Build Leaflet map without Shiny UI elements to test
leaflet(region) %>%
  setView(lng = -96.8, lat = 62.4, zoom = 4) %>% #Set at Canadian geographic centre
  addTiles(
    urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    attribution = 'Base from <a href="http://www.mapbox.com/">Mapbox</a>') %>%
    addPolygons(color = "#672146", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorNumeric("YlOrRd", visitors)(visitors),
              highlightOptions = highlightOptions(color = "white", weight = 1)) %>%
  addCircleMarkers(lng = ~cities$long, lat = ~cities$lat, weight = 1,
                   radius = ~(cities$visitors*4)^(1/3),
                   fillColor = ~colorNumeric("BuGn", cities$location_quotient)(cities$location_quotient),
                   fillOpacity = .9,
                   label = ~paste0(cities$cities,", ", visitors, " visitors")) #Style labels with Brookfield brand - https://rstudio.github.io/leaflet/popups.html

