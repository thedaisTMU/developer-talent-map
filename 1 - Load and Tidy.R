#StackOverflow Developer Talent Map for Canadian Cities and Provinces
##Script for static preprocessing 

library(rnaturalearth)
library(leaflet)
library(tidyverse)
library(sf)

#Download and subset map from www.naturalearthdata.com
provinces <- ne_states(country = 'canada', returnclass = c('sf'))
provinces <- provinces[provinces$OBJECTID_1 != 5682,] #Filter out Canada as a whole

#Process provincial data
provincedev <- read_csv("devroles-province.csv") #%>%
  #filter(dev_role == "android developers") #Test with single dev role for now

provincelookup <- read_csv("provincelookup.csv")
provincedev <- left_join(provincedev, provincelookup)
provinces <- sp::merge(provinces, provincedev)

#Process city data
cities <- read_csv("devroles-city.csv") #%>%
  #filter(dev_role == "android developers") #Test with single dev role for now
citylookup <- read_csv("citylookup.csv")
cities <- left_join(cities, citylookup)

#Build Leaflet map without Shiny UI elements to test
leaflet(provinces) %>%
  setView(lng = -96.8, lat = 62.4, zoom = 4) %>% #Set at Canadian geographic centre
  addTiles(
    urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    attribution = 'Base from <a href="http://www.mapbox.com/">Mapbox</a>') %>%
    addPolygons(color = "#672146", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorNumeric("YlOrRd", visitors)(visitors),
              highlightOptions = highlightOptions(color = "white", weight = 1),
              label = ~paste0(gn_name,": ", visitors, " visitors"),
              labelOptions = labelOptions(style = list(
                "color" = "#002B49",
                "font-family" = "sans-serif",
                "box-shadow" = "3px 3px rgba(0,0,0,0.25)","font-family" = "sans",
                "border-width" = "1px",
                "border-color" = "rgba(0,0,0,0.5)"))) %>%
  addCircleMarkers(lng = ~cities$long, lat = ~cities$lat, weight = 1,
                   radius = ~(cities$visitors*4)^(1/3),
                   fillColor = ~colorNumeric("BuGn", cities$location_quotient)(cities$location_quotient),
                   fillOpacity = .9,
                   label = ~paste0(cities$cities,": ", visitors, " visitors"),
                   labelOptions = labelOptions(style = list(
                                                 "color" = "#002B49",
                                                 "font-family" = "sans-serif",
                                                 "box-shadow" = "3px 3px rgba(0,0,0,0.25)","font-family" = "sans",
                                                 "border-width" = "1px",
                                                 "border-color" = "rgba(0,0,0,0.5)")))

#Write files for app
write_sf(provinces, "provinces.shp")
write_csv(cities, "cities.csv")
rm(cities, citylookup, provincedev, provincelookup, provinces)
