#StackOverflow Developer Talent Map for Canadian Cities and Provinces
##Script for static preprocessing 

library(rnaturalearth)
library(leaflet)
library(tidyverse)
library(sf)
library(tools)

setwd("~/GitHub/developer-talent-map")

#Download and subset map from www.naturalearthdata.com
provinces <- ne_states(country = 'canada', returnclass = c('sf'))
provinces <- provinces[provinces$OBJECTID_1 != 5682,] #Filter out Canada as a whole

#Process provincial data
provincedev <- read_csv("devroles-province.csv")

provincelookup <- read_csv("provincelookup.csv")
provincedev <- left_join(provincedev, provincelookup)
provinces <- sp::merge(provinces, provincedev)

#Process city data
cities <- read_csv("devroles-city.csv")
citylookup <- read_csv("citylookup.csv")
cities <- left_join(cities, citylookup)
cities <- rename(cities, share = city_visitors_share, loc_quo = location_quotient)

#Proper names for developer roles
cities$dev_role <- toTitleCase(cities$dev_role)
cities$dev_role[cities$dev_role=="Ios Developers"] <- "iOS Developers"

provinces$dev_role <- toTitleCase(provinces$dev_role)
provinces$dev_role[provinces$dev_role=="Ios Developers"] <- "iOS Developers"

#Write files for app
write_sf(provinces, "provinces.shp")
write_csv(cities, "cities.csv")
rm(list=ls())
