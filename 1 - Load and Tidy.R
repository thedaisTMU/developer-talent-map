#StackOverflow Developer Talent Map for Canadian Cities and Provinces
##Script for static preprocessing 

library(rnaturalearth)
library(leaflet)
library(tidyverse)
library(sf)
library(tools)

setwd("~/GitHub/developer-talent-map")

#Provinces===========================
#Download and subset map from www.naturalearthdata.com
provinces <- ne_states(country = 'canada', returnclass = c('sf'))
provinces <- provinces[provinces$OBJECTID_1 != 5682,] #Filter out Canada as a whole

#Process provincial data
provincedev <- read_csv("devroles-province.csv")

provincelookup <- read_csv("provincelookup.csv")
provincedev <- left_join(provincedev, provincelookup)
provinces <- sp::merge(provinces, provincedev)

provinces$dev_role <- toTitleCase(provinces$dev_role)
provinces$dev_role[provinces$dev_role=="Ios Developers"] <- "iOS Mobile"
provinces$dev_role[provinces$dev_role=="Android Developers"]  <- "Android Mobile"
provinces$dev_role[provinces$dev_role=="Desktop Developers"] <- "Desktop"
provinces$dev_role[provinces$dev_role=="Embedded Developers"]  <- "Embedded"
provinces$dev_role[provinces$dev_role=="all Developers"] <- "All Developers"

#Simplify province polygons
provinces <- rmapshaper::ms_simplify(provinces)

#Cities===========================
#Process and filter city data
cities <- read_csv("devroles-city.csv")
citylookup <- read_csv("citylookup.csv")
cities <- left_join(cities, citylookup)
cities <- rename(cities, share = city_visitors_share, loc_quo = location_quotient)
cities <- filter(cities, !(dev_role %in% c("biz intel developers", "highly technical designers", "highly technical product managers", "qa engineers")), 
                 city %in% c("calgary", "edmonton", "guelph", "halifax", "hamilton", "kitchener_waterloo",
                                     "london", "montreal", "ottawa", "quebec city", "regina", "saskatoon", 
                                     "toronto", "vancouver", "victoria", "winnipeg", "nyc_metro_area", "sf_silicon_valley")) %>%
  select(-region, -dev_role_parent_group)

#Proper names for developer roles
cities$dev_role <- toTitleCase(cities$dev_role)
cities$dev_role[cities$dev_role=="Ios Developers"] <- "iOS Mobile"
cities$dev_role[cities$dev_role=="Android Developers"] <- "Android Mobile"
cities$dev_role[cities$dev_role=="Desktop Developers"] <- "Desktop"
cities$dev_role[cities$dev_role=="Embedded Developers"] <- "Embedded"
cities$dev_role[cities$dev_role=="all Developers"] <- "All Developers"
#cities <- cities[complete.cases(cities),]

#Reformat cities to shapefile
cities <- st_as_sf(cities, coords = c("long", "lat"), crs = 4326)

#Write files for app
write_sf(provinces, "provinces.shp")
write_sf(cities, "cities.shp")

rm(list=ls())
