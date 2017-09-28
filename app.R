#Stack Overflow Developer Talent Map for Canadian Cities and Provinces
##R Shiny Map Widget
#setwd("~/GitHub/developer-talent-map")

##To do
library(leaflet)
library(formattable)
library(tidyverse)
library(sf)
library(shiny)

#Test variables if needed
# input <- NA
# input$role <- "android developers"
# input$metric <- "share"

#Import data from load scripts
provinces <- read_sf("provinces.shp")
cities <- read_csv("cities.csv")

#Change names on province fields and preserve SF object type
names(provinces)[names(provinces)=="dev_rol"] <- "dev_role"
names(provinces)[names(provinces)=="visitrs"] <- "visitors"
names(provinces)[names(provinces)=="prvnc__"] <- "share"
names(provinces)[names(provinces)=="lctn_qt"] <- "loc_quo"

#Format values
cities$visitors <- comma(cities$visitors, 0)
cities$share <- percent(cities$share, 1)
cities$loc_quo <- comma(cities$loc_quo ,2)

provinces$visitors <- comma(provinces$visitors, 0)
provinces$share <- percent(provinces$share)
provinces$loc_quo <- comma(provinces$loc_quo, 2)

#Dropdown choices
role <- unique(cities$dev_role)

metric <- c(
  "Visitors" = "visitors",
  "% Local developers in role" = "share",
  "Location quotient" = "loc_quo"
)

# Define UI
ui <- function(request) {
  fillPage(theme = "styles.css",
               title = "Stack Overflow Canadian Developer Talent Map",
    div(style = "width: 100%; height: 100%;",
        leafletOutput("map", width = "100%", height = "100%"),
        absolutePanel(id = "controls", class = "panel panel-default", draggable = TRUE, fixed = TRUE,
                      top = 90, left = 20, right = "auto", bottom = "auto", 
                      width = "30%", height = "auto",
                      selectInput("metric", "Web Traffic Metric", metric),
                      selectInput("role", "Developer Role",
                                  list("Role Groups" = c("All Developers", "Mobile Developers",
                                                         "Web Developers", "Other Developers"),
                                       "Roles" = role)),
                      bookmarkButton(title = "Bookmark your choices and get a URL for sharing")
                      ),
        h3(tags$div(id="apptitle",
                    tags$a(href="http://brookfieldinstitute.ca/", img(src='brookfield_mark_small.png', align = "left")),
                    "Stack Overflow Canadian Developer Talent Map"
                    )
           ),
        tags$div(id="cite",
                 'Application developed by ',
                 tags$a(href="", "Asher Zafar"),
                 " for the ",
                 tags$a(href="http://brookfieldinstitute.ca/"," Brookfield Institute for Innovation and Entrepreneurship (BII+E)."),
                 tags$a(href="", "Full report"),
                  "by David Rubinger and Creig Lamb")
    )
  )
}
                
#Draw Leaflet maps
server <- function(input, output) {
   
   output$map <- renderLeaflet({
     
     #Filter roles
     cities <- cities[cities$dev_role == input$role,]
     provinces <- provinces[provinces$dev_role == input$role,]
     
     if (input$metric == "loc_quo"){
       cities <- cities[is.na(cities$loc_quo) == FALSE,]
     }
     
     #Define metric
     provmetric <- provinces[[input$metric]]
     citymetric <- cities[[input$metric]]
     
     #Will consider preprocessing and normalizing these by developer role
     if (input$metric == "visitors") {
       cityrad <- (citymetric*4)^(1/3)
       labelmetric <- names(metric[1])
     } else if (input$metric == "share") {
       cityrad <- citymetric*500
       labelmetric <- names(metric[2])
     } else {
       cities <- cities[is.na(cities$loc_quo) == FALSE,]
       cityrad <- citymetric*20
       labelmetric <- names(metric[3])
     }
     
     #Make color palettes
     metricpal <- colorBin(
       palette = c("#DDDDDD","#E24585"),
       #domain = provmetric, 
       domain = c(min(provmetric, citymetric), max(provmetric, citymetric)),
       n=7, pretty=TRUE
     )
       
     #Draw map
     leaflet(provinces) %>%
       
       fitBounds(lng1 = -124, 
                 lat1 = 42, 
                 lng2 = -63, 
                 lat2 = 54) %>%
       
       addTiles(
         urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
         attribution = 'Base from <a href="http://www.mapbox.com/">Mapbox</a>') %>%
       
       addPolygons(color = ~metricpal(provmetric), weight = 1, smoothFactor = 0.5, 
                   opacity = 1.0, fillOpacity = 0.7,
                   highlightOptions = highlightOptions(color = "white", weight = 1),
                   label = ~paste0(gn_name," - ", labelmetric, ": ", provmetric),
                   labelOptions = labelOptions(style = list(
                     "font-family" = "sans-serif",
                     "box-shadow" = "3px 3px rgba(0,0,0,0.25)","font-family" = "sans",
                     "border-width" = "1px",
                     "border-color" = "rgba(0,0,0,0.5)"))) %>%
       
       addCircleMarkers(lng = ~cities$long, lat = ~cities$lat, weight = 1,
                        #radius = ~cityrad, #May leave off unless we figure out numbers that work
                        color = "#E24585",
                        fillColor = ~metricpal(citymetric),
                        fillOpacity = .65,
                        label = ~paste0(cities$cities," - ", labelmetric, ": ", citymetric),
                        labelOptions = labelOptions(style = list(
                          "font-family" = "sans-serif",
                          "box-shadow" = "3px 3px rgba(0,0,0,0.25)","font-family" = "sans",
                          "border-width" = "1px",
                          "border-color" = "rgba(0,0,0,0.5)"))) %>%
       
       addLegend("bottomright", pal = metricpal, values = provinces[[input$metric]],
                 title = labelmetric, opacity = .65
       )
     
   })
}

# Run the application
enableBookmarking()
shinyApp(ui = ui, server = server)

