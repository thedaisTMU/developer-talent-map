#StackOverflow Developer Talent Map for Canadian Cities and Provinces
##R Shiny Map Widget

##To do
#Shiny UI - make metric and dev role single select, then dev role multiselect
#Add legend
#Brookfield colors, fonts and style


library(leaflet)
library(tidyverse)
library(sf)
library(shiny)

#Import data from load scripts
provinces <- read_sf("provinces.shp")
cities <- read_csv("cities.csv")

#Dropdown choices
role <- c(
#put devroles here
)

metric <- c(
  "Visitors" = "visitrs",
  "Share" = "share",
  "Location Quotient" = "locquo"
)

# Define UI
ui <- navbarPage("StackOverflow Developer Talent Map for Canadian Cities and Provinces", id="nav",
  
  #tabPanel("Interactive map"),
  tabPanel("Interactive map",
           div(class="outer",
               
               tags$head(
                 # Include custom CSS
                 includeCSS("styles.css")
               ),
  
  tags$style(type = "text/css", "#map {height: calc(100vh) !important;}"),
  leafletOutput("map", width = "100%"),

  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "auto",
                h2("About this app"),
                selectInput("color", "Color", metric)
  )
)
)
)
                
#Draw Leaflet maps
server <- function(input, output) {
   
   output$map <- renderLeaflet({
     #Filter roles
     cities <- filter(cities, dev_role == input$cityrole)
     provinces <- filter(cities, provinces == input$provrole)
     
     #Draw map
     leaflet(provinces) %>%
       setView(lng = -96.8, lat = 62.4, zoom = 4) %>% #Set at Canadian geographic centre
       addTiles(
         urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
         attribution = 'Base from <a href="http://www.mapbox.com/">Mapbox</a>') %>%
       addPolygons(color = "#672146", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5,
                   fillColor = ~colorNumeric("YlOrRd", visitrs)(visitrs),
                   highlightOptions = highlightOptions(color = "white", weight = 1),
                   label = ~paste0(gn_name,": ", visitrs, " visitors"),
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
                        label = ~paste0(cities$cities,": ", cities$visitors, " visitors"),
                        labelOptions = labelOptions(style = list(
                          "color" = "#002B49",
                          "font-family" = "sans-serif",
                          "box-shadow" = "3px 3px rgba(0,0,0,0.25)","font-family" = "sans",
                          "border-width" = "1px",
                          "border-color" = "rgba(0,0,0,0.5)")))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

