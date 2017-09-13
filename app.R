#StackOverflow Developer Talent Map for Canadian Cities and Provinces
##R Shiny Map Widget

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
cities$loc_quo <- percent(cities$loc_quo ,1)

provinces$visitors <- comma(provinces$visitors, 0)
provinces$share <- percent(provinces$share)
provinces$loc_quo <- percent(provinces$loc_quo, 1)

#Dropdown choices - properly name for next version
role <- unique(cities$dev_role)

metric <- c(
  "Visitors" = "visitors",
  "Percentage of local developers in selected roles" = "share",
  "Relative share of develeopers compared to Canadian average" = "loc_quo"
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
                selectInput("metric", "Web Traffic Metric", metric),
                selectInput("role", "Developer Role", role)
  ),
  tags$div(id="cite",
           'Application developed by ', tags$a(href="https://asherzafar.github.io/", "Asher Zafar"), ' for the Brookfield Institute for Innovation and Entrepreneurship (BII+E)'
  )
)
)
)
                
#Draw Leaflet maps
server <- function(input, output) {
   
   output$map <- renderLeaflet({
     
     #Filter roles
     cities <- cities[cities$dev_role == input$role,]
     provinces <- provinces[provinces$dev_role == input$role,]
     
     #Define metric
     provmetric <- provinces[[input$metric]]
     citymetric <- cities[[input$metric]]
     
     #Will consider preprocessing and normalize these by developer role
     if (input$metric == "visitors") {
       cityrad <- (citymetric*4)^(1/3)
       labelmetric <- "Visitors"
     } else if (input$metric == "share") {
       cityrad <- citymetric*500
       labelmetric <- "Percentage of local developers in selected roles"
     } else {
       cityrad <- citymetric*20
       labelmetric <- "Relative share of develeopers compared to Canadian average"
     }
       
     #Draw map
     leaflet(provinces) %>%
       setView(lng = -96.8, lat = 62.4, zoom = 4) %>% #Set at Canadian geographic centre
       addTiles(
         urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
         attribution = 'Base from <a href="http://www.mapbox.com/">Mapbox</a>') %>%
       addPolygons(color = "#672146", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5,
                   fillColor = ~colorNumeric("BuGn", provmetric)(provmetric),
                   highlightOptions = highlightOptions(color = "white", weight = 1),
                   label = ~paste0(gn_name," - ", labelmetric, ": ", provmetric),
                   labelOptions = labelOptions(style = list(
                     "color" = "#002B49",
                     "font-family" = "sans-serif",
                     "box-shadow" = "3px 3px rgba(0,0,0,0.25)","font-family" = "sans",
                     "border-width" = "1px",
                     "border-color" = "rgba(0,0,0,0.5)"))) %>%
       addCircleMarkers(lng = ~cities$long, lat = ~cities$lat, weight = 1,
                        radius = ~cityrad,
                        fillColor = ~colorNumeric("BuGn", citymetric)(citymetric),
                        fillOpacity = .9,
                        label = ~paste0(cities$cities," - ", labelmetric, ": ", citymetric),
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

