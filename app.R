#Stack Overflow Developer Talent Map for Canadian Cities and Provinces
##R Shiny Map Widget
#setwd("~/GitHub/developer-talent-map")

##To do
library(leaflet)
library(formattable)
library(tidyverse)
library(sf)
library(shiny)

#Preprocessing ===================
#Import data from load scripts
provinces <- read_sf("provinces.shp")
cities <- read_csv("cities.csv")

#Change names on province fields and preserve SF object type
names(provinces)[names(provinces)=="dev_rol"] <- "dev_role"
names(provinces)[names(provinces)=="visitrs"] <- "visitors"
names(provinces)[names(provinces)=="prvnc__"] <- "share"
names(provinces)[names(provinces)=="lctn_qt"] <- "loc_quo"

#Format values
cities <- st_as_sf(cities, coords = c("long", "lat"), crs = 4326)

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

#DefineUI ===================
ui <- function(request) {
  fillPage(theme = "styles.css",
               title = "Stack Overflow Canadian Developer Talent Map",
    div(style = "width: 100%; height: 100%;",
        leafletOutput("map", width = "100%", height = "100%"),
        absolutePanel(id = "controls", class = "panel panel-default", draggable = TRUE, fixed = TRUE,
                      top = 90, left = 20, right = "auto", bottom = "auto", 
                      width = "250px", height = "auto",
                      selectInput("metric", "Web Traffic Metric", metric),
                      selectInput("role", "Developer Role", role),
                                  # list("Role Groups" = c("All Developers", "Mobile Developers",
                                  #                        "Web Developers", "Other Developers"),
                                  #      "Roles" = role))
                      radioButtons("juris", "Jurisdiction", choices = c("Cities", "Provinces"), selected = "Cities", inline = TRUE),
                      bookmarkButton(title = "Bookmark this view and get a URL to share")
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
                
#Server ===================
server <- function(input, output, session) {
  
  #If metric changes, set metric label name
  labelmetric <- reactive({
    if (input$metric == "visitors") {
      labelmetric <- names(metric[1])
    } else if (input$metric == "share") {
      labelmetric <- names(metric[2])
    } else {
      labelmetric <- names(metric[3])
    }
    })
  
  
  metricpal <- reactive({
    mapmetric = 5
    metricpal <- colorBin(
      palette = c("#DDDDDD","#E24585"),
      domain = c(min(mapmetric), max(mapmetric)), #generalize metric
      n=7, pretty=TRUE)
    })
  
  # #Group roles - need input from team
  # if(input$role == "All Developers"){
  #   input$role <- role
  # } else if(input$role == "Mobile Developers"){
  #   input$role <- c("android developers", "embedded developers", "ios developers")
  # } else if(input$role == "Web Developers"){
  #   input$role <- c("back-end web", "front-end web", "full-stack web", "graphics programmers")
  # } else if(input$role == "Other Developers"){
  #   input$role <- c("data scientists", "database administrators", "desktop developers", 
  #                   "machine learning specialists", "systems administrators")
  # }
  
  #Draw base map
   output$map <- renderLeaflet({
     labelmetric <- labelmetric()
     metricpal <- metricpal()
     
     leaflet() %>%
       fitBounds(lng1 = -124, 
                 lat1 = 42, 
                 lng2 = -63, 
                 lat2 = 54) %>%
       addProviderTiles(providers$Stamen.TonerLite) %>% #CartoDB Positron looks good, too, but a little busier
       addLegend("bottomright", pal = metricpal, values = provinces[[input$metric]], #change to mapdata
                 title = labelmetric, opacity = .95)
     })
   
   observe({
     if (input$juris == "Provinces") {

     #Select and filter jurisdictional data for role and metric
     provinces <- provinces[provinces$dev_role == input$role,]
     provmetric <- provinces[[input$metric]]

     #Add province polygons
     leafletProxy("map", data = provinces) %>%
       clearShapes() %>%
       addPolygons(color = ~metricpal(provmetric), weight = 1, smoothFactor = 0.5,
                   opacity = 1.0, fillOpacity = 0.7,
                   highlightOptions = highlightOptions(color = "white", weight = 1),
                   label = ~paste0(gn_name," - ", labelmetric, ": ", provmetric),
                   labelOptions = labelOptions(style = list(
                     "font-family" = "sans-serif",
                     "box-shadow" = "3px 3px rgba(0,0,0,0.25)","font-family" = "sans",
                     "border-width" = "1px",
                     "border-color" = "rgba(0,0,0,0.5)"))) #%>%
   } else if (input$juris == "Cities") {
     #Select and filter jurisdictional data for role and metric
     cities <- cities[cities$dev_role == input$role,]
     if (input$metric == "loc_quo") {cities <- cities[is.na(cities$loc_quo) == FALSE,]}

     citymetric <- cities[[input$metric]]
     cityrad <- (citymetric/mean(citymetric)*500)^.4

     #Add city markers
     leafletProxy("map", data = cities) %>%
       addCircleMarkers(
         #lng = ~cities$long, lat = ~cities$lat,
         weight = 1,
         radius = ~cityrad,
         color = "#E24585",
         fillColor = ~metricpal(citymetric),
         fillOpacity = .8,
         label = ~paste0(cities$cities," - ", labelmetric, ": ", citymetric),
         labelOptions = labelOptions(style = list(
           "font-family" = "sans-serif",
           "box-shadow" = "3px 3px rgba(0,0,0,0.25)","font-family" = "sans",
           "border-width" = "1px",
           "border-color" = "rgba(0,0,0,0.5)")))
   } #Else close
   }) #Observe close
   } #Server close

# Run the application
enableBookmarking()
shinyApp(ui = ui, server = server)

