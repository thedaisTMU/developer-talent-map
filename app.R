#Stack Overflow Developer Talent Map for Canadian Cities and Provinces
##R Shiny Map Widget
#setwd("~/GitHub/developer-talent-map")

##To do
library(leaflet)
library(formattable)
library(tidyverse)
library(sf)
library(shiny)

#Preprocessing - move some to load script ===================
#Import data from load scripts
provinces <- read_sf("provinces.shp")
cities <- read_sf("cities.shp")
#cities <- read_csv("cities.csv")

#Change names on province fields and preserve SF object type
names(provinces)[names(provinces)=="dev_rol"] <- "dev_role"
names(provinces)[names(provinces)=="visitrs"] <- "visitors"
names(provinces)[names(provinces)=="prvnc__"] <- "share"
names(provinces)[names(provinces)=="lctn_qt"] <- "loc_quo"

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
               #title = "Stack Overflow Canadian Developer Talent Map",
    div(style = "width: 100%; height: 100%;",
        leafletOutput("map", width = "100%", height = "100%"),
        absolutePanel(id = "controls", class = "panel panel-default", draggable = TRUE, fixed = TRUE,
                      top = 10, left = 65, right = "auto", bottom = "auto", 
                      width = "250px", height = "auto",
                      h1("Canadian Developer Talent Map"),
                      selectInput("metric", "Web Traffic Metric", metric, selectize = FALSE), #Draggable and selectize seem incompatible for scrolling
                      selectInput("role", "Developer Role", role, selectize = FALSE),
                                  # list("Role Groups" = c("All Developers", "Mobile Developers",
                                  #                        "Web Developers", "Other Developers"),
                                  #      "Roles" = role))
                      radioButtons("juris", "Jurisdiction", choices = c("Cities", "Provinces"), selected = "Cities", inline = TRUE),
                      bookmarkButton(title = "Bookmark this view and get a URL to share")
                      ),
        # h3(tags$div(id="apptitle",
        #             tags$a(href="http://brookfieldinstitute.ca/", img(src='brookfield_mark_small.png', align = "left")),
        #             "Stack Overflow Canadian Developer Talent Map"
        #             )
        #    ),
        tags$div(id="cite",
                 'Application developed by ',
                 tags$a(href="", "Asher Zafar"),
                 " for the ",
                 tags$a(href="http://brookfieldinstitute.ca/"," Brookfield Institute for Innovation + Entrepreneurship (BII+E)."),
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

  #Draw base map
   output$map <- renderLeaflet({
     leaflet() %>%
       fitBounds(lng1 = -124, 
                 lat1 = 42, 
                 lng2 = -63, 
                 lat2 = 54) %>%
       addProviderTiles(providers$Stamen.TonerLite) #CartoDB Positron looks good, too, but a little busier
     })
   
     #Add city markers
     observe({
       if (input$juris == "Cities") {
         labelmetric <- labelmetric()
         
       #Select and filter jurisdictional data for role and metric
       cities.c <- cities[cities$dev_role == input$role,]
       if (input$metric == "loc_quo") {cities.c <- cities.c[is.na(cities.c$loc_quo) == FALSE,]}
       citymetric <- cities.c[[input$metric]]
       cityrad <- (citymetric/mean(citymetric)*500)^.4
       
       
       #Create color palette based on metrics - put in its own observe function later
       metricpal.c <- colorBin(
         palette = c("#F48EBD","#79133E"),
         domain = c(min(citymetric), max(citymetric)),
         n=7, pretty=TRUE)
       
       #Draw city markers
       leafletProxy("map", data = cities.c) %>% clearShapes() %>% clearMarkers() %>%
         
         addCircleMarkers(
           weight = 1,
           radius = ~cityrad,
           color = "#E24585",
           fillColor = ~metricpal.c(citymetric),
           fillOpacity = .8,
           label = ~paste0(cities.c$cities," - ", labelmetric, ": ", citymetric),
           labelOptions = labelOptions(style = list(
             "font-family" = "rooneysansmed",
             "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
             "border-width" = "1px",
             "border-color" = "rgba(0,0,0,0.5)")))%>%
         #Add legend - put in own observe function later
         clearControls() %>%
         addLegend("bottomright", pal = metricpal.c, values = citymetric,
                   title = labelmetric)
       }
       })
     
     #Add province polygons
     observe({
       if (input$juris == "Provinces") {
         
         labelmetric <- labelmetric()
         
         #Select and filter jurisdictional data for role and metric
         provinces.p <- provinces[provinces$dev_role == input$role,]
         provmetric <- provinces.p[[input$metric]]
         
         
         #Create color palette based on metrics - put in its own observe function later
         metricpal.p <- colorBin(
           palette = c("#F48EBD","#79133E"),
           domain = c(min(provmetric), max(provmetric)),
           n=7, pretty=TRUE)
         
         #Add province polygons
         leafletProxy("map", data = provinces.p) %>% clearShapes() %>% clearMarkers() %>%
           addPolygons(color = ~metricpal.p(provmetric), weight = 1, smoothFactor = 0.5,
                         opacity = 1.0, fillOpacity = 0.7,
                         highlightOptions = highlightOptions(color = "white", weight = 1),
                         label = ~paste0(gn_name," - ", labelmetric, ": ", provmetric),
                         labelOptions = labelOptions(style = list(
                           "font-family" = "rooneysansmed",
                           "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                           "border-width" = "1px",
                           "border-color" = "rgba(0,0,0,0.5)"))) %>%
           
           #Add legend - put in own observe function later
           clearControls() %>%
           addLegend("bottomright", pal = metricpal.p, values = provmetric,
                     title = labelmetric)
       }
     })
   } #Server close

# Run the application
enableBookmarking()
shinyApp(ui = ui, server = server)

