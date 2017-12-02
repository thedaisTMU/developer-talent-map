#Stack Overflow Developer Talent Map for Canadian Cities and Provinces
##R Shiny Map Widget
#setwd("~/GitHub/developer-talent-map")

##To do
library(leaflet)
library(formattable)
library(tidyverse)
library(sf)
library(shiny)
library(bsplus)

#Preprocessing - move some to load script ===================
#Import data from load scripts
provinces <- read_sf("provinces.shp")
cities <- read_sf("cities.shp")

#Change names on fields and preserve SF object type
names(provinces)[names(provinces)=="dev_rol"] <- "dev_role"
names(provinces)[names(provinces)=="visitrs"] <- "visitors"
names(provinces)[names(provinces)=="prvnc__"] <- "share"
names(provinces)[names(provinces)=="lctn_qt"] <- "loc_quo"

names(cities)[names(cities)=="dev_rol"] <- "dev_role"
names(cities)[names(cities)=="visitrs"] <- "visitors"

cities$visitors <- comma(cities$visitors, 0)
cities$share <- percent(cities$share, 1)
cities$loc_quo <- comma(cities$loc_quo ,2)

provinces$visitors <- comma(provinces$visitors, 0)
provinces$share <- percent(provinces$share)
provinces$loc_quo <- comma(provinces$loc_quo, 2)

#Dropdown choices
rolegroups <- c("All Developers", "Mobile Developers", "Web Developers", "Other Kinds of Developers")
role <- unique(cities$dev_role)
role <- role[(!role %in% rolegroups)]

metric <- c(
  "Stack Overflow visitors" = "visitors",
  "Share of local Stack Overflow visitors in role" = "share",
  "Location quotient" = "loc_quo"
)

#DefineUI ===================
ui <- function(request) {
  fillPage(title= "Developer Talent Map - StackOverflow + BII+E", theme = "styles.css",
               #title = "Stack Overflow Canadian Developer Talent Map",
    div(style = "width: 100%; height: 100%;",
        leafletOutput("map", width = "100%", height = "100%"),
        absolutePanel(id = "controls", class = "panel panel-default", draggable = TRUE, fixed = TRUE,
                      top = 85, left = 10, right = "auto", bottom = "auto", 
                      width = "200px", height = "auto",
                      h1("Canadian Developer Talent Map"),
                      selectInput("metric", "Metric", metric, selectize = FALSE) %>% #Draggable and selectize seem incompatible for scrolling
                        shinyInput_label_embed( #Move all help text into one modal with markdown with title https://ijlyttle.shinyapps.io/tooltip_popover_modal/#
                          shiny_iconlink(name = "question-circle", class = "fa-2x") %>%
                            bs_embed_popover(
                              title = "'Visitors' are visitors to Stack Overflow. 'Share of local...' is the number of visitors from a selected role divided by total traffic from the city/province. Location quotient is the share of a city or province's Stack Overflow visitors in a particular role divided by the share of that same role nationally.",
                              placement = "right")),
                      selectInput("role", "Developer Role", list("Role Groups" = rolegroups, "Roles" = role), selectize=FALSE) %>%
                        shinyInput_label_embed(
                          shiny_iconlink() %>%
                            bs_embed_popover(
                              title = "Developer Role Help",
                              content = "Select the developer role you'd like to view the metrics for. Groups are aggreated from other roles.",
                              placement = "right")),
                      radioButtons("juris", "Jurisdiction", choices = c("Cities", "Provinces"), selected = "Cities", inline = TRUE),
                      bookmarkButton(label = "Share your selections", title = "Save your selections to a URL you can share")
                      ),
        tags$div(id="icons",
                 tags$a(href="http://brookfieldinstitute.ca/", img(src='brookfield_institute_esig_small.png', hspace = "5px", align = "left")),
                 #tags$br(),
                 tags$a(href="https://stackoverflow.com/", img(src='so-logo-small.png', hspace = "5px", align = "left")),
                 #tags$br(),
                 tags$a(href="https://github.com/BrookfieldIIE/", icon("github-square", "fa-2x")),
                 tags$a(href="https://twitter.com/BrookfieldIIE", icon("twitter-square", "fa-2x")),
                 tags$a(href="https://www.facebook.com/BrookfieldIIE/", icon("facebook-square", "fa-2x")),
                 tags$a(href="https://www.youtube.com/channel/UC7yVYTU2QPmY8OYh85ym-2w", icon("youtube-square", "fa-2x")),
                 tags$a(href="https://www.linkedin.com/company/the-brookfield-institute-for-innovation-entrepreneurship", icon("linkedin-square", "fa-2x"))
                 ),
        tags$div(id="cite", #Set links
                 'Wondering what this map is for? Read the ',
                 tags$a(href="", "full report"),
                 "by David Rubinger and Creig Lamb. ",
                 "This map is open source - ",
                 tags$a(href="https://github.com/BrookfieldIIE/developer-talent-map","contribute on GitHub. "),
                 'App developed by Asher Zafar',
                 " for the ",
                 tags$a(href="http://brookfieldinstitute.ca/"," Brookfield Institute for Innovation + Entrepreneurship (BII+E).")
        )
    ),
    use_bs_popover(),
    use_bs_tooltip()
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
   
   #Select and filter city data for role and metric
   cities.r <- reactive({
     cities.r <- cities[cities$dev_role == input$role,]
     if (input$metric == "loc_quo") {cities.r <- cities.r[is.na(cities.r$loc_quo) == FALSE,]}
     if (input$metric == "share") {cities.r <- cities.r[is.na(cities.r$share) == FALSE,]}
     if (input$metric == "visitors") {cities.r <- cities.r[is.na(cities.r$visitors) == FALSE,]}
     return(cities.r)
   })
   
     #Add city markers
     observe({
       if (input$juris == "Cities") {
         labelmetric <- labelmetric()
         
       #Create metrics for shading/sizing markers
       cities.c <- cities.r()
       citymetric <- cities.c[[input$metric]]
       cityrad <- (citymetric/mean(citymetric)*100)^.55 #300^.5
       
       #Create color palette based on metrics - put in its own observe function later
       metricpal.c <- colorBin(
         palette = c("#FFD5F0","#79133E"),
         domain = c(min(citymetric), max(citymetric)),
         pretty=TRUE)
       
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

