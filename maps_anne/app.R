library(leaflet)
library(shiny)
library(dplyr)
library(rgdal)

dairypoints <- read.csv("Dairies September.csv")
rdmareas <- readOGR("The New Dairy Areas.shp")

ui <- fluidPage(
  titlePanel("Search Dairy Producers in the UK by Area"),
  selectInput("dropdown", "Select Area", choices = (dairypoints$area), multiple = FALSE),
  leafletOutput("dairypointsmap", height = 1000, width= 1000)
)

pal <- colorFactor(c("blue", "black"), domain = c("Dairy Farm", "RDM Farm"))

server <- function(input, output){
  filtered <- reactive({
    dairypoints %>% filter(dairypoints$area %in% input$dropdown)
  })
  output$dairypointsmap <- renderLeaflet({
    dairypointsmap <- leaflet(filtered()) %>%
      setView(lng = -2.916976, lat = 54.815864, zoom = 6) %>%
      addTiles() %>%
      addLegend("topright", pal = pal, values = ~Type) %>%
      addPolygons(data = rdmareas, 
                  stroke = TRUE, fillOpacity = 0, 
                  smoothFactor = 0.1, color = "black", opacity = 1, weight = 1) %>%
      addCircleMarkers(
        radius = 4,
        color = ~pal(Type),
        stroke = FALSE, fillOpacity = 1,
        popup = paste("Name", filtered()[,"Name"],"<br>",
                      "Producer ID", filtered()[,"ProducerID"], "<br>",
                      "Area Number", filtered()[,"area"], "<br>",
                      "Post Code", filtered()[,"PostCode"], "<br>",
                      "CPH", filtered()[,"CPH"], "<br>",
                      "Farm Type",filtered()[,"Type"]))
  })
  options(shiny.sanitize.errors = FALSE)
}

shinyApp(ui = ui, server = server)

