library(shiny)
library(leaflet)
library(rmarkdown)
library(RColorBrewer)

df1<-read.csv("./data/df1.csv", header=T)

# Define UI for app that draws a histogram ----
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                h1(id="big-heading", "Rental Market Data"),
                tags$style(HTML("#big-heading{color: red;}")),
                sliderInput("range", "Apartment Size", min(df1$size1), max(df1$size1),
                            value = range(df1$size1), step = 5
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

# Define server logic required to draw a histogram ----

server <- shinyServer(function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    df1[df1$size1 >= input$range[1] & df1$size1 <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, df1$size1)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(df1) %>% addTiles() %>%
      fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearMarkers() %>%
      clearShapes() %>%
      addCircleMarkers(color = "#777777", weight = 1,
                 fillColor = ~pal(size1), fillOpacity = 0.7, 
                 label = paste("Rent per Month:", df1$rent),
                 labelOptions = labelOptions(noHide = F, direction = 'top',style = list(
                   "color" = "red",
                   "font-family" = "serif",
                   "font-size" = "15px",
                   "border-color" = "rgba(0,0,0,0.5)"))
      )%>%
      fitBounds(.,min(filteredData()$lon),min(filteredData()$lat),
                max(filteredData()$lon),max(filteredData()$lat))
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = df1)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~size1,title = "Apartment Size"
      )
    }
  })
})

shinyApp(ui, server)