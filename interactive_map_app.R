#This is a Shiny web application for interactive map.

library(shiny)
library(leaflet)
library(tidyverse)

#Amebiasis data
urlfile <- "https://raw.githubusercontent.com/mosuzuki/IDSC_IDEpi/master/amebiasis.csv"

dat <- read_csv(url(urlfile))

dat <- gather(dat, key="year", value="case",
              X2014, X2015, X2016, X2017, X2018)

dat$y <- 0 
dat$y[dat$year=="X2014"]<-2014
dat$y[dat$year=="X2015"]<-2015
dat$y[dat$year=="X2016"]<-2016
dat$y[dat$year=="X2017"]<-2017
dat$y[dat$year=="X2018"]<-2018

#Define UI for application that filters map points based on year
ui <- fluidPage(
  
  #Application title
  titlePanel("Imported amebiasis cases by year and country of origin"),
  
  #Sidebar with a slider input for year
  sidebarLayout(
    sidebarPanel(
      sliderInput("year",
                  "Year",
                  min = 2014,
                  max = 2018,
                  step = 1,
                  sep = "",
                  value = 2014)
    ),
    #Show the map
    mainPanel(
      leafletOutput("map")
    )
  )
)

#Define server logic required to draw a map and table
server <- function(input, output) {
  output$map <- renderLeaflet({
    case_by_y <- filter(dat, y == input$year, case>0)
    leaflet(data = case_by_y) %>%
      addTiles() %>%
      setView(50, 24, zoom=1.5) %>%
      addCircleMarkers(lat = ~lat, lng = ~lon, radius=case_by_y$case*0.5, weight=5, col="red",
                       label = case_by_y$case, labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE))
  })
}

#Run the application 
shinyApp(ui = ui, server = server)

#####