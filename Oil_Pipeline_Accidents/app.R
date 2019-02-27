#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(sf)
library(shinythemes)
library(tmap)
library(leaflet)

oil_accidents_US <- read_csv("cleaned_oil_data.csv")
oil_geom1 <- st_as_sf(oil_accidents_US, coords = c("accident_longitude", "accident_latitude"), 
                      crs = 4326, agr = "constant") %>%
  select(accident_city, everything()) 

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("superhero"),
  # Application title
  titlePanel("United States Oil Accidents (2010-2016)"),
  
  navbarPage("",
             
             tabPanel("Summary",
                      h1("A header!"),
                      h2("A secondary header..."),
                      p("Then some paragraph text. Old Faithful Geyser Data Description: Waiting time between eruptions and the duration of the eruption for the Old Faithful geyser in Yellowstone National Park, Wyoming, USA."),
                      p("Followed by another paragraph of text..."),
                      h1("Then another header"),
                      p("You get the idea...)")
                      
             ),
  
  # Sidebar with a slider input for number of bins 
  tabPanel("Map",
           sidebarLayout(
    sidebarPanel(
      sliderInput("all_costs",
                  "Cost of Spill (USD):",
                  min = 0,
                  max = 840526118,
                  value = 0),
      selectInput("pipelinet",
                  "Select Pipeline Type", 
                  choices = c("Aboveground" ="ABOVEGROUND", "Underground"= "UNDERGROUND", "Tank"="TANK", "Transition Area"="TRANSITION AREA", "Not Specified in Data" = "Not Specified")
        ##filter out NA pipeline types? 
      )
      
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("map")
    )
  )
)
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Creating the reactive output ('map')
  output$map <- renderLeaflet({
    
    costs_inc <- oil_geom1 %>% 
      filter(all_costs >= input$all_costs, 
             pipeline_type == input$pipelinet) # Filter based on input selection from height widget
    
    # Creating map
    cost_map <- 
      tm_shape(costs_inc) +
      tm_dots(size = "all_costs", alpha = 0.5,
              col = "all_costs") 
    
    
    # Leaflet 
    tmap_leaflet(cost_map)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
