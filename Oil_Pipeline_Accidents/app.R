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
library(plotly)
library(tidyr)
library(RColorBrewer)
library(janitor)
library(chron)
library(ggplot2)

#Load the data
oil_accidents_raw <- read_csv("raw_database.csv")

# Initial data cleanup and wrangling

oil_accidents <- oil_accidents_raw %>%
  clean_names() %>% #clean the names snake case
  
  select(report_number, accident_year:liquid_type, accident_city:cause_category, net_loss_barrels:restart_date_time, property_damage_costs:all_costs) %>% #select relevant columns
  separate(accident_date_time, c("month", "day", "year", "hour", "minute" ,"am_or_pm"), by = c("/", " ")) %>%  #separate the date time column
  
  unite("time", hour, minute, sep = ":") %>% #bring time back together
  
  unite("time", time, am_or_pm, sep = " ") %>% #bring time back together
  
  filter(accident_year != 2017) %>% #filter out the 2 2017 observations
  
  unite("date", year,month,day, sep = "-") %>% #make the date column again
  
  mutate( 
         pipeline_type = case_when(
           is.na(pipeline_type) ~ "Not Specified",
           pipeline_type == pipeline_type ~ pipeline_type), 
         accident_city = case_when(
           pipeline_type == "Not Specified" & is.na(accident_city) ~ "Gulf of Mexico", 
           pipeline_type == "Not Specified" & !is.na(accident_city) ~ accident_city, 
           is.na(accident_city) & !is.na(accident_county) ~ accident_county,
           TRUE ~ accident_city))
#change date to date standard international date format. Chagne NA pipeline types to "Not Specified". Change those "Not Specified" without city names to "Gulf of Mexico" for city name. If there is a county name but not a city name, assign the city name as county name. 

oil_accidents_US <- oil_accidents %>%
  filter(accident_longitude < -60) %>%
  mutate(liquid_type = case_when(
    liquid_type == "HVL OR OTHER FLAMMABLE OR TOXIC FLUID, GAS" ~ "Flammable or toxic fluid/gas", 
    liquid_type == "REFINED AND/OR PETROLEUM PRODUCT (NON-HVL), LIQUID" ~ "Refined Liquid Petroleum Product",
    liquid_type == "CRUDE OIL" ~ "Crude Oil", 
    liquid_type == "CO2 (CARBON DIOXIDE)" ~ "CO2 (Carbon Dioxide)",
    liquid_type == "BIOFUEL / ALTERNATIVE FUEL(INCLUDING ETHANOL BLENDS)" ~ "Biofuel",
    liquid_type == liquid_type ~ liquid_type
  ))

write.csv(oil_accidents_US, "cleaned_oil_data.csv")

oil_geom1 <- st_as_sf(oil_accidents_US, coords = c("accident_longitude", "accident_latitude"), 
                      crs = 4326, agr = "constant") %>%
  select(accident_city, everything()) 

graph1_oil_accidents_US <- oil_accidents_US %>% 
  rename(County = accident_county) %>% 
  rename(Operator = operator_name)

county_or_company <- graph1_oil_accidents_US %>% 
  select(County, Operator) %>% 
  names()


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("spacelab"),
  # Application title
  titlePanel(title = img(src='brenlogo.png', height = 100)),
  
  navbarPage("messy spills",
             
             tabPanel("Summary",
                      p(strong("Gage Clawson, Nelson Gould, Max Russer")),
                  
                      p(div(img(src='Plainspipelinespill.jpg'), a(br(em("Source: NPR")), href = "https://www.npr.org/sections/thetwo-way/2016/05/17/478388898/pipeline-company-indicted-over-2015-california-oil-spill")), h1("Background"), br("Despite advances in renewable energy, the United States transportation sector is predominantly run on crude oil and the petroleum products the oil is used to make (i.e gasoline). Because oil is not refined and consumed in the same location that it is extracted, transportation of vast amounts of oil around the country is necessary. In the U.S, the primary mode of oil transportation is via pipelines (Forbes). Oil pipeline spills are disturbingly common in the U. S. The extent of their impact can be difficult to fully fathom. This app is designed to help users explore the geographic spread, cost, and liquid types involved in U.S oil pipeline accidents. 

                                                                                 ") ), 
                      p("A dataset created by the U.S Department of Transportation consisting of oil pipeline accidents in the U.S from January 2010 to January 2017 was used.
Data Source: https://www.kaggle.com/usdot"),
                      h3("Map of Accidents and Costs"),
                      p("The second tab of our app, 'Map of Accidents and Costs', shows the location of different oil accidents across the United States. It also shows the cost of each accident, represented by the size of the dot. The drop down menu allows the user to select the type of pipeline the accident occurred on. Upon clicking on an individual dot, information on the City, Total Cost, Location (Onshore or Offshore), and Date of accident appears. Within the map, the user has the choice of selecting the basemap to view, one with a light theme, and one with a dark theme."), 
                      h3("Top Spills by State"),
                      p("asdfasdfas"),
                      h3("Liquid Type by State"),
                      p("The third tab, 'Liquid Type by State', indicates the percent amount of a liquid type spilled by state. Users can choose a state from the drop down menu. Once a state is chosen, an interactive donut graph is generated indicating the percent amount of liquid types for all oil pipeline accidents in that particular state. Liquid types include crude oil, flammable or toxic fluid/gas, refined liquid petrolum product, carbon dioxide, and biofuel.")
                      
             ),
             
             tabPanel("Map of Accidents and Costs",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("pipelinet",
                                      "Select Pipeline Type", 
                                      choices = c("Underground"= "UNDERGROUND", "Aboveground" ="ABOVEGROUND",  "Tank"="TANK", "Transition Area"="TRANSITION AREA", "Not Specified in Data" = "Not Specified") 
                          ), width =3
                        ),
                        
                        mainPanel(
                          leafletOutput("map")
                        )
                      )
             ), 
             tabPanel("Top Spills by State",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("acc_state_graph1",
                                      "Select State", 
                                      choices = c(sort(oil_accidents_US$accident_state)),
                                      selected = 1
                          ), width = 3,
          
                        radioButtons("graph1_filltype", label = "Divide by County or Operator Responsible",
                                     choices = county_or_company
                        )
                        ),
                      
                      # Show graph of top cost spills by 
                      mainPanel(
                        plotOutput("graph1")
                      )
                      
             )
             ),
             # Graph 2 tab of liquid type by state
             tabPanel("Liquid Type By State",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("acc_state_graph2",
                                      "Select State",
                                      choices = c(sort(oil_accidents_US$accident_state)),
                                      selected = 4
                          ), width = 2
                        ),
                        mainPanel(
                          plotlyOutput("graph2")
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
      filter(
             pipeline_type == input$pipelinet)
            
    
    # Creating map
    cost_map <- 
      tm_shape(costs_inc) +
      tm_bubbles(size = "all_costs", alpha = 0.5,  col = "all_costs",  popup.vars = c("City: " = "accident_city", "Total Cost (USD): " = "all_costs", "Location: " = "pipeline_location", "Date:" = "date"), title.col = "Total Cost of Accident (USD)") +
      #tm_view(view.legend.position = c("left", "center")) +
      tm_basemap(c("Esri.OceanBasemap", "CartoDB.DarkMatter"))
    
    #all basemaps:
    #http://leaflet-extras.github.io/leaflet-providers/preview/index.html
    
    # Leaflet 
    tmap_leaflet(cost_map)
})
  
  #Creating Graph 1: Top Costliest Spills by State and County
    
  
  
  
  output$graph1 <- renderPlot({

    
    top_cost_bystate <- graph1_oil_accidents_US %>% 
      filter(accident_state == input$acc_state_graph1) %>% 
      filter(County != "NA") %>% 
      filter(County != "N/A") %>% 
      filter(Operator != "NA") %>% 
      arrange(desc(all_costs)) %>% 
      head(10) %>% 
      arrange(all_costs) %>% 
      mutate(report_number = factor(report_number, levels = report_number))
    
    state_length <- length(top_cost_bystate$report_number)
    
    top_cost_bystate <- top_cost_bystate %>% 
      mutate(rank = seq(from = state_length, to = 1, by = -1)) %>% 
      mutate(rank = factor(rank, levels = rank))
    
    
    ggplot(top_cost_bystate, aes(x = rank, y = all_costs/100000)) +
      geom_col(aes_string(fill = input$graph1_filltype)) +
      scale_fill_brewer(palette = "Set3") +
      theme_classic() +
      labs(x = "", y = "Total Cost of Accident ($100,000)")+
      coord_flip() +
      theme(legend.position = "right") +
      scale_x_discrete(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      guides(fill=guide_legend(title= input$graph1_filltype))
    
    
  })
  
  #Graph 2: Liquid type by State
  
  output$graph2 <- renderPlotly({
    
    liquid_types <- oil_accidents_US %>%
      filter(accident_state == input$acc_state_graph2) %>%
      group_by(liquid_type, accident_state) %>%
      summarize(liquid_count = n())
    
    liquid_types$fraction = liquid_types$liquid_count / sum(liquid_types$liquid_count)
    liquid_types = liquid_types[order(liquid_types$fraction), ]
    liquid_types$ymax = cumsum(liquid_types$fraction)
    liquid_types$ymin = c(0, head(liquid_types$ymax, n=-1))
    
    
    ##non plotly version##
   # ggplot(liquid_types, aes(fill=liquid_type, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  #    geom_rect(colour = "grey30") +
   #   coord_polar(theta="y") +
    #  xlim(c(0, 4)) +
     # theme(panel.grid=element_blank()) +
      #theme(axis.text=element_blank()) +
      #theme(axis.ticks=element_blank()) +
      #annotate("text", x = 0, y = 0, label = NA) +
      #labs(title="") +
      #guides(fill=guide_legend(title="Liquid Type")) +
      #theme_void()+ 
    #theme(legend.position = "bottom", legend.direction = "vertical")
    
    
##Plotly version##
    
    plot_ly(liquid_types, labels = ~liquid_type, values = ~fraction, fill = ~liquid_type) %>%
      add_pie(hole = 0.75) %>% 
      layout(showlegend = T,
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            legend = list(orientation = "h", x = 0.4, y = -0.2))
    
    
  })

}


# Run the application 
shinyApp(ui = ui, server = server)
