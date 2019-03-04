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
  separate(accident_date_time, c("day", "month", "year", "hour", "minute" ,"am_or_pm"), by = c("/", " ")) %>%  #separate the date time column
  
  unite("time", hour, minute, sep = ":") %>% #bring time back together
  
  unite("time", time, am_or_pm, sep = " ") %>% #bring time back together
  
  filter(accident_year != 2017) %>% #filter out the 2 2017 observations
  
  unite("date", year,month,day, sep = "-") %>% #make the date column again
  
  mutate(date = as.Date(date), 
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
  filter(accident_longitude < -60)

write.csv(oil_accidents_US, "cleaned_oil_data.csv")

oil_geom1 <- st_as_sf(oil_accidents_US, coords = c("accident_longitude", "accident_latitude"), 
                      crs = 4326, agr = "constant") %>%
  select(accident_city, everything()) 

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("superhero"),
  # Application title
  titlePanel("United States Oil Pipeline Accidents (2010-2016)"),
  
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
      #sliderInput("all_costs",
       #           "Cost of Spill (USD):",
        #           min = 0,
         #         max = 840526118,
          #        value = 0),
      selectInput("pipelinet",
                  "Select Pipeline Type", 
                  choices = c("Aboveground" ="ABOVEGROUND", "Underground"= "UNDERGROUND", "Tank"="TANK", "Transition Area"="TRANSITION AREA", "Not Specified in Data" = "Not Specified") ##filter out NA pipeline types? 
      )
       #sliderInput("net_loss_barrels", 
        #            "Number of Barrels Lost",
         #           min = 0,
          #         max = 31000,
           #         value = 0)   
      
    
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("map")
    )
  )
  ), 
  tabPanel("Graph1",
           sidebarLayout(
    sidebarPanel(
      selectInput("acc_state_graph1",
                  "Select State", 
                  choices = c(sort(oil_accidents_US$accident_state)),
                  selected = 4
            ),
      sliderInput("head_graph1",
                  "Number of Spills Shown",
                  min = 0,
                  max = 20,
                  value = 10)
            ),
      radioButtons("graph1_filltype", label = "Select Fill Variable",
                 choices = list("County of Spill", "Company Responsible")
            )),
           
# Show graph of top cost spills by 
    mainPanel(
      plotOutput("graph1")
    )
  
),
# Graph 2 tab of liquid type by state
tabPanel("Graph2",
         sidebarLayout(
           sidebarPanel(
             selectInput("acc_state_graph2",
                         "Select State",
                         choices = c(sort(oil_accidents_US$accident_state)),
                         selected = 4
                         )
           ),
             mainPanel(
               plotOutput("graph2")
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
      filter(#all_costs >= input$all_costs, 
             pipeline_type == input$pipelinet)
             #net_loss_barrels >= input$net_loss_barrels) 
    
    # Creating map
    cost_map <- 
      tm_shape(costs_inc) +
      tm_bubbles(size = "all_costs", alpha = 0.5,  col = "all_costs",  popup.vars = c("City: " = "accident_city", "Total Cost (USD): " = "all_costs", "Location: " = "pipeline_location"), title.col = "Total Cost of Accident (USD)") +
      tm_view(view.legend.position = c("left", "bottom")) +
      tm_basemap(c("Esri.OceanBasemap", "CartoDB.DarkMatter"))
    
    #all basemaps:
    #http://leaflet-extras.github.io/leaflet-providers/preview/index.html
   #Esri.OceanBasemap
    #CartoDB.DarkMatter
  #Esri.NatGeoWorldMap
    
    # Leaflet 
    tmap_leaflet(cost_map)
})
  
  #Creating Graph 1: Top Costliest Spills by State and County
    
  output$graph1 <- renderPlot({
    
    top_cost_bystate <- oil_accidents_US %>% 
      filter(accident_state == input$acc_state_graph1) %>% 
      filter(accident_county != "NA") %>% 
      arrange(desc(all_costs)) %>% 
      head(10) %>% 
      arrange(all_costs) %>% 
      mutate(report_number = factor(report_number, levels = report_number)) 
    
    graph1_fill <- ifelse(input$graph1_filltype == "County of Spill", oil_accidents_US$accident_county, oil_accidents_US$operator_name)
    
    ifelse(input$graph1_filltype == "County of Spill",
                  (ggplot(top_cost_bystate, aes(x = report_number, y = all_costs/100000)) +
                    geom_col(aes(fill = accident_county)) +
                    theme_bw() +
                    labs(x = "", y = "Total Cost of Accident ($100,000)")+
                    coord_flip() +
                    theme(legend.position = "right") +
                    scale_x_discrete(expand = c(0,0), labels = rev(seq(1:10))) +
                    scale_y_continuous(expand = c(0,0)) +
                    guides(fill=guide_legend(title= "County of Spill"))),
                  
                  (ggplot(top_cost_bystate, aes(x = report_number, y = all_costs/100000)) +
                     geom_col(aes(fill = operator_name)) +
                     theme_bw() +
                     labs(x = "", y = "Total Cost of Accident ($100,000)")+
                     coord_flip() +
                     theme(legend.position = "right") +
                     scale_x_discrete(expand = c(0,0), labels = rev(seq(1:10))) +
                     scale_y_continuous(expand = c(0,0)) +
                     guides(fill=guide_legend(title= "Company Responsible")))
    )

    
  })
  
  #Graph 2: Liquid type by State
  
  output$graph2 <- renderPlot({
    
    liquid_types <- oil_accidents_US %>%
      filter(accident_state == input$acc_state_graph2) %>%
      group_by(liquid_type, accident_state) %>%
      summarize(liquid_count = n())
    
    liquid_types$fraction = liquid_types$liquid_count / sum(liquid_types$liquid_count)
    liquid_types = liquid_types[order(liquid_types$fraction), ]
    liquid_types$ymax = cumsum(liquid_types$fraction)
    liquid_types$ymin = c(0, head(liquid_types$ymax, n=-1))
    
    ggplot(liquid_types, aes(fill=liquid_type, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
      geom_rect() +
      coord_polar(theta="y") +
      xlim(c(0, 4)) +
      theme(panel.grid=element_blank()) +
      theme(axis.text=element_blank()) +
      theme(axis.ticks=element_blank()) +
      annotate("text", x = 0, y = 0, label = NA) +
      labs(title="") +
      guides(fill=guide_legend(title="Liquid Type"))
    
    
  })
      

}

# Run the application 
shinyApp(ui = ui, server = server)
