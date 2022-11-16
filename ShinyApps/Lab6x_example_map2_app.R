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
library(lubridate)
library(maps)
library(mapdata)
library(wesanderson)

### Preparing the times series data

time_series_confirmed_long <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")) %>%
    rename(Province_State = "Province/State", Country_Region = "Country/Region")  %>% 
    pivot_longer(-c(Province_State, Country_Region, Lat, Long),
                 names_to = "Date", values_to = "Confirmed") 
# Let's get the times series data for deaths
time_series_deaths_long <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")) %>%
    rename(Province_State = "Province/State", Country_Region = "Country/Region")  %>% 
    pivot_longer(-c(Province_State, Country_Region, Lat, Long),
                 names_to = "Date", values_to = "Deaths")

# Create Keys 
time_series_confirmed_long <- time_series_confirmed_long %>% 
    unite(Key, Province_State, Country_Region, Date, sep = ".", remove = FALSE)
time_series_deaths_long <- time_series_deaths_long %>% 
    unite(Key, Province_State, Country_Region, Date, sep = ".") %>% 
    select(Key, Deaths)

# Join tables
time_series_long_joined <- full_join(time_series_confirmed_long,
                                     time_series_deaths_long, by = c("Key"))  %>% 
    select(-Key)
# Reformat the data 
time_series_long_joined$Date <- mdy(time_series_long_joined$Date)

# rename the data
global_time_series <- time_series_long_joined

# Get first and last date for graph ***There are NA in the date field to consider
first_date = min(global_time_series$Date, na.rm = TRUE)
last_date = max(global_time_series$Date, na.rm = TRUE)

# Defining reporting types
Report_Type = c("Confirmed", "Deaths")
    
# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Example Graphs from COVID-19 Reporting data"),
    p("Data for this application are from the Johns Hopkins Center for Systems Science and Engineering",
      tags$a("GitHub Repository", href="https://github.com/CSSEGISandData")
    ),
    tags$br(),
    tags$hr(),  # Adds line to page

    sidebarLayout(
        sidebarPanel(
             # Select Reporting type
            selectInput("select_type", 
                        label = "Report Type", 
                        choices = Report_Type, selected = "Confirmed"),
            # Select Date 
            sliderInput("slider_date", label = "Report Date", min = first_date, 
                          max = last_date, value = first_date, step = 7)
        ),

        # Show a plots
        mainPanel(
           plotOutput("Plot1")
        )
    )
)

# Define server logic required to make the plot
server <- function(input, output) {

    output$Plot1 <- renderPlot({
        # develop data set to graph
        pick_date <- global_time_series %>% 
            # Fix mapping to map_data of US != USA  
            # mutate(Country_Region = recode(Country_Region, US = "USA")) %>% 
# *** This is where the slider input with the date goes
            filter(Date == input$slider_date) %>% 
            group_by(Country_Region) %>% 
            summarise_at(c("Confirmed", "Deaths"), sum)

        # load the world map data
        world <- as_tibble(map_data("world")) %>% 
        mutate(region = str_replace_all(region, c("USA" = "US", "Czech Republic" = "Czechia",  
                  "Ivory Coast" = "Cote d'Ivoire", "Democratic Republic of the Congo" = "Congo (Kinshasa)", 
                  "Republic of Congo" = "Congo (Brazzaville)")))
        
        # We need to join the us map data with our daily report to make one data frame/tibble
        country_join <- left_join(world, pick_date, by = c("region" = "Country_Region"))
        
        # plot world map
        ggplot(data = world, mapping = aes(x = long, y = lat, group = group)) + 
            coord_fixed(1.5) + 
            # Add data layer
            geom_polygon(data = country_join, aes_string(fill = input$select_type), color = "black") +
            scale_fill_gradientn(colours = 
                                     wes_palette("Zissou1", 100, type = "continuous"),
                                 trans = "log10") +
            ggtitle("JHU COVID-19 data for reporting type:", input$select_type)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
