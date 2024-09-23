

library(shiny)
library(plotly)
library(ggplot2)
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(sf)
library(maps)
library(ggthemes)
library(tidyverse)
library(bit64)


## Read data into R, and do some pre processing and preliminary analysis

country_vaccinations_data <- read.csv("/Users/presidentoliver/Desktop/504_Final_Project/vaccinations.csv")
country_vaccinations_by_manufacturer_data <- read.csv("/Users/presidentoliver/Desktop/504_Final_Project/vaccinations-by-manufacturer.csv")
#View(country_vaccinations_data)
#View(country_vaccinations_by_manufacturer_data)

# Select all observations where the year column of the date is 2020
year_equal_2020 <- filter(country_vaccinations_data, year(date) == 2020)

# Drop those observations
country_vaccinations_data <- country_vaccinations_data %>% filter(year(date) != 2020)


#str(country_vaccinations_data)

# Preprocessing country vaccinations data
country_vaccinations_processed <- country_vaccinations_data %>%
  mutate(country_vaccinations_data, date = as.Date(date), total_vaccinations = as.integer64(total_vaccinations), 
         people_vaccinated = as.integer(people_vaccinated), daily_vaccinations_raw = as.integer(daily_vaccinations_raw),
         daily_vaccinations = as.integer(daily_vaccinations), daily_vaccinations_per_million = as.integer(daily_vaccinations_per_million))

#View(country_vaccinations_processed)

## obtain the vaccinations over time

immunizations_over_time <- country_vaccinations_processed %>%
  drop_na() %>%
  group_by(date, total_vaccinations) %>%
  arrange(date)

immunizations_over_time <- immunizations_over_time %>%
  group_by(date) %>%
  summarise(total_immunizations = sum(total_vaccinations, na.rm = TRUE), .groups = 'drop') %>%
  arrange(date)

immunizations_over_time <- immunizations_over_time %>%
  mutate(immunizations_over_time,  total_immunizations = as.integer64(total_immunizations))




immunizations_applied_country <- country_vaccinations_processed %>%
  group_by(location, iso_code) %>%
  summarise(total_immunizations = sum(total_vaccinations, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(total_immunizations))

people_immunized_per_hundred <- country_vaccinations_processed %>%
  group_by(location, iso_code) %>%
  summarise(people_fully_vaccinated_per_hundred = max(people_fully_vaccinated_per_hundred, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(people_fully_vaccinated_per_hundred))

people_immunized_per_hundred <- people_immunized_per_hundred[-c(1, 2),]

#change column name to region
colnames(people_immunized_per_hundred)[1] <- "region"



#world map data

world_map = map_data("world") %>% 
  filter(! long > 180)


unique_elements_world<- unique(world_map$region)

unique_elements_immunized_data<- unique(people_immunized_per_hundred$region)

elements_not_in_column2 <- unique_elements_immunized_data[!unique_elements_immunized_data %in% unique_elements_world]

elements_not_in_column1 <- unique_elements_world[!unique_elements_world %in% unique_elements_immunized_data]

#elements_not_in_column2
#elements_not_in_column1


people_immunized_per_hundred$region[people_immunized_per_hundred$region == 'United Kingdom'] <- 'UK'
people_immunized_per_hundred$region[people_immunized_per_hundred$region == 'United States'] <- 'USA'
people_immunized_per_hundred$region[people_immunized_per_hundred$region == 'Congo'] <- 'Republic of Congo'
people_immunized_per_hundred$region[people_immunized_per_hundred$region == "Cote d'Ivoire"] <- 'Ivory Coast'
people_immunized_per_hundred$region[people_immunized_per_hundred$region == "Cote d'Ivoire"] <- 'Ivory Coast'
people_immunized_per_hundred$region[people_immunized_per_hundred$region == 'Eswatini'] <- 'Swaziland'
people_immunized_per_hundred$region[people_immunized_per_hundred$region == 'Democratic Republic of Congo'] <- 'Democratic Republic of the Congo'
people_immunized_per_hundred$region[people_immunized_per_hundred$region == 'Czechia'] <- 'Czech Republic'
people_immunized_per_hundred$region[people_immunized_per_hundred$region == 'Trinidad and Tobago'] <- 'Trinidad'
people_immunized_per_hundred$region[people_immunized_per_hundred$region == 'Timor'] <- 'Timor-Leste'



final_merge <- left_join( world_map, people_immunized_per_hundred, by = "region") 

final_merge <- final_merge[!is.na(final_merge$people_fully_vaccinated_per_hundred), ] #remove the NA values


# Now let's group the countries according to regions so that we can a new tab for the world map

regions_data <- read_csv("/Users/presidentoliver/Desktop/504_Final_Project/world-regions-according-to-the-world-bank.csv")
#View(regions_data)

# Rename a Code column to iso_code to match what we have in the world map data
regions_data <- regions_data %>%
  rename(iso_code = Code) %>%
  rename(world_bank_assigns_regions = "World Region according to the World Bank")

# Select specific columns
filtered_regions_data <- regions_data %>%
  select(iso_code, world_bank_assigns_regions)

#Now let's join the filtered_regions_data with the final_merge data

new_final_merge <- left_join( final_merge, filtered_regions_data, by = "iso_code") 


# third visualization

# Preprocessing country vaccinations by manufacturer data
country_vaccinations_by_manufacturer_processed <- country_vaccinations_by_manufacturer_data %>%
  mutate(country_vaccinations_by_manufacturer_data, date = as.Date(date))

#obtain daily vaccine use by manufacturers

immnunizations_manufacturer <- country_vaccinations_by_manufacturer_processed %>%
  drop_na() %>%
  group_by(date,
           vaccine) %>%
  summarise(total_vaccinations = sum(total_vaccinations), .groups = 'drop')

# group by vaccine manufacturers and get the total vaccinations produced by them 

summarized_vaccine_manufacturers <- immnunizations_manufacturer %>%
  group_by(vaccine) %>%
  summarise(Total_vaccine_produced = sum(total_vaccinations), .groups = "drop") %>%
  arrange(desc(Total_vaccine_produced)) %>%
  head(5)

# select only those observations where manufacturers are amongst the top 5 above

immnunizations_manufacturer_filtered <- immnunizations_manufacturer %>%
  filter(vaccine %in% summarized_vaccine_manufacturers$vaccine)



# Shiny App

# Define UI for the combined app
ui <- fluidPage(
  titlePanel("COVID-19 Vaccination Dashboard"),
  tabsetPanel(
    tabPanel("Introduction",
             p("Vaccines are essential for reducing the spread of infectious diseases and protecting public health. Several vaccinations were produced during the COVID-19 pandemic to mitigate the disease's severity and spread. These vaccinations have helped save lives and restore normalcy. This Shiny app offers a complete analysis of vaccine data, highlighting patterns, crucial statistics, and the effectiveness of immunization efforts in preventing the pandemic. "),
             p("This app allows users to explore global trends in daily vaccinations, providing an in-depth view of the progress made in combating the COVID-19 pandemic. With the latest update, users can now select specific geographical locations to visualize vaccination trends, as well as view detailed data on vaccine manufacturers and the quantity of vaccines produced. This comprehensive tool offers valuable insights into the global vaccination effort, highlighting the contributions of various regions and manufacturers in mitigating the pandemic's effects. ")
    ),
    tabPanel("World Map",
             sidebarLayout(
               sidebarPanel(
                 selectInput("region", "Select region:", choices = unique(new_final_merge$world_bank_assigns_regions), multiple = TRUE),
                 sliderInput("vaccination_range", "Select Vaccination Range:",
                             min = 0, max = 100, value = c(0, 100))
               ),
               mainPanel(
                 plotlyOutput("world_map")
               )
             )
    ),
    tabPanel("Vaccine Manufacturers",
             sidebarLayout(
               sidebarPanel(
                 selectInput("vaccine_manufacturers", "Select Vaccine Manufacturers:",
                             choices = unique(immnunizations_manufacturer_filtered$vaccine), multiple = TRUE),
                 uiOutput("date_range_input")
               ),
               mainPanel(
                 plotlyOutput("vaccine_manufacturers_plot")
               )
             )
    ),
    tabPanel("Vaccination Trends",
             sidebarLayout(
               sidebarPanel(
                 selectInput("year", "Choose a year:",
                             choices = unique(format(immunizations_over_time$date, "%Y"))),
                 selectInput("interval", "Choose a time interval:",
                             choices = c("Daily", "Weekly", "Monthly"))
               ),
               mainPanel(
                 plotlyOutput("line_plot")
               )
             )
    )
  )
)

# Define server logic for the combined app
server <- function(input, output, session) {
  # Filter data based on selected vaccination range and region for World Map
  filtered_data <- reactive({
    filter(new_final_merge, 
           people_fully_vaccinated_per_hundred >= input$vaccination_range[1] &
             people_fully_vaccinated_per_hundred <= input$vaccination_range[2] & 
             world_bank_assigns_regions %in% input$region)
  })
  
  # Create world map plot
  output$world_map <- renderPlotly({
    my_plot <- ggplot(filtered_data(), aes(x = long, y = lat, group = group,
                                           fill = people_fully_vaccinated_per_hundred,
                                           text = paste("Country: ", region,
                                                        "\nPeople vaccinated per hundred: ",
                                                        people_fully_vaccinated_per_hundred))) +
      geom_polygon() +
      scale_fill_viridis_c(option = "mako", direction = -1) +
      labs(fill = "", title = "Number of people fully vaccinated per hundred across countries") +
      theme_map() +
      theme(legend.position = "right",
            title = element_text(hjust = 0.5, face = "bold", vjust = 0.5))
    
    ggplotly(my_plot, tooltip = c("text"))
  })
  
  # Filter data based on selected vaccine manufacturers and date range for Vaccine Manufacturers
  filtered_manufacturer_data <- reactive({
    filter(immnunizations_manufacturer_filtered, 
           vaccine %in% input$vaccine_manufacturers &
             date >= input$date_range[1] & date <= input$date_range[2])
  })
  
  # Render date range input for Vaccine Manufacturers
  output$date_range_input <- renderUI({
    dateRangeInput("date_range", "Select Date Range:",
                   start = min(immnunizations_manufacturer_filtered$date),
                   min = min(immnunizations_manufacturer_filtered$date), 
                   max = max(immnunizations_manufacturer_filtered$date),
                   end = max(immnunizations_manufacturer_filtered$date))
  })
  
  # Create vaccine manufacturers plot
  output$vaccine_manufacturers_plot <- renderPlotly({
    pp <- ggplot(filtered_manufacturer_data(), aes(x = date, y = total_vaccinations, color = vaccine)) +
      geom_line() +
      labs(x = "Date", y = "Total Vaccinations", title = "Total Vaccinations by Vaccine Manufacturer", color = "Vaccine Manufacturer") +
      scale_color_brewer(palette = "Dark2") +
      theme_classic() +
      theme(legend.position = "bottom")  # Adjust legend position
    
    ggplotly(pp, tooltip = c("x", "y", "color"))  # Ensure tooltip displays relevant information
  })
  
  # Filter data based on selected year and time interval for Vaccination Trends
  filtered_data_trends <- reactive({
    year_filter <- format(immunizations_over_time$date, "%Y") == input$year
    df_filtered <- immunizations_over_time[year_filter, ]
    if (input$interval == "Weekly") {
      df_filtered <- df_filtered %>%
        mutate(interval_date = lubridate::floor_date(date, "week")) %>%
        group_by(interval_date) %>%
        summarise(total_immunizations = sum(total_immunizations))
    } else if (input$interval == "Monthly") {
      df_filtered <- df_filtered %>%
        mutate(interval_date = lubridate::floor_date(date, "month")) %>%
        group_by(interval_date) %>%
        summarise(total_immunizations = sum(total_immunizations))
    } else {
      df_filtered <- df_filtered %>%
        mutate(interval_date = date)
    }
    return(df_filtered)
  })  
  
  # Create vaccination trends plot
  output$line_plot <- renderPlotly({
    # Plot data using ggplot and convert to Plotly object
    p <- ggplot(filtered_data_trends(), aes(x = interval_date, y = total_immunizations)) +
      geom_line(color = "#4e9b84") +
      labs(x = ifelse(input$interval == "Monthly", "Month", ifelse(input$interval == "Weekly", "Week", "Date")),
           y = "Total Immunizations",
           title = paste(input$interval, "Vaccinations Over Time for", input$year)) +
      theme_classic()
    
    ggplotly(p)
  })
}

# Run the combined application
shinyApp(ui = ui, server = server)
