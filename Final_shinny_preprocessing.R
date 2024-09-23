library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(sf)
library(maps)
library(ggthemes)
library(tidyverse)
library(bit64)

## read data into r, and do some preliminary analysis

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







save(immunizations_over_time, people_immunized_per_hundred, new_final_merge, immnunizations_manufacturer_filtered, file = "final_shinny.RData" )
load("final_shinny.RData")

