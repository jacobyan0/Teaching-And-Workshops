#### Make sure that you have installed the packages needed here
list.of.packages <- c("tidyverse", "sf","tigris","tidycensus","mapview","viridis","viridisLite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

### Clear workspace
rm(list = ls())

library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
library(mapview)
library(viridis)
library(viridisLite)

options(tigris_use_cache = TRUE)
tigris_cache_dir("C:\\Users\\jacob\\Dropbox (UFL)\\Jacob Box\\Data\\Census Data") 

### We need an API key to access the Census Data from census.gov 
#To obtain your Census API key: http://api.census.gov/data/key_signup.html 
#Insert your key to the current environment

census_api_key<-"94a6c385b3259b65826e0213b375e8d779332b52"

### Two main functions:
# get_decennial(): access to 1990, 2000, and 2010 decennial US Census
# get_acs(): Amercian Communicty Survey

#######################################################################################
#######    The load_variable function can help you search the data code    ############
#######################################################################################
ACSvariablelist <- load_variables(year=2018,dataset = "acs1",cache = TRUE)
View(ACSvariablelist)

#write.csv(variablelist,file="C:\\Users\\jacob\\Desktop\\Odum_Workshop_R_Census\\ACSVarList.csv",
#          row.names = FALSE)

### Get 2014-2018 median household income for Florida counties
## Key parameters: geography, variable, year, state, county, survey, key
# B19013_001 is the variable for median housheold income
NC_County_MedHHInc <- get_acs(geography = "county", year=2018, state = "NC", 
                              variables = "B19013_001",
                              survey="acs5",key = census_api_key,cache_table = TRUE)
View(NC_County_MedHHInc)

#NC_County_MedHHInc_table <- get_acs(geography = "county", year=2018, state = "NC", 
#                              table="B19013",#variables = "B19013_001"
#                              survey="acs5",key = census_api_key,cache_table = TRUE)
#View(NC_County_MedHHInc_table)


#NC_County_SexAge <- get_acs(geography = "county", year=2018, state = "NC", 
#                              variables = "B01001_001",
#                              survey="acs5",key = census_api_key,cache_table = TRUE)
#View(NC_County_SexAge)

#NC_County_SexAge_table <- get_acs(geography = "county", year=2018, state = "NC", 
#                                    table="B01001",
#                                    survey="acs5",key = census_api_key,cache_table = TRUE)
#View(NC_County_SexAge_table)

#########################################################################################
#######                             Making graphs                            ############
#########################################################################################

### Let us make a graph that ranks the median household income across counties
NC_County_MedHHInc %>%                               
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Household income by county in North Carolina",
       subtitle = "2013-2017 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")

NC_County_MedHHInc %>%                               
  # keep top 10 wealthiest counties
  mutate(rank = min_rank(desc(estimate))) %>%  filter(rank<=10) %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Household income by county in North Carolina",
       subtitle = "2013-2017 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")

###############################################################################
###########################   Making maps  ####################################
###############################################################################

#1. Make a map of median household income in 

### We need to download data with "geometry" information
NC_County_MedHHInc <- get_acs(geography = "county", year=2018, state = "FL", 
                              variables = "B19013_001", geometry = TRUE,
                              survey="acs5",key = census_api_key,cache_table = TRUE)
View(NC_County_MedHHInc)
## 'geometry' is a list column that contains geographic information (e.g., geometry 
# type and points) for individual observations, or census tracts here. It comes from 
# the US Census Cartographic Boundary Shapefiles, which is a low-resolution version 
# of the TIGER Shapefiles and helps faster processing (By default, 'cb = TRUE').

NC_County_MedHHInc %>%
  ggplot(aes(fill = estimate, color = estimate)) + 
  geom_sf() +         # geom_sf helps plot sf objects on a 2D plane.  
  scale_fill_viridis(option = "viridis", direction = -1) + 
  scale_color_viridis(option = "viridis", direction = -1)

# 2. Map the 2010 racial compositions in tracts within Orange County
#load_variables(year=2010,dataset = "sf1",cache = TRUE)
racevars <- c(White = "P005003", 
              Black = "P005004", 
              Asian = "P005006", 
              Hispanic = "P004003")

Wake_PopulationByRace <-get_decennial(geography = "tract", variables = racevars, year=2010,  
                                        state = "NC", county = "Wake County", geometry = TRUE,   
                                        summary_var = "P001001",key=census_api_key,cache_table = TRUE)  
Wake_PopulationByRace %>%
  mutate(pct = 100 * (value / summary_value)) %>%         # compute the percent of individual racial groups 
  ggplot(aes(fill = pct)) +
  facet_wrap(~variable) +                                 # create faceted plots by race 
  geom_sf(color = "white", size = 0.1) +
  #coord_sf(crs = 26915) + 
  scale_fill_viridis(direction = -1) #+

#3. Interactive mapping
mapview(NC_County_MedHHInc, zcol = "estimate", legend = TRUE)

# 4. Making many interactive maps with one line of code
WA_HousingVar <- read.csv(file="C:/Users/jacob/Desktop/Odum_Workshop_R_Census/HousingExample.csv",header = TRUE)
View(WA_HousingVar)

WA_Tract <- get_acs(geography = "tract", variables = "B19013_001", table=NULL, year=2017,
                    state = "WA", county = NULL, geometry = TRUE,
                    key = census_api_key, survey="acs5",cache_table = TRUE)
WA_Tract <- WA_Tract[,c("GEOID","geometry")]
WA_HousingVar <- merge(WA_Tract,WA_HousingVar,by="GEOID")  # WA_Tract is the spatial object
View(WA_HousingVar)

mapview(WA_HousingVar, burst=TRUE, hide=TRUE)
