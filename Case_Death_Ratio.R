library(tidyverse)
library(ggiraph)
library(glue)
library(patchwork)
library(vroom)
library(USAboundaries)
library(tigris)
library(scales)

state_choice <- "Kansas"

# Load in Data ------------------------------------------------------
#Read in state names and fips
State_Names <- read_rds("data/State_Names.RDS") %>% mutate(state_fips=usmap::fips(state))

#Read in county populations
county_pop_clean <- read_rds("data/county_pop_clean.RDS")

#Read in state populations
state_pop_clean <- read_rds("data/state_pop_clean.RDS")

#Read in Coronavirus Data
US_Data_Raw <- vroom(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))

#Read in Shapefile data
#Read in Shapefiles
Counties <- read_sf(here('data/cb_2018_us_county_20m/cb_2018_us_county_20m.shp'))


# Clean and Wrangle Data --------------------------------------------------
#Add state names and pop data to NYT Data
US_Data <- US_Data_Raw %>% 
  #keep only states I can map
  filter(state %in% State_Names$state | state=="District of Columbia") %>%
  #Add state names 
  left_join(State_Names, by=c("state")) %>% 
  #Add state pops
  left_join(state_pop_clean,by="state") %>%
  #Add county pops
  left_join(county_pop_clean, by=c("state","county")) %>% 
  #Rename some columns
  rename("County"=county,"State"=state, 
         "Date"=date, "Cases"=cases,"Deaths"=deaths) %>% 
  #Grab the county fips and add NYC pop
  mutate(county_fips=substr(fips,3,5),
         County_Population=ifelse(County=="New York City", 8419000, County_Population))

#Generate new variables
US_Data_Agg <- US_Data %>% 
  #For each county
  group_by(fips) %>% 
  #Get the total cases and total deaths, and keep info on State pop,
  # County pop, county fips, state fips, and county and state name
  summarise(Cases=sum(Cases),
            Deaths=sum(Deaths),
            State_Population=max(State_Population),
            County_Population=max(County_Population),
            county_fips=max(county_fips),
            state_fips=max(state_fips),
            County=max(County),
            State=max(State), 
            state_abb=max(abb)) %>% 
  ungroup() %>% 
  group_by(State) %>% 
  #Get the share of a given state's pop made up by a county, 
  # the share of cases/deaths made up by a given county,
  # the ratio between percent of pop and percent of cases/deaths 
  # made up by a given county, and the percent of cases that end in deaths
  mutate(Share_of_Pop=County_Population/State_Population*100,
         Share_of_Cases=Cases/sum(Cases,na.rm = T)*100,
         Share_of_Deaths=Deaths/sum(Deaths,na.rm = T)*100,
         Case_Ratio=Share_of_Cases/Share_of_Pop,
         Death_Ratio=Share_of_Deaths/Share_of_Pop,
         Deaths_per_Case=Deaths/Cases*100) %>% 
  ungroup()


# Choose State ------------------------------------------------------------
state_choice="New York"

plot_data <- US_Data_Agg %>% filter(State==state_choice)

# Ratio Scatter -----------------------------------------------------------

(Ratio_Scatter <- plot_data %>% 
  filter(Cases!=0,
         Deaths!=0) %>% 
  ggplot(aes(Case_Ratio,Death_Ratio)) +
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = 1) +
  geom_point_interactive(aes(tooltip=glue("{County} County, {state_abb}
                                          Case to Population Ratio: {round(Case_Ratio,2)}
                                          Death to Population Ratio: {round(Death_Ratio,2)}"),
                            data_id=fips)) +
  theme_minimal())
  
girafe(
  ggobj = Ratio_Scatter
)



# bar plot ----------------------------------------------------------------
Ratio_Bar <- NY %>% 
  filter(Cases!=0,
         Deaths!=0) %>% 
  ggplot(aes(fct_reorder(County,Deaths_per_Case),Deaths_per_Case)) +
  geom_col_interactive(aes(tooltip=glue("{County} County, Since January 1, 2020
                                          % of Cases Resulting in Death: {round(Deaths_per_Case,2)}%"),
                           data_id=County,
                           fill=Deaths_per_Case)) +
  scale_fill_viridis_c(option = "inferno") + 
  labs(x=NULL,
       y=NULL) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank())

girafe(
  ggobj = Ratio_Bar
)

Full_Plot <- Ratio_Scatter + Ratio_Bar
Full_Plot
girafe(
  ggobj = Full_Plot
)



# Map ---------------------------------------------------------------------
Map_Plot <- NY_Counties_SF %>% 
  ggplot() +
  geom_sf_interactive(aes(fill=Deaths_per_Case,
                          tooltip=glue("{County} County
                                          % of Cases Resulting in Death: {round(Deaths_per_Case,2)}%"),
                          data_id=County),
                      size = 0.2, 
                      color = "black",
                      show.legend=F) +
  scale_fill_viridis_c(option = "inferno") + 
  theme_void()

girafe(
  ggobj = Map_Plot
)

Map_and_Bar <- Ratio_Bar + Map_Plot

girafe(
  ggobj = Map_and_Bar
)

