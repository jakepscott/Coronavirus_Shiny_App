library(tidyverse)
library(ggiraph)
library(glue)
library(patchwork)
library(vroom)
library(tigris)
library(scales)
library(lubridate)
library(zoo)
library(sf)
library(here)
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


# Set Parameters ----------------------------------------------------------
state_choice <- "Wyoming"
rollmean <- 7
date_choice <- ymd("2020-12-15") - 7

# Clean and Wrangle Data --------------------------------------------------
#Add state names and pop data to NYT Data
US_Data <- US_Data_Raw %>% 
  #keep only selected state
  filter(state==state_choice,
         date>=date_choice-7 & date<=date_choice) %>% # This filters the data to just be the seven days leading up to the date of interest, so we can get the 7 day rolling average
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
  arrange(County,Date) %>% 
  group_by(fips) %>% 
  mutate(Cases_Per_Million=Cases/County_Population*1000000,
         Deaths_Per_Million=Deaths/County_Population*1000000,
         
         New_Cases=Cases-lag(Cases), ##New cases is today's cases minus yesterdays
         New_Cases=ifelse(New_Cases<0,0,New_Cases), # Set it to zero if it is negative
         New_Cases_Avg=rollmean(New_Cases,k = 7,fill = NA, align = "right"),
         New_Cases_Per_Million=(New_Cases/County_Population)*1000000,
         New_Cases_Per_Million_Avg=(New_Cases_Avg/County_Population)*1000000,
         
         New_Deaths=Deaths-lag(Deaths), 
         New_Deaths=ifelse(New_Deaths<0,0,New_Deaths),
         New_Deaths_Avg=rollmean(New_Deaths,k = rollmean,fill = NA, align = "right"),
         New_Deaths_Per_Million=(New_Deaths/County_Population)*1000000,
         New_Deaths_Per_Million_Avg=(New_Deaths_Avg/County_Population)*1000000) %>% 
  ungroup()

#Keep only date of interest
US_Data_Agg <- US_Data_Agg %>% filter(Date==as.Date(date_choice))

#Add ratio measures
US_Data_Agg <- US_Data_Agg %>% 
  mutate(Share_of_Pop=County_Population/State_Population*100,
         Share_of_Cases=Cases/sum(Cases,na.rm = T)*100,
         Share_of_Deaths=Deaths/sum(Deaths,na.rm = T)*100,
         Case_Ratio=Share_of_Cases/Share_of_Pop,
         Death_Ratio=Share_of_Deaths/Share_of_Pop,
         Deaths_per_Case=Deaths/Cases*100)

#Make labels for tooltip
US_Data_Agg <- US_Data_Agg %>% 
  mutate(#Labels for cases: total, total per million, new, new per million, new on average, new on average per million
         Cases_Label=glue("{prettyNum(round(Cases,0),big.mark = ',',big.interval = 3)} total cases were reported in {County} County as of {date_choice}"),
         Cases_Per_Million_Label=glue("{prettyNum(round(Cases_Per_Million,0),big.mark = ',',big.interval = 3)} cases per million residents were reported in {County} County as of {date_choice}"),
         New_Cases_Label=glue("{prettyNum(round(New_Cases,0),big.mark = ',',big.interval = 3)} new cases were reported in {County} County on {date_choice}"),
         New_Cases_Avg_Label=glue("{prettyNum(round(New_Cases_Avg,0),big.mark = ',',big.interval = 3)} new cases were reported on average in {County} County for the week ending on {date_choice}"),
         New_Cases_Per_Million_Label=glue("{prettyNum(round(New_Cases_Per_Million,0),big.mark = ',',big.interval = 3)} new cases per million residents reported in {County} County on {date_choice}"),
         New_Cases_Per_Million_Avg_Label=glue("{prettyNum(round(New_Cases_Per_Million_Avg,0),big.mark = ',',big.interval = 3)} new cases were per million residents on average reported in {County} County for the week ending on {date_choice}"),
         
         #Labels for deaths: total, total per million, new, new per million, new on average, new on average per million
         Deaths_Label=glue("{prettyNum(round(Deaths,0),big.mark = ',',big.interval = 3)} total deaths were reported in {County} County as of {date_choice}"),
         Deaths_Per_Million_Label=glue("{prettyNum(round(Deaths_Per_Million,0),big.mark = ',',big.interval = 3)} deaths per million residents were reported in {County} County as of {date_choice}"),
         New_Deaths_Label=glue("{prettyNum(round(New_Deaths,0),big.mark = ',',big.interval = 3)} new deaths were reported in {County} County on {date_choice}"),
         New_Deaths_Avg_Label=glue("{prettyNum(round(New_Deaths_Avg,0),big.mark = ',',big.interval = 3)} new deaths were reported on average in {County} County for the week ending on {date_choice}"),
         New_Deaths_Per_Million_Label=glue("{prettyNum(round(New_Deaths_Per_Million,0),big.mark = ',',big.interval = 3)} new deaths per million residents reported in {County} County on {date_choice}"),
         New_Deaths_Per_Million_Avg_Label=glue("{prettyNum(round(New_Deaths_Per_Million_Avg,0),big.mark = ',',big.interval = 3)} new deaths were per million residents on average reported in {County} County for the week ending on {date_choice}"),
         
         #Labels for ratios
         Case_Ratio_Label=glue("{prettyNum(round(Case_Ratio,2),big.mark = ',',big.interval = 3)} is the ratio of case share to population share in {County} County as of {date_choice}"),
         Death_Ratio_Label=glue("{prettyNum(round(Death_Ratio,2),big.mark = ',',big.interval = 3)} is the ratio of death share to population share in {County} County as of {date_choice}"),
         Deaths_per_Case_Label=glue("{round(Deaths_per_Case,2)}% percent of cases resulted in death in {County} County as of {date_choice}")
         )

# Choose Measure Parameter ------------------------------------------------
measure <- "Cases_Per_Million"

# bar plot ----------------------------------------------------------------
(Ratio_Bar <- US_Data_Agg %>% 
   filter(Cases!=0,
          Deaths!=0) %>% 
   ggplot(aes(fct_reorder(County,!!as.symbol(measure)),!!as.symbol(measure))) +
   geom_col_interactive(aes(tooltip=!!as.symbol(glue("{measure}_Label")),
                            data_id=fips,
                            fill=!!as.symbol(measure))) +
   scale_fill_viridis_c(option = "inferno") + 
   labs(x=NULL,
        y=NULL,
        fill=str_replace(measure,"_"," ")) +
   coord_flip() +
   theme_minimal(base_size = 12) +
   theme(panel.grid = element_blank(),
         legend.position = "top",
         axis.text.y = element_text(size=rel(.5))))

girafe(
  ggobj = Ratio_Bar
)


# Map ---------------------------------------------------------------------
#Get map data
US_Data_Agg_SF <-   tigris::geo_join(spatial_data = Counties,
                                     data_frame = US_Data_Agg, 
                                     by_sp = "GEOID",
                                     by_df = "fips") %>% 
  filter(State==state_choice)

(Map_Plot <- US_Data_Agg_SF %>% 
    ggplot() +
    geom_sf_interactive(aes(tooltip=!!as.symbol(glue("{measure}_Label")),
                            data_id=GEOID,
                            fill=!!as.symbol(measure)),
                        size = 0.2, 
                        color = "black",
                        show.legend=F) +
    scale_fill_viridis_c(option = "inferno") + 
    theme_void())

girafe(
  ggobj = Map_Plot
)


# Join Map, bar, and scatter ----------------------------------------------

(Map_and_Bar <- Ratio_Bar + Map_Plot + 
   plot_layout(guides = 'collect') & 
   theme(legend.position = 'top') &
   guides(guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))))

girafe(
  ggobj = Map_and_Bar
)

