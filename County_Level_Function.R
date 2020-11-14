# Loading Libraries--------------------------------------------------------
library(shiny)
library(tidyverse)
library(lubridate)
library(geofacet)
library(scales)
library(zoo)
library(ggtext) 
library(ggthemes)
library(shinythemes)
library(gganimate)
library(sf)
library(tidycensus)

# Loading Raw Data --------------------------------------------------------
#Prereq data
State_Names <- read_rds("data/State_Names.RDS")
county_pop_clean <- read_rds("data/county_pop_clean.RDS")
state_pop_clean <- read_rds("data/state_pop_clean.RDS")

US_Data_Raw <- read_csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))

US_Data <- left_join(US_Data_Raw,State_Names, by=c("state"))
US_Data <- left_join(US_Data,state_pop_clean,by="state")
US_Data <- left_join(US_Data,county_pop_clean, by=c("state","county")) %>%
  rename("County"=county,"State"=state, "Date"=date, "Cases"=cases,"Deaths"=deaths)
US_Data <- US_Data %>% filter(State %in% State_Names$state | State=="District of Columbia")

US_Data <- US_Data %>% 
  filter(County!="Unknown") %>% mutate(County_Population=ifelse(County=="New York City",8399000,County_Population))

# Getting Dates -----------------------------------------------------------
latest_date <- last(US_Data$Date)
number_of_days <- ymd(latest_date) - ymd("2020-03-25")

dates <- vector(mode = "character", length = number_of_days)
for (i in 1:number_of_days) {
  dates[i] <- as.character(ymd(latest_date) - i)
}
dates <- as_tibble(dates)


# Making the Function Itself ----------------------------------------------
county_graph <- function(state="Alabama",
                         county="Autauga",
                         measure="New Cases Per 100k",
                         rollmean=7,
                         date_min=ymd("2020-03-25"),
                         date_max=Sys.Date()){
  

# Filtering the data ------------------------------------------------------
  Data <- US_Data %>% 
    filter(State==state,County==county)
 

# Setting Up The Parameters -----------------------------------------------

#Making sure they used an appropriate measure
  if(!(measure %in% c("New Cases", "New Cases Per 100k", "New Deaths", "New Deaths Per 100k"))){
    return("Method must be one of the following: New Cases, New Cases Per 100k, New Deaths, New Deaths Per 100k")
  }
  

 #Translating the Measure
  measure_var <- case_when(measure=="New Cases"~"New_Cases",
                       measure=="New Cases Per 100k"~"New_Cases_Per_100k",
                       measure=="New Deaths"~"New_Deaths",
                       measure=="New Deaths Per 100k"~"New_Deaths_Per_100k")
##Rolling Average Line
  line <- case_when(str_detect(measure,"Case")==TRUE~"New_Cases_Avg",
                    str_detect(measure,"Death")==TRUE~"New_Deaths_Avg")
  
  
# Making the cases and deaths variables -----------------------------------
  Data <- 
    Data %>%
    arrange(State, County,Date) %>% 
    mutate(cases_per_100k=(Cases/County_Population)*100000,
           deaths_per_100k=(Deaths/County_Population)*100000, 
           
           New_Cases=Cases-lag(Cases), ##New cases is today's cases minus yesterdays
           New_Cases=ifelse(New_Cases<0,0,New_Cases),
           New_Cases_Avg=rollmean(New_Cases,k = rollmean,fill = NA, align = "right"),
           New_Cases_Per_100k=(New_Cases/County_Population)*100000,
           New_Cases_Per_100k_Avg=(New_Cases_Avg/County_Population)*100000,
           
           New_Deaths=Deaths-lag(Deaths), 
           New_Deaths=ifelse(New_Deaths<0,0,New_Deaths),
           New_Deaths_Avg=rollmean(New_Deaths,k = rollmean,fill = NA, align = "right"),
           New_Deaths_Per_100k=(New_Deaths/County_Population)*100000,
           New_Deaths_Per_100k_Avg=(New_Deaths_Avg/County_Population)*100000)
  

# Getting whether new cases/deaths are increasing or decreasing -----------
  Increasing_or_Decreasing <- 
    Data %>% 
    select(County,State,Date,New_Cases_Avg, New_Deaths_Avg) %>% 
    mutate(change_in_new_cases=New_Cases_Avg-lag(New_Cases_Avg),
           change_in_new_deaths=New_Deaths_Avg-lag(New_Deaths_Avg),
           Cases_Up_or_Down=case_when(change_in_new_cases>0~"Increasing",
                                      change_in_new_cases<0~"Decreasing",
                                      change_in_new_cases==0~"Steady"),
           Deaths_Up_or_Down=case_when(change_in_new_deaths>0~"Increasing",
                                       change_in_new_deaths<0~"Decreasing",
                                       change_in_new_deaths==0~"Steady")) %>% 
    select(Date,State,County,change_in_new_cases,change_in_new_deaths,
           Cases_Up_or_Down, Deaths_Up_or_Down)
  
  #Up or down measure
  Up_or_Down_Measure <- case_when(str_detect(measure,"Case")==TRUE~"Cases_Up_or_Down",
                                  str_detect(measure,"Death")==TRUE~"Deaths_Up_or_Down")
  Up_or_Down_Label <- case_when(str_detect(measure,"Case")==TRUE~
                                  last(Increasing_or_Decreasing$Cases_Up_or_Down),
                                str_detect(measure,"Death")==TRUE~
                                  last(Increasing_or_Decreasing$Deaths_Up_or_Down))

# Making Labels for all the cases/deaths variables ------------------------
  Data <- left_join(Data,Increasing_or_Decreasing, by=c("State","County","Date")) %>% 
    mutate(Label_New_Cases=paste("There were ",round(New_Cases,0), " new cases reported in ",
                                 county, " county on ", as.character(month(Date, label = T,abbr = F)),
                                 " ", day(Date), ", ", year(Date), 
                                 ".", " The rolling average was ", round(New_Cases_Avg,0),".",sep=""),
           Label_New_Cases_Per_100k=paste("There were ",round(New_Cases_Per_100k,0), " new cases per 100,000 residents reported in ",
                                          county, " county on ", as.character(month(Date, label = T,abbr = F)),
                                          " ", day(Date), ", ", year(Date), 
                                          ".", " The rolling average was ", round(New_Cases_Per_100k_Avg,0), ".",sep=""),
           Label_New_Deaths=paste("There were ", round(New_Deaths,0), " new deaths reported in ",
                                  county, " county on ", as.character(month(Date, label = T,abbr = F)),
                                  " ", day(Date), ", ", year(Date), 
                                  ".", " The rolling average was ", round(New_Deaths_Avg,0), ".",sep=""),
           Label_New_Deaths_Per_100k=paste("There were ", round(New_Deaths_Per_100k,0), " new deaths per 100,000 residents reported in ",
                                           county, " county on ", as.character(month(Date, label = T,abbr = F)),
                                           " ", day(Date), ", ", year(Date), 
                                           ".", " The rolling average was ", round(New_Deaths_Per_100k_Avg,0),sep=""))
  


# Plotting ----------------------------------------------------------------
  ggplot(Data, aes_string(x="Date",y=measure_var)) +
    geom_col(aes_string(text=paste("Label_",measure_var,sep=""),
                 fill=Up_or_Down_Measure,color=Data$Up_or_Down_Measure), alpha=.7) +
    geom_line(aes_string(y=paste(measure_var,"_Avg",sep="")),lwd=1) +
    scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), 
                 guide = guide_axis(check.overlap = T)) +
    scale_y_continuous(expand = c(0,0),label = comma) +
    scale_fill_manual(values = c("#91cf60","grey70","red"), 
                      breaks = c("Decreasing", "Steady","Increasing")) +
    scale_color_manual(values = c("#91cf60","grey70","red"), 
                       breaks = c("Decreasing", "Steady","Increasing")) +
    coord_cartesian(xlim = c(date_min,date_max)) +
    labs(y=NULL,
         x=NULL,
         fill=NULL,
         title=paste("The Rolling Average of ", measure, " is " , Up_or_Down_Label, 
                     " in ", county, ", ", state, sep=""),
         subtitle = paste(rollmean,"day rolling average"),
         caption = "Plot: @jakepscott2020 | Data: New York Times") +
    theme_bw(base_family = "Source Sans Pro",base_size = 12) +
    theme(panel.grid = element_blank(),
          plot.title = element_text(face = "bold", size = rel(1.2)),
          plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
          plot.caption = element_text(face = "italic", size = rel(0.8), 
                                      color = "grey70"),
          legend.position = "none",
          axis.text.x = element_text(size=rel(.8)),
          axis.text.y = element_text(size=rel(.8)),
          plot.title.position = "plot")
}
county_graph(rollmean = 10,measure = "New Deaths Per 100k")

