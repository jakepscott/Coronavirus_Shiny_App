options(scipen = 999)
# Getting Raw US Data -------------------------------------------------------------
#Read in state names, state pops, and county pops
State_Names <- read_rds("data/State_Names.RDS")
county_pop_clean <- read_rds("data/county_pop_clean.RDS")
state_pop_clean <- read_rds("data/state_pop_clean.RDS")

#Read in cases and deaths data from NYT
US_Data_Raw <- read_csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))

#Add state names and pop data to NYT Data
US_Data <- left_join(US_Data_Raw,State_Names, by=c("state")) %>% 
  left_join(state_pop_clean,by="state") %>% 
  left_join(county_pop_clean, by=c("state","county")) %>%
  rename("County"=county,"State"=state, "Date"=date, "Cases"=cases,"Deaths"=deaths) %>% #Rename some columns
  #keep only states I can map
  filter(State %in% State_Names$state | State=="District of Columbia") 

# Getting Grouped Data ----------------------------------------------------
##Grouping by Date and State to see cases per day by state
US_Grouped <- US_Data %>%
  arrange(Date) %>%
  group_by(Date, State) %>%
  summarise(Cases=sum(Cases),
            Deaths=sum(Deaths),
            abb=last(abb),
            Population=last(State_Population)) %>% 
  ungroup() %>%
  mutate(cases_per_million=(Cases/Population)*1000000,
         Deaths_Per_Million=(Deaths/Population)*1000000)

##Getting a Column for Current Cases by State
current_cases <- US_Grouped %>% 
  group_by(State) %>%
  dplyr::summarise(current_cases=last(Cases)) 

#Joining current cases with grouped data, adding per capita column
US_Grouped <- left_join(US_Grouped,current_cases, by="State") %>%
  group_by(State) %>%
  mutate(current_cases_per_million=(current_cases/Population)*1000000)#Making things in terms of 1 million residents

##Getting a Variable for Current Deaths by State
Current_Deaths <- US_Grouped %>% 
  group_by(State) %>%
  dplyr::summarise(Current_Deaths=last(Deaths)) 

US_Grouped <- left_join(US_Grouped,Current_Deaths, by="State") %>% 
  group_by(State) %>% 
  mutate(Current_Deaths_Per_Million=(Current_Deaths/Population)*1000000)#Making things in terms of 1 million residents

#Getting a column for new cases and deaths
US_Grouped <- US_Grouped %>%
  group_by(State) %>%
  mutate(New_Cases=Cases-lag(Cases),
         New_Deaths=Deaths-lag(Deaths)) %>%  ##New cases is today's cases minus yesterdays
  ungroup() %>%
  #setting to zero instances where new cases or new deaths are negative (usually due to reporting issues)
  mutate(New_Cases=ifelse(New_Cases<0,0,New_Cases),
         New_Deaths=ifelse(New_Deaths<0,0,New_Deaths)) %>% 
  #Making things in terms of 1 million residents
  mutate(New_Cases_Per_Million=(New_Cases/Population)*1000000,
         New_Deaths_Per_Million=(New_Deaths/Population)*1000000) 

#Making a column for whether cases are increasing or decreasing
New_Cases_Increasing <- US_Grouped %>% 
  group_by(State) %>%
  slice(c(n(),n()-1)) %>%
  mutate(change_in_new_cases=New_Cases-lead(New_Cases))  %>%
  ungroup() %>%
  filter(!is.na(change_in_new_cases)) %>%
  mutate(Up_or_Down=case_when(change_in_new_cases>0~"Increasing",
                              change_in_new_cases<0~"Decreasing",
                              change_in_new_cases==0~"Steady")) %>% 
  select(State,change_in_new_cases,Up_or_Down)

US_Grouped <- left_join(US_Grouped,New_Cases_Increasing, by="State")

##Getting whether new deaths are increasing or decreasing
New_Deaths_Increasing <- US_Grouped %>% 
  group_by(abb) %>%
  slice(c(n(),n()-1)) %>%
  mutate(Change_In_New_Deaths=New_Deaths-lead(New_Deaths))  %>%
  ungroup() %>%
  filter(!is.na(Change_In_New_Deaths)) %>%
  mutate(Up_or_Down_Deaths=case_when(Change_In_New_Deaths>0~"Increasing",
                                     Change_In_New_Deaths<0~"Decreasing",
                                     Change_In_New_Deaths==0~"Steady")) %>% 
  select(State,Change_In_New_Deaths,Up_or_Down_Deaths)

US_Grouped <- left_join(US_Grouped,New_Deaths_Increasing, by="State")

