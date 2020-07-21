library(tidyverse)
library(readxl)
library(lubridate)
library(maps)
library(readr)
##############################################################
##Cleaning Raw Data- Should only have to do this once
##############################################################
#State Names and Abbreviations
states <- tibble(state = state.name) %>%
  bind_cols(tibble(abb = state.abb)) %>% 
  bind_rows(tibble(state = "District of Columbia", abb = "DC")) 

saveRDS(states, "data/State_Names.RDS")

#County pop
county_pop_raw <- read_xlsx("data/county_pop_estimates.xlsx")
county_pop_clean <- county_pop_raw %>% 
  tail(-4) %>%
  head(-6) %>%
  select(`table with row headers in column A and column headers in rows 3 through 4 (leading dots indicate sub-parts)`,
         ...13) %>%
  rename("County"=`table with row headers in column A and column headers in rows 3 through 4 (leading dots indicate sub-parts)`,
         "County_Population"=...13) %>%
  mutate(County=str_remove(County,"."),
         County=str_remove(County," County")) %>%
  separate(County,c("county","state"), sep = ", ") 

saveRDS(county_pop_clean,"data/county_pop_clean.RDS")


#State Pop
state_pop_raw <- read_xlsx("data/state_population_estimates.xlsx")

state_pop_clean <- state_pop_raw %>% tail(n = -8)
state_pop_clean <- state_pop_clean %>% select(`table with row headers in column A and column headers in rows 3 through 4. (leading dots indicate sub-parts)`,
                                              `...13`) %>% 
  rename("State_Population"=`...13`,
         "state"=`table with row headers in column A and column headers in rows 3 through 4. (leading dots indicate sub-parts)`) %>%
  mutate(state=str_remove(state,"."))
saveRDS(state_pop_clean,"data/state_pop_clean.RDS")
          