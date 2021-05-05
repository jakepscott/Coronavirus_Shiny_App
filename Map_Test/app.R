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
library(shinythemes)
library(shinycssloaders)
library(shinyalert)
library(shinyjs)

# Load in Data ------------------------------------------------------
#Read in state names and fips
State_Names <- read_rds(here("data/State_Names.RDS")) %>% mutate(state_fips=usmap::fips(state))

#Read in county populations
county_pop_clean <- read_rds(here("data/county_pop_clean.RDS"))

#Read in state populations
state_pop_clean <- read_rds(here("data/state_pop_clean.RDS"))

#Read in Coronavirus Data
US_Data_Raw <- vroom(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))

#Read in Shapefile data
#Read in Shapefiles
Counties <- read_sf(here('data/cb_2018_us_county_20m/cb_2018_us_county_20m.shp'))

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyjs(),
    sidebarLayout(
        sidebarPanel(selectInput("statenameformap","State", choices=state.name),
                     selectInput("map_measure", "Measure", 
                                 choices = c("New Cases"="New_Cases", 
                                             "New Deaths"="New_Deaths",
                                             "Total Cases"="Cases",
                                             "Total Deaths"="Deaths",
                                             "Cases to Population Ratio" = "Case_Ratio",
                                             "Deaths to Population Ratio" = "Death_Ratio",
                                             "Percent of Cases Resulting in Death" = "Deaths_per_Case"),
                                 selected = "Percent of Cases Resulting in Death"),
                     dateInput("map_date", "Date:", value = Sys.Date()-1,
                               min = "2020-01-22", max = Sys.Date()-1),
                     checkboxInput("PerMilMap", "Per Million Residents",FALSE),
                     checkboxInput("RollingAvgMap", "Mean Over Last 7 Days",FALSE)
                     
        ),
        
        mainPanel(
            withSpinner(girafeOutput("bar_and_map",height = 800),type = 6)
        )
    )
)


# Server ------------------------------------------------------------------
server <- function(input, output) {
    # Clean and Wrangle Data --------------------------------------------------
    #Add state names and pop data to NYT Data
    US_Data <- reactive(US_Data_Raw %>% 
        #keep only selected state
        filter(state==input$statenameformap,
               date>=input$map_date-7 & date<=input$map_date,
               county!="Unknown",
               !str_detect(county,"Aleutians West")) %>% # This filters the data to just be the seven days leading up to the date of interest, so we can get the 7 day rolling average
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
               County_Population=ifelse(County=="New York City", 8419000, County_Population)) %>% 
    #Generate new variables
        arrange(County,Date) %>% 
        group_by(fips) %>% 
        mutate(Cases_Per_Million=Cases/County_Population*1000000,
               Deaths_Per_Million=Deaths/County_Population*1000000,
               
               New_Cases=Cases-lag(Cases), ##New cases is today's cases minus yesterdays
               New_Cases=ifelse(New_Cases<0,0,New_Cases), # Set it to zero if it is negative
               New_Cases_Average=rollmean(New_Cases,k = 7,fill = NA, align = "right"),
               New_Cases_Per_Million=(New_Cases/County_Population)*1000000,
               New_Cases_Per_Million_Average=(New_Cases_Average/County_Population)*1000000,
               
               New_Deaths=Deaths-lag(Deaths), 
               New_Deaths=ifelse(New_Deaths<0,0,New_Deaths),
               New_Deaths_Average=rollmean(New_Deaths,k = 7,fill = NA, align = "right"),
               New_Deaths_Per_Million=(New_Deaths/County_Population)*1000000,
               New_Deaths_Per_Million_Average=(New_Deaths_Average/County_Population)*1000000) %>% 
        ungroup() %>% 
    #Keep only date of interest
        filter(Date==as.Date(input$map_date)) %>% 
    #Add ratio measures
        mutate(Share_of_Pop=County_Population/State_Population*100,
               Share_of_Cases=Cases/sum(Cases,na.rm = T)*100,
               Share_of_Deaths=Deaths/sum(Deaths,na.rm = T)*100,
               Case_Ratio=Share_of_Cases/Share_of_Pop,
               Death_Ratio=Share_of_Deaths/Share_of_Pop,
               Deaths_per_Case=Deaths/Cases*100) %>% 
    #Make labels for tooltip
        mutate(#Labels for cases: total, total per million, new, new per million, new on average, new on average per million
            Cases_Label=glue("{prettyNum(round(Cases,0),big.mark = ',',big.interval = 3)} total cases were reported in {County} County as of {input$map_date}"),
            Cases_Per_Million_Label=glue("{prettyNum(round(Cases_Per_Million,2),big.mark = ',',big.interval = 3)} total cases per million residents were reported in {County} County as of {input$map_date}"),
            New_Cases_Label=glue("{prettyNum(round(New_Cases,0),big.mark = ',',big.interval = 3)} new cases were reported in {County} County on {input$map_date}"),
            New_Cases_Average_Label=glue("{prettyNum(round(New_Cases_Average,2),big.mark = ',',big.interval = 3)} new cases were reported on average in {County} County for the week ending on {input$map_date}"),
            New_Cases_Per_Million_Label=glue("{prettyNum(round(New_Cases_Per_Million,2),big.mark = ',',big.interval = 3)} new cases per million residents were reported in {County} County on {input$map_date}"),
            New_Cases_Per_Million_Average_Label=glue("{prettyNum(round(New_Cases_Per_Million_Average,2),big.mark = ',',big.interval = 3)} new cases per million residents on average were reported in {County} County for the week ending on {input$map_date}"),
            
            #Labels for deaths: total, total per million, new, new per million, new on average, new on average per million
            Deaths_Label=glue("{prettyNum(round(Deaths,0),big.mark = ',',big.interval = 3)} total deaths were reported in {County} County as of {input$map_date}"),
            Deaths_Per_Million_Label=glue("{prettyNum(round(Deaths_Per_Million,2),big.mark = ',',big.interval = 3)} total deaths per million residents were reported in {County} County as of {input$map_date}"),
            New_Deaths_Label=glue("{prettyNum(round(New_Deaths,0),big.mark = ',',big.interval = 3)} new deaths were reported in {County} County on {input$map_date}"),
            New_Deaths_Average_Label=glue("{prettyNum(round(New_Deaths_Average,2),big.mark = ',',big.interval = 3)} new deaths were reported on average in {County} County for the week ending on {input$map_date}"),
            New_Deaths_Per_Million_Label=glue("{prettyNum(round(New_Deaths_Per_Million,2),big.mark = ',',big.interval = 3)} new deaths per million residents were reported in {County} County on {input$map_date}"),
            New_Deaths_Per_Million_Average_Label=glue("{prettyNum(round(New_Deaths_Per_Million_Average,2),big.mark = ',',big.interval = 3)} new deaths per million residents on average were reported in {County} County for the week ending on {input$map_date}"),
            
            #Labels for ratios
            Case_Ratio_Label=glue("{prettyNum(round(Case_Ratio,2),big.mark = ',',big.interval = 3)} is the ratio of percent of cases reported in {County} County divided by the county's share of the state population"),
            Death_Ratio_Label=glue("{prettyNum(round(Death_Ratio,2),big.mark = ',',big.interval = 3)} is the ratio of percent of deaths reported in {County} County divided by the county's share of the state population"),
            Deaths_per_Case_Label=glue("{round(Deaths_per_Case,2)}% of cases resulted in death in {County} County as of {input$map_date}")
        ))
    
    #This disables the per million and rolling mean checkboxes if a relevant measure isn't selected.
    # So they are enabled if the user selects New Cases, but they are disabled if the user selects
    # Case_Ratio
    observe({
        shinyjs::toggleState(id = "PerMilMap", condition = input$map_measure %in% c("New_Cases",
                                                                                    "New_Deaths",
                                                                                    "Cases",
                                                                                    "Deaths"))
        shinyjs::toggleState(id = "RollingAvgMap", condition = input$map_measure %in% c("New_Cases",
                                                                                    "New_Deaths",
                                                                                    "Cases",
                                                                                    "Deaths"))
    })
    
    output$bar_and_map <- renderGirafe({
        # Set the Measure ---------------------------------------------------------
        measure <- input$map_measure
        
        #If the measure is new cases, check whether it should be rolling aversge and/or per million
        if (measure == "New_Cases") {
            if (input$PerMilMap & input$RollingAvgMap) {
                measure <- "New_Cases_Per_Million_Average"
            } else if (input$PerMilMap) {
                measure <- "New_Cases_Per_Million"
            } else if (input$RollingAvgMap) {
                measure <- "New_Cases_Average"
            } else {
                measure <- "New_Cases"
            }
        }
        
        #If the measure is cases, check if it should be per million
        if (measure == "Cases") {
            measure <- ifelse(input$PerMilMap,"Cases_Per_Million","Cases")
        }
        
        #If the measure is new deaths, check whether it should be rolling aversge and/or per million
        if (measure == "New_Deaths") {
            if (input$PerMilMap & input$RollingAvgMap) {
                measure <- "New_Deaths_Per_Million_Average"
            } else if (input$PerMilMap) {
                measure <- "New_Deaths_Per_Million"
            } else if (input$RollingAvgMap) {
                measure <- "New_Deaths_Average"
            } else {
                measure <- "New_Deaths"
            }
        }
        
        #If the measure is deaths, check if it should be per million
        if (measure == "Deaths") {
            measure <- ifelse(input$PerMilMap,"Deaths_Per_Million","Deaths")
        }
        

        # Plot the bar graph------------------------------------------------------------
        Ratio_Bar <- US_Data() %>% 
             filter(!!as.symbol(measure)>0) %>% 
             slice_max(order_by = !!as.symbol(measure), n = 30) %>% 
             ggplot(aes(fct_reorder(County,!!as.symbol(measure)),y = !!as.symbol(measure))) +
             geom_col_interactive(aes(tooltip=!!as.symbol(glue("{measure}_Label")),
                                      data_id=fips,
                                      fill=!!as.symbol(measure)),
                                  show.legend=F) +
             scale_fill_viridis_c(option = "plasma") + 
             labs(x=NULL,
                  y=NULL,
                  fill=str_replace_all(measure,"_"," "),
                  title=glue("Top counties by {str_replace_all(measure,'_',' ')}")) +
             coord_flip() +
             theme_minimal(base_size = 12) +
             theme(panel.grid = element_blank(),
                   legend.position = "bottom",
                   axis.text.y = element_text(size=rel(.5)),
                   plot.title.position = "plot")
        
        
        # Map ---------------------------------------------------------------------
        #Get map data
        US_Data_Agg_SF <-   tigris::geo_join(spatial_data = Counties,
                                             data_frame = US_Data(), 
                                             by_sp = "GEOID",
                                             by_df = "fips") %>% 
            filter(State==input$statenameformap)
        
        Map_Plot <- US_Data_Agg_SF %>% 
                ggplot() +
                geom_sf_interactive(aes(tooltip=!!as.symbol(glue("{measure}_Label")),
                                        data_id=GEOID,
                                        fill=!!as.symbol(measure)),
                                    size = 0.2, 
                                    color = "black") +
                scale_fill_viridis_c(option = "inferno") + 
                labs(fill=glue("{str_replace_all(measure,'_',' ')}")) +
                theme_void(base_size = 12) +
                theme(legend.text = element_text(size = rel(.5)))
        
        # Join Map, bar, and scatter ----------------------------------------------
        
        Map_and_Bar <- Ratio_Bar + Map_Plot + 
             plot_layout(guides = 'collect') & 
             theme(legend.position = 'bottom') &
             guides(guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5)))
        
        girafe(
            ggobj = Map_and_Bar
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
