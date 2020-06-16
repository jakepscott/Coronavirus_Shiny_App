library(shiny)
library(tidyverse)
library(lubridate)
library(geofacet)
library(scales)
library(zoo)
library(ggtext) 
library(ggthemes)
library(shinythemes)
library(plotly)
# Define UI for application that draws a histogram
ui <- fluidPage(
  ##Setting Theme
  theme = shinytheme("journal"),
  # Application title
  titlePanel("Coronavirus in the United States"),
  navbarPage("",
    tabPanel("New Cases", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(dateRangeInput('dateRangeNew',
                                           label = 'Date range input: yyyy-mm-dd',
                                           start = "2020-01-22", end = Sys.Date()-1,
                                           min = "2020-01-22", max = Sys.Date()-1),
                            checkboxInput("PerMilNew", "Per Million Residents",TRUE),
                            sliderInput("RollingAverage", 
                                        label="Window for rolling average",
                                        min=1,
                                        max=14,
                                        value=7,
                                        step=1)
               ),
               mainPanel(
                 plotOutput("new_cases_by_state", height = 600)
               )
             )
    ),
    tabPanel("New Deaths", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(dateRangeInput('dateRangeNewDeaths',
                                           label = 'Date range input: yyyy-mm-dd',
                                           start = "2020-01-22", end = Sys.Date()-1,
                                           min = "2020-01-22", max = Sys.Date()-1),
                            checkboxInput("PerMilNewDeaths", "Per Million Residents",TRUE),
                            sliderInput("RollingAverageDeaths", 
                                        label="Window for rolling average",
                                        min=1,
                                        max=14,
                                        value=7,
                                        step=1)
               ),
               mainPanel(
                 plotOutput("new_deaths_by_state", height = 600)
               )
             )
    ),
    tabPanel("Total Cases", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(dateRangeInput('dateRangeTotal',
                                           label = 'Date range input: yyyy-mm-dd',
                                           start = "2020-01-22", end = Sys.Date()-1,
                                           min = "2020-01-22", max = Sys.Date()-1),
                            checkboxInput("PerMil", "Per Million Residents",TRUE)
               ),
               mainPanel(
                 plotOutput("cases_by_state", height = 600)
               )
             )
    ),
    tabPanel("Total Deaths", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(dateRangeInput('dateRangeDeaths',
                                           label = 'Date range input: yyyy-mm-dd',
                                           start = "2020-01-22", end = Sys.Date()-1,
                                           min = "2020-01-22", max = Sys.Date()-1),
                            checkboxInput("PerMilDeaths", "Per Million Residents",TRUE)
               ),
               mainPanel(
                 plotOutput("deaths_by_state", height = 600)
               )
             )
    ),
    tabPanel("State View", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput("statename","State", choices=state.name),
                            selectInput("measure",label = "Measure",choices = c("New Cases", "New Deaths",
                                                                                "New Cases Per Million","New Deaths Per Million",
                                                                                "Total Cases", "Total Deaths",
                                                                                "Total Cases Per Million", "Total Deaths Per Million"),
                                        selected = "New Cases"),
                            dateRangeInput('dateRangeStateView',
                                           label = 'Date range input: yyyy-mm-dd',
                                           start = "2020-01-22", end = Sys.Date()-1,
                                           min = "2020-01-22", max = Sys.Date()-1)
                            
               ),
               mainPanel(
                 plotlyOutput("state_view", height = 600)
               )
             )
    ),
    navbarMenu("More",
               tabPanel("About Me",
                        sidebarPanel(
                          img(src="ProfileImage.png", height=300,width=300),
                          
                          tags$article("My name is Jake Scott. I'm a recent graduate of Colgate University and an
                                          incoming research assistant at the Federal Reserve Board in DC. I am interested 
                                          in data visualization, statistics, and economics, as well as their application 
                                          to public policy. I can be reached at Jakepscott16@gmail.com or via social media.", style = "font-size:20px"),
                          tags$a(
                            href="https://twitter.com/jakepscott2020", 
                            tags$img(src="twitter.png", 
                                     title="Example Image Link", 
                                     width="50",
                                     height="50")
                            ),
                            tags$a(
                              href="https://www.linkedin.com/in/jacob-scott-689875130/", 
                              tags$img(src="linkedin.png", 
                                       title="Example Image Link", 
                                       width="38",
                                       height="38")
                        )
               )),
               tabPanel("About the Data",
                        sidebarPanel(
                        tags$article("The data used in these figures comes from the New York Times, which is collecting and publically sharing Coronavirus 
                                     data for every county in the United States. The data can be accessed via", tags$a(href="https://github.com/nytimes/covid-19-data", "Github."), 
                                     style = "font-size:20px")
                        ))
  )
))


# Define server logic required to draw a histogram
server <- function(input, output) {
  ######################################################################################################
  ######Set Up Code####
  ######################################################################################################
  State_Names <- read_rds("data/State_Names.RDS")
  county_pop_clean <- read_rds("data/county_pop_clean.RDS")
  state_pop_clean <- read_rds("data/state_pop_clean.RDS")
  US_Data_Raw <- read_csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))

  US_Data <- left_join(US_Data_Raw,State_Names, by=c("state"))
  US_Data <- left_join(US_Data,state_pop_clean,by="state")
  US_Data <- left_join(US_Data,county_pop_clean, by=c("state","county")) %>%
    rename("County"=county,"State"=state, "Date"=date, "Cases"=cases,"Deaths"=deaths)
  US_Data <- US_Data %>% filter(State %in% State_Names$state | State=="District of Columbia")
  options(scipen = 999)
  ######################################################################################################
  ######Cases by State Plot####
  ######################################################################################################
  
  #####################################
  ####Total Cases by State####
  #####################################
  output$cases_by_state <- renderPlot({
    ##Grouping by Date and State to see cases per day by state
    US_Cases_Grouped <- US_Data %>%
      arrange(Date) %>%
      filter(Date>=input$dateRangeTotal[1] & Date<=input$dateRangeTotal[2]) %>%
      group_by(Date, State) %>%
      summarise(Cases=sum(Cases),
                abb=last(abb),
                Population=last(State_Population)) %>% 
      ungroup() %>%
      mutate(cases_per_million=(Cases/Population)*1000000)
    
    ##Getting a Variable for Current Cases by State
    current_cases <- US_Cases_Grouped %>% 
      group_by(State) %>%
      dplyr::summarise(current_cases=last(Cases)) 
    
    US_Cases_Grouped <- left_join(US_Cases_Grouped,current_cases, by="State")
    
    ##Current Cases Per Capita
    US_Cases_Grouped <- US_Cases_Grouped %>%
      group_by(State) %>%
      mutate(current_cases_per_million=(current_cases/Population)*1000000)#Making things in terms of 1 million residents
    
    ##Plotting
    if (input$PerMil==TRUE) {
      ggplot(US_Cases_Grouped, aes(x=Date,y=cases_per_million)) +
        geom_line() +
        geom_area(aes(fill=current_cases_per_million)) +
        facet_geo(~abb) +
        scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
        scale_y_continuous(expand = c(0,0),label = comma) +
        scale_fill_viridis_c(option = "plasma", label = comma) + 
        labs(y=NULL,
             x=NULL,
             title="Cases Per Million Residents",
             caption = "Plot: @jakepscott2020 | Data: New York Times") +
        theme_bw(base_size = 16) +
        theme(panel.grid = element_blank(),
              plot.title = element_text(face = "bold", size = rel(1.2)),
              plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
              plot.caption = element_text(face = "italic", size = rel(0.8), 
                                          color = "grey70"),
              axis.text.x = element_text(size=rel(.6)),
              axis.text.y = element_text(size=rel(.6)),
              legend.title = element_text(size = rel(.6), face="bold"),
              legend.position = "none",
              strip.text = element_text(size=rel(.6), margin = margin(.05,0,.05,0, "cm")),
              strip.background.y=element_rect(color = "grey70",  fill=NA),
              plot.title.position = "plot")
    } else {
      ggplot(US_Cases_Grouped, aes(x=Date,y=Cases)) +
        geom_line() +
        geom_area(aes(fill=current_cases)) +
        facet_geo(~abb) +
        scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
        scale_y_continuous(expand = c(0,0), label = comma) +
        scale_fill_viridis_c(option = "plasma", label = comma) + 
        labs(y=NULL,
             x=NULL,
             title="Cases",
             caption = "Plot: @jakepscott2020 | Data: New York Times") +
        theme_bw(base_size = 16) +
        theme(panel.grid = element_blank(),
              plot.title = element_text(face = "bold", size = rel(1.2)),
              plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
              plot.caption = element_text(face = "italic", size = rel(0.8), 
                                          color = "grey70"),
              axis.text.x = element_text(size=rel(.6)),
              axis.text.y = element_text(size=rel(.6)),
              legend.position = "none",
              strip.text = element_text(size=rel(.6), margin = margin(.05,0,.05,0, "cm")),
              strip.background.y=element_rect(color = "grey70",  fill=NA),
              plot.title.position = "plot")
    }
  })
  
  ##############################
  ####New Cases By State####
  ##############################
  output$new_cases_by_state <- renderPlot({
    US_Cases_Grouped <- US_Data %>%
      arrange(Date) %>%
      filter(Date>=input$dateRangeNew[1] & Date<=input$dateRangeNew[2]) %>%
      group_by(Date, State) %>%
      summarise(Cases=sum(Cases),
                abb=last(abb),
                Population=last(State_Population)) %>% 
      ungroup() %>%
      mutate(cases_per_million=(Cases/Population)*1000000)
    
    ##Getting a Variable for Current Cases by State
    current_cases <- US_Cases_Grouped %>% 
      group_by(State) %>%
      dplyr::summarise(current_cases=last(Cases)) 
    
    US_Cases_Grouped <- left_join(US_Cases_Grouped,current_cases, by="State")
    
    ##Getting a Rolling Average of New Cases
    US_Cases_Grouped <- US_Cases_Grouped %>%
      group_by(State) %>%
      mutate(New_Cases=Cases-lag(Cases), ##New cases is today's cases minus yesterdays
             New_Cases=rollmean(New_Cases,k = input$RollingAverage,fill = NA, align = "right")) %>% # this just gets a 7 day rolling average
      ungroup() %>%
      mutate(current_cases_per_million=(current_cases/Population)*1000000, #Making things in terms of 1 million residents
             New_Cases_Per_Million=(New_Cases/Population)*1000000)
    
    ##Getting whether new cases are increasing or decreasing
    New_Cases_Increasing <- US_Cases_Grouped %>% 
      group_by(State) %>%
      slice(c(n(),n()-1)) %>%
      mutate(change_in_new_cases=New_Cases_Per_Million-lead(New_Cases_Per_Million))  %>%
      ungroup() %>%
      filter(!is.na(change_in_new_cases)) %>%
      mutate(Up_or_Down=case_when(change_in_new_cases>0~"Increasing",
                                  change_in_new_cases<0~"Decreasing",
                                  change_in_new_cases==0~"Steady")) %>% 
      select(State,change_in_new_cases,Up_or_Down)
    
    US_Cases_Grouped <- left_join(US_Cases_Grouped,New_Cases_Increasing, by="State")
    
    if (input$PerMilNew==TRUE) {
      ##Plotting
      ggplot(US_Cases_Grouped, aes(x=Date,y=New_Cases_Per_Million)) +
        geom_line() +
        facet_geo(~abb) +
        geom_area(aes(fill=Up_or_Down)) +
        scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
        scale_y_continuous(expand = c(0,0)) +
        scale_fill_manual(values = c("#91cf60","grey70","red"), breaks = c("Decreasing", "Steady","Increasing")) +
        labs(y=NULL,
             x=NULL,
             fill=NULL,
             subtitle="Rolling average of new cases per million residents",
             title = "Which states are seeing new cases <span style='color: #91cf60'>**fall**</span> versus <span style='color: red'>**rise**</span>",
             caption = "Plot: @jakepscott2020 | Data: New York Times") +
        theme_bw(base_size = 16) +
        theme(panel.grid = element_blank(),
              plot.title = element_markdown(face = "bold", size = rel(1.2)),
              plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
              plot.caption = element_text(face = "italic", size = rel(0.8), 
                                          color = "grey70"),
              legend.position = "none",
              axis.text.x = element_text(size=rel(.6)),
              axis.text.y = element_text(size=rel(.6)),
              strip.text = element_text(size=rel(.6), margin = margin(.05,0,.05,0, "cm")),
              strip.background.y=element_rect(color = "grey70",  fill=NA),
              plot.title.position = "plot")
    } else {
      ggplot(US_Cases_Grouped, aes(x=Date,y=New_Cases)) +
        geom_line() +
        facet_geo(~abb) +
        geom_area(aes(fill=Up_or_Down)) +
        scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
        scale_y_continuous(expand = c(0,0),label = comma) +
        scale_fill_manual(values = c("#91cf60","grey70","red"), breaks = c("Decreasing", "Steady","Increasing")) +
        labs(y=NULL,
             x=NULL,
             fill=NULL,
             subtitle="Rolling average of new cases",
             title = "Which states are seeing new cases <span style='color: #91cf60'>**fall**</span> versus <span style='color: red'>**rise**</span>",
             caption = "Plot: @jakepscott2020 | Data: New York Times") +
        theme_bw(base_size = 16) +
        theme(panel.grid = element_blank(),
              plot.title = element_markdown(face = "bold", size = rel(1.2)),
              plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
              plot.caption = element_text(face = "italic", size = rel(0.8), 
                                          color = "grey70"),
              legend.position = "none",
              axis.text.x = element_text(size=rel(.6)),
              axis.text.y = element_text(size=rel(.6)),
              strip.text = element_text(size=rel(.6), margin = margin(.05,0,.05,0, "cm")),
              strip.background.y=element_rect(color = "grey70",  fill=NA),
              plot.title.position = "plot")
    }
  })
  ######################################################################################################
  ####Total Deaths by State Plot####
  ######################################################################################################
  output$deaths_by_state <- renderPlot({
    US_Deaths_Grouped <- US_Data %>%
      arrange(Date) %>%
      filter(Date>=input$dateRangeDeaths[1] & Date<=input$dateRangeDeaths[2]) %>%
      group_by(Date, State) %>%
      summarise(Deaths=sum(Deaths),
                abb=last(abb),
                Population=last(State_Population)) %>% 
      ungroup() %>%
      mutate(Deaths_Per_Million=(Deaths/Population)*1000000)
    
    ##Getting a Variable for Current Deaths by State
    Current_Deaths <- US_Deaths_Grouped %>% 
      group_by(State) %>%
      dplyr::summarise(Current_Deaths=last(Deaths)) 
    
    US_Deaths_Grouped <- left_join(US_Deaths_Grouped,Current_Deaths, by="State")
    
    ##Current Deaths Per Capita
    US_Deaths_Grouped <- US_Deaths_Grouped %>%
      group_by(State) %>%
      mutate(Current_Deaths_Per_Million=(Current_Deaths/Population)*1000000)#Making things in terms of 1 million residents
    
    ##Plotting
    if (input$PerMilDeaths==TRUE) {
      ggplot(US_Deaths_Grouped, aes(x=Date,y=Deaths_Per_Million)) +
        geom_line() +
        geom_area(aes(fill=Current_Deaths_Per_Million)) +
        facet_geo(~abb) +
        scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
        scale_y_continuous(expand = c(0,0),label = comma) +
        scale_fill_viridis_c(option = "plasma", label = comma) + 
        labs(y=NULL,
             x=NULL,
             title="Deaths Per Million Residents",
             caption = "Plot: @jakepscott2020 | Data: New York Times") +
        theme_bw(base_size = 16) +
        theme(panel.grid = element_blank(),
              plot.title = element_text(face = "bold", size = rel(1.2)),
              plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
              plot.caption = element_text(face = "italic", size = rel(0.8), 
                                          color = "grey70"),
              axis.text.x = element_text(size=rel(.6)),
              axis.text.y = element_text(size=rel(.6)),
              legend.position = "none",
              strip.text = element_text(size=rel(.6), margin = margin(.05,0,.05,0, "cm")),
              strip.background.y=element_rect(color = "grey70",  fill=NA),
              plot.title.position = "plot")
    } else {
      ggplot(US_Deaths_Grouped, aes(x=Date,y=Deaths)) +
        geom_line() +
        geom_area(aes(fill=Current_Deaths)) +
        facet_geo(~abb) +
        scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
        scale_y_continuous(expand = c(0,0),label = comma) +
        scale_fill_viridis_c(option = "plasma", label = comma) + 
        labs(y=NULL,
             x=NULL,
             title="Total Deaths",
             caption = "Plot: @jakepscott2020 | Data: New York Times") +
        theme_bw(base_size = 16) +
        theme(panel.grid = element_blank(),
              plot.title = element_text(face = "bold", size = rel(1.2)),
              plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
              plot.caption = element_text(face = "italic", size = rel(0.8), 
                                          color = "grey70"),
              axis.text.x = element_text(size=rel(.6)),
              axis.text.y = element_text(size=rel(.6)),
              legend.position = "none",
              strip.text = element_text(size=rel(.6), margin = margin(.05,0,.05,0, "cm")),
              strip.background.y=element_rect(color = "grey70",  fill=NA),
              plot.title.position = "plot")
    }
  })
  ###########################
  ####New Deaths####
  ###########################
  output$new_deaths_by_state <- renderPlot({
    US_Deaths_Grouped <- US_Data %>%
      arrange(Date) %>%
      filter(Date>=input$dateRangeNewDeaths[1] & Date<=input$dateRangeNewDeaths[2]) %>%
      group_by(Date, State) %>%
      summarise(Deaths=sum(Deaths),
                abb=last(abb),
                Population=last(State_Population)) %>% 
      ungroup() %>%
      mutate(Deaths_Per_Million=(Deaths/Population)*1000000)
    
    ##Getting a Variable for Current Deaths by State
    Current_Deaths <- US_Deaths_Grouped %>% 
      group_by(State) %>%
      dplyr::summarise(Current_Deaths=last(Deaths)) 
    
    US_Deaths_Grouped <- left_join(US_Deaths_Grouped,Current_Deaths, by="State")
    
    ##Getting a Rolling Average of New deaths
    US_Deaths_Grouped <- US_Deaths_Grouped %>%
      group_by(State) %>%
      mutate(New_Deaths=Deaths-lag(Deaths), ##New deaths is today's deaths minus yesterdays
             New_Deaths=rollmean(New_Deaths,k = input$RollingAverageDeaths,fill = NA, align = "right")) %>% # this just gets a 7 day rolling average
      ungroup() %>%
      mutate(Current_Deaths_Per_Million=(Current_Deaths/Population)*1000000, #Making things in terms of 1 million residents
             New_Deaths_Per_Million=(New_Deaths/Population)*1000000)
    
    ##Getting whether new deaths are increasing or decreasing
    New_Deaths_Increasing <- US_Deaths_Grouped %>% 
      group_by(abb) %>%
      slice(c(n(),n()-1)) %>%
      mutate(Change_In_New_Deaths=New_Deaths_Per_Million-lead(New_Deaths_Per_Million))  %>%
      ungroup() %>%
      filter(!is.na(Change_In_New_Deaths)) %>%
      mutate(Up_or_Down=case_when(Change_In_New_Deaths>0~"Increasing",
                                  Change_In_New_Deaths<0~"Decreasing",
                                  Change_In_New_Deaths==0~"Steady")) %>% 
      select(State,Change_In_New_Deaths,Up_or_Down)
    
    US_Deaths_Grouped <- left_join(US_Deaths_Grouped,New_Deaths_Increasing, by="State")
    
    if (input$PerMilNewDeaths==TRUE) {
      ##Plotting
      ggplot(US_Deaths_Grouped, aes(x=Date,y=New_Deaths_Per_Million)) +
        geom_line() +
        facet_geo(~abb) +
        geom_area(aes(fill=Up_or_Down)) +
        scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
        scale_y_continuous(expand = c(0,0),label = comma) +
        scale_fill_manual(values = c("#91cf60","grey70","red"), breaks = c("Decreasing", "Steady","Increasing")) +
        labs(y=NULL,
             x=NULL,
             fill=NULL,
             subtitle="Rolling average of new deaths per million residents",
             title = "Which states are seeing new deaths <span style='color: #91cf60'>**fall**</span> versus <span style='color: red'>**rise**</span>",
             caption = "Plot: @jakepscott2020 | Data: New York Times") +
        theme_bw(base_size = 16) +
        theme(panel.grid = element_blank(),
              plot.title = element_markdown(face = "bold", size = rel(1.2)),
              plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
              plot.caption = element_text(face = "italic", size = rel(0.8), 
                                          color = "grey70"),
              legend.position = "none",
              axis.text.x = element_text(size=rel(.6)),
              axis.text.y = element_text(size=rel(.6)),
              strip.text = element_text(size=rel(.6), margin = margin(.05,0,.05,0, "cm")),
              strip.background.y=element_rect(color = "grey70",  fill=NA),
              plot.title.position = "plot") 
    } else {
       ggplot(US_Deaths_Grouped, aes(x=Date,y=New_Deaths)) +
        geom_line() +
        facet_geo(~abb) +
        geom_area(aes(fill=Up_or_Down)) +
        scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
        scale_y_continuous(expand = c(0,0),label = comma) +
        scale_fill_manual(values = c("#91cf60","grey70","red"), breaks = c("Decreasing", "Steady","Increasing")) +
        labs(y=NULL,
             x=NULL,
             fill=NULL,
             subtitle="Rolling average of new deaths",
             title = "Which states are seeing new deaths <span style='color: #91cf60'>**fall**</span> versus <span style='color: red'>**rise**</span>",
             caption = "Plot: @jakepscott2020 | Data: New York Times") +
        theme_bw(base_size = 16) +
        theme(panel.grid = element_blank(),
              plot.title = element_markdown(face = "bold", size = rel(1.2)),
              plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
              plot.caption = element_text(face = "italic", size = rel(0.8), 
                                          color = "grey70"),
              legend.position = "none",
              axis.text.x = element_text(size=rel(.6)),
              axis.text.y = element_text(size=rel(.6)),
              strip.text = element_text(size=rel(.6), margin = margin(.05,0,.05,0, "cm")),
              strip.background.y=element_rect(color = "grey70",  fill=NA),
              plot.title.position = "plot")
    }
  })
  
  ##############################
  ####State View####
  ##############################
  output$state_view <- renderPlotly({
    entry <- input$statename
    ##Grouping by Date and State to see cases per day by state
    US_Cases_Grouped <- US_Data %>%
      filter(State==input$statename) %>% 
      arrange(Date) %>%
      group_by(Date, State) %>%
      summarise(Cases=sum(Cases),
                abb=last(abb),
                Population=last(State_Population)) %>% 
      ungroup() %>%
      mutate(cases_per_million=(Cases/Population)*1000000)
    
    ##Getting a Variable for Current Cases by State
    current_cases <- US_Cases_Grouped %>% 
      filter(Date<=input$dateRangeStateView[2]) %>%
      group_by(State) %>%
      dplyr::summarise(current_cases=last(Cases)) 
    
    US_Cases_Grouped <- left_join(US_Cases_Grouped,current_cases, by="State")
    
    ##Current Cases Per Capita
    US_Cases_Grouped <- US_Cases_Grouped %>%
      group_by(State) %>%
      mutate(current_cases_per_million=(current_cases/Population)*1000000)#Making things in terms of 1 million residents
    
    
    US_Cases_Grouped <- US_Cases_Grouped %>%
      group_by(State) %>%
      mutate(New_Cases=Cases-lag(Cases), ##New cases is today's cases minus yesterdays
             New_Cases_Avg=rollmean(New_Cases,k = 7,fill = NA, align = "right")) %>% # this just gets a 7 day rolling average
      ungroup() %>%
      mutate(current_cases_per_million=(current_cases/Population)*1000000, #Making things in terms of 1 million residents
             New_Cases_Per_Million=(New_Cases/Population)*1000000,
             New_Cases_Per_Million_Avg=(New_Cases_Avg/Population)*1000000,
             New_Cases=ifelse(New_Cases<0,NA,New_Cases),
             New_Cases_Per_Million=ifelse(New_Cases_Per_Million<0,NA,New_Cases_Per_Million))
    
    ##Getting whether new cases are increasing or decreasing
    New_Cases_Increasing <- 
      US_Cases_Grouped %>% 
      select(State,Date,New_Cases_Avg) %>% 
      group_by(State) %>%
      mutate(change_in_new_cases=New_Cases_Avg-lag(New_Cases_Avg)) %>% 
      ungroup() %>%
      mutate(Up_or_Down=case_when(change_in_new_cases>0~"Increasing",
                                  change_in_new_cases<0~"Decreasing",
                                  change_in_new_cases==0~"Steady")) %>% 
      select(Date,State,change_in_new_cases,Up_or_Down)
    
    US_Cases_Grouped <- left_join(US_Cases_Grouped,New_Cases_Increasing, by=c("State","Date")) %>% 
      filter(Date>=input$dateRangeStateView[1] & Date<=input$dateRangeStateView[2]) %>% 
      mutate(label_new_cases=paste("There were ",round(New_Cases,0), " new cases reported in ",
                                   State, " on ", as.character(month(Date, label = T,abbr = F)),
                                       " ", day(Date), ", ", year(Date),sep=""),
             label_new_cases_per_million=paste("There were ",round(New_Cases_Per_Million,0), " new cases per million residents reported in ",
                                               State, " on ", as.character(month(Date, label = T,abbr = F)),
                                           " ", day(Date), ", ", year(Date),sep=""),
             label_cases=paste("There were ", round(Cases,0), " cumulative cases in ",
                               State, " as of ", as.character(month(Date, label = T,abbr = F)),
                                               " ", day(Date), ", ", year(Date),sep=""),
             label_cases_per_million=paste("There were ", round(cases_per_million,0), " cumulative cases per million residents in ",
                                           State," as of ", as.character(month(Date, label = T,abbr = F)),
                                           " ", day(Date), ", ", year(Date), ",", sep=""))
      
    
    #Deaths
    US_Deaths_Grouped <- US_Data %>%
      arrange(Date) %>%
      filter(State==input$statename) %>% 
      group_by(Date, State) %>%
      summarise(Deaths=sum(Deaths),
                abb=last(abb),
                Population=last(State_Population)) %>% 
      ungroup() %>%
      mutate(Deaths_Per_Million=(Deaths/Population)*1000000)
    
    ##Getting a Variable for Current Deaths by State
    Current_Deaths <- US_Deaths_Grouped %>% 
      filter(Date<=input$dateRangeStateView[2]) %>%
      group_by(State) %>%
      dplyr::summarise(Current_Deaths=last(Deaths)) 
    
    US_Deaths_Grouped <- left_join(US_Deaths_Grouped,Current_Deaths, by="State")
    
    ##Current Deaths Per Capita
    US_Deaths_Grouped <- US_Deaths_Grouped %>%
      group_by(State) %>%
      mutate(Current_Deaths_Per_Million=(Current_Deaths/Population)*1000000)#Making things in terms of 1 million residents
    ##Getting a Rolling Average of New deaths
    US_Deaths_Grouped <- US_Deaths_Grouped %>%
      group_by(State) %>%
      mutate(New_Deaths=Deaths-lag(Deaths), ##New deaths is today's deaths minus yesterdays
             New_Deaths_Avg=rollmean(New_Deaths,k = 7,fill = NA, align = "right")) %>% # this just gets a 7 day rolling average
      ungroup() %>%
      mutate(Current_Deaths_Per_Million=(Current_Deaths/Population)*1000000, #Making things in terms of 1 million residents
             New_Deaths_Per_Million=(New_Deaths/Population)*1000000,
             New_Deaths_Per_Million_Avg=(New_Deaths_Avg/Population)*1000000,
             New_Deaths=ifelse(New_Deaths<0,NA,New_Deaths),
             New_Deaths_Per_Million=ifelse(New_Deaths_Per_Million<0,NA,New_Deaths_Per_Million))
    
    ##Getting whether new deaths are increasing or decreasing
    New_Deaths_Increasing <- US_Deaths_Grouped %>% 
      select(State,Date,New_Deaths_Avg) %>%
      group_by(State) %>%
      mutate(Change_In_New_Deaths=New_Deaths_Avg-lag(New_Deaths_Avg))  %>%
      ungroup() %>%
      mutate(Up_or_Down=case_when(Change_In_New_Deaths>0~"Increasing",
                                  Change_In_New_Deaths<0~"Decreasing",
                                  Change_In_New_Deaths==0~"Steady")) %>% 
      select(Date,State,Change_In_New_Deaths,Up_or_Down)
    
    US_Deaths_Grouped <- left_join(US_Deaths_Grouped,New_Deaths_Increasing, by=c("State","Date")) %>% 
      filter(Date>=input$dateRangeStateView[1] & Date<=input$dateRangeStateView[2]) %>% 
      mutate(label_new_deaths=paste("There were ", round(New_Deaths,0), " new deaths reported in ",
                                    State, " on ", as.character(month(Date, label = T,abbr = F)),
                                   " ", day(Date), ", ", year(Date), ",", sep=""),
             label_new_deaths_per_million=paste("There were ", round(New_Deaths_Per_Million,0), " new deaths per million residents reported in ",
                                                State, " on ", as.character(month(Date, label = T,abbr = F)),
                                               " ", day(Date), ", ", year(Date), ",", sep=""),
             label_deaths=paste("There were ", round(Deaths,0), " cumulative deaths in ",
                                State," as of ", as.character(month(Date, label = T,abbr = F)),
                                " ", day(Date), ", ", year(Date), sep=""),
             label_deaths_per_million=paste("There were ", round(Deaths_Per_Million,0), " cumulative deaths per million residents in ",
                                            State," as of ", as.character(month(Date, label = T,abbr = F)),
                                            " ", day(Date), ", ", year(Date), sep=""))
    
    ##Plotting
    if (input$measure=="Total Cases Per Million") {
      ggplotly(
        ggplot(US_Cases_Grouped, aes(x=Date,y=cases_per_million)) +
          geom_area(aes(fill=current_cases_per_million)) +
          geom_line(color="black") +
          geom_line(aes(text=label_cases_per_million)) +
          scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
          scale_y_continuous(expand = c(0,0),label = comma) +
          scale_fill_viridis_c(option = "plasma", label = comma) + 
          labs(y=NULL,
               x=NULL,
               title=paste("Cumulative Cases Per Million Residents in", entry, sep=" "),
               caption = "Plot: @jakepscott2020 | Data: New York Times") +
          theme_bw(base_family = "Source Sans Pro",base_size = 16) +
          theme(panel.grid = element_blank(),
                plot.title = element_text(face = "bold", size = rel(1.2)),
                plot.caption = element_text(face = "italic", size = rel(0.8), 
                                            color = "grey70"),
                axis.text.x = element_text(size=rel(.8)),
                axis.text.y = element_text(size=rel(.8)),
                legend.position = "none",
                plot.title.position = "plot"),
        tooltip="text") 
    } else {
      if (input$measure=="Total Cases") {
        ggplotly(
          ggplot(US_Cases_Grouped, aes(x=Date,y=Cases)) +
          geom_area(aes(fill=current_cases_per_million)) +
          geom_line(color="black") +
          geom_line(aes(text=label_cases)) +  
          scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
          scale_y_continuous(expand = c(0,0),label=comma) +
          scale_fill_viridis_c(option = "plasma", label = comma) + 
          labs(y=NULL,
               x=NULL,
               title=paste("Cumulative Cases in", entry, sep=" "),
               caption = "Plot: @jakepscott2020 | Data: New York Times") +
          theme_bw(base_family = "Source Sans Pro",base_size = 16) +
          theme(panel.grid = element_blank(),
                plot.title = element_text(face = "bold", size = rel(1.2)),
                plot.caption = element_text(face = "italic", size = rel(0.8), 
                                            color = "grey70"),
                axis.text.x = element_text(size=rel(.8)),
                axis.text.y = element_text(size=rel(.8)),
                legend.position = "none",
                plot.title.position = "plot"),
          tooltip="text")
      } else {
        if (input$measure=="Total Deaths") {
          ggplotly(
            ggplot(US_Deaths_Grouped, aes(x=Date,y=Deaths)) +
              geom_area(aes(fill=Current_Deaths_Per_Million)) +
              geom_line(color="black") +
              geom_line(aes(text=label_deaths)) +  
              scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
              scale_y_continuous(expand = c(0,0),label=comma) +
              scale_fill_viridis_c(option = "plasma", label = comma) + 
              labs(y=NULL,
                   x=NULL,
                   title=paste("Cumulative Deaths in", entry, sep=" "),
                   caption = "Plot: @jakepscott2020 | Data: New York Times") +
              theme_bw(base_family = "Source Sans Pro",base_size = 16) +
              theme(panel.grid = element_blank(),
                    plot.title = element_text(face = "bold", size = rel(1.2)),
                    plot.caption = element_text(face = "italic", size = rel(0.8), 
                                                color = "grey70"),
                    axis.text.x = element_text(size=rel(.8)),
                    axis.text.y = element_text(size=rel(.8)),
                    legend.position = "none",
                    plot.title.position = "plot"),
            tooltip="text")
        } else {
          if (input$measure=="Total Deaths Per Million") {
            ggplotly(
              ggplot(US_Deaths_Grouped, aes(x=Date,y=Deaths_Per_Million)) +
                geom_area(aes(fill=Current_Deaths_Per_Million)) +
                geom_line(color="black") +
                geom_line(aes(text=label_deaths_per_million)) +  
                scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
                scale_y_continuous(expand = c(0,0),label=comma) +
                scale_fill_viridis_c(option = "plasma", label = comma) + 
                labs(y=NULL,
                     x=NULL,
                     title=paste("Deaths Per Million Residents in", entry, sep=" "),
                     caption = "Plot: @jakepscott2020 | Data: New York Times") +
                theme_bw(base_family = "Source Sans Pro",base_size = 16) +
                theme(panel.grid = element_blank(),
                      plot.title = element_text(face = "bold", size = rel(1.2)),
                      plot.caption = element_text(face = "italic", size = rel(0.8), 
                                                  color = "grey70"),
                      axis.text.x = element_text(size=rel(.8)),
                      axis.text.y = element_text(size=rel(.8)),
                      legend.position = "none",
                      plot.title.position = "plot"),
              tooltip="text")
          } else {
            if (input$measure=="New Cases") {
              ggplotly(ggplot(US_Cases_Grouped, aes(x=Date,y=New_Cases)) +
                         geom_col(aes(text=label_new_cases,fill=Up_or_Down,color=Up_or_Down), alpha=.7) +
                         geom_line(aes(y=New_Cases_Avg),lwd=1) +
                         scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
                         scale_y_continuous(expand = c(0,0),label = comma) +
                         scale_fill_manual(values = c("#91cf60","grey70","red"), breaks = c("Decreasing", "Steady","Increasing")) +
                         scale_color_manual(values = c("#91cf60","grey70","red"), breaks = c("Decreasing", "Steady","Increasing")) +
                         labs(y=NULL,
                              x=NULL,
                              fill=NULL,
                              title=paste("The Rolling Average of New Cases is", last(US_Cases_Grouped$Up_or_Down), "in", entry, sep=" "),
                              subtitle = "7 day rolling average of new cases",
                              caption = "Plot: @jakepscott2020 | Data: New York Times") +
                         theme_bw(base_family = "Source Sans Pro",base_size = 16) +
                         theme(panel.grid = element_blank(),
                               plot.title = element_text(face = "bold", size = rel(1.2)),
                               plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
                               plot.caption = element_text(face = "italic", size = rel(0.8), 
                                                           color = "grey70"),
                               legend.position = "none",
                               axis.text.x = element_text(size=rel(.8)),
                               axis.text.y = element_text(size=rel(.8)),
                               plot.title.position = "plot"),
                       tooltip = "text")
            } else {
              if (input$measure=="New Cases Per Million") {
                ggplotly(ggplot(US_Cases_Grouped, aes(x=Date,y=New_Cases_Per_Million)) +
                           geom_col(aes(text=label_new_cases_per_million,fill=Up_or_Down,color=Up_or_Down), alpha=.7) +
                           geom_line(aes(y=New_Cases_Per_Million_Avg),lwd=1) +
                           scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
                           scale_y_continuous(expand = c(0,0),label = comma) +
                           scale_fill_manual(values = c("#91cf60","grey70","red"), breaks = c("Decreasing", "Steady","Increasing")) +
                           scale_color_manual(values = c("#91cf60","grey70","red"), breaks = c("Decreasing", "Steady","Increasing")) +
                           labs(y=NULL,
                                x=NULL,
                                fill=NULL,
                                title=paste("The Rolling Average of New Cases is", last(US_Cases_Grouped$Up_or_Down), "in", entry, sep=" "),
                                subtitle = "7 day rolling average of new cases",
                                caption = "Plot: @jakepscott2020 | Data: New York Times") +
                           theme_bw(base_family = "Source Sans Pro",base_size = 16) +
                           theme(panel.grid = element_blank(),
                                 plot.title = element_text(face = "bold", size = rel(1.2)),
                                 plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
                                 plot.caption = element_text(face = "italic", size = rel(0.8), 
                                                             color = "grey70"),
                                 legend.position = "none",
                                 axis.text.x = element_text(size=rel(.8)),
                                 axis.text.y = element_text(size=rel(.8)),
                                 plot.title.position = "plot"),
                         tooltip = "text")
              } else {
                if (input$measure=="New Deaths Per Million") {
                  ggplotly(ggplot(US_Deaths_Grouped, aes(x=Date,y=New_Deaths_Per_Million)) +
                             geom_col(aes(text=label_new_deaths_per_million,fill=Up_or_Down,color=Up_or_Down), alpha=.7) +
                             geom_line(aes(y=New_Deaths_Per_Million_Avg),lwd=1) +
                             scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
                             scale_y_continuous(expand = c(0,0),label = comma) +
                             scale_fill_manual(values = c("#91cf60","grey70","red"), breaks = c("Decreasing", "Steady","Increasing")) +
                             scale_color_manual(values = c("#91cf60","grey70","red"), breaks = c("Decreasing", "Steady","Increasing")) +
                             labs(y=NULL,
                                  x=NULL,
                                  fill=NULL,
                                  title=paste("The Rolling Average of New Deaths Per Million Residents is", last(US_Deaths_Grouped$Up_or_Down), "in", entry, sep=" "),
                                  subtitle = "7 day rolling average of new deaths per million residents",
                                  caption = "Plot: @jakepscott2020 | Data: New York Times") +
                             theme_bw(base_family = "Source Sans Pro",base_size = 16) +
                             theme(panel.grid = element_blank(),
                                   plot.title = element_text(face = "bold", size = rel(1.2)),
                                   plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
                                   plot.caption = element_text(face = "italic", size = rel(0.8), 
                                                               color = "grey70"),
                                   legend.position = "none",
                                   axis.text.x = element_text(size=rel(.8)),
                                   axis.text.y = element_text(size=rel(.8)),
                                   plot.title.position = "plot"),
                           tooltip = "text")
                } else {
                  if (input$measure=="New Deaths") {
                    ggplotly(ggplot(US_Deaths_Grouped, aes(x=Date,y=New_Deaths)) +
                               geom_col(aes(text=label_new_deaths,fill=Up_or_Down,color=Up_or_Down), alpha=.7) +
                               geom_line(aes(y=New_Deaths_Avg),lwd=1) +
                               scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
                               scale_y_continuous(expand = c(0,0),label = comma) +
                               scale_fill_manual(values = c("#91cf60","grey70","red"), breaks = c("Decreasing", "Steady","Increasing")) +
                               scale_color_manual(values = c("#91cf60","grey70","red"), breaks = c("Decreasing", "Steady","Increasing")) +
                               labs(y=NULL,
                                    x=NULL,
                                    fill=NULL,
                                    title=paste("The Rolling Average of New Deaths is", last(US_Deaths_Grouped$Up_or_Down), "in", entry, sep=" "),
                                    subtitle = "7 day rolling average of new deaths",
                                    caption = "Plot: @jakepscott2020 | Data: New York Times") +
                               theme_bw(base_family = "Source Sans Pro",base_size = 16) +
                               theme(panel.grid = element_blank(),
                                     plot.title = element_text(face = "bold", size = rel(1.2)),
                                     plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
                                     plot.caption = element_text(face = "italic", size = rel(0.8), 
                                                                 color = "grey70"),
                                     legend.position = "none",
                                     axis.text.x = element_text(size=rel(.8)),
                                     axis.text.y = element_text(size=rel(.8)),
                                     plot.title.position = "plot"),
                             tooltip = "text")
                  }
                }
              }
            }
          }
        }
      }
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


