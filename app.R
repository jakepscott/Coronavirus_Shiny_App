
# Libraries ---------------------------------------------------------------


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
library(shinycssloaders)

source("County_Level_Function.R")
source("Setup.R")


# Setting Theme for Graphs ------------------------------------------------
#Theme for facets
total_facet_theme <- theme(panel.grid = element_blank(),
                     plot.title = element_text(face = "bold", size = rel(2)),
                     plot.caption = element_text(face = "italic", size = rel(0.8), 
                                                 color = "grey70"),
                     plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
                     axis.text.x = element_text(size=rel(.9)),
                     axis.text.y = element_text(size=rel(.9)),
                     legend.title = element_text(size = rel(.9), face="bold"),
                     legend.position = "none",
                     strip.text = element_text(size=rel(.8), face = "bold", margin = margin(.05,0,.05,0, "cm")),
                     strip.background=element_rect(color = "grey70",  fill="grey90"),
                     plot.title.position = "plot")

new_facet_theme <- theme(panel.grid = element_blank(),
                         plot.title = element_markdown(face = "bold", size = rel(2)),
                         plot.subtitle = element_text(face = "plain", size = rel(1.4), color = "grey70"),
                         plot.caption = element_text(face = "italic", size = rel(0.8), 
                                                     color = "grey70"),
                         legend.position = "none",
                         axis.text.x = element_text(size=rel(.9)),
                         axis.text.y = element_text(size=rel(.9)),
                         strip.text = element_text(size=rel(.8), face = "bold", margin = margin(.05,0,.05,0, "cm")),
                         strip.background=element_rect(color = "grey70",  fill="grey90"),
                         plot.title.position = "plot")

state_view_theme <- theme(panel.grid = element_blank(),
                          plot.title = element_text(face = "bold", size = rel(1.2)),
                          plot.caption = element_text(face = "italic", size = rel(0.8), 
                                                      color = "grey70"),
                          axis.text.x = element_text(size=rel(.8)),
                          axis.text.y = element_text(size=rel(.8)),
                          legend.position = "none",
                          plot.title.position = "plot")


# UI ----------------------------------------------------------------------
ui <- fluidPage(
  ##Setting Theme
  theme = shinytheme("journal"),
  # Application title
  navbarPage("Coronavirus in the United States", theme = shinytheme("lumen"),
             
             # UI: New Cases ---------------------------------------------------------------
             tabPanel("New Cases", fluid = TRUE,
                      sidebarLayout(
                        sidebarPanel(dateRangeInput('dateRangeNew',
                                                    label = 'Date range input: yyyy-mm-dd',
                                                    start = "2020-03-10", end = Sys.Date()-1,
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
                          withSpinner(plotOutput("new_cases_by_state", height = 600),type = 6)
                        )
                      )
             ),
             
             #  UI: New Deaths ---------------------------------------------------------
             tabPanel("New Deaths", fluid = TRUE,
                      sidebarLayout(
                        sidebarPanel(dateRangeInput('dateRangeNewDeaths',
                                                    label = 'Date range input: yyyy-mm-dd',
                                                    start = "2020-03-10", end = Sys.Date()-1,
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
                          withSpinner(plotOutput("new_deaths_by_state", height = 600),type = 6)
                        )
                      )
             ),
             
             #  UI: Total Cases --------------------------------------------------------
             tabPanel("Total Cases", fluid = TRUE,
                      sidebarLayout(
                        sidebarPanel(dateRangeInput('dateRangeTotal',
                                                    label = 'Date range input: yyyy-mm-dd',
                                                    start = "2020-03-10", end = Sys.Date()-1,
                                                    min = "2020-01-22", max = Sys.Date()-1),
                                     checkboxInput("PerMil", "Per Million Residents",TRUE)
                        ),
                        mainPanel(
                          withSpinner(plotOutput("cases_by_state", height = 600),type=6)
                        )
                      )
             ),
             
             #  UI: Total Deaths -------------------------------------------------------
             tabPanel("Total Deaths", fluid = TRUE,
                      sidebarLayout(
                        sidebarPanel(dateRangeInput('dateRangeDeaths',
                                                    label = 'Date range input: yyyy-mm-dd',
                                                    start = "2020-03-10", end = Sys.Date()-1,
                                                    min = "2020-01-22", max = Sys.Date()-1),
                                     checkboxInput("PerMilDeaths", "Per Million Residents",TRUE)
                        ),
                        mainPanel(
                          withSpinner(plotOutput("deaths_by_state", height = 600),type=6)
                        )
                      )
             ),
             
             # State View -------------------------------------------------------------
             tabPanel("State View", fluid = TRUE,
                      sidebarLayout(
                        sidebarPanel(selectInput("statename","State", choices=sort(c(state.name,"District of Columbia"))),
                                     selectInput("measure",label = "Measure",choices = c("New Cases", "New Deaths",
                                                                                         "New Cases Per Million","New Deaths Per Million",
                                                                                         "Total Cases", "Total Deaths",
                                                                                         "Total Cases Per Million", "Total Deaths Per Million"),
                                                 selected = "New Cases"),
                                     sliderInput("RollingAverageforstates", 
                                                 label="Window for rolling average",
                                                 min=1,
                                                 max=14,
                                                 value=7,
                                                 step=1),
                                     dateRangeInput('dateRangeStateView',
                                                    label = 'Date range input: yyyy-mm-dd',
                                                    start = "2020-01-22", end = Sys.Date()-1,
                                                    min = "2020-01-22", max = Sys.Date()-1)
                                     
                        ),
                        mainPanel(
                          withSpinner(plotlyOutput("state_view", height = 600),type = 6)
                        )
                      )
             ),
             
             #  UI: County View --------------------------------------------------------
             tabPanel("County View", fluid = TRUE,
                      sidebarLayout(
                        sidebarPanel(selectInput("statenameforcounties","State", choices=state.name),
                                     uiOutput("countySelection"),
                                     selectInput("measureforcounties", "Measure", 
                                                 choices=c("New Cases", 
                                                           "New Deaths",
                                                           "New Cases Per 100k",
                                                           "New Deaths Per 100k"),
                                                 selected = "New Cases"),
                                     sliderInput("RollingAverageforcounties", 
                                                 label="Window for rolling average",
                                                 min=1,
                                                 max=14,
                                                 value=7,
                                                 step=1)
                        ),
                        
                        mainPanel(
                          withSpinner(plotlyOutput("plot",height=600),type = 6)
                        )
                      )
             ),
             navbarMenu("More",icon = icon("info-circle"),
                        tabPanel("About Me",
                                 HTML('<center><img src="ProfileImage.png" width="300" height="300"></center>'),
                                   tags$br(),
                                   tags$br(),
                                   tags$article("My name is Jake Scott. I'm a recent graduate of Colgate University and a
                                                research assistant at the Federal Reserve Board in DC. I am interested 
                                                in data visualization, statistics, and economics, as well as their application 
                                                to public policy. I can be reached at Jakepscott16@gmail.com or via social media.", 
                                                style = "font-size:30px"),
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
                                   ),
                        tabPanel("About the Data",
                                 sidebarPanel(
                                   tags$article("The data used in these figures comes from the New York Times, which is collecting and publically sharing Coronavirus 
                                                data for every county in the United States. The data can be accessed via", tags$a(href="https://github.com/nytimes/covid-19-data", "Github."), 
                                                style = "font-size:20px")
                                 ))
                        )
                        ))


# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  # Total Cases by State --------------------------------------------------
  output$cases_by_state <- renderPlot({
    #Filtering data
    total_cases_data <- US_Grouped
    
    ##Plotting
    if (input$PerMil==TRUE) {
      ggplot(total_cases_data, aes(x=Date,y=cases_per_million)) +
        geom_line() +
        geom_area(aes(fill=current_cases_per_million)) +
        facet_geo(~abb) +
        scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
        scale_y_continuous(expand = c(0,0),label = comma) +
        scale_fill_viridis_c(option = "plasma", label = comma) + 
        coord_cartesian(xlim=c(input$dateRangeTotal[1],input$dateRangeTotal[2])) +
        labs(y=NULL,
             x=NULL,
             title="Cases Per Million Residents",
             caption = "Plot: @jakepscott2020 | Data: New York Times") +
        theme_bw(base_size = 16) +
        total_facet_theme
        
    } else {
      ggplot(total_cases_data, aes(x=Date,y=Cases)) +
        geom_line() +
        geom_area(aes(fill=current_cases)) +
        facet_geo(~abb) +
        scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
        scale_y_continuous(expand = c(0,0), label = comma) +
        scale_fill_viridis_c(option = "plasma", label = comma) + 
        coord_cartesian(xlim=c(input$dateRangeTotal[1],input$dateRangeTotal[2])) +
        labs(y=NULL,
             x=NULL,
             title="Cases",
             caption = "Plot: @jakepscott2020 | Data: New York Times") +
        theme_bw(base_size = 16) +
        total_facet_theme
    }
  })
  
  
  # New Cases By State --------------------------------------------
  output$new_cases_by_state <- renderPlot({
    ##Getting a Rolling Average of New Cases
    new_cases_data <- US_Grouped %>%
      group_by(State) %>%
      mutate(New_Cases_Avg=rollmean(New_Cases,k = input$RollingAverage,fill = NA, align = "right")) %>% # this just gets a k day rolling average
      ungroup() %>%
      mutate(New_Cases_Per_Million_Avg=(New_Cases_Avg/Population)*1000000)
    
    if (input$PerMilNew==TRUE) {
      ##Plotting
      ggplot(new_cases_data, aes(x=Date,y=New_Cases_Per_Million_Avg)) +
        geom_line() +
        facet_geo(~abb) +
        geom_area(aes(fill=Up_or_Down)) +
        scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
        scale_y_continuous(expand = c(0,0)) +
        scale_fill_manual(values = c("#91cf60","grey70","red"), breaks = c("Decreasing", "Steady","Increasing")) +
        coord_cartesian(xlim=c(input$dateRangeNew[1],input$dateRangeNew[2])) +
        labs(y=NULL,
             x=NULL,
             fill=NULL,
             subtitle="Rolling average of new cases per million residents",
             title = "Which states are seeing new cases <span style='color: #91cf60'>**fall**</span> versus <span style='color: red'>**rise**</span>",
             caption = "Plot: @jakepscott2020 | Data: New York Times") +
        theme_bw(base_size = 16) +
        new_facet_theme
    } else {
      ggplot(new_cases_data, aes(x=Date,y=New_Cases_Avg)) +
        geom_line() +
        facet_geo(~abb) +
        geom_area(aes(fill=Up_or_Down)) +
        scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
        scale_y_continuous(expand = c(0,0),label = comma) +
        scale_fill_manual(values = c("#91cf60","grey70","red"), breaks = c("Decreasing", "Steady","Increasing")) +
        coord_cartesian(xlim=c(input$dateRangeNew[1],input$dateRangeNew[2])) +
        labs(y=NULL,
             x=NULL,
             fill=NULL,
             subtitle="Rolling average of new cases",
             title = "Which states are seeing new cases <span style='color: #91cf60'>**fall**</span> versus <span style='color: red'>**rise**</span>",
             caption = "Plot: @jakepscott2020 | Data: New York Times") +
        theme_bw(base_size = 16) +
        new_facet_theme
    }
  })
  
  # Total Deaths by State Plot ------------------------------------
  
  output$deaths_by_state <- renderPlot({
    #Filtering data
    total_deaths_data <- US_Grouped 
    
    ##Plotting
    if (input$PerMilDeaths==TRUE) {
      ggplot(total_deaths_data, aes(x=Date,y=Deaths_Per_Million)) +
        geom_line() +
        geom_area(aes(fill=Current_Deaths_Per_Million)) +
        facet_geo(~abb) +
        scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
        scale_y_continuous(expand = c(0,0),label = comma) +
        scale_fill_viridis_c(option = "plasma", label = comma) + 
        coord_cartesian(xlim=c(input$dateRangeDeaths[1],input$dateRangeDeaths[2])) +
        labs(y=NULL,
             x=NULL,
             title="Deaths Per Million Residents",
             caption = "Plot: @jakepscott2020 | Data: New York Times") +
        theme_bw(base_size = 16) +
        total_facet_theme
      
    } else {
      ggplot(total_deaths_data, aes(x=Date,y=Deaths)) +
        geom_line() +
        geom_area(aes(fill=Current_Deaths)) +
        facet_geo(~abb) +
        scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
        scale_y_continuous(expand = c(0,0),label = comma) +
        scale_fill_viridis_c(option = "plasma", label = comma) + 
        coord_cartesian(xlim=c(input$dateRangeDeaths[1],input$dateRangeDeaths[2])) +
        labs(y=NULL,
             x=NULL,
             title="Total Deaths",
             caption = "Plot: @jakepscott2020 | Data: New York Times") +
        theme_bw(base_size = 16) +
        total_facet_theme
    }
  })
  
  # New Deaths --------------------------------------------------------------
  
  output$new_deaths_by_state <- renderPlot({
    ##Getting a Rolling Average of New deaths
    new_deaths_data <- US_Grouped %>%
      group_by(State) %>%
      mutate(New_Deaths_Avg=rollmean(New_Deaths,k = input$RollingAverageDeaths,fill = NA, align = "right")) %>% # this just gets a 7 day rolling average
      ungroup() %>%
      mutate(New_Deaths_Per_Million_Avg=(New_Deaths_Avg/Population)*1000000)
    
    if (input$PerMilNewDeaths==TRUE) {
      ##Plotting
      ggplot(new_deaths_data, aes(x=Date,y=New_Deaths_Per_Million_Avg)) +
        geom_line() +
        facet_geo(~abb) +
        geom_area(aes(fill=Up_or_Down_Deaths)) +
        scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
        scale_y_continuous(expand = c(0,0),label = comma) +
        scale_fill_manual(values = c("#91cf60","grey70","red"), breaks = c("Decreasing", "Steady","Increasing")) +
        coord_cartesian(xlim=c(input$dateRangeNewDeaths[1],input$dateRangeNewDeaths[2])) +
        labs(y=NULL,
             x=NULL,
             fill=NULL,
             subtitle="Rolling average of new deaths per million residents",
             title = "Which states are seeing new deaths <span style='color: #91cf60'>**fall**</span> versus <span style='color: red'>**rise**</span>",
             caption = "Plot: @jakepscott2020 | Data: New York Times") +
        theme_bw(base_size = 16) +
        new_facet_theme
      
    } else {
      ggplot(new_deaths_data, aes(x=Date,y=New_Deaths_Avg)) +
        geom_line() +
        facet_geo(~abb) +
        geom_area(aes(fill=Up_or_Down_Deaths)) +
        scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
        scale_y_continuous(expand = c(0,0),label = comma) +
        scale_fill_manual(values = c("#91cf60","grey70","red"), breaks = c("Decreasing", "Steady","Increasing")) +
        coord_cartesian(xlim=c(input$dateRangeNewDeaths[1],input$dateRangeNewDeaths[2])) +
        labs(y=NULL,
             x=NULL,
             fill=NULL,
             subtitle="Rolling average of new deaths",
             title = "Which states are seeing new deaths <span style='color: #91cf60'>**fall**</span> versus <span style='color: red'>**rise**</span>",
             caption = "Plot: @jakepscott2020 | Data: New York Times") +
        theme_bw(base_size = 16) +
        new_facet_theme
      
    }
  })
  
  
  # State View --------------------------------------------------------------
  output$state_view <- renderPlotly({
    entry <- input$statename
    #Filtering just the state of interest
    State_Data <- US_Grouped %>%
      select(-Up_or_Down,-Up_or_Down_Deaths) %>% ##Don't want these because it is the same for all entries. For state view I want it colored by day
      filter(State==input$statename) %>% 
      mutate(New_Cases_Avg=rollmean(New_Cases,k = input$RollingAverageforstates,fill = NA, align = "right"),
             New_Deaths_Avg=rollmean(New_Deaths,k = input$RollingAverageforstates,fill = NA, align = "right")) %>% # this just gets a 7 day rolling average
      ungroup() %>%
      mutate(New_Cases_Per_Million_Avg=(New_Cases_Avg/Population)*1000000,
             New_Deaths_Per_Million_Avg=(New_Deaths_Avg/Population)*1000000)
    
    #Getting whether cases are increasing or decreasing each day for selected state
    State_Data <- 
      State_Data %>% 
      group_by(State) %>%
      mutate(change_in_new_cases=New_Cases_Avg-lag(New_Cases_Avg),
             change_in_new_deaths=New_Deaths_Avg-lag(New_Deaths_Avg)) %>% 
      ungroup() %>%
      mutate(Up_or_Down=case_when(change_in_new_cases>0~"Increasing",
                                  change_in_new_cases<0~"Decreasing",
                                  change_in_new_cases==0~"Steady"),
             Up_or_Down_Deaths=case_when(change_in_new_deaths>0~"Increasing",
                                         change_in_new_deaths<0~"Decreasing",
                                         change_in_new_deaths==0~"Steady")) 
    
    #Adding case labels
    State_Data <- State_Data %>% 
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
    
    #Adding death labels
    State_Data <- State_Data %>% 
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
        ggplot(State_Data, aes(x=Date,y=cases_per_million)) +
          geom_area(aes(fill=current_cases_per_million)) +
          geom_line(color="black") +
          geom_line(aes(text=label_cases_per_million)) +
          scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
          scale_y_continuous(expand = c(0,0),label = comma) +
          scale_fill_viridis_c(option = "plasma", label = comma) + 
          coord_cartesian(xlim=c(input$dateRangeStateView[1],input$dateRangeStateView[2])) +
          labs(y=NULL,
               x=NULL,
               title=paste("Cumulative Cases Per Million Residents in", entry, sep=" "),
               caption = "Plot: @jakepscott2020 | Data: New York Times") +
          theme_bw(base_family = "Source Sans Pro",base_size = 16) +
          state_view_theme,
        tooltip="text") 
    } else {
      if (input$measure=="Total Cases") {
        ggplotly(
          ggplot(State_Data, aes(x=Date,y=Cases)) +
            geom_area(aes(fill=current_cases_per_million)) +
            geom_line(color="black") +
            geom_line(aes(text=label_cases)) +  
            scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
            scale_y_continuous(expand = c(0,0),label=comma) +
            scale_fill_viridis_c(option = "plasma", label = comma) + 
            coord_cartesian(xlim=c(input$dateRangeStateView[1],input$dateRangeStateView[2])) +
            labs(y=NULL,
                 x=NULL,
                 title=paste("Cumulative Cases in", entry, sep=" "),
                 caption = "Plot: @jakepscott2020 | Data: New York Times") +
            theme_bw(base_family = "Source Sans Pro",base_size = 16) +
            state_view_theme,
          tooltip="text")
      } else {
        if (input$measure=="Total Deaths") {
          ggplotly(
            ggplot(State_Data, aes(x=Date,y=Deaths)) +
              geom_area(aes(fill=Current_Deaths_Per_Million)) +
              geom_line(color="black") +
              geom_line(aes(text=label_deaths)) +  
              scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
              scale_y_continuous(expand = c(0,0),label=comma) +
              scale_fill_viridis_c(option = "plasma", label = comma) + 
              coord_cartesian(xlim=c(input$dateRangeStateView[1],input$dateRangeStateView[2])) +
              labs(y=NULL,
                   x=NULL,
                   title=paste("Cumulative Deaths in", entry, sep=" "),
                   caption = "Plot: @jakepscott2020 | Data: New York Times") +
              theme_bw(base_family = "Source Sans Pro",base_size = 16) +
              state_view_theme,
            tooltip="text")
        } else {
          if (input$measure=="Total Deaths Per Million") {
            ggplotly(
              ggplot(State_Data, aes(x=Date,y=Deaths_Per_Million)) +
                geom_area(aes(fill=Current_Deaths_Per_Million)) +
                geom_line(color="black") +
                geom_line(aes(text=label_deaths_per_million)) +  
                scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
                scale_y_continuous(expand = c(0,0),label=comma) +
                scale_fill_viridis_c(option = "plasma", label = comma) + 
                coord_cartesian(xlim=c(input$dateRangeStateView[1],input$dateRangeStateView[2])) +
                labs(y=NULL,
                     x=NULL,
                     title=paste("Deaths Per Million Residents in", entry, sep=" "),
                     caption = "Plot: @jakepscott2020 | Data: New York Times") +
                theme_bw(base_family = "Source Sans Pro",base_size = 16) +
                state_view_theme,
              tooltip="text")
          } else {
            if (input$measure=="New Cases") {
              ggplotly(ggplot(State_Data, aes(x=Date,y=New_Cases)) +
                         geom_col(aes(text=label_new_cases,fill=Up_or_Down,color=Up_or_Down), alpha=.7) +
                         geom_line(aes(y=New_Cases_Avg),lwd=1) +
                         scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
                         scale_y_continuous(expand = c(0,0),label = comma) +
                         scale_fill_manual(values = c("#91cf60","grey70","red"), breaks = c("Decreasing", "Steady","Increasing")) +
                         scale_color_manual(values = c("#91cf60","grey70","red"), breaks = c("Decreasing", "Steady","Increasing")) +
                         coord_cartesian(xlim=c(input$dateRangeStateView[1],input$dateRangeStateView[2])) +
                         labs(y=NULL,
                              x=NULL,
                              fill=NULL,
                              title=paste("The Rolling Average of New Cases is", last(State_Data$Up_or_Down), "in", entry, sep=" "),
                              subtitle = "7 day rolling average of new cases",
                              caption = "Plot: @jakepscott2020 | Data: New York Times") +
                         theme_bw(base_family = "Source Sans Pro",base_size = 16) +
                         state_view_theme,
                       tooltip = "text")
            } else {
              if (input$measure=="New Cases Per Million") {
                ggplotly(ggplot(State_Data, aes(x=Date,y=New_Cases_Per_Million)) +
                           geom_col(aes(text=label_new_cases_per_million,fill=Up_or_Down,color=Up_or_Down), alpha=.7) +
                           geom_line(aes(y=New_Cases_Per_Million_Avg),lwd=1) +
                           scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
                           scale_y_continuous(expand = c(0,0),label = comma) +
                           scale_fill_manual(values = c("#91cf60","grey70","red"), breaks = c("Decreasing", "Steady","Increasing")) +
                           scale_color_manual(values = c("#91cf60","grey70","red"), breaks = c("Decreasing", "Steady","Increasing")) +
                           coord_cartesian(xlim=c(input$dateRangeStateView[1],input$dateRangeStateView[2])) +
                           labs(y=NULL,
                                x=NULL,
                                fill=NULL,
                                title=paste("The Rolling Average of New Cases is", last(State_Data$Up_or_Down), "in", entry, sep=" "),
                                subtitle = "7 day rolling average of new cases",
                                caption = "Plot: @jakepscott2020 | Data: New York Times") +
                           theme_bw(base_family = "Source Sans Pro",base_size = 16) +
                           state_view_theme,
                         tooltip = "text")
              } else {
                if (input$measure=="New Deaths Per Million") {
                  ggplotly(ggplot(State_Data, aes(x=Date,y=New_Deaths_Per_Million)) +
                             geom_col(aes(text=label_new_deaths_per_million,fill=Up_or_Down_Deaths,color=Up_or_Down_Deaths), alpha=.7) +
                             geom_line(aes(y=New_Deaths_Per_Million_Avg),lwd=1) +
                             scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
                             scale_y_continuous(expand = c(0,0),label = comma) +
                             scale_fill_manual(values = c("#91cf60","grey70","red"), breaks = c("Decreasing", "Steady","Increasing")) +
                             scale_color_manual(values = c("#91cf60","grey70","red"), breaks = c("Decreasing", "Steady","Increasing")) +
                             coord_cartesian(xlim=c(input$dateRangeStateView[1],input$dateRangeStateView[2])) +
                             labs(y=NULL,
                                  x=NULL,
                                  fill=NULL,
                                  title=paste("The Rolling Average of New Deaths Per Million Residents is", last(State_Data$Up_or_Down_Deaths), "in", entry, sep=" "),
                                  subtitle = "7 day rolling average of new deaths per million residents",
                                  caption = "Plot: @jakepscott2020 | Data: New York Times") +
                             theme_bw(base_family = "Source Sans Pro",base_size = 16) +
                             state_view_theme,
                           tooltip = "text")
                } else {
                  if (input$measure=="New Deaths") {
                    ggplotly(ggplot(State_Data, aes(x=Date,y=New_Deaths)) +
                               geom_col(aes(text=label_new_deaths,fill=Up_or_Down_Deaths,color=Up_or_Down_Deaths), alpha=.7) +
                               geom_line(aes(y=New_Deaths_Avg),lwd=1) +
                               scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
                               scale_y_continuous(expand = c(0,0),label = comma) +
                               scale_fill_manual(values = c("#91cf60","grey70","red"), breaks = c("Decreasing", "Steady","Increasing")) +
                               scale_color_manual(values = c("#91cf60","grey70","red"), breaks = c("Decreasing", "Steady","Increasing")) +
                               coord_cartesian(xlim=c(input$dateRangeStateView[1],input$dateRangeStateView[2])) +
                               labs(y=NULL,
                                    x=NULL,
                                    fill=NULL,
                                    title=paste("The Rolling Average of New Deaths is", last(State_Data$Up_or_Down_Deaths), "in", entry, sep=" "),
                                    subtitle = "7 day rolling average of new deaths",
                                    caption = "Plot: @jakepscott2020 | Data: New York Times") +
                               theme_bw(base_family = "Source Sans Pro",base_size = 16) +
                               state_view_theme,
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
  
  # County View -------------------------------------------------------------
  output$countySelection <- renderUI({
    counties <- US_Data %>% filter(State==input$statenameforcounties & County!="Unknown") %>% arrange(County)
    selectInput("county", "County", choices = unique(counties$County))
  })
  
  output$plot <- renderPlotly({
    req(input$statenameforcounties!="")
    req(input$county!="")
    ggplotly(county_graph(state = input$statenameforcounties, county = input$county,
                          measure = input$measureforcounties, rollmean = input$RollingAverageforcounties),
             tooltip="text")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


