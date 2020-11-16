
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
source("State_Level_Function.R")
source("National_Level_Function.R")
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
             # UI: Nation View -------------------------------------------------------------
             tabPanel("National View",icon = icon("fas fa-flag-usa"), fluid = TRUE,
                      sidebarLayout(
                        sidebarPanel(selectInput("nationalmeasure",label = "Measure",choices = c("New Cases"="New_Cases", "New Deaths"="New_Deaths",
                                                                                         "Total Cases"="Cases",
                                                                                         "Total Deaths"="Deaths"),
                                                 selected = "New Cases"),
                                     checkboxInput("PerMilNationView", "Per Million Residents",FALSE),
                                     sliderInput("RollingAveragefornation", 
                                                 label="Window for rolling average",
                                                 min=1,
                                                 max=14,
                                                 value=7,
                                                 step=1),
                                     dateRangeInput('dateRangeNationView',
                                                    label = 'Date range input: yyyy-mm-dd',
                                                    start = "2020-01-22", end = Sys.Date()-1,
                                                    min = "2020-01-22", max = Sys.Date()-1)
                        ),
                        mainPanel(
                          withSpinner(plotlyOutput("nation_view", height = 600),type = 6)
                        )
                      )
             ),
             
             
             # UI: State View -------------------------------------------------------------
             tabPanel("State View", icon = icon("fas fa-building"),fluid = TRUE,
                      sidebarLayout(
                        sidebarPanel(selectInput("statename","State", choices=sort(c(state.name,"District of Columbia"))),
                                     selectInput("statemeasure",label = "Measure",choices = c("New Cases"="New_Cases", "New Deaths"="New_Deaths",
                                                                                              "Total Cases"="Cases",
                                                                                              "Total Deaths"="Deaths"),
                                                 selected = "New Cases"),
                                     checkboxInput("PerMilStateView", "Per Million Residents",FALSE),
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
             
             # UI: County View --------------------------------------------------------
             tabPanel("County View",icon = icon("fas fa-home"), fluid = TRUE,
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
                                                 step=1),
                                     dateRangeInput('dateRangeCountyView',
                                                    label = 'Date range input: yyyy-mm-dd',
                                                    start = "2020-01-22", end = Sys.Date()-1,
                                                    min = "2020-01-22", max = Sys.Date()-1)
                        ),
                        
                        mainPanel(
                          withSpinner(plotlyOutput("countyplot",height=600),type = 6)
                        )
                      )
             ),
             navbarMenu("Maps",icon = icon("fas fa-map"),
                        # UI: New Cases ---------------------------------------------------------------
                        tabPanel("New Cases",fluid = TRUE,
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
                        
                        # UI: New Deaths ---------------------------------------------------------
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
                        
                        # UI: Total Cases --------------------------------------------------------
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
                        
                        # UI: Total Deaths -------------------------------------------------------
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
                        )
                        
             ),
             
             # UI: More tab ------------------------------------------------------------

             
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
  
  # Nation View --------------------------------------------------------------
  output$nation_view <- renderPlotly({
    ggplotly(national_graph(Data = US_Data,
                         measure = input$nationalmeasure,
                         per_million = input$PerMilNationView,
                         rollmean = input$RollingAveragefornation,
                         date_min = input$dateRangeNationView[1],
                         date_max = input$dateRangeNationView[2]),
             tooltip="text")
  })
  # State View --------------------------------------------------------------
  output$state_view <- renderPlotly({
    ggplotly(state_graph(Data = US_Data,
                         state = input$statename,
                         measure = input$statemeasure,
                         per_million = input$PerMilStateView,
                         rollmean = input$RollingAverageforstates,
                         date_min = input$dateRangeStateView[1],
                         date_max = input$dateRangeStateView[2]),
             tooltip="text")
  })
  
  # County View -------------------------------------------------------------
  output$countySelection <- renderUI({
    counties <- US_Data %>% filter(State==input$statenameforcounties & County!="Unknown") %>% arrange(County)
    selectInput("county", "County", choices = unique(counties$County))
  })
  
  output$countyplot <- renderPlotly({
    req(input$statenameforcounties!="")
    req(input$county!="")
    ggplotly(county_graph(state = input$statenameforcounties, 
                          county = input$county,
                          measure = input$measureforcounties, 
                          rollmean = input$RollingAverageforcounties,
                          date_min = input$dateRangeCountyView[1],
                          date_max = input$dateRangeCountyView[2]),
             tooltip="text")
  })
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
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


