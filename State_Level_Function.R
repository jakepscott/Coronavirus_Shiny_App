# Loading Libraries--------------------------------------------------------

library(tidycensus)


# Making the Function Itself ----------------------------------------------
state_graph <- function(Data,
                         state="Alabama",
                         measure="New_Cases",
                         per_million=F,
                         rollmean=7,
                         date_min=ymd("2020-03-25"),
                         date_max=Sys.Date()){
  

# Getting True_Measure, adjusts for per million and such ------------------
True_Measure <- if (per_million==T) {
  paste0(measure,"_Per_Million")
} else {
  measure
}
  
  
  # Filtering the data ------------------------------------------------------
  Data <- Data %>% 
    filter(State==state) %>%
      group_by(Date, State) %>%
      summarise(Cases=sum(Cases),
                Deaths=sum(Deaths),
                abb=last(abb),
                Population=last(State_Population)) %>% 
      ungroup() %>%
      mutate(Cases_Per_Million=(Cases/Population)*1000000,
             Deaths_Per_Million=(Deaths/Population)*1000000)
  
  
  # Setting Up The Parameters -----------------------------------------------
  
  ##Rolling Average Line
  line <- case_when(str_detect(True_Measure,"Case")==TRUE~"New_Cases_Avg",
                    str_detect(True_Measure,"Death")==TRUE~"New_Deaths_Avg")
  
  
  # Making the cases and deaths variables -----------------------------------
  Data <- Data %>%
    arrange(Date) %>% 
    mutate(New_Cases=Cases-lag(Cases), ##New cases is today's cases minus yesterdays
           New_Cases=ifelse(New_Cases<0,0,New_Cases),
           New_Cases_Avg=rollmean(New_Cases,k = rollmean,fill = NA, align = "right"),
           New_Cases_Per_Million=(New_Cases/Population)*1000000,
           New_Cases_Per_Million_Avg=(New_Cases_Avg/Population)*1000000,
           
           New_Deaths=Deaths-lag(Deaths), 
           New_Deaths=ifelse(New_Deaths<0,0,New_Deaths),
           New_Deaths_Avg=rollmean(New_Deaths,k = rollmean,fill = NA, align = "right"),
           New_Deaths_Per_Million=(New_Deaths/Population)*1000000,
           New_Deaths_Per_Million_Avg=(New_Deaths_Avg/Population)*1000000)
  
  
  # Getting whether new cases/deaths are increasing or decreasing -----------
  Increasing_or_Decreasing <- 
    Data %>% 
    select(State,Date,New_Cases_Avg, New_Deaths_Avg) %>% 
    mutate(change_in_new_cases=New_Cases_Avg-lag(New_Cases_Avg),
           change_in_new_deaths=New_Deaths_Avg-lag(New_Deaths_Avg),
           Cases_Up_or_Down=case_when(change_in_new_cases>0~"Increasing",
                                      change_in_new_cases<0~"Decreasing",
                                      change_in_new_cases==0~"Steady"),
           Deaths_Up_or_Down=case_when(change_in_new_deaths>0~"Increasing",
                                       change_in_new_deaths<0~"Decreasing",
                                       change_in_new_deaths==0~"Steady")) %>% 
    select(Date,State,change_in_new_cases,change_in_new_deaths,
           Cases_Up_or_Down, Deaths_Up_or_Down)
  
  Data <- left_join(Data,Increasing_or_Decreasing, by=c("Date","State")) 
  

# This will tell ggplot which up or down column to look at ----------------
  Up_or_Down_Measure <- case_when(str_detect(True_Measure,"Case")==TRUE~"Cases_Up_or_Down",
                                  str_detect(True_Measure,"Death")==TRUE~"Deaths_Up_or_Down")
  Up_or_Down_Label <- case_when(str_detect(True_Measure,"Case")==TRUE~
                                  last(Increasing_or_Decreasing$Cases_Up_or_Down),
                                str_detect(True_Measure,"Death")==TRUE~
                                  last(Increasing_or_Decreasing$Deaths_Up_or_Down))
  
  # Making Labels for all the cases/deaths variables ------------------------
  Data <-  Data %>% mutate(Label=glue("There were {value} {measure} reported in {State} on {date_string}",
                                      #This looks like a lot, but is just saying if the measure is per million, round to 2 decimals, otherwise
                                      #round to 0 decimals. In addition, I am adding a comma every three numbers with prettyNum
                                      value = prettyNum(round(get(True_Measure),2),big.mark = ','),
                                      measure = True_Measure %>%
                                        str_replace_all('_',' ') %>% 
                                        str_to_lower(),
                                      #Just getting the date in Month, day, Year format
                                      date_string = glue("{as.character(month(Date, label = T,abbr = F))} {day(Date)}, {year(Date)}")))
  
  
  # Plotting ----------------------------------------------------------------
  if (str_detect(True_Measure,"New")) {
    ggplot(Data, aes(x=Date,y=get(True_Measure))) +
      geom_col(aes(text=Label,
                   fill=get(Up_or_Down_Measure),
                   color=get(Up_or_Down_Measure)), alpha=.7) +
      geom_line(aes_string(y=paste(True_Measure,"_Avg",sep="")),lwd=1) +
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
           title=paste("The Rolling Average of ", str_replace_all(True_Measure,"_"," "), " is " , Up_or_Down_Label, 
                       " in ", state, sep=""),
           subtitle = paste(rollmean,"day rolling average"),
           caption = "Plot: @jakepscott2020 | Data: New York Times") +
      theme_bw(base_family = "Source Sans Pro",base_size = 12) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            plot.title = element_text(face = "bold", size = rel(1.8)),
            plot.subtitle = element_text(face = "plain", size = rel(1.5), color = "grey70"),
            plot.caption = element_text(face = "italic", size = rel(0.8), 
                                        color = "grey70"),
            legend.position = "none",
            axis.text.x = element_text(size=rel(1.5)),
            axis.text.y = element_text(size=rel(1.5)),
            plot.title.position = "plot")
  } else {
    title_var <- case_when(True_Measure=="Cases"~"Cumulative Cases",
                           True_Measure=="Deaths"~"Cumulative Deaths",
                           True_Measure=="Cases_Per_Million"~"Cumulative Cases Per Million Residents",
                           True_Measure=="Deaths_Per_Million"~"Cumulative Deaths Per Million Residents")
    color_var <- case_when(str_detect(True_Measure,"Cases")~"#D41804",
                           str_detect(True_Measure,"Deaths")~"grey20")
    
    ggplot(Data, aes(x=Date,y=get(True_Measure))) +
      geom_area(fill=color_var) +
      geom_line(color="black") +
      geom_line(aes_string(text=paste("Label_",True_Measure,sep=""))) +
      scale_x_date(expand = c(0,0), breaks = pretty_breaks(n=3, min.n=3), guide = guide_axis(check.overlap = T)) +
      scale_y_continuous(expand = c(0,0),label = comma) +
      scale_fill_viridis_c(option = "plasma", label = comma) +
      coord_cartesian(xlim = c(date_min,date_max)) +
      labs(y=NULL,
           x=NULL,
           title=paste(title_var, "in", state, sep=" "),
           caption = "Plot: @jakepscott2020 | Data: New York Times") +
      theme_bw(base_family = "Source Sans Pro",base_size = 12) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            plot.title = element_text(face = "bold", size = rel(1.8)),
            plot.caption = element_text(face = "italic", size = rel(0.8), 
                                        color = "grey70"),
            axis.text.x = element_text(size=rel(1.5)),
            axis.text.y = element_text(size=rel(1.5)),
            legend.position = "none",
            plot.title.position = "plot")
  }
  
  
}

