# Coronavirus Tracking Application
This repo contains code for a [Shiny app](https://jake-scott.shinyapps.io/Coronavirus_Shiny_App/) I built that, broadly speaking, can be used to explore the reach of the virus across the United States. More specifically, it provides an interactive interface through which users can look at measures such as total cases, new cases, total deaths, and new deaths on the national, state, and county level. Users can specify parameters as they see fit, including how wide the window for the rolling average is, what data range they look at, and whether measures are adjusted for population.  

![image](https://user-images.githubusercontent.com/56490913/88104223-2114ac00-cb70-11ea-8694-4186bffc14f3.png)


## Getting Started

The easiest way to use the web application is to go to the webpage itself, which can be found [here](https://jake-scott.shinyapps.io/CoronaMap/). 

If you want to run or edit the code on your own computer, simply download the code and data from this repo, and run the app.R file. The Cleaning_Data.R file prepares background data such as population numbers and state names/abbreviation, so it is not necessary to run it individually unless you want to make changes. Similarly, the County_Level_Function.R file simply holds the code for a function used in the Shiny app, and does not need to be run individually unless you want to change the function itself. 

### Prerequisites

To run the code on your computer you just need R and the following packages installed and loaded:

```
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
```

### Data Sets and Notable Packages
This app automatically downloads, cleans, and uses Coronavirus data from the New York Times Coronavirus [repo](https://github.com/nytimes/covid-19-data). The population estimates used for per capita adjustments come from the [Census Bureau](https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-total.html). 

## Author

* **Jake Scott** - [Twitter](https://twitter.com/jakepscott2020), [Medium](https://medium.com/@jakepscott16), [LinkedIn](https://www.linkedin.com/in/jacob-scott-689875130/)


