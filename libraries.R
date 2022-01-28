# Packages ####
# import et manipulation de donnees
require(dplyr) #v 1.0.6
library(R.utils) #2.10.1
require(ggplot2) #3.3.5
require(tidyr) #1.1.4
library(lubridate) #1.7.10
library(data.table) #1.14.0
library(DT) #0.19
library(forcats) #0.5.1
library(stringr) #1.4.0
library(abind) #1.4-5
options(dplyr.summarise.inform = FALSE)

# table + graph
require(plotly) #4.9.4.1
library(scales) #1.1.1
library(rjson) #0.2.20
library(markdown) #1.1
filter <- dplyr::filter
library(randomcoloR) #1.1.0.1

# Excel
# library(xlsx)
library(writexl) #1.4.0
library(readxl) #1.3.1
library(openxlsx) #4.2.3

#Shiny
library(shinydashboard) #0.7.1
library(shinycssloaders) #1.0.0
library(shinyWidgets) #0.6.1
library(shinydashboardPlus) #2.0.2
library(shiny) #1.6.0
library(shinyjs) #2.0.0