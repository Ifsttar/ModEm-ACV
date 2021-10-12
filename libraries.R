# Packages ####
# import et manipulation de donnees
require(dplyr)
require(ggplot2)
require(tidyr)
library(lubridate)
library(data.table)
library(DT)
library(forcats)
library(stringr)
library(abind)
options(dplyr.summarise.inform = FALSE)

# table + graph
require(plotly)
library(scales)
library(rjson)
library(markdown)
filter <- dplyr::filter
library(randomcoloR)

# Excel
#library(xlsx)
library(writexl)
library(readxl)
library(openxlsx)