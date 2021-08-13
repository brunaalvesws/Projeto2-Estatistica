library(quantmod)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyverse)
library(lubridate)

master_df <- read.csv('world_time_series.csv')
column_list <- c('Doses Administradas', 'Doses por 1000 habitantes', 'População completamente vacinada')

master_df$X <- NULL

master_df <- master_df %>% drop_na()
master_df$Date <- strptime(master_df$Date, format="%Y-%m-%d %H:%M:%S")