library(quantmod)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(hrbrthemes)


master_df <- read.csv("brazil_coronavirus_daily_data.csv")
column_list <- c("cumulative_total_cases","daily_new_cases","active_cases","cumulative_total_deaths","daily_new_deaths")

master_df$X <- NULL

master_df <- master_df %>% drop_na()
master_df$Date <- strptime(master_df$Date, format="%Y-%m-%d")