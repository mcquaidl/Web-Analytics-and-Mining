#Larry McQuaid
#MET CS 688
#Due: 4/2/19

#google analytics install
#install.packages("googleAnalyticsR")

"""
library(googleAnalyticsR)
library(ggplot2)

?google_analytics_4

rm(list=ls()); cat("\014") #clear workspace & console

ga_auth() #authorize googleAnalyticsR to access your data
my_accounts <- ga_account_list()
class(my_accounts)
is.data.frame(my_accounts)
dim(my_accounts)
my_accounts$accountName
"""

install.packages(c("quantmod", "lubridate"))
library(quantmod) #stock price functions
library(lubridate) #lubridate make math involving dates easier


PGdiv <- getDividends("PG", auto.assign = F)
PGdiv
hw2.function <- function(){
  
  
  return()
}

