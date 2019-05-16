#Larry McQuaid
#4/30/19
#copying over Professor Joner's code to start
rm(list=ls()); cat("\014") # clear all
#set your working directory; You need to modify it to point to your working directory
setwd("/users/larrymcquaid/Downloads/") 

#install.packages("googleVis")
library(googleVis)
library(RCurl)
library(RJSONIO)
library(ggplot2)
library(stringr)
library(rvest)

webpage <- "12months_departures_joiners.json"    #naming the information from the web page
data <- fromJSON(webpage)    #pulling the information from the JSON file


nodes.info <- do.call("rbind", lapply(data$nodes, data.frame))    #put nodes info into a data frame
head(nodes.info)    #check the nodes.info variable that was created

##Part 1
#a) Using the aggregate function, or appropriate code from the tidyverse, compute the data frame for 
##the total players joining each month. Names the columns as Month and Joining.
month.join <- aggregate(nodes.info$joining, by=list(nodes.info$month),     #create the variable using the aggregate function
                FUN = sum)    #function is 'sum' to add the players joined
colnames(month.join) <- c('Month', 'Joining')    #change the column names
month.join    #check

#b)Using the aggregate function, or appropriate code from the tidyverse, compute the data 
#frame for the total players departing each month. Names the columns as Month and Departing.
month.depart<- aggregate(nodes.info$departing, by=list(nodes.info$month),     #create the variable using the aggregate function
                        FUN = sum)    #function is 'sum' to add the players departing
colnames(month.depart) <- c('Month', 'Departing')    #change the column names
month.depart    #check

#c) Merge the two data frames by Month column, using the merge function with the sort option
#as FALSE, or appropriate code from the tidyverse.
month.jd <- merge(month.join, month.depart, by = 'Month', sort = FALSE)    #merge the two data frames
month.jd


#d) Use appropriate data visualization principles to compare the joining and departing data. 
month.jd.chart <- gvisColumnChart(month.jd,   #creation of the chart with the depart and join data
                                  xvar='Month',    #determining of the x axis
                                  yvar=c('Joining', 'Departing'),    #determining bar chart values
                                  options = list( 
                                    legend='top',    #putting the legend above the graph
                                    title= 'Comparing Joining & Departing Players by Month'))    #putting the title in the graph
plot(month.jd.chart)    #open google viz


#e) Use appropriate data visualization principles to explore the trend in departing data.

line.depart <- gvisLineChart(month.depart,    #creation of the line chart with the departure data
                             options = list(
                               legend='right',    #putting the legend on the right side of the chart
                               title= 'Players Departing by Month'    #putting the title on the chart
                             ))
plot(line.depart)    #open google viz

#f) Modify your chart from part (e) in some way that focuses on and/or identifies the months with the 
#least departures (you don’t want to lose players).
 
#find the lowest departure rates
sort(month.depart$Departing)

month.depart %>% 
  mutate(highlight_flag = ifelse(month.depart$Departing <= '1200', T, F)) %>%     #creating a flag for the lowest occurring 
  ggplot(aes(x = month.depart$Month, y= month.depart$Departing)) +
  geom_bar(aes(fill = highlight_flag), stat = 'identity') +    #change the stat to identity to make sure it is picking up the values from the departures 
  labs(x = 'Months'   #labeling the x axis
       ,y = 'Number of Departures'   #labeling the y axis
       ,title = str_c("Number of Player Departures Over Time")   #adding a title 
  ) +
  theme(legend.position = 'none')   #removing the legend


##Part 2
#a) Retrieve the NBA data for the 17-18 season.
# install.packages("SportsAnalytics")
library(SportsAnalytics)
nba17.18 <- fetch_NBAPlayerStatistics("17-18")
#save(nba17.18, file="nba1718.RData")
#load(file="nba1718.RData")
head(nba17.18)    #check the data 

#b) Which player has the best field goal percentage? 
#Require >300 shots made. (why 300? see: https://stats.nba.com/help/statminimums/)
typeof(nba17.18)
data.frame(nba17.18)   #put into dataframe
nba17.18$FieldGoalPercentage <- (nba17.18$FieldGoalsMade/ nba17.18$FieldGoalsAttempted) * 100   #create new column for pct
head(nba17.18)   #check data


bst.fg.pct <- filter(nba17.18, nba17.18$FieldGoalsMade >= 300)    #filter out all players how have not made greater than or equal to 300 field goals
bst.fg.pct <- filter(bst.fg.pct, bst.fg.pct$FieldGoalPercentage== max(bst.fg.pct$FieldGoalPercentage, na.rm = TRUE))   #find the player with the maximum field goal pct
bst.fg.pct    #check the outcome
bst.fg.pct$Name    #output the name of the player with the top field goal pct


#c) Which player has the best free throw percentage? Require >125 shots made.
names(nba17.18)    #check names of the data
nba17.18$FreeThrowPercentage <- (nba17.18$FreeThrowsMade / nba17.18$FreeThrowsAttempted)* 100 #create new column for pct
head(nba17.18)    #check data

bst.ft.pct <- filter(nba17.18, nba17.18$FreeThrowsMade >= 125)    #filter out all players how have not made greater than or equal to 125 free throws 
bst.ft.pct <- filter(bst.ft.pct, bst.ft.pct$FreeThrowPercentage== max(bst.ft.pct$FreeThrowPercentage, na.rm = TRUE))   #find the player with the maximum free throw pct
bst.ft.pct    #check the outcome
bst.ft.pct$Name    #output the name of the player with the top free throw pct


#d) Which player has the best three-point percentage? Require >82 shots made
names(nba17.18)    #check names of the data
nba17.18$ThreePercentage <- (nba17.18$ThreesMade / nba17.18$ThreesAttempted)* 100 #create new column for pct
head(nba17.18)    #check data

bst.3.pct <- filter(nba17.18, nba17.18$ThreesMade >= 82)    #filter out all players how have not made greater than or equal to 82 threes
bst.3.pct <- filter(bst.3.pct, bst.3.pct$ThreePercentage== max(bst.3.pct$ThreePercentage, na.rm = TRUE))   #find the player with the maximum three pct
bst.3.pct    #check the outcome
bst.3.pct$Name    #output the name of the player with the top three pct


#e) Show the top 10 players in terms of TotalPoints, arranged from the highest to lowest.
names(nba17.18)    #check names of the data
total.pts.srt <- nba17.18[order(nba17.18$TotalPoints, decreasing = TRUE) ,]    #order the data frame by total points scored
total.pts.srt    #check to see if this worked
total.pts.srt <- head(total.pts.srt, n = 10)    #only display the top 10 scorers
total.pts.srt    #check work
total.pts.srt$Name    #only display the names

#f) Create five data visualizations of your choice, using good data visualization principles, 
#that highlight interesting elements in this dataset.


#column chart with the total points data
total.pts.chart <- gvisColumnChart(total.pts.srt,   #creation of the chart with the depart and join data
                                  xvar='Name',    #determining of the x axis
                                  yvar=c('TotalPoints'),    #determining bar chart values
                                  options = list( 
                                    legend='top',    #putting the legend above the graph
                                    title= 'Comparing Joining & Departing Players by Month'))    #putting the title in the graph
plot(total.pts.chart)    #open google viz


#stacked column chart on point spread for top scorers
top.scorer.chart <- gvisColumnChart(total.pts.srt,
                          xvar="Name", 
                          yvar=c("FieldGoalsMade","ThreesMade", "FreeThrowsMade"),
                          options=list(isStacked = TRUE,
                          legend = 'top'
                          ))
plot(top.scorer.chart)

#offensive rebounds vs total rebounds scatter plot 
rebounds <- data.frame(OffensiveRebounds = c(nba17.18$OffensiveRebounds), TotalRebounds = c(nba17.18$TotalRebounds))   #create dataframe with offesive and total rebounds

rebounds <- rebounds[order(rebounds$TotalRebounds, decreasing = TRUE) ,]    #sort the rebounds by decreasing
rebounds <- head(rebounds, n=100)    #only display the top 100 reboudns
head(rebounds)           #check

rebounds.chart <-     #create scatter chart
  gvisScatterChart(rebounds,
                   options=list(
                     legend="none",
                    pointSize=5,
                     pointShape="diamond",
                     title="Offensive Rebounds vs. Total Rebounds", 
                     vAxis="{title:'Total Rebounds'}",
                     hAxis="{title:'Offensive Rebounds'}", 
                     width=400, height=400))
plot(rebounds.chart)

#Scatter plot of total minutes played & total points
minutes <- data.frame(MinutesPlayed = c(nba17.18$TotalMinutesPlayed), TotalPoints = c(nba17.18$TotalPoints))    #create dataframe with minutes played and total points

minutes <- minutes[order(minutes$MinutesPlayed, decreasing = TRUE) ,]    #sort by decreasing in minutes played
minutes <- head(minutes, n=100)    #only display the top 100
head(rebounds)           #check

minutes.chart <-     #create scatter plot
  gvisScatterChart(minutes,
                   options=list(
                     legend="none",
                     pointSize=5,
                     pointShape="diamond",
                     title="Minutes vs. Total Points", 
                     vAxis="{title:'Total Points'}",
                     hAxis="{title:'Minutes Played'}", 
                     width=400, height=400))
plot(minutes.chart)


#find the teams the top scorers are on
total.pts.tm <- nba17.18[order(nba17.18$TotalPoints, decreasing = TRUE) ,]    #order the data frame by total points scored
total.pts.tm   #check to see if this worked
total.pts.tm <- head(total.pts.tm, n = 100)    #only display the top 100 scorers
total.pts.tm    #check work
total.pts.tm <- table(total.pts.tm$Team)    #buckets the players with the most points into the teams
total.pts.tm <- sort(total.pts.tm)    #sort the teams with the least being first
total.pts.tm <- data.frame(total.pts.tm)   #put the data in a dataframe so it can be put into a graph

total.pts.tm.chart <- gvisColumnChart(total.pts.tm,
                       xvar="Var1", 
                       yvar=c("Freq"),
                       options=list(legend='top'))
plot(total.pts.tm.chart)

##Part 3
#before we do any analysis, let's import and clean the data
webpage <- 
  paste0("https://www.landofbasketball.com/",
         "championships/year_by_year.htm")
webpage

data1 <- html_table(read_html(webpage))[[1]]
data2 <- html_table(read_html(webpage))[[2]]
data <- rbind(data1,data2)


data[1:2, ] # note includes header row, we want to take this out
data <- data.frame(Input=data[-1,],stringsAsFactors = FALSE)    #subtract the first header row, and ensuring strings are not factors
data$Input[1]   #naming data as input
data$Split <- strsplit(data$Input, split='\r\n|\t')    #anywhere there is a new line or tab, split into a vector & remove \ characters
data$Split    #display split column

#str_trim removes the  whitespaces before and after
data$Year <- sapply(data$Split, FUN = function (x)    #creating a function to take the 3rd thru 7th characters for year
  substr(x[1], 3, 7))
data$Winner <- sapply(data$Split, FUN = function (x)    #creating a functon to gather the team who won, which is in the 7th element
  str_trim(x[7]))
data$Series <- sapply(data$Split, FUN = function (x)    #creating a functon to gather the series score, which is in the 11th element
  str_trim(x[11]))
data$Opponent <- sapply(data$Split, FUN = function (x)    #creating a function to gather who lost the series, which is the 13th element
  str_trim(x[13]))
#data$Split <- NULL    #decided not to remove as we need this for question d's mvp data
data$Input <- NULL    #decided to remove the input data
dim(data)    #look at the dimensions of the new data
head(data)    #look at the head for the data

#a) How many times was the series swept, i.e., decided by the series score 4-0?
table.series <- table(data$Series)    #create a table on the series data
table.series #check
swept.series <- table.series['4-0']    #specifically lookup the information for when a series was swept
swept.series    #display data

#b) How many times was the series decided by game 7?  (Series score 4-3)
game.7 <- table.series['4-3']    #lookup the frequency for how many times a series was won 4-3
game.7    #display data

#c) Show 5 teams that have the most wins in descending order.
table.wins <- table(data$Winner)    #create a table of the winners  not accounting for teams that have moved
table.wins    #check
table.wins <- sort(table.wins, decreasing = TRUE)    #sort the winners by descending value
table.wins <- table.wins[1:5]   #refine variable to only hold the teams with the top 5 wins
table.wins    #check

#d)Create a subset of the lecture data frame with championship data from the last championship to the 1968 season. 
#Using the split data column from the lecture example, add a new column showing the Finals MVP. Show the players who 
#won the FinalsMVP award more than once. Note some players have won the MVP while playing for different teams 
#(they were traded/moved to a new city). Find a way to count all of the MVP awards for each player, no matter which 
#team(s) the player was playing for.
data.sub <- data[1:50,]    #create a subset of the data encapsulating 68-69 to present day championships
data.sub    #check data

data.sub$MVP <- sapply(data.sub$Split, FUN = function (x)    #creating a function to insert the finals mvp column into the data subset
  str_trim(x[22]))
data.sub    #check data

finals.mvp <- sort(table(data.sub$MVP), decreasing = TRUE)    #create a table with all of the finals mvp's and sort descending
finals.mvp    #check

finals.mvp <- finals.mvp[finals.mvp >= 2]    #only display the finals mvp's who have won it more than once
finals.mvp    #check


