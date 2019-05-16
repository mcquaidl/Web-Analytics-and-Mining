##Part 3
#before we do any analysis, let's import and clean the data

install.packages("devtools")
devtools::install_github(repo = "maksimhorowitz/nflscrapR")
library(devtools)
library(nflscrapR)
library(XML)
library(RCurl)
library(bitops)
library(googleVis)
library(tidyverse)
library(gridExtra)

#a) Select a league and a season (for example, NBA 2016-2017, or MLB 2018, or any other league and season of your choice),
#selection is NFL

#loading data on each player from the regular season
season.18 <- agg_player_season(2018, Weeks = 16)    #this takes a while to load

names(season.18)    #check the names of the data frame

#b) Load player statistics into R for all players who played in the league in the
#season you selected. This data should include, at a minimum:
#Athlete name
#Team(s) the athlete played for
#At least three meaningful stats (such as points or goals or saves, minutes, assists, etc.)

new.season.18 <- data.frame(Year = season.18$Season, PlayerName = season.18$name, 
                            TeamName = season.18$Team, PassYards = season.18$passyds, PassTDs = season.18$pass.tds, 
                            RushYards = season.18$rushyds, RushTDs = season.18$rushtds, ReceptionYards = season.18$recyds,
                            ReceptionTDs = season.18$rec.tds, Tackles = season.18$tackles, Sacks = season.18$sacks)
new.season.18
grid.table(head(new.season.18, n = 15))    #output for slides

#c) Subset the data from part a to include only players for your favorite team.
#Find the players who are “best” for at least three meaningful stats.

pats.season.18 <- new.season.18[new.season.18$TeamName=='NE',]    #create a subset of the new england patrtiots
pats.season.18 <- pats.season.18[complete.cases(pats.season.18),]    #remove the NA's that were showing at the top
pats.season.18    #check


#find the players who has the most reception yards thus far in the season
rec.yds <- pats.season.18[order(pats.season.18$ReceptionYards, decreasing = TRUE), ]    #order the data by the most receiving yards, descending
rec.yds <- head(rec.yds, n = 5)
rec.yds
grid.table(rec.yds)

#find the players who has the most reception td's thus far in the season
rec.tds <- pats.season.18[order(pats.season.18$ReceptionTDs, decreasing = TRUE), ]    #order the data by the most receiving touchdowns, descending
rec.tds <- head(rec.tds, n = 5)
rec.tds
grid.table(rec.tds)

#find the players who has the most tackles thus far in the season
pats.tackles <- pats.season.18[order(pats.season.18$Tackles, decreasing = TRUE), ]    #order the data by the most tackles, descending
pats.tackles <- head(pats.tackles, n = 5)
pats.tackles
grid.table(pats.tackles)

#find the players who has the most sacks
pats.sacks <- pats.season.18[order(pats.season.18$Sacks, decreasing = TRUE), ]    #order the data by the most tackles, descending
pats.sacks <- head(pats.sacks, n = 5)
pats.sacks
grid.table(pats.sacks)

#find the players who has the most rushing yards
rush.yds <- pats.season.18[order(pats.season.18$RushYards, decreasing = TRUE), ]    #order the data by the most tackles, descending
rush.yds <- head(rush.yds, n = 5)
rush.yds
grid.table(rush.yds)

#d)  Show 5 teams for the season you selected that have the most wins (or ranking
# points) in descending order. You will need to do some web scraping to get this
# information. For example, if you choose the NBA, you can follow landofbasketball.com
# as shown in the modules. Note many leagues are divided into conferences or divisions
# (AFC and NFC, Eastern Conference and Western Conference, AL and NL, etc.). If your
# league is divided, please ensure that you include teams from all conferences and all
# divisions

#establish new web page to specifically get the records of the teams
webpage <-     #create variable for the webpage
  paste0("https://www.pro-football-reference.com/boxscores/",
         "standings.cgi?month=2&day=10&year=2019&league=NFL")
webpage    #check

data1 <- html_table(read_html(webpage))[[1]]    #read the html data into the webpage
data2 <- html_table(read_html(webpage))[[2]]    #read the html data into the webpage
team.record.data <- rbind(data1,data2)    #bind the data together
team.record.data    #check

grid.table(head(total.records, n = 15))    #output for slides

#bucket the teams into different dataframes
afc.east <- team.record.data[2:5, ]    #create afc.east bucket
afc.east #check
afc.north <- team.record.data[7:10, ]    #create afc.north bucket
afc.north #check
afc.south <- team.record.data[12:15, ]    #create afc.south bucket
afc.south #check
afc.west <- team.record.data[17:20, ]    #create afc.west bucket
afc.west #check
nfc.east <- team.record.data[22:25, ]    #create nfc.east bucket
nfc.east #check
nfc.north <- team.record.data[27:30, ]    #create nfc.north bucket
nfc.north #check
nfc.south <- team.record.data[32:35, ]    #create nfc.south bucket
nfc.south #check
nfc.west <- team.record.data[37:40, ]    #create nfc.west bucket
nfc.west #check

#remove the conferences from the entire dataset
total.records <- team.record.data[-c(1, 6, 11, 16, 21, 26, 31, 36), ]

#put the divisions into the dataframe as a column
a.east <- rep("AFC East",length(1:4))
a.north <- c(rep("AFC North",length(1:4)))
a.south <- c(rep("AFC South",length(1:4)))
a.west <- c(rep("AFC West",length(1:4)))
n.east <- c(rep("NFC East",length(1:4)))
n.north <- c(rep("NFC North",length(1:4)))
n.south <- c(rep("NFC South",length(1:4)))
n.west <- c(rep("NFC West",length(1:4)))
division <- c(a.east, a.north, a.south, a.west, n.east, n.north, n.south, n.west)    #put all of the variables into one dataset
data.frame(division)    #put the variables into a dataframe
total.records$Division <- (division)    #add the dataframe to the overall dataset
total.records    #check
grid.table(head(total.records, n= 15))
           

total.records <- data.frame(total.records, stringsAsFactors = FALSE)
#put all of the columns as numeric
total.records$W<- as.numeric(total.records$W)
total.records$L<- as.numeric(total.records$L)
total.records$T<- as.numeric(total.records$T)
total.records$W.L.<- as.numeric(total.records$W.L.)
total.records$Pts<- as.numeric(total.records$Pts)
total.records$PtsO<- as.numeric(total.records$PtsO)
total.records$PtDif<- as.numeric(total.records$PtDif)
total.records$MoV<- as.numeric(total.records$MoV)

total.records <- total.records[order(total.records$W, decreasing = TRUE), ]    #order the teams by the most wins
total.records    #check

top.5.wins <- head(total.records, n=5)    #show the top 5 teams with the most wins
top.5.wins    #check
grid.table(top.5.wins)

# e) Create at least five different data visualizations, each using the principles of
# Module 6, that highlight the strengths and/or weaknesses of the teams and/or the players
# on the teams. Use the data from the part a and/or part b datasets. Explain why these charts
# are relevant.
#rename the columns
colnames(total.records)
total.records <- total.records %>% 
  rename(
    Team = Tm, 
    Wins = W,
    Losses = L,
    Ties = 'T',
    'Win Loss %' = W.L.,
    'Points Scored' = Pts,
    'Points Against' = PtsO,
    'Point Differential' = PtDif,
    'Margin of Victory' = MoV
  )
#points scored vs points scored by opposition

#stacked column chart on points scored
total.records$TotalPoints<- (total.records$`Points Scored` + total.records$`Points Against`)   #create a new column in the data set for total points
total.records    #check
total.pts.sort<- total.records[order(total.records$TotalPoints, decreasing = TRUE), ]    #order the teams by the most points
total.pts.sort<- head(total.pts.sort, n = 10)
total.pts.sort    #check

pts.scored.chart <- gvisColumnChart(total.pts.sort,    #use the new total points sort data set
                                    xvar="Team", 
                                    yvar=c("Points Scored","Points Against"),
                                    options=list(isStacked = TRUE,
                                                 title = 'Points Scored vs. Points Against',
                                                 legend = 'right',
                                                 width=600, height=400
                                    ))
plot(pts.scored.chart)


#wins vs margin of victory
w.pts.dif <- data.frame(Wins = total.records$Wins, 'Margin of Victory' = total.records$`Margin of Victory`)
w.pts.dif.chart <-     #create scatter chart
  gvisScatterChart(w.pts.dif,    #create scatter chart on wins vs margin of victory, insert trendline
                   options=list(
                     legend="none",
                     pointSize=5,
                     pointShape="diamond",
                     title="Wins vs  Margin of Victory", 
                     vAxis="{title:'Margin of Victory'}",
                     hAxis="{title:'Wins in a Season'}", 
                     width=600, height=600,
                   trendlines =  "{0: {
                   color: 'blue',
                   lineWidth: 2,
                   opacity: 0.2}}")
                   )
plot(w.pts.dif.chart)


#wins and losses by division, area chart
w.l.total <- total.records    #create new data variable to not change the original one
w.l.total <- w.l.total %>%    #for the new variable, sum both the overall wins and losses in each diviision
  group_by(Division) %>%
  summarize(Wins = sum(Wins[1:4]),
            Losses = sum(Losses[1:4]))

sum(w.l.total$Losses)    #check work
sum(w.l.total$Wins)    #check work

w.l.total.chart<-    #create area chart
  gvisAreaChart(w.l.total, 
                xvar = "Division", 
              yvar = c("Wins", "Losses"),
              options=list(isStacked=FALSE))
plot(w.l.total.chart)
#review the afc south as they had the biggest win-loss differential
grid.table(afc.south)
mean(total.records$Wins)    #calculating the mean number of wins per team for the report


#Top 15 players with the most tackles, data from part A 
top.tackles.season.18 <- new.season.18[order(new.season.18$Tackles, decreasing = TRUE), ]    #sort data set by tackles
top.tackles.season.18    #check
top.tackles.season.18 <- head(top.tackles.season.18, n = 15)    #only take the top 15
top.tackles.season.18    #check

tackle.bubble <- gvisBubbleChart(top.tackles.season.18, idvar="PlayerName",     #id is player name, size variable is based on number of tackles
                          xvar="Tackles",
                          yvar="Sacks",
                          sizevar="Tackles",
                          options=list(
                            vAxis='{minValue:-1, maxValue:10}',
                            width=1500, height=700))    #put negative on this chart so that you could see the full size of the circle (there are no negative sacks awarded)
plot(tackle.bubble)

#Team with the most passing, rushing & reception td's, data from part b
total.touchdowns <- new.season.18    #create new data variable to not change the original one
total.touchdowns <- total.touchdowns %>%    #for the new variable, sum the categories that we are concerned with
  group_by(TeamName) %>%
  summarize(PassTDs = sum(PassTDs),
            RushTDs = sum(RushTDs)
            )

total.touchdowns$TotalTouchdowns <- (total.touchdowns$PassTDs + total.touchdowns$RushTDs)    #create a new column totaling up the total touchdowns that we are concerned with
total.touchdowns    #check data

total.touchdowns <- total.touchdowns[order(total.touchdowns$TotalTouchdowns, decreasing = TRUE), ]    #sort data set by total touchdowns, descending
total.touchdowns    #check

top.total.touchdowns <- head(total.touchdowns, n = 10)    #only take the top 10
top.total.touchdowns    #check

total.touchdowns.chart <- gvisColumnChart(top.total.touchdowns,    #use the new total points sort data set
                                    xvar="TeamName", 
                                    yvar=c("PassTDs","RushTDs"),
                                    options=list(isStacked = TRUE,
                                                 title = 'Teams With the Most Touchdowns Separated by Category',
                                                 legend = 'right',
                                                 width=1500, height=800
                                    ))
plot(total.touchdowns.chart)

#Total Passing Yards & TD's
names(pass.stats)
pass.stats <- new.season.18    #create new data variable to not change the original one
pass.stats<- data.frame('Player Name' = new.season.18$PlayerName, 'Passing Yards' =new.season.18$PassYards, 'Passing TDs' = new.season.18$PassTDs)    #create new dataframe with name, pass yards and pass tds

pass.stats<- pass.stats[order(pass.stats$Passing.TDs, decreasing = TRUE), ]    #sort data set by total passing td's, descending
pass.stats    #check

top.pass.stats<- head(pass.stats, n = 15)    #only outline the QB's with the highes passing yards
top.pass.stats    #check

pass.bubble <- gvisBubbleChart(top.pass.stats, idvar="Player.Name",     #id is player name, size variable is based on number of pass yards
                                 xvar="Passing.TDs",
                                 yvar="Passing.Yards",
                                 sizevar="Passing.Yards",
                               options=list(
                                 #vAxis='{minValue:1500, maxValue:6000}',
                                 hAxis='{minValue:15, maxValue:55}',
                                 width=1500, height=700))    #plotted the points to look proportionate in the graph
plot(pass.bubble)

# f) Use a mapping function (perhaps gvisGeoChart())to display the location on a
# map the home locations of the last 10 champions of a basketball league of your choice.
# Again, you will need to do some web scraping from landofbasketball.com or website
# appropriate for the league you have chosen.

#get the superbowl data
webpage <-     #create variable for the superbowl webpage
  paste0("https://www.pro-football-reference.com/super-bowl/")
webpage    #check

super.bowl.data <- html_table(read_html(webpage))[[1]]    #read the html data into the webpage
super.bowl.data   #check

#past 15 champions
super.bowl.w <- super.bowl.data[1:15,]    #only display the previous 15 winners
super.bowl.w$Winner    #display the winners

super.bowl.w$WinnerCity<- c('Boston', 'Philadelphia', 'Boston', 'Denver',     #add the winning cities given 'New England Patriots' is not a specific area
                          'Boston', 'Seattle', 'Baltimore', 
                          'New York City', 'Green Bay', 'New Orleans',
                          'Pittsburgh', 'New York City', 'Indianapolis',
                          'Pittsburgh', 'Boston')
super.bowl.w$LatLong<- c('42.3:-71.0', '39.9:-75.1', '42.3:-71.0', '39.7:-104.9',     #add the latitude and longitude data for the winning cities
                          '42.3:-71.0', '47.6:-122.3', '39.2:-76.6', 
                         '40.7:-74.0', '44.5:-88.0', '29.9:-90.0',
                         '40.4:-79.9', '40.7:-74.0', '39.7:-86.1',
                         '40.4:-79.9', '42.3:-71.0')


head(super.bowl.w)    #review the information to make it was inputted correctly
super.bowl.winner.city <-     #create the chart
  gvisGeoChart(super.bowl.w, 
               locationvar="LatLong",
               hovervar = 'WinnerCity' & 'Winner',
               options=list(region="US",
                            displayMode="Markers", 
                            resolution="provinces",
                            width=600, height=600,
                            colorAxis="{colors:['red', 'grey']}"))
plot(super.bowl.winner.city)

table(super.bowl.w$Winner)    #check to see how many superbowl winners per city over the past 15 years


#extra credit
total.records

#create a linear regression model with the factors below looking at explaining wins
my.model<- lm(total.records$Wins ~ total.records$`Points Scored` +  total.records$`Margin of Victory` + total.records$TotalPoints)
summary(my.model)






