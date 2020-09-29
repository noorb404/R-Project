# Noor bakrieh  -  318586302 
# Mohammad shihada  -  318397841
library(dplyr)
library(lubridate)
library(data.table)
library(ggplot2)


dataTable1 = fread("C:/Users/zohar/Desktop/d/2015-2016.csv")
dataTable2= fread("C:/Users/zohar/Desktop/d/2016-2017.csv")
dataTable3 = fread("C:/Users/zohar/Desktop/d/2017-2018.csv")
dataTable4 = fread("C:/Users/zohar/Desktop/d/2018-2019.csv")

#Getting Data all together in one TABLE
dataTablet1=rbind(dataTable1,dataTable2)
dataTablet2=rbind(dataTablet1,dataTable3)
dataTable=rbind(dataTablet2,dataTable4)
dataTable.temp = data.table(dataTable)


# CLEANING NOT USED COLUMS
dataTable.temp = subset(dataTable, select = -c(HTHG,HTR,Referee,HS,AS,HTAG,HST,AST,HF,AF,HC,AC,HY,AY,HR,AR))



#GAME THAT HAVE THE HIGHEST NUMBER OF GOALS
GameLocation=which.max(dataTable.temp$FTAG + dataTable.temp$FTHG)
print('Highest number of goals in all games is:')
print(paste0(max(dataTable.temp$FTHG + dataTable.temp$FTAG)," Goals ->",dataTable.temp[GameLocation,2]," VS ",dataTable.temp[GameLocation,3],"  ",dataTable.temp[GameLocation,4],"-",dataTable.temp[GameLocation,5],", ",dataTable.temp[GameLocation,1]))



#GAME THAT HAVE HIGHEST SCORE OF ALL GAMES
print("Highest Score in all games is:")
GameLocation=which.max(abs(dataTable.temp$FTAG - dataTable.temp$FTHG))
print(paste0(dataTable.temp[GameLocation,2]," VS ",dataTable.temp[GameLocation,3],"  ",dataTable.temp[GameLocation,4],"-",dataTable.temp[GameLocation,5] ,", ",dataTable.temp[GameLocation,1]))



#TEAM THAT HAVE MOST DEFEAT GOALS - Making two temp tables for the AWAY and HOME teams
tempTable=aggregate(dataTable.temp$FTHG, by=list(Category=dataTable.temp$AwayTeam), FUN=sum)
tempTable2=aggregate(dataTable.temp$FTAG, by=list(Category=dataTable.temp$HomeTeam), FUN=sum)
tempTable3=rbind(tempTable,tempTable2)
tempTable=aggregate(tempTable3$x, by=list(Category=tempTable3$Category), FUN=sum)
maxLocation=which.max(tempTable$x)
print(paste0("Team with maximum defeat goals : ",tempTable[maxLocation,1]," -> ",tempTable[maxLocation,2] , " Goals"))



#TEAM THAT HAVE MOST WINNING GOALS - Making two temp tables for the AWAY and HOME teams
tempTable=aggregate(dataTable.temp$FTAG, by=list(Category=dataTable.temp$AwayTeam), FUN=sum)
tempTable2=aggregate(dataTable.temp$FTHG, by=list(Category=dataTable.temp$HomeTeam), FUN=sum)
tempTable3=rbind(tempTable,tempTable2)
tempTable=aggregate(tempTable3$x, by=list(Category=tempTable3$Category), FUN=sum)
maxLocation=which.max(tempTable$x)
print(paste0("Team with maximum winning goals : ",tempTable[maxLocation,1]," -> ",tempTable[maxLocation,2] , " Goals"))



#TEAM THAT HAVE MAX LOSES  - Making two temp tables for the AWAY and HOME teams
tempTable=aggregate(dataTable.temp$FTR=='H', by=list(Category=dataTable.temp$AwayTeam), FUN=sum)
tempTable2=aggregate(dataTable.temp$FTR=='A', by=list(Category=dataTable.temp$HomeTeam), FUN=sum)
tempTable3=rbind(tempTable,tempTable2)
tempTable=aggregate(tempTable3$x, by=list(Category=tempTable3$Category), FUN=sum)
maxLocation=which.max(tempTable$x)
print(paste0("Team with maximum defeat Games : ",tempTable[maxLocation,1]," -> ",tempTable[maxLocation,2] , " Game loss"))




#TEAM THAT HAVE MAX WINS  - Making two temp tables for the AWAY and HOME teams
tempTable=aggregate(dataTable.temp$FTR=='H', by=list(Category=dataTable.temp$HomeTeam), FUN=sum)
tempTable2=aggregate(dataTable.temp$FTR=='A', by=list(Category=dataTable.temp$AwayTeam), FUN=sum)
tempTable3=rbind(tempTable,tempTable2)
tempTable=aggregate(tempTable3$x, by=list(Category=tempTable3$Category), FUN=sum)
maxLocation=which.max(tempTable$x)
print(paste0("Team with maximum win Games : ",tempTable[maxLocation,1]," -> ",tempTable[maxLocation,2] , " Game Win"))

#CREATE THE MONTH COLUMN




#THE NUMBER OF THE HOME GOALS IN EVERY MONTH FOR THE 4 SEASONS
dataTable.temp %>%
  ggplot(aes(x = month(Date), y = FTHG )) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Home Goals Per Month",
       y = "Goals",
       x = "Month") 

#THE NUMBER OF THE AWAY GOALS IN EVERY MONTH FOR THE 4 SEASONS
dataTable.temp %>%
  ggplot(aes(x = month(Date), y = FTAG )) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Away Goals Per Month",
       y = "Goals",
       x = "Month") 

#THE NUMBER OF ALL GOALS IN EVERY MONTH FOR THE 4 SEASONS
dataTable.temp %>%
  ggplot(aes(x = month(Date), y = FTHG + FTAG )) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "All Goals Per Month",
       y = "Goals",
       x = "Month") 


#RATE OF TIE GAMES
print(paste0("The Rate of games tie : " , mean(dataTable.temp$FTR == 'D'), "%"))


#PLOT THE RATE OF THE TIE GAMES IN EVERY MONTH FOR THE 4 SEASONS
dataTable.D = dataTable.temp
dataTable.D[,D:=sum(FTR=="D"),by=month(Date)]
dataTable.D[,games_in_month:= .N ,by=month(Date)]
dataTable.D %>%
  ggplot(aes(x = month(Date), y = D/games_in_month )) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Rate Of Tie",
       y = "Rate",
       x = "Month") 


#PREMIER LEAGUE STANDINGS FOR LAST 4 SEASONS
dt.home = dataTable.temp
dt.away = dataTable.temp

dt.home[,wins:=sum(FTHG>FTAG),by=HomeTeam]
dt.home[,loses:=sum(FTHG<FTAG),by=HomeTeam]
dt.home[,ties:=sum(FTHG==FTAG),by=HomeTeam]
dt.home[,goals_for:=sum(FTHG),by=HomeTeam]
dt.home[,goals_against:=sum(FTAG),by=HomeTeam]
dt.home=unique(arrange(select(dt.home,c(HomeTeam,wins,loses,ties,goals_for,goals_against)),HomeTeam))

dt.away[,wins:=sum(FTHG<FTAG),by=AwayTeam]
dt.away[,loses:=sum(FTHG>FTAG),by=AwayTeam]
dt.away[,ties:=sum(FTHG==FTAG),by=AwayTeam]
dt.away[,goals_for:=sum(FTHG),by=AwayTeam]
dt.away[,goals_against:=sum(FTHG),by=AwayTeam]
dt.away=unique(arrange(select(dt.away,c(AwayTeam,wins,loses,ties,goals_for,goals_against)),AwayTeam))

dt.leage = data.table(team=dt.home$HomeTeam,wins=dt.home$wins+dt.away$wins,loses=dt.home$loses+dt.away$loses,ties=dt.home$ties+dt.away$ties
                      ,goals_for=dt.home$goals_for+dt.away$goals_for,goals_against=dt.home$goals_against+dt.away$goals_against)
dt.leage[,points:={3*wins+ties}]
dt.leage=arrange(dt.leage,desc(goals_for-goals_against))
dt.leage=arrange(dt.leage,desc(points))

print(dt.leage)
