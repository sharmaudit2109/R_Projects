library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(reshape2)

IPL <- IPL_Matches_2008_2020

IPL$date <- as.Date(IPL$date, "%d-%m-%Y")
IPL$Season <- format(IPL$date, "%Y")
table(IPL$Season)
matchesPerSeason <- IPL %>% group_by(Season) %>% summarise(Total = n())

mean(matchesPerSeason$Total)

for (var in matchesPerSeason) {
  print(var[1][1])
  
}

ggplot(matchesPerSeason, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Season)) +
  geom_rect() +
  theme_void()+
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4))

ggplot(matchesPerSeason, aes(x=Season, y = Total, group = 2))+
  ylab("Total Matches Played") +
  geom_point() +
  geom_smooth(color="royalblue4") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


ggplot(matchesPerSeason, aes(x=Season, y = Total)) +
  geom_bar(stat = "identity", 
           fill = c("tan","tomato3","seagreen","goldenrod2",
                    "lightblue4","honeydew4","lightsalmon3","indianred3",
                    "lemonchiffon4","ivory4","sandybrown","coral4",
                    "cornsilk4")) +
  ylab("Total Matches Played") +
  theme_classic()


match = IPL %>% select(team1,team2)
matchesPerTeam = data.frame(Team = unlist(match)) %>% group_by(Team) %>% summarise(Total = n())
shapiro.test(matchesPerTeam$Total)

ggplot(matchesPerTeam) + 
  aes(x = Total, y = Team) +
  geom_bar(stat = "identity", fill = "indianred3") +
  ylab("Teams") +
  xlab("Total matches played") +
  theme_classic() 

matchesWonByEachTeam = IPL %>% group_by(winner) %>% summarise(No_of_matches_won = n())


ggplot(matchesWonByEachTeam) + 
  aes(x = No_of_matches_won, y = winner) +
  geom_bar(stat = "identity", fill = "turquoise4") +
  ylab("Teams") +
  xlab("Total matches won") +
  theme_classic()

tossWin = ifelse(IPL$toss_winner == IPL$winner, "Yes", "No")

tossWinTeamFreq = IPL %>% group_by(toss_winner) %>% summarise(Total = n())

tossWinnerMatchWinner = IPL %>% 
  filter(tossWin == "Yes") %>% 
  group_by(winner) %>% 
  summarise(Toss_Winner = n())

ggplot(tossWinnerMatchWinner) + 
  aes(x = Toss_Winner, y = winner, size = Toss_Winner) +
  geom_point(alpha = 0.6, shape = 20, color = "black") +
  theme_classic() +
  xlab("Total matches won") +
  ylab("Toss Winning Team") +
  scale_size(range = c(1, 12), name="Matches Won")

finalTable <-  merge(tossWinnerMatchWinner, tossLoserMatchWinner, by = "winner")

finalTable <- melt(finalTable,id.vars = 'winner')


ggplot(data = finalTable,aes(x=value,y=winner,fill=variable)) + 
  geom_bar(stat = "identity", position=position_dodge(width = 0.5)) + 
  xlab("Matches Won") + 
  ylab("Teams") +
  scale_fill_manual(values = c("indianred3", "turquoise4"), name = "Toss Outcomes",
                      labels = c("Toss Winner","Toss Loser"))



# ggplot(tossWinnerMatchWinner) + 
#   aes(x = total, y = winner) +
#   geom_bar(stat = "identity") +
#   ylab("Teams") +
#   xlab("Total matches won") +
#   theme_classic()

tossLoserMatchWinner = IPL %>% 
  filter(tossWin == "No") %>% 
  group_by(winner) %>% 
  summarise(Toss_Loser = n())


IPL$loser <- ifelse(IPL$team1 == IPL$winner,IPL$team2,IPL$team1)
tossWin = ifelse(IPL$toss_winner != IPL$winner, "Yes", "No")

tossWinnerMatchLoser = IPL %>% 
  filter(tossWin == "Yes") %>% 
  group_by(loser) %>% 
  summarise(Toss_Winner = n())

tossLoserMatchLoser = IPL %>% 
  filter(tossWin == "No") %>% 
  group_by(loser) %>% 
  summarise(Toss_Loser = n())

loserData <- merge(tossWinnerMatchLoser,tossLoserMatchLoser, by = "loser")
loserData <- melt(loserData,id.vars = 'loser')

ggplot(data = loserData,aes(x=value,y=loser,fill=variable)) + 
  geom_bar(stat = "identity", position=position_dodge(width = 0.5)) + 
  xlab("Matches Lost") + 
  ylab("Teams") +
  scale_fill_manual(values = c("indianred3", "turquoise4"), name = "Toss Outcomes",
                    labels = c("Toss Winner","Toss Loser"))




