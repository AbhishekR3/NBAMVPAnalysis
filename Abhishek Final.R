bsds <- read.csv(file="C:/Users/Abhishek Ramesh/Google Drive/1USF/Fall 2018/BSDS 100/Final Project/bsds.csv")

#4. What school has produced the most MVPs? How many has it produced?
summary(bsds$College)
#Based on this data, the college which produced the most number of players is University of California, Los Angeles.

#7. Do MVPs tend to have a high assist average?
mean(bsds$AST)

# Currently in the 2018-2019 season, only 20 out of 494 players have an average above this.
# This gives 4% of players scoring above the MVP average,
# yet still includes MVPs such as James Harden and Russel Westbrook.
# which proves MVPs tend to have higher assist averages.

#13. What teams produce the most MVPs? Why do you think this is?
summary(bsds$Tm)
#As shown in the data below, the Lakers have the most MVPs.
# A reason could be the Lakers have more unique players who have earned MVP, which helps in securing it over the years.
# While a single player may be very good for a couple of years, such as Michael Jordan who won MVP 5 times,
# the more number of players who are good can gaurantee MVPs over the years.
# Lakers have the most unique MVPS players with 4. 

#11. Across each year, how have MVP trends changed?
# Consider changes in regular season success, playoff success, position, seeding, height, and statistical measures
library(ggplot2)

#ASSIST
p <- ggplot(bsds, aes(x = Season, y = AST, colour = heat.colors(1)))+
  geom_col() + ggtitle("MVP Players Assists Per Game") + theme(plot.title = element_text(hjust = 0.5))
p + labs(x = "Season") + labs(y = "Assist")

#The number of assists per game by the MVP has generally icnreased from the beginning
#with minor spikes by Magic Johnson in the 90s, but generally the assist overall
#from various MVP Players has increased.

#AGE
p <- ggplot(bsds, aes(x = Season, y = Age, colour = heat.colors(1)))+
  geom_col() + ggtitle("MVP Players Age") + theme(plot.title = element_text(hjust = 0.5))
p + labs(x = "Season") + labs(y = "Age")

#The general age of MVP players is usually below 30 across the years, with the exception of
#3 years from '97 to '99.
#This could be well linked to below 30 players have a good mix 
#between their experience and physicall fitness.

#TEAM PLAYED
p <- ggplot(bsds, aes(x = Season, y = Tm, colour = heat.colors(1)))+
  geom_boxplot() + ggtitle("MVP Players Team") + theme(plot.title = element_text(hjust = 0.5))
p + labs(x = "Season") + labs(y = "Team")
#The teams with MVP have varied across the years, while some teams such as the Lakers and Rockets
#have had a few years of continous success, genearlly the MVP has changed across teams.


#GAME
p <- ggplot(bsds, aes(x = Season, y = G, colour = heat.colors(1)))+
  geom_col() + ggtitle("MVP Players Game Played") + theme(plot.title = element_text(hjust = 0.5))
p + labs(x = "Season") + labs(y = "Games")
#MVPs have generally played nearly every game in the basketball season;
#because they're at their peak knowledge of the game,
#and the coaches want to use that to their advantage.
#There are a few minor exceptiions to this where players have played around 60 games,
#but these exceptions are very few.


#MINUTES PLAYED
p <- ggplot(bsds, aes(x = Season, y = MP, colour = heat.colors(1)))+
  geom_col() + ggtitle("MVP Players Minutes Played Per Game") + theme(plot.title = element_text(hjust = 0.5))
p + labs(x = "Season") + labs(y = "Minutes Played")
#Like the number of games played, the players would usually be giving their peak performance.
#Coaches would use the players to get the most out of them by allowing them to play nearly the whole game.
mean(bsds$MP)
#The MVP players generally play around 37.8 minutes.


#POINTS SCORED
p <- ggplot(bsds, aes(x = Season, y = PTS, colour = heat.colors(1)))+
  geom_col() + ggtitle("MVP Players Points Scored Per Game") + theme(plot.title = element_text(hjust = 0.5))
p + labs(x = "Season") + labs(y = "Points")
#MVPs generally have a high scoring per game, which can be seen by the graph.
mean(bsds$PTS)
#Players generally have around 26.5 points per game with the recent years having more than that.
#There could be an emerging trend that the recent MVPs are scoring more.

#REBOUNDS
p <- ggplot(bsds, aes(x = Season, y = TRB, colour = heat.colors(1)))+
  geom_col() + ggtitle("MVP Players Rebounds Per Game") + theme(plot.title = element_text(hjust = 0.5))
p + labs(x = "Season") + labs(y = "Rebounds")
#The number of rebounds per game for MVPs have generally decreased.
#This could be a link towards the number of points scored per game.
#MVPs maybe be scoring more by themselves rather than passing it other players on the team.

#STEAL
p <- ggplot(bsds, aes(x = Season, y = STL, colour = heat.colors(1)))+
  geom_col() + ggtitle("MVP Players Steal Per Game") + theme(plot.title = element_text(hjust = 0.5))
p + labs(x = "Season") + labs(y = "Steal")
#The number of steals have generally increased till the late 90s, but decreased during the early 2000s.
#While this has occured, the trend for increasing the number of steals has increased in recent years.

#BLOCKS
p <- ggplot(bsds, aes(x = Season, y = BLK, colour = heat.colors(1)))+
  geom_col() + ggtitle("MVP Players Blocks Per Game") + theme(plot.title = element_text(hjust = 0.5))
p + labs(x = "Season") + labs(y = "Blocks")
#The number of blocks by an MVP per game has significantly decreased.
#As seen above, the blocks of recent years are below the mean of blocks over the past seasons.

#FIELD GOALS PERCENTAGE
p <- ggplot(bsds, aes(x = Season, y = FG., colour = heat.colors(1)))+
  geom_col() + ggtitle("MVP Players Field Goals Percentage") + theme(plot.title = element_text(hjust = 0.5))
p + labs(x = "Season") + labs(y = "Field Goals Percentage")
#The field goal percentage remains nearly constant with a minor towards the current years.

#3 POINT PERCENTAGE
p <- ggplot(bsds, aes(x = Season, y = X3P., colour = heat.colors(1)))+
  geom_col()+ ggtitle("MVP Players 3 Point Percentage") + theme(plot.title = element_text(hjust = 0.5))
p + labs(x = "Season") + labs(y = "3 Point Percentage")
#As seen in the graph the field goal percentage has increased overall with more MVPs actually shooting 3s.
#(Note: 3s only started being recorded during the 79-80 season)

#FREE THROW
p <- ggplot(bsds, aes(x = Season, y = FT., colour = heat.colors(1)))+
  geom_col() + ggtitle("MVP Players Free Throw Percentage") + theme(plot.title = element_text(hjust = 0.5))
p + labs(x = "Season") + labs(y = "Free Throw Percentage")
#The free throw percentage has generally increased with an exception during 2000.

#WIN SHARES
p <- ggplot(bsds, aes(x = Season, y = WS, colour = heat.colors(1)))+
  geom_col() + ggtitle("MVP Players Win Shares") + theme(plot.title = element_text(hjust = 0.5))
p + labs(x = "Season") + labs(y = "Win Shares")
#The win shares had an increasing graph initially, but then started to fluctuate after 2000.
#The graph has increased genearlly till 2015 then decreased slowly agian.
#This could indicate that the MVP doesn't have as big of a role in the team as they used to.
#The weight of the team could be more spread out.

#WIN SHARES 48
p <- ggplot(bsds, aes(x = Season, y = WS.48, colour = heat.colors(1)))+
  geom_col() + ggtitle("MVP Players Win Share Per Minute") + theme(plot.title = element_text(hjust = 0.5))
p + labs(x = "Season") + labs(y = "Win Share Per Minute")
#The win shares per minute per game has generally increased with a few exception of strict decrease.


#POSITION
p <- ggplot(bsds, aes(x = Season, y = MVP.Position, colour = heat.colors(1)))+
  geom_col() + ggtitle("MVP Players Position Played") + theme(plot.title = element_text(hjust = 0.5))
p + labs(x = "Season") + labs(y = "Position Played")
#As seen by the graph, the position played by MVPs were generally centers in the early years.
#It changed to guards for 5 of the early years,because Jordan and Johnson were guards and both had MVP titles interchanging.
#Recently it has changed to mostly guards and forwards.

#All the nationalities of the players are American, except for 3 players: Hakeem Olajuwon, Dirk Nowitzki, and Steve Nash.
#These players attended College in America except for Dirk Nowitzki.
#It could be said that, since all the players, except Dirk Nowitzki, are American or attended American Colleges,
#There does appear to be a link to help them become a better player with understanding basketball in the NBA.


#Height
p <- ggplot(bsds, aes(x = Season, y = MVP.Height..ft., colour = heat.colors(1)))+
  geom_col() + ggtitle("MVP Players Position Played") + theme(plot.title = element_text(hjust = 0.5))
p + labs(x = "Season") + labs(y = "Heigh in Feet")
#While the height of MVPs do stay above 6 over the years,
#there is a visible slight decrease over the years as most players are falling below the mean height.

#Finals Finish, Win Percentage, and Seeding are highly fluctuating throughout the years.
#This is probably due to the fact basketball is a team game, and only if the team is 
#overall successful and have a good coach is could succeed.
#The success of a team isn't entirely dependant on one player, such as the MVP.
