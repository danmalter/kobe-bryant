library(dplyr)
library(fields)
library(spacetime)
library(rgeos)
library(sp)
library(SimilarityMeasures)
library(jsonlite)
#install.packages("remotes")
remotes::install_github("imadmali/NBAsportvu")
source("~/GitHub/kobe-bryant/_functions.R")
#source("_function_fullcourt.R")

all.movements <- sportvu_convert_json("~/GitHub/kobe-bryant/data/12.02.2015.LAL.at.WAS.json")

event_df <- all.movements %>% 
  dplyr::arrange(quarter,desc(game_clock),x_loc) %>% 
  filter(event.id==3)

df_ball <- all.movements %>% 
  filter(player_id == 977) %>% 
  filter (shot_clock != 24)  #Remove some extra spillover data from the next event

#Plot ball movement
fullcourt() + 
  geom_point(data=df_ball,aes(x=x_loc,y=y_loc), color='purple', size = .1)


### Extract Movement for Kobe ###
##Extract all data for event ID 303
id2 <- all.movements[which(all.movements$event.id == 4),]
##Extract all data for Kobe on event ID #2
bryant <- all.movements[which(all.movements$lastname == "Bryant" & all.movements$event.id == 2),]

library(plotly)
p <- plot_ly(data = bryant, x = ~x_loc, y = ~y_loc, mode = "markers", color=cut(bryant$game_clock, breaks=3)) %>% 
  layout(xaxis = list(range = c(0, 100)), 
         yaxis = list(range = c(0, 50))) 
p

travelDist(bryant$x_loc, bryant$y_loc)

### Distance for All Players ###
deduped.data <- unique( all.movements[ , 1:12 ] )  ##This takes about 30 seconds to run
player.groups <- group_by(deduped.data, lastname)
dist.traveled.players <- summarise(player.groups, totalDist=travelDist(x_loc,y_loc),playerid = max(player_id))
total <- arrange(dist.traveled.players, desc(totalDist))
total


### Distance between Kobe and Ball ###
#Get Clock Info
clockinfo <- get_game_clock("Bryant",580)
#Get Distance
playerdistance <- player_dist("Bryant","ball",580)
#Plot
plot_ly(data = clockinfo, x=~game_clock, y=~playerdistance,mode = "markers")


bryant <- all.movements[which((all.movements$lastname == "Bryant" | all.movements$lastname == "ball") & (all.movements$event.id >=0 & all.movements$event.id <= 30)),]
#Get distance for each player/ball
distgino <- bryant %>% filter (lastname=="Bryant") %>% select (x_loc,y_loc) 
distball <- bryant %>% filter (lastname=="ball") %>% select (x_loc,y_loc) 
distlength <- 1:nrow(distgino)
#Use the R function dist for calculating distance
distsdf <- unlist(lapply(distlength,function(x) {dist(rbind(distgino[x,], distball[x,]))}))
#Add the game_clock
ball_distance <- bryant %>% filter (lastname=="ball") %>% select (game_clock) %>% mutate(distance=distsdf)
plot_ly(data = ball_distance, x=~game_clock, y=~distsdf,mode = "markers") %>%
  layout(title = 'Kobe Distance From Ball - 12/2/2015 Lakers @ Wizards',
         yaxis = list(zeroline = FALSE),
         xaxis = list(zeroline = FALSE))

ggplot() + 
  # We use a different alpha value for jump shots to improve the visualization
  geom_point(data=ball_distance,
             aes(x=-game_clock, y=distsdf, color = distsdf), alpha=0.8) +
  scale_x_continuous(breaks = seq(-720, -512, 20), lim = c(-720, -512)) +
  labs(title="Kobe's Distance From Ball - 12/2/2015 Lakers @ Wizards", color='Legend') +
  xlab('Time Remaining in Quarter 1 (Seconds)') + 
  ylab('Distance from Ball (Feet)') + 
  theme(legend.position="none",
        plot.title=element_text(hjust=0.5)) +
  theme(legend.position="none",
        axis.text.x=element_text(angle=45, hjust=1),
        plot.title=element_text(hjust=0.5)) +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 10, hjust = 0.5, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10),
        legend.key=element_rect(fill="white", colour="white")) +
  theme(legend.position="none")



