library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)

kobe <- read.csv('~/Kaggle/kobe-bryant/kobe_shots.csv') 

### clean data ###

# Remove rows with NAs
shots <- na.omit(kobe)

# Add column for number of pts attempted
shots$point_type <- ifelse(shots$shot_type == '2PT Field Goal', 2, 3)

#change data format
shots$game_date <- as.Date(shots$game_date, format = "%Y-%m-%d")

# create a nba court
court_img <- "http://robslink.com/SAS/democd54/nba_court_dimensions.jpg"
court <- rasterGrob(readJPEG(getURLContent(court_img)),
                    width=unit(1,"npc"), height=unit(1,"npc"))

# Scatterplot - All Shots
ggplot(subset(shots), aes(x=loc_x, y=loc_y)) + 
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point(aes(colour = as.factor(shot_made_flag))) +
  scale_color_manual(values = c('gold', 'purple'), labels = c("Miss", "Make")) +
  labs(title="Kobe Bryant - All 4th Quarter Career Shots", color='Legend') +
  xlim(-250, 250) +
  ylim(-50, 420) + 
  theme_void() +
  theme(legend.title=element_blank(),
        plot.title=element_text(hjust=0.5))

# Scatterplot - All Shots by Shot Type
shots <- shots %>% group_by(combined_shot_type) %>% mutate(shot_type_count = n())  # get count by group to order point layers in ggplot
ggplot(shots %>% arrange(desc(shot_type_count)), aes(x=loc_x, y=loc_y)) + 
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point(aes(colour = combined_shot_type)) +
  scale_color_manual(values = c('green4', 'deeppink', 'blue', 'purple', 'gold', 'turquoise1')) +
  labs(title="Kobe Bryant - All Career Shots by Shot Type", color='Legend') +
  xlim(-250, 250) +
  ylim(-50, 420) + 
  theme_void() +
  theme(legend.title=element_blank(),
        plot.title=element_text(hjust=0.5))

# Scatterplot - All Shots by Shot Zone Area
shots <- shots %>% group_by(shot_zone_area) %>% mutate(shot_zone_area_count = n()) # get count by group to order point layers in ggplot
ggplot(shots %>% arrange(desc(shot_zone_area)), aes(x=loc_x, y=loc_y)) + 
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point(aes(colour = shot_zone_area)) +
  labs(title="Kobe Bryant - All Career Shots by Shot Zone", color='Legend') +
  xlim(-250, 250) +
  ylim(-50, 420) + 
  theme_void() +
  theme(legend.title=element_blank(),
        plot.title=element_text(hjust=0.5))

# 4th Quarter Shots
ggplot(subset(shots, period == 4), aes(x=loc_x, y=loc_y)) + 
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point(aes(colour = as.factor(shot_made_flag))) +
  scale_color_manual(values = c('gold', 'purple'), labels = c("Miss", "Make")) +
  labs(title="Kobe Bryant - All 4th Quarter Career Shots", color='Legend') +
  xlim(-250, 250) +
  ylim(-50, 420) + 
  theme_void() +
  theme(legend.title=element_blank(),
        plot.title=element_text(hjust=0.5))

# 4th Quarter Shots with Less than 10 Seconds Remaining
title <- paste("Kobe Bryant - All 4th Quarter Career Shots")
subheader <- paste("Less than 10 Seconds Remaining")

ggplot(subset(shots, period == 4 & minutes_remaining == 0 & seconds_remaining <= 10), aes(x=loc_x, y=loc_y)) + 
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point(aes(colour = as.factor(shot_made_flag))) +
  scale_color_manual(values = c('gold', 'purple'), labels = c("Miss", "Make")) +
  labs(title=paste0(title,"\n",subheader), color='Legend') +
  xlim(-250, 250) +
  ylim(-50, 420) + 
  theme_void() +
  theme(legend.title=element_blank(),
        plot.title=element_text(hjust=0.5))
             
# Scatterplot - By Shot Zone
ggplot() + 
  # We use a different alpha value for jump shots to improve the visualization
  geom_point(data=shots,
             aes(x=lon, y=lat, colour=shot_zone_area), alpha=0.8) +
  #scale_fill_gradient(low="yellow", high="purple") +
  scale_color_manual(values = c('grey', 'purple', 'red', 'yellow', 'blue', 'violet')) +
  labs(title="All Shots by Zone", color='Legend') +
  ylim(c(33.7, 34.0883)) +
  theme_void() +
  theme(legend.title=element_blank(), 
        plot.title=element_text(hjust=0.5)) 


# Count of shots by season in 2PT Field Goal and 3PT Field Goal
### Shots by Season ###
shot_season <- as.data.frame.matrix(table(shots$season, shots$shot_type))
shot_season$season <- rownames(shot_season)
names(shot_season)[1]<-"TwoPoint"
names(shot_season)[2]<-"ThreePoint"

shot_season %>%
  ggplot(aes(x=season, group=1)) +
  geom_line(aes(y=TwoPoint, colour="TwoPoint")) +
  geom_line(aes(y=ThreePoint, colour="ThreePoint")) +
  geom_point(aes(y=TwoPoint, colour="TwoPoint"), size=3) +
  geom_point(aes(y=ThreePoint, colour="ThreePoint"), size=3) +
  labs(title="Accuracy by season", 
       subtitle="2PT Field Goal and 3PT Field Goal",
       x="Season", y="Accuracy") +
  theme_bw() +
  theme(legend.title=element_blank(),
        legend.position="bottom",
        axis.text.x=element_text(angle=45, hjust=1),
        plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5)) 


### Shot Accuracy by Range ###
shots %>%
  group_by(shot_zone_range) %>%
  summarise(Accuracy=mean(shot_made_flag)) %>%
  ggplot(aes(x=shot_zone_range, y=Accuracy, group=1, fill = Accuracy)) +
  geom_bar(stat = "identity") +
  scale_fill_continuous(low="mediumpurple1", high="purple3") +
  labs(title="Accuracy by Shot Range", x="Shot Zone Range") +
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

### Shot Accuracy by Shot Type ###
shots %>%
  group_by(combined_shot_type) %>%
  summarise(Accuracy=mean(shot_made_flag)) %>%
  ggplot(aes(x=combined_shot_type, y=Accuracy, group=1, fill = Accuracy)) +
  geom_bar(stat = "identity") +
  scale_fill_continuous(low="yellow", high="purple") +
  labs(title="Accuracy by Shot Type", x="Shot Type") +
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

### Shot Accuracy over Game Time ###
shots %>%
  group_by(period) %>%
  summarise(Accuracy=mean(shot_made_flag)) %>%
  ggplot(aes(x=period, y=Accuracy, group=1, fill = Accuracy)) +
  geom_bar(stat = "identity") +
  geom_line(aes(color=Accuracy)) +
  geom_point(aes(color=Accuracy), size=3) +
  scale_fill_continuous(low="yellow", high="purple") +
  labs(title="Accuracy by Quarter", x="Quarter") +
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

# Shots taken over course of the game
shots$game_minute <- ifelse(shots$period == 1, 12-shots$minutes_remaining, 
                            ifelse(shots$period == 2, 24-shots$minutes_remaining,
                                   ifelse(shots$period == 3, 36-shots$minutes_remaining,
                                          ifelse(shots$period == 4, 48-shots$minutes_remaining,
                                                 ifelse(shots$period == 5, 60-shots$minutes_remaining,
                                                        ifelse(shots$period == 6, 72-shots$minutes_remaining,
                                                               ifelse(shots$period == 7, 84-shots$minutes_remaining,
                                                                      NA)))))))

shots %>%
  filter(game_minute <= 48) %>%
  group_by(game_minute) %>%
  count(game_minute, name = "shot_count") %>%
  ggplot(aes(x=game_minute, y=shot_count)) + 
  geom_bar(aes(fill=shot_count), stat="identity") +
  scale_fill_gradient(low="purple", high="gold") +
  labs(title="Shots Taken Over Course of Games", x="Game Minute", y = "Career Shots Taken")  +
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
        legend.key=element_rect(fill="white", colour="white"))

### Shot Accuracy by Opponent ###
shots %>%
  group_by(opponent) %>%
  summarise(Accuracy=mean(shot_made_flag)) %>%
  ggplot(aes(x=reorder(opponent, -Accuracy), y=Accuracy, group=1, fill = Accuracy)) +
  geom_bar(stat = "identity") +
  scale_fill_continuous(low="yellow", high="purple") +
  labs(title="Accuracy by Opponent", x="Opponent") +
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


# shots made by opponent final 10
shots %>%
  filter(seconds_remaining <= 24 & period >= 4 & minutes_remaining == 0) %>%
  group_by(opponent) %>%
  summarise(shots_made=sum(shot_made_flag)) %>%
  arrange(-shots_made) %>%
  mutate(Conference=c("Western", "Western", "Western", "Western", "Eastern",
                      "Western", "Eastern", "Western", "Western", "Western",
                      "Western", "Western", "Western", "Eastern", "Eastern",
                      "Eastern", "Eastern", "Western", "Western", "Eastern",
                      "Eastern", "Eastern", "Eastern", "Western", "Eastern",
                      "Eastern", "Eastern", "Eastern", "Eastern", "Western",
                      "Western" )) %>%
  ggplot(aes(x=reorder(opponent, -shots_made), y=shots_made)) + 
  geom_bar(aes(fill=Conference), stat="identity") +
  scale_fill_manual(values=c("gold", "purple"))+
  labs(title="Final 24 Seconds of Game Shots Made", x="Opponent", y = "Career Shots Made") +
  theme(legend.position="none",
        axis.text.x=element_text(angle=45, hjust=1),
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
  theme(legend.position="bottom")


# Accuracy by season in Playoff and Regular Season
shots %>%
  group_by(season) %>%
  summarise(Playoff=mean(shot_made_flag[playoffs==1]),
            RegularSeason=mean(shot_made_flag[playoffs==0])) %>%
  ggplot(aes(x=season, group=1)) +
  geom_line(aes(y=Playoff, colour="Playoff")) +
  scale_color_manual(values = c('gold', 'purple')) +
  geom_line(aes(y=RegularSeason, colour="RegularSeason")) +
  geom_point(aes(y=Playoff, colour="Playoff"), size=3) +
  geom_point(aes(y=RegularSeason, colour="RegularSeason"), size=3) +
  labs(title="Shot Accuracy by Season", 
       subtitle="Regular Season and Playoffs",
       x="Season", y="Accuracy") +
  theme_bw() +
  theme(legend.title=element_blank(),
        legend.position="bottom",
        axis.text.x=element_text(angle=45, hjust=1),
        plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5)) +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 10, hjust = 0.5, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10),
        legend.key=element_rect(fill="white", colour="white")) 


# Accuracy by season in 2PT Field Goal and 3PT Field Goal
shots %>%
  group_by(season) %>%
  summarise(TwoPoint=mean(shot_made_flag[shot_type=="2PT Field Goal"]),
            ThreePoint=mean(shot_made_flag[shot_type=="3PT Field Goal"])) %>%
  ggplot(aes(x=season, group=1)) +
  geom_line(aes(y=TwoPoint, colour="TwoPoint")) +
  scale_color_manual(values = c('gold', 'purple')) +
  geom_line(aes(y=ThreePoint, colour="ThreePoint")) +
  geom_point(aes(y=TwoPoint, colour="TwoPoint"), size=3) +
  geom_point(aes(y=ThreePoint, colour="ThreePoint"), size=3) +
  labs(title="Shot Accuracy by Season", 
       subtitle="2 Point FG and 3 Point FG",
       x="Season", y="Accuracy") +
  theme_bw() +
  theme(legend.title=element_blank(),
        legend.position="bottom",
        axis.text.x=element_text(angle=45, hjust=1),
        plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5)) +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(size = 10, hjust = 0.5, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 10),
        axis.text.y=element_text(colour="black", size = 10),
        legend.key=element_rect(fill="white", colour="white")) 