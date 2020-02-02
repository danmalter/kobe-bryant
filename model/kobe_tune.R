library(caret)
library(xgboost)
library(e1071)
library(pROC)
library(plyr)
library(dplyr)
library(dummies)
library(randomForest)

##### LOAD DATA #####

df <- read.csv("C:/Users/dmalter/Desktop/kb/data.csv", header = T)

##### PREPROCESSING ####

#summarize action_type, remove levels where less than 30 shots occurred
action_type_rare <- df %>% 
  select(action_type) %>% 
  group_by(action_type) %>% 
  summarise(count=n()) %>% 
  filter(count < 30) %>%
  select(action_type) %>% 
  droplevels

#store as vector
action_type_rare <- action_type_rare$action_type

#grab action_type vector
action_type <- df$action_type

#create new factor with fewer levels and 'Rare Shot' level
action_type_2 <- factor(ifelse(action_type %in% action_type_rare,0,action_type), labels=c('Rare Shot',levels(action_type)[!levels(action_type) %in% levels(action_type_rare)]))

#add new level to d
df <- cbind(df,action_type_2)

### Add New Features ###
df$matchup <- substr(df$matchup,5,5)
df$location[df$matchup == "@"] <- 'Away'
df$location[df$matchup == "v"] <- 'Home'
df$location <- as.factor(df$location)
df$matchup <- NULL

df$game_date <- as.Date(df$game_date)
df$game_month <- format(df$game_date, "%b")
df$game_month <- as.factor(df$game_month)
df$game_date <- NULL

#Add Division column with teams
df$opponentConference[df$opponent=='ATL']<-'East'
df$opponentConference[df$opponent=='BKN']<-'East'
df$opponentConference[df$opponent=='BOS']<-'East'
df$opponentConference[df$opponent=='CHA']<-'East'
df$opponentConference[df$opponent=='CHI']<-'East'
df$opponentConference[df$opponent=='CLE']<-'East'
df$opponentConference[df$opponent=='DAL']<-'West'
df$opponentConference[df$opponent=='DEN']<-'West'
df$opponentConference[df$opponent=='DET']<-'East'
df$opponentConference[df$opponent=='GSW']<-'West'
df$opponentConference[df$opponent=='HOU']<-'West'
df$opponentConference[df$opponent=='IND']<-'East'
df$opponentConference[df$opponent=='LAC']<-'West'
df$opponentConference[df$opponent=='MEM']<-'West'
df$opponentConference[df$opponent=='MIA']<-'East'
df$opponentConference[df$opponent=='MIL']<-'East'
df$opponentConference[df$opponent=='MIN']<-'West'
df$opponentConference[df$opponent=='NJN']<-'East'
df$opponentConference[df$opponent=='NOH']<-'West'
df$opponentConference[df$opponent=='NOP']<-'West'
df$opponentConference[df$opponent=='NYK']<-'East'
df$opponentConference[df$opponent=='OKC']<-'West'
df$opponentConference[df$opponent=='ORL']<-'East'
df$opponentConference[df$opponent=='PHI']<-'East'
df$opponentConference[df$opponent=='PHX']<-'West'
df$opponentConference[df$opponent=='POR']<-'West'
df$opponentConference[df$opponent=='SAC']<-'West'
df$opponentConference[df$opponent=='SAS']<-'West'
df$opponentConference[df$opponent=='SEA']<-'West'
df$opponentConference[df$opponent=='TOR']<-'East'
df$opponentConference[df$opponent=='UTA']<-'West'
df$opponentConference[df$opponent=='VAN']<-'West'
df$opponentConference[df$opponent=='WAS']<-'East'

df$opponentConference <- as.factor(df$opponentConference)
df$period <- as.factor(df$period)
df$playoffs <- as.factor(df$playoffs)

#Correct a couple of teams
#New Jersy Became Broklyn
df$opponent[df$opponent=='NJN']<-'BKN'
#NOP is NOH
df$opponent[df$opponent=='NOP']<-'NOH'


### Remove Features ###
df$team_id <- NULL  # remove ID
df$action_type <- NULL
df$matchup <- NULL
df$game_id <- NULL
df$game_event_id <- NULL
df$shot_id <- NULL
df$team_name <- NULL
df$opponent <- NULL
df$lat <- NULL
#df$loc_x <- NULL
#df$loc_y <- NULL
df$lon <- NULL


#########################################

#Extract train and test data sets
trainData <- df%>%
  filter(!is.na(shot_made_flag))

testData <- df%>%
  filter(is.na(shot_made_flag))


### Random Forest ###
fit <- randomForest(shot_made_flag ~ ., data=trainData, importance = TRUE, ntree = 250)

fit
summary(fit)
importance(fit)
varImpPlot(fit)
fit$importance
fit$test
fit$mse

#Tune Forest
tuneRF(x, y, mtryStart, ntreeTry=50, stepFactor=2, improve=0.05,
       trace=TRUE, plot=TRUE, doBest=FALSE, ...)

prediction <- predict(fit, testData)
testData$shot_made_flag = prediction
submit <- data.frame(shot_id = testShotIDs, shot_made_flag=prediction)
write.csv(submit, file = "Model 19 - RandomForest (new time and division features).csv", row.names = FALSE)


### XGBoost Tuning ###
dat <- df[,-11]  #11th variable is the shot made flag
dat <- dummy.data.frame(dat)
dat <- cbind(df$shot_made_flag, dat)
colnames(dat)[colnames(dat) == 'df$shot_made_flag'] <- 'shot_made_flag'
dat$shot_made_flag <- as.factor(dat$shot_made_flag)
revalue(dat$shot_made_flag, c("0"="Zero", "1"="One"))

dat$minutes_remaining <- as.numeric(dat$minutes_remaining)
dat$seconds_remaining <- as.numeric(dat$seconds_remaining)
dat$shot_distance <- as.numeric(dat$shot_distance)


train <- subset(dat, shot_made_flag != 'NA')
test <- subset(dat, is.na(dat$shot_made_flag))

z <- unlist(lapply("Label_", paste0, train$shot_made_flag ))

xgbGrid <- expand.grid(
  nrounds = c(500),
  #nrounds = c(150,200,250,300),
  eta = c(0.03, 0.01, 0.001),
  max_depth = c(4, 6, 8),
  gamma = c(0,1),
  colsample_bytree = c(0.4, 0.6, .8),    #default=1
  min_child_weight = 1     #default=1
)

cctrl1 <- trainControl(method = "cv", number = 2, 
                       verboseIter = TRUE,
                       returnData = FALSE,
                       returnResamp = "all",
                       #classProbs = TRUE, 
                       summaryFunction = defaultSummary,
                       allowParallel = TRUE)


test_class_cv_model <- train(y = as.factor(z),
                             x = as.matrix(train %>%
                                             select(-shot_made_flag)),
                             method = "xgbTree", 
                             trControl = cctrl1,
                             metric = "Accuracy", 
                             tuneGrid = xgbGrid)

plot(test_class_cv_model)
