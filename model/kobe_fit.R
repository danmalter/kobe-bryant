############################################################################
### load libraries
############################################################################
library(xgboost)
library(methods)
library(caret)

############################################################################
### load train data and create matrices for xgb
############################################################################
in.train <- createDataPartition(y = train$shot_made_flag, p = 0.80, list = F)  # use this to train model
in.train <- in.train[1:20558,]  # this makes it a vector

# create Active_Customer vector
train.y <- train$shot_made_flag
train.y <- gsub('Class_','', train.y)
train.y <- as.integer(train.y)  #xgboost take features in [0, number of classes)

# create matrix of original features for train.x
train.x <- train
train.x$shot_made_flag <- NULL
train.x <- as.matrix(train.x)
train.x <- matrix(data = as.numeric(train.x), nrow = nrow(train.x), ncol = ncol(train.x))


############################################################################
### xgb using original + aggregated features
# aggregated features being row sum, row var, and no. of cols filled
############################################################################
# Set necessary parameter
xg.param <- list("objective" = "multi:softprob",
                 'eval_metric' = "mlogloss",
                 'num_class' = 2,
                 'eta' = 0.03,
                 'gamma' = 0,
                 'max.depth' = 4,
                 'min_child_weight' = 1,
                 #'subsample' = 0.9,
                 'colsample_bytree' = 0.4,
                 'nthread' = 3)

# run cross validation
xgb.fit.cv <- xgb.cv(param = xg.param, data = train.x[in.train, ], label = train.y[in.train], 
                     nfold = 5, nrounds = 250)

# check best iteration
cv.min <- min(xgb.fit.cv$test.mlogloss.mean)
cv.min.rounds <- which(xgb.fit.cv$test.mlogloss.mean == min(xgb.fit.cv$test.mlogloss.mean))  

cv.rounds <- cv.min.rounds + 5


# fit model on training set
xgb.fit <- xgboost(param = xg.param, data = train.x[in.train, ], 
                   label = train.y[in.train], nrounds = 500)

# fit model on full training data
xgb.fit <- xgboost(param = xg.param, data = train.x, 
                   label = train.y, nrounds = 500)

##### Prediction #####

submission <- read.csv("C:/Users/dmalter/Desktop/kb/sample_submission.csv")
submission[,2] <- 0

new.test = test[,-1]

# create matrix of original features for test.x
new.test <- as.matrix(new.test)
new.test <- matrix(data = as.numeric(new.test), nrow = nrow(new.test), ncol = ncol(new.test))


# create predictions on test data 
# using original + aggregated features
xgb.pred <- predict(xgb.fit, new.test)
xgb.pred <- t(matrix(xgb.pred, nrow = 2, ncol = length(xgb.pred)/2))
xgb.pred <- data.frame(1:nrow(xgb.pred), xgb.pred)
colnames(xgb.pred)[colnames(xgb.pred) == 'X1'] <- 'shot_made_flag'
colnames(xgb.pred)[colnames(xgb.pred) == 'X2'] <- 'shot_made_1'
submission[,2] <- xgb.pred[,2]
write.csv(submission, file='submission2.csv', quote=FALSE, row.names=FALSE)
