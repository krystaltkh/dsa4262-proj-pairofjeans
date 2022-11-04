library(randomForest)
library(caret)
library(pROC)
library(MLmetrics)

######## model
train <- read.csv("../data/train0_final.csv")
test <- read.csv("../data/test0_final.csv")
train <- select(train, -c(1:4,leftC,leftA,prop_A,rightT))
test <- select(test, -c(1:4,leftC,leftA,prop_A,rightT))
train$label <- as.character(train$label)
train$label <- as.factor(train$label)
test$label <- as.character(test$label)
test$label <- as.factor(test$label)

set.seed(4262)
rf <- randomForest(label ~ ., train, mtry=floor(sqrt(ncol(train))), ntree=300, importance = TRUE, do.trace = 10)
yhat.rf <- predict(rf, newdata = test[,-ncol(test)])
mean((as.numeric(levels(yhat.rf))[yhat.rf] - as.numeric(levels(test$label))[test$label])^2) 
confusionMatrix(yhat.rf, test$label) 
AUC(as.numeric(levels(yhat.rf))[yhat.rf],as.numeric(levels(test$label))[test$label]) 
PRAUC(as.numeric(levels(yhat.rf))[yhat.rf],as.numeric(levels(test$label))[test$label]) 

#########

# without dropping features
# AUC: 0.5568476
# PRAUC: 0.3414952
train <- read.csv("../data/train0_final.csv")
test <- read.csv("../data/test0_final.csv")
train$label <- as.character(train$label)
train$label <- as.factor(train$label)
test$label <- as.character(test$label)
test$label <- as.factor(test$label)
X <- train
Y <- test

rf <- randomForest(label ~ ., X, mtry=floor(sqrt(ncol(X))), ntree=300, importance = TRUE, do.trace = 10)
yhat.rf <- predict(rf, newdata = X[,-ncol(X)])
mean((as.numeric(levels(yhat.rf))[yhat.rf] - as.numeric(levels(X$label))[X$label])^2) 
confusionMatrix(yhat.rf, X$label) 
AUC(as.numeric(levels(yhat.rf))[yhat.rf],as.numeric(levels(Y$label))[Y$label]) # 
PRAUC(as.numeric(levels(yhat.rf))[yhat.rf],as.numeric(levels(Y$label))[Y$label]) # 
varImp(rf)

### subin subset feature selection
# leftC, leftA, propA, rightT dropped for subset
train <- read.csv("../data/train0_final.csv")
test <- read.csv("../data/test0_final.csv")
train <- select(train, -c(1:4,leftC,leftA,prop_A,rightT))
test <- select(test, -c(1:4,leftC,leftA,prop_A,rightT))
train$label <- as.character(train$label)
train$label <- as.factor(train$label)
test$label <- as.character(test$label)
test$label <- as.factor(test$label)

### sol's method
train <- read.csv("../data/train0_final.csv")
test <- read.csv("../data/test0_final.csv")
train <- select(train, c(mean_time1,mean_time2,mean_time3,freqRatio,prop_G,prop_T,label))
test <- select(test, c(mean_time1,mean_time2,mean_time3,freqRatio,prop_G,prop_T,label))
train$label <- as.character(train$label)
train$label <- as.factor(train$label)
test$label <- as.character(test$label)
test$label <- as.factor(test$label)

### without oversampling
# subin
# AUC: 0.5662123
# PRAUC: 0.3333126
# sol
# AUC: 0.501832
# PRAUC: 0.16947
set.seed(4262)
rf <- randomForest(label ~ ., train, mtry=floor(sqrt(ncol(train))), ntree=300, importance = TRUE, do.trace = 10)
yhat.rf <- predict(rf, newdata = test[,-ncol(test)])
mean((as.numeric(levels(yhat.rf))[yhat.rf] - as.numeric(levels(test$label))[test$label])^2) 
confusionMatrix(yhat.rf, test$label) 
AUC(as.numeric(levels(yhat.rf))[yhat.rf],as.numeric(levels(test$label))[test$label]) 
PRAUC(as.numeric(levels(yhat.rf))[yhat.rf],as.numeric(levels(test$label))[test$label]) 

### with oversampling

train <- read.csv("../data/train0_final.csv")
test <- read.csv("../data/test0_final.csv")
train <- select(train, -c(1:4,leftC,leftA,prop_A,rightT))
test <- select(test, -c(1:4,leftC,leftA,prop_A,rightT))
test$label <- as.character(test$label)
test$label <- as.factor(test$label)

# SMOTE, K = 5
# AUC: 0.6654197
# PRAUC: 0.1717343
smoted5 <- SMOTE(train, train$label, K = 5)
smoted5_df <- smoted5$data %>% select(-c(class))
smoted5_df$label <- as.character(smoted5_df$label)
smoted5_df$label <- as.factor(smoted5_df$label)
X <- smoted5_df

# SMOTE, K = 10
# AUC: 0.669206
# PRAUC: 0.1682007
smoted10 <- SMOTE(train, train$label, K = 10)
smoted10_df <- smoted10$data %>% select(-c(class))
smoted10_df$label <- as.character(smoted10_df$label)
smoted10_df$label <- as.factor(smoted10_df$label)
X <- smoted10_df

# ADASYN, K = 5
# AUC: 0.6439096
# PRAUC: 0.1693203
adas5 <- ADAS(train, train$label, K = 5)
adas5_df <- adas5$data %>% select(-c(class))
adas5_df$label <- as.character(adas5_df$label)
adas5_df$label <- as.factor(adas5_df$label)
X <- adas5_df

set.seed(4262)
rf <- randomForest(label ~ ., X, mtry=floor(sqrt(ncol(X))), ntree=300, importance = TRUE, do.trace = 10)
yhat.rf <- predict(rf, newdata = test[,-ncol(test)])
mean((as.numeric(levels(yhat.rf))[yhat.rf] - as.numeric(levels(test$label))[test$label])^2) 
confusionMatrix(yhat.rf, test$label) 
AUC(as.numeric(levels(yhat.rf))[yhat.rf],as.numeric(levels(test$label))[test$label]) # 
PRAUC(as.numeric(levels(yhat.rf))[yhat.rf],as.numeric(levels(test$label))[test$label]) # 

#######################

# x <- read.csv("../data/parsed_A549_rep6_run1.txt")
# write.csv(x, "../data/parsed_A549_rep6_run1.csv")
test <- read.csv("../data/test.csv")
test <- test[,-c(1:4,12,14,16)]
test <- filter(test,!is.na(gtf_rel_len))
test$label <- as.character(test$label)
test$label <- as.factor(test$label)

# cor(train[,c(1:9,13)])
# pairs(train[,c(1:9,13)], upper.panel = NULL)

## random forest
# running with smoted5
set.seed(123)
smoted5_df$label <- as.character(smoted5_df$label)
smoted5_df$label <- as.factor(smoted5_df$label)
rf <- randomForest(label ~ ., smoted5_df, mtry=floor(sqrt(ncol(smoted5_df))),importance = TRUE, do.trace=10)
yhat.rf <- predict(rf, newdata = test[,-ncol(test)])
mean((as.numeric(levels(yhat.rf))[yhat.rf] - as.numeric(levels(test$label))[test$label])^2) 
confusionMatrix(yhat.rf, test$label)
varImp(rf)
# importance(rf)
# varImpPlot(rf)
auc(as.numeric(levels(test$label))[test$label], as.numeric(levels(yhat.rf))[yhat.rf]) #0.7157

# running with smoted10
set.seed(123)
smoted10_df$label <- as.character(smoted10_df$label)
smoted10_df$label <- as.factor(smoted10_df$label)
rf2 <- randomForest(label ~ ., smoted10_df, mtry=floor(sqrt(ncol(smoted10_df))),importance = TRUE, do.trace=10)
yhat.rf2 <- predict(rf2, newdata = test[,-ncol(test)])
confusionMatrix(yhat.rf2, test$label)
varImp(rf2)
auc(as.numeric(levels(test$label))[test$label], as.numeric(levels(yhat.rf2))[yhat.rf2]) #0.7267
does 
# running rf with adasyn5
set.seed(123)
adas5_df$label <- as.character(adas5_df$label)
adas5_df$label <- as.factor(adas5_df$label)
rf3 <- randomForest(label ~ ., adas5_df, mtry=floor(sqrt(ncol(adas5_df))),ntree=300,importance = TRUE, do.trace=10)
yhat.rf3 <- predict(rf3, newdata = test[,-ncol(test)])
confusionMatrix(yhat.rf3, test$label)
varImp(rf3)
auc(as.numeric(levels(test$label))[test$label], as.numeric(levels(yhat.rf3))[yhat.rf3]) #0.7109

prob.pred <- predict(rf,newdata=test[,-ncol(test)],type="prob")
prob.pred[,2]

# output file for submission
# dataset1 <- read.csv("../data/dataset1.csv")
# prob.pred <- predict(rf,newdata=dataset1[,-c(1:3,11,13,15:16)],type="prob")
# sum(is.na(prob.pred))
# cbind(prob.pred, dataset1$tr_id)
