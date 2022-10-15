library(readr)
library(dplyr)
library(corrplot)
library(tidyverse)
library(caret)
library(leaps)
library(MASS)
library(pROC)


train <- read_csv("train.csv")
View(train)
summary(train)
y <- as.factor(train$label)
X <-train[,5:17]

################################################################
# running model: xgboost
############################################################################

library(xgboost)
xgb <- xgboost(data = data.matrix(X), 
               label = data.matrix(y) 
               #eta = 0.1,
               #max_depth = 15, 
               ,nround=100 ,
               #subsample = 0.5,
               #colsample_bytree = 0.5,
               #eval_metric = "merror",
               objective = "multi:softprob",
               num_class = 2,
               #nthread = 1
)

testX <- read_csv("test.csv")
#View(testX)
testy <- as.factor(testX$label)
testX <- testX[,5:17]
# predict values in test set
y_pred <- predict(xgb, data.matrix(testX))
y_pred = y_pred[ !c(TRUE,FALSE) ]
y_pred[y_pred>0.5] = 1
y_pred[y_pred<=0.5] = 0
y_pred
confusionMatrix(as.factor(y_pred), as.factor(testy)) #????
auc(testy, y_pred)

model <- xgb.dump(xgb, with_stats = T)
model[1:10] #This statement prints top 10 nodes of the model
names <- dimnames(data.matrix(X))[[2]]
# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = xgb)
# Nice graph
xgb.plot.importance(importance_matrix)



############################################################################
# smote + xgboost
############################################################################
library(smotefamily)
library(readr)
library(dplyr)
# library(ROSE)

set.seed(4262)

df <- read_csv("./train.csv")

# create unique_id for combining the over-sampled data back with original data later

df <- df %>% 
  filter(!is.na(gtf_rel_len))

# SMOTE, K = 5
smoted5 <- SMOTE(df[,-c(1:4)], df$label, K = 5)
smoted5_df <- smoted5$data %>% dplyr::select(-class)

# SMOTE, K = 10
smoted10 <- SMOTE(df[,-c(1:4)], df$label, K = 10)
smoted10_df <- smoted10$data %>%
  dplyr::select(-class)

# ADASYN, K = 5
adas5 <- ADAS(df[,-c(1:4)], df$label, K = 5)
adas5_df <- adas5$data %>%
  dplyr::select(-class)

# ADASYN, K = 10
adas10 <- ADAS(df[,-c(1:4)], df$label, K = 10)
adas10_df <- adas10$data %>%
  dplyr::select(-class)

# density-based: DOESNT WORK??
#dbsmoted <- DBSMOTE(df[,-c(1:4)], df$label)
#dbsmoted_df <- dbsmoted$data %>%
#  dplyr::select(-class)


###########################################################################
# TESTING EACH DF
###########################################################################

# SMOTED5
colnames(smoted5_df)
smoted5_df = smoted5_df[,-c(8,10,12)]
xgb <- xgboost(data = data.matrix(smoted5_df[,1:10]), 
               label = data.matrix(smoted5_df[,11]) 
               #eta = 0.1,
               #max_depth = 15, 
               ,nround=100,
               #subsample = 0.5,
               #colsample_bytree = 0.5,
               #eval_metric = "merror",
               objective = "multi:softprob",
               num_class = 2,
               #nthread = 1
)

#split1 <- y_pred[1:24198]
#split2 <- y_pred[24199:48396]

testX <- read_csv("test.csv")
#View(testX)
#testy <- as.factor(testX$label)
testX <- testX[,c(5,6,7,8,9,10,11,13,15,17)]
# predict values in test set
y_pred <- predict(xgb, data.matrix(testX))
y_pred = y_pred[ !c(TRUE,FALSE) ]
y_pred[y_pred>0.5] = 1
y_pred[y_pred<=0.5] = 0

confusionMatrix(as.factor(y_pred), as.factor(testy))

auc(testy, y_pred)

# SMOTED10
colnames(smoted10_df)
smoted10_df = smoted10_df[,-c(8,10,12)]
xgb <- xgboost(data = data.matrix(smoted10_df[,1:10]), 
               label = data.matrix(smoted10_df[,11]),
               #eta = 0.1,
               #max_depth = 15, 
               nround=100,
               #subsample = 0.5,
               #colsample_bytree = 0.5,
               eval_metric = "merror",
               objective = "multi:softprob",
               num_class = 2
               #nthread = 1
)
#xgboost(param, data = x_mat, label = y_mat,nround = 3000, objective='multi:softprob')


#testX <- read_csv("test.csv")
#View(testX)
#testy <- testX$label
#testX <- testX[,c(5,6,7,8,9,10,11,13,15,17)]
# predict values in test set
y_pred <- predict(xgb, newdata = data.matrix(testX))
y_pred = y_pred[ !c(TRUE,FALSE) ]
y_pred[y_pred>0.5] = 1
y_pred[y_pred<=0.5] = 0

confusionMatrix(as.factor(y_pred), as.factor(testy))
auc(testy, y_pred)


# ADAS5
adas5_df = adas5_df[,-c(8,10,12)]
xgb <- xgboost(data = data.matrix(adas5_df[,1:10]), 
               label = data.matrix(adas5_df[,11]) 
               #eta = 0.1,
               #max_depth = 15, 
               ,nround=100,
               #subsample = 0.5,
               #colsample_bytree = 0.5,
               #eval_metric = "merror",
               objective = "multi:softprob",
               num_class = 2
               #nthread = 1
)

#testX <- read_csv("test.csv")
#View(testX)
#testy <- testX$label
#testX <- testX[,c(5,6,7,8,9,10,11,13,15,17)]
# predict values in test set
y_pred <- predict(xgb, data.matrix(testX))
y_pred = y_pred[ !c(TRUE,FALSE) ]
y_pred[y_pred>0.5] = 1
y_pred[y_pred<=0.5] = 0

confusionMatrix(as.factor(y_pred), as.factor(testy))
auc(testy, y_pred)

# ADAS10
adas10_df = adas10_df[,-c(8,10,12)]
xgb <- xgboost(data = data.matrix(adas10_df[,1:10]), 
               label = data.matrix(adas10_df[,11]) 
               #eta = 0.1,
               #max_depth = 15, 
               ,nround=100 ,
               #subsample = 0.5,
               #colsample_bytree = 0.5,
               #eval_metric = "merror",
               objective = "multi:softprob",
               num_class = 2
               #nthread = 1
)

#testX <- read_csv("test.csv")
#View(testX)
#testy <- testX$label
#testX <- testX[,c(5,6,7,8,9,10,11,13,15,17)]
# predict values in test set
y_pred <- predict(xgb, data.matrix(testX))
y_pred = y_pred[ !c(TRUE,FALSE) ]
y_pred[y_pred>0.5] = 1
y_pred[y_pred<=0.5] = 0
confusionMatrix(as.factor(y_pred), as.factor(testy))
auc(testy, y_pred)


################################################################################
# USING SMOTED5_DF ON DATASETS1,2
################################################################################

xgb <- xgboost(data = data.matrix(smoted10_df[,1:10]), 
               label = data.matrix(smoted10_df[,11]),
               nround=100,
               eval_metric = "merror",
               objective = "multi:softprob",
               num_class = 2
)

dataset1 <- read_csv("./dataset1.csv")
testX <- dataset1[,c(4,5,6,7,8,9,10,12,14,17)]
y_pred <- predict(xgb, data.matrix(testX))
y_pred = y_pred[ !c(TRUE,FALSE) ]
y_pred

dataset2 <- read_csv("./dataset2.csv")
testX <- dataset2[,c(4,5,6,7,8,9,10,12,14,17)]
# predict values in test set
y_pred <- predict(xgb, data.matrix(testX))
y_pred = y_pred[ !c(TRUE,FALSE) ]
#y_pred[y_pred>0.5] = 1
#y_pred[y_pred<=0.5] = 0
y_pred

#############################################################################
