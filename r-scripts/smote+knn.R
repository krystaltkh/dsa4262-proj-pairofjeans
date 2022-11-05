install.packages(c("missForest", "e1071",
                   "readr", "dplyr", "caret",
                   "stringr", "smotefamily",
                   "PRROC"))
# Loading package
library(missForest)
library(e1071)
library(readr)
library(dplyr)
library(caret)
library(stringr)
library(smotefamily)
library(PRROC)

set.seed(4262)

df <- read_csv("../data/train0_final.csv")
test <- read_csv("../data/test0_final.csv")
test_x <- test[, -29]

####### READ IN DATASETS ########
# SMOTE, K = 5
smoted5 <- SMOTE(df[,-c(1:4)], df$label, K = 5)
smoted5_df.subin <- smoted5$data %>%
  select(-c(class, leftC, leftA, prop_A, rightT))
smoted5_df.subin$label <- as.factor(smoted5_df.subin$label)

smoted5_df.sol <- smoted5$data %>%
  select(c(mean_time1, mean_time2, mean_time3, freqRatio, prop_G, prop_T, label))
smoted5_df.sol$label <- as.factor(smoted5_df.sol$label)

# SMOTE, K = 10
smoted10 <- SMOTE(df[,-c(1:4)], df$label, K = 10)
smoted10_df.subin <- smoted10$data %>%
  select(-c(class, leftC, leftA, prop_A, rightT))
smoted10_df.subin$label <- as.factor(smoted10_df.subin$label)

smoted10_df.sol <- smoted10$data %>%
  select(c(mean_time1, mean_time2, mean_time3, freqRatio, prop_G, prop_T, label))
smoted10_df.sol$label <- as.factor(smoted10_df.sol$label)

# ADASYN, K = 5
adas5 <- ADAS(df[,-c(1:4)], df$label, K = 5)

adas5_df.subin <- adas5$data %>%
  select(-c(class, leftC, leftA, prop_A, rightT))
adas5_df.subin$label <- as.factor(adas5_df.subin$label)

adas5_df.sol <- adas5$data %>%
  select(c(mean_time1, mean_time2, mean_time3, freqRatio, prop_G, prop_T, label))
adas5_df.sol$label <- as.factor(adas5_df.sol$label)

# ADASYN, K = 10
adas10 <- ADAS(df[,-c(1:4)], df$label, K = 10)

adas10_df.subin <- adas10$data %>%
  select(-c(class, leftC, leftA, prop_A, rightT))
adas10_df.subin$label <- as.factor(adas10_df.subin$label)

adas10_df.sol <- adas10$data %>%
  select(c(mean_time1, mean_time2, mean_time3, freqRatio, prop_G, prop_T, label))
adas10_df.sol$label <- as.factor(adas10_df.sol$label)

# density-based SMOTE
dbsmoted <- DBSMOTE(df[,-c(1:4)], df$label)

dbsmoted_df.subin <- dbsmoted$data %>%
  select(-c(class, leftC, leftA, prop_A, rightT))
dbsmoted_df.subin$label <- as.factor(dbsmoted_df.subin$label)

dbsmoted_df.sol <- dbsmoted$data %>%
  select(c(mean_time1, mean_time2, mean_time3, freqRatio, prop_G, prop_T, label))
dbsmoted_df.sol$label <- as.factor(dbsmoted_df.sol$label)

#### LOOP: SUBIN'S ####
datasets.subin <- c("smoted5_df.subin", "smoted10_df.subin", 
                    "adas5_df.subin", "adas10_df.subin", "dbsmoted_df.subin")

modelData_df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("dataset", "k", "ROC", "PR"))

# subin's
for (i in datasets.subin) {
  if (substr(i, str_length(i), str_length(i)) == "5") {
    k <- 5
  } else if (substr(i, str_length(i), str_length(i)) == "0") {
    k <- 10
  }
  
  if (substr(i, 1, 1) == "s") {
    tempdf <- SMOTE(df[,-c(1:4)], df$label, K = k)
  } else if (substr(i, 1, 1) == "a") {
    tempdf <- ADAS(df[,-c(1:4)], df$label, K = k)
  } else {
    tempdf <- DBSMOTE(df[,-c(1:4)], df$label)
  }
  
  newdf <- tempdf$data %>%
    select(-c(class, leftC, leftA, prop_A, rightT))
  newdf$label <- as.factor(newdf$label)
  
  # TRAINING
  for (k_val in 1:20) {
    # Probability-KNN, K = 10; knn3
    knn3.train <- knn3(label ~ ., data=newdf, k = k_val)
    knn3.prob <- predict(knn3.train, test_x, type = "prob")
    probs <- cbind(knn3.prob, test$label)
    
    fg <- probs[test$label == 1]
    bg <- probs[test$label == 0]
    
    # ROC Curve
    roc <- roc.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
    # plot(roc)
    
    # PR Curve
    pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
    # plot(pr)
    
    temp <- as.vector(c(i, k_val, roc$auc, pr$auc.integral))
    print(temp)
    modelData_df <- rbind(modelData_df, temp)
  }
}
View(modelData_df)

# Most suitable dataset: SMOTED K=10, with knn's k=19
# Highest AUC of 0.741339875861314

#### LOOP" SOL'S ####
datasets.sol <- c("smoted5_df.sol", "smoted10_df.sol", 
                  "adas5_df.sol", "adas10_df.sol", "dbsmoted_df.sol")

# sol's
for (i in datasets.sol) {
  if (substr(i, str_length(i), str_length(i)) == "5") {
    k <- 5
  } else if (substr(i, str_length(i), str_length(i)) == "0") {
    k <- 10
  }
  
  if (substr(i, 1, 1) == "s") {
    tempdf <- SMOTE(df[,-c(1:4)], df$label, K = k)
  } else if (substr(i, 1, 1) == "a") {
    tempdf <- ADAS(df[,-c(1:4)], df$label, K = k)
  } else {
    tempdf <- DBSMOTE(df[,-c(1:4)], df$label)
  }
  
  newdf <- tempdf$data %>%
    select(c(mean_time1, mean_time2, mean_time3, freqRatio, prop_G, prop_T, label))
  newdf$label <- as.factor(newdf$label)
  
  # TRAINING
  for (k_val in 1:20) {
    # Probability-KNN, K = 10; knn3
    knn3.train <- knn3(label ~ ., data=newdf, k = k_val)
    knn3.prob <- predict(knn3.train, test_x, type = "prob")
    probs <- cbind(knn3.prob, test$label)
    
    fg <- probs[test$label == 1]
    bg <- probs[test$label == 0]
    
    # ROC Curve
    roc <- roc.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
    # plot(roc)
    
    # PR Curve
    pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
    # plot(pr)
    
    temp <- as.vector(c(i, k_val, roc$auc, pr$auc))
    print(temp)
    modelData_df <- rbind(modelData_df, temp)
  }
}
View(modelData_df)

final_modelData_df <- cbind(modelData_df.subin, modelData_df.sol)

######## PROBABILITY MODEL TRAINING #########
# Probability-KNN, K = 10; knn3
knn3.train <- knn3(label ~ ., data=smoted5_df, k = 10)

knn3.class <- predict(knn3.train, test_x, type = "class")
confusionMatrix(knn3.class, as.factor(test$label)) # 0.8179

knn3.prob <- predict(knn3.train, test_x, type = "prob")
knn3.prob <- as.data.frame(knn3.prob)
submission <- cbind(test[, c(2:3)], knn3.prob$`1`)

# we get a submission of probability here:
tempdf <- SMOTE(df[,-c(1:4)], df$label, K = 10)
train <- tempdf$data %>%
  select(-c(class, combined_sd3, DRACH1, DRACH3))
train$label <- as.factor(train$label)

knn3.train <- knn3(label ~ ., data=train, k = 19)

knn3.class <- predict(knn3.train, test_x, type = "class")
confusionMatrix(knn3.class, as.factor(test$label))

knn3.prob <- predict(knn3.train, test_x, type = "prob")
knn3.prob <- as.data.frame(knn3.prob)
submission <- cbind(test[, c(2:3)], knn3.prob$`1`)


######## TRADITIONAL MODEL TRAINING #########
# returns probability of MAJORITY class
# Traditional KNN, K = 10
# knn.pred <- knn(smoted5_df[,-14], test_x, smoted5_df$label, k = 10, prob=TRUE)
# 
# knn.prob <- as.vector(attributes(knn.pred)$prob)
# submission <- cbind(test[, c(2:3)], knn.prob)
# 
# confusionMatrix(knn.pred, as.factor(test$label))


