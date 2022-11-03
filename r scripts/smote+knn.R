install.packages(c("missForest", "e1071",
                   "readr", "dplyr", "caret",
                   "stringr", "smotefamily",
                   "pROC"))
# Loading package
library(missForest)
library(e1071)
library(readr)
library(dplyr)
library(caret)
library(stringr)
library(smotefamily)
library(pROC)

set.seed(4262)

df <- read_csv("../data/imputed_train0.csv")
test <- read_csv("../data/imputed_test0.csv")
test_x <- test[, -19]

####### READ IN DATASETS ########
# SMOTE, K = 5
smoted5 <- SMOTE(df[,-c(1:4)], df$label, K = 5)
smoted5_df <- smoted5$data %>%
  select(-c(class, combined_sd3, DRACH1, DRACH3))
smoted5_df$label <- as.factor(smoted5_df$label)

# SMOTE, K = 10
smoted10 <- SMOTE(df[,-c(1:4)], df$label, K = 10)
smoted10_df <- smoted10$data %>%
  select(-c(class, combined_sd3, DRACH1, DRACH3))
smoted10_df$label <- as.factor(smoted10_df$label)

# ADASYN, K = 5
adas5 <- ADAS(df[,-c(1:4)], df$label, K = 5)
adas5_df <- adas5$data %>%
  select(-c(class, combined_sd3, DRACH1, DRACH3))
adas5_df$label <- as.factor(adas5_df$label)

# ADASYN, K = 10
adas10 <- ADAS(df[,-c(1:4)], df$label, K = 10)
adas10_df <- adas10$data %>%
  select(-c(class, combined_sd3, DRACH1, DRACH3))
adas10_df$label <- as.factor(adas10_df$label)

# density-based SMOTE
dbsmoted <- DBSMOTE(df[,-c(1:4)], df$label)
dbsmoted_df <- dbsmoted$data %>%
  select(-c(class, combined_sd3, DRACH1, DRACH3))
dbsmoted_df$label <- as.factor(dbsmoted_df$label)

#### LOOP ####
datasets <- c("smoted5", "smoted10", "adas5", "adas10", "dbsmoted")

modelData_df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("dataset", "k", "AUC"))

for (i in datasets) {
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
    select(-c(class, combined_sd3, DRACH1, DRACH3))
  newdf$label <- as.factor(newdf$label)

  # TRAINING
  for (k_val in 1:20) {
    # Probability-KNN, K = 10; knn3
    knn3.train <- knn3(label ~ ., data=newdf, k = k_val)
    
    knn3.class <- predict(knn3.train, test_x, type = "class")

    # Calculate AUC
    roc_obj <- roc(test$label, as.numeric(knn3.class))
    temp <- as.vector(c(i, k_val, auc(roc_obj)[1]))
    print(temp)
    modelData_df <- rbind(modelData_df, temp)
  }
}
View(modelData_df)

# Most suitable dataset: SMOTED K=10, with knn's k=19
# Highest AUC of 0.741339875861314

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


