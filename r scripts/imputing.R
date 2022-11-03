####### IMPUTE MISSING DATA #########
# missForest to impute missing data
library(missForest)
library(readr)
train <- read_csv("../data/train0.csv")
train <- as.data.frame(train)
train_x <- missForest(train[, c(5:18)])$ximp
train_x_final <- cbind(train[, c(1:4)], train_x, train$label)
write_csv(train_x_final, "../data/imputed_train0.csv")

test <-  read_csv("../data/test0.csv")
test <- as.data.frame(test)
test_x <- missForest(test[, c(5:18)])$ximp
test_x_final <- cbind(test[, c(1:4)], test_x, test$label)
write_csv(test_x_final, "../data/imputed_test0.csv")

# impute dataset1, as it has NA rows
dataset1 <- read_csv("../data/dataset1.csv")
dataset1 <- as.data.frame(dataset1)
imp_dataset1 <- missForest(dataset1[, c(4:17)])$ximp
imp_dataset1_final <- cbind(dataset1[, c(1:3)], imp_dataset1)
write_csv(imp_dataset1_final, "../data/imputed_dataset1.csv")