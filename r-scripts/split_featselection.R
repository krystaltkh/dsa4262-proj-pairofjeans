library(readr)
library(dplyr)
library(corrplot)
library(tidyverse)
library(caret)
library(leaps)
library(MASS)

# downloading parsed data and split into train-test sets
df <- read_csv("./data-features.csv")
View(df)
sample <- sample(c(1, 0), length(unique(df$gene_id)), replace=TRUE, prob=c(0.7,0.3))
tmpdf <- data.frame(unique(df$gene_id),sample)
colnames(tmpdf)[1] = "gene_id"
df <- merge(df, tmpdf)
train <- df[df$sample == 1,]
test <- df[df$sample == 0,]
nrow(test)
train <- train[-c(19)]
test <- test[-c(19)]
head(train)

# export split data
write.csv(train,"train.csv", row.names = FALSE)
write.csv(test,"test.csv", row.names = FALSE)

# load data
train <- read_csv("train.csv")
View(train)
summary(train)
y <- train['label']
X <-train[,5:17]
m <- cor(X[-c(10,12,13)])
corrplot(m, method="color")

# attempt ANOVA
model <- lm(label ~ mean_time1 + mean_time2 + mean_time3 + 
               combined_sd1 + combined_sd2 + combined_sd3 +
               mean_cur1 + mean_cur2 + mean_cur3 +
               DRACH1 + DRACH2 + DRACH3 + gtf_rel_len, data = train)
anova(model)

# attempt STEPWISE SELECTION (AIC, mixed selection)
# Fit the full model STEPWISE SELECTION
full.model <- lm(label ~ mean_time1 + mean_time2 + mean_time3 + 
                   combined_sd1 + combined_sd2 + combined_sd3 +
                   mean_cur1 + mean_cur2 + mean_cur3 +
                   DRACH1 + DRACH2 + DRACH3 + gtf_rel_len, data = train)
# indicates that DRACH1, DRACH3 should be removed

# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

# remove combined_sd3
model2 <- lm(label ~ mean_time1 + mean_time2 + mean_time3 + 
                   combined_sd1 + combined_sd2 +
                   mean_cur1 + mean_cur2 + mean_cur3 +
                   DRACH1 + DRACH2 + DRACH3 + gtf_rel_len, data = train)

step.model <- stepAIC(model2, direction = "both", 
                      trace = FALSE)
summary(step.model)
# indicates DRACH1, DRACH3, combined_sd3 should be removed
