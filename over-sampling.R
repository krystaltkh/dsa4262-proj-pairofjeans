# install.packages("smotefamily")
# install.packages("ROSE")
library(smotefamily)
library(readr)
library(dplyr)
# library(ROSE)

set.seed(4262)

df <- read_csv("../data/train.csv")

# create unique_id for combining the over-sampled data back with original data later

df <- df %>% 
  filter(!is.na(gtf_rel_len))

# SMOTE, K = 5
smoted5 <- SMOTE(df[,-c(1:4)], df$label, K = 5)
smoted5_df <- smoted5$data %>%
  select(-class)

# SMOTE, K = 10
smoted10 <- SMOTE(df[,-c(1:4)], df$label, K = 10)
smoted10_df <- smoted10$data %>%
  select(-class)

# ADASYN, K = 5
adas5 <- ADAS(df[,-c(1:4)], df$label, K = 5)
adas5_df <- adas5$data %>%
  select(-class)

# ADASYN, K = 10
adas10 <- ADAS(df[,-c(1:4)], df$label, K = 10)
adas10_df <- adas10$data %>%
  select(-class)

# density-based SMOTE
dbsmoted <- DBSMOTE(df[,-c(1:4)], df$label)
dbsmoted_df <- dbsmoted$data %>%
  select(-class)



##### Can explore these after 1st submission #####

# over <- ovun.sample(label~., data=df, method="over", seed = 4262)
# over_df <- over$data
# 
# under <- ovun.sample(label~., data=df, method="under", seed=4262)
# under_df <- under$data
# 
# ovun <- ovun.sample(label~., data=df, method="both", seed=4262)
# ovun_df <- ovun$data