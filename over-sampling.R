install.packages("smotefamily")
library(smotefamily)
library(readr)
library(dplyr)

set.seed(4262)

df <- read_csv("../data/train.csv")

# create unique_id for combining the over-sampled data back with original data later

df <- df %>% 
  mutate(unique_id = row_number()) %>%
  filter(!is.na(gtf_rel_len))

# SMOTE, K = 5
smoted5 <- SMOTE(df[,-c(1, 2, 4)], df$label, K = 5)
smoted5_df <- smoted5$data %>%
  full_join(df[, c(1, 2, 19)], by = "unique_id") %>%
  select(-c(class))

# SMOTE, K = 10
smoted10 <- SMOTE(df[,-c(1, 2, 4)], df$label, K = 10)
smoted10_df <- smoted10$data %>%
  full_join(df[, c(1, 2, 19)], by = "unique_id") %>%
  select(-c(class))

# ADASYN, K = 5
adas5 <- ADAS(df[,-c(1, 2, 4)], df$label, K = 5)
adas5_df <- adas5$data %>%
  full_join(df[, c(1, 2, 19)], by = "unique_id") %>%
  select(-c(class))

# ADASYN, K = 10
adas10 <- ADAS(df[,-c(1, 2, 4)], df$label, K = 10)
adas10_df <- adas10$data %>%
  full_join(df[, c(1, 2, 19)], by = "unique_id") %>%
  select(-c(class))

# density-based SMOTE
dbsmoted <- DBSMOTE(df[,-c(1, 2, 4)], df$label)
dbsmoted_df <- dbsmoted$data %>%
  full_join(df[, c(1, 2, 19)], by = "unique_id") %>%
  select(-c(class))
