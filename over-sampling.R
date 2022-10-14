# install.packages("smotefamily")
# install.packages("ROSE")
library(smotefamily)
library(readr)
library(dplyr)
library(ROSE)

set.seed(4262)

df <- read_csv("../data/train.csv")

# create unique_id for combining the over-sampled data back with original data later

df <- df %>% 
  filter(!is.na(gtf_rel_len))


over <- ovun.sample(label~., data=df, method="over", seed = 4262)
over_df <- over$data

under <- ovun.sample(label~., data=df, method="under", seed=4262)
under_df <- under$data

ovun <- ovun.sample(label~., data=df, method="both", seed=4262)
ovun_df <- ovun$data
