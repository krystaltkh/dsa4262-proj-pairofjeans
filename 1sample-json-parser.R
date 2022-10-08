library(jsonlite)
library(readr)
library(tidyr)
library(dplyr)

data <- sapply(readLines("../data/data.json"), fromJSON)
df <- data.frame(sapply(fromJSON(read_lines("../data/data.json", n_max=1, skip=55), flatten=TRUE), unlist, rec=FALSE))

df <- df %>% 
  mutate(
    "tr_id" = unlist(strsplit(names(df)[1], split="\\."))[1],
    "pos" = unlist(strsplit(names(df)[1], split="\\."))[2],
    "segment" = unlist(strsplit(names(df)[1], split="\\."))[3]) %>% 
  dplyr::rename(dwell_time1 = names(df)[1],
                current_sd1 = names(df)[2],
                current_mean1 = names(df)[3],
                dwell_time2 = names(df)[4],
                current_sd2 = names(df)[5],
                current_mean2 = names(df)[6],
                dwell_time3 = names(df)[7],
                current_sd3 = names(df)[8],
                current_mean3 = names(df)[9])

# cnames <- c("tr_id", "position", "segment", 
#             "dwell_time1", "current_sd1", "current_mean1", 
#             "dwell_time2", "current_sd2", "current_mean2", 
#             "dwell_time3", "current_sd3", "current_mean3")