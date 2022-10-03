library(jsonlite)
library(tidyr)
library(dplyr)

con <- file("../data/data.json", open="r")
df.final <- data.frame()

while (length(line <- readLines(con, n=1)) > 0) {
  df <- data.frame(sapply(fromJSON(line, flatten=TRUE), unlist, rec=FALSE))

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
           current_mean3 = names(df)[9]) %>%
    select(tr_id, pos, segment, everything()) 
  
  df.final <- rbind(df.final, df)
}
close(con)
# TO SAVE DF.FINAL AFTER PARSING
# write.csv(df.final, "../data/parsedData.csv", row.names = FALSE)

# cnames <- c("tr_id", "position", "segment", 
#             "dwell_time1", "current_sd1", "current_mean1", 
#             "dwell_time2", "current_sd2", "current_mean2", 
#             "dwell_time3", "current_sd3", "current_mean3")

# duplicated(df.final) # to check for duplicate rows in case json parser fails
