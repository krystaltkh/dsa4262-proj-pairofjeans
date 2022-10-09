library(jsonlite)
library(tidyr)
library(dplyr)

# DRACH motif check, returns 1 if true 0 if false
is.drach <- function(x){
  if (substr(x,3,3) == "A" & substr(x,4,4) == "C") {
    if (substr(x,1,1) %in% c("A","G","U") & substr(x,2,2) %in% c("A","G") & substr(x,5,5) %in% c("A","C","T")) {
      return(1)
    }
  }
  return(0)
}

#JSON parser
labels <- read.csv("../data/data.info")
con <- file("../data/data.json", open="r")
df.full <- data.frame()

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
  
  df.full <- rbind(df.full, df)
}
close(con)

# Feature engineering
df.final <-  df.full %>%
  group_by(tr_id, pos, segment) %>%
  dplyr::summarize("num_reads" = n(),
                   "mean_dwell_time1" = mean(dwell_time1),
                   "mean_current_sd1" = mean(current_sd1),
                   "mean_current_mean1" = mean(current_mean1),
                   "mean_dwell_time2" = mean(dwell_time2),
                   "mean_current_sd2" = mean(current_sd2),
                   "mean_current_mean2" = mean(current_mean2),
                   "mean_dwell_time3" = mean(dwell_time3),
                   "mean_current_sd3" = mean(current_sd3),
                   "mean_current_mean3" = mean(current_mean3)) %>%
  mutate("DRACH1" = is.drach(substr(segment,1,5)), 
         "DRACH2"=is.drach(substr(segment,2,6)), 
         "DRACH3"=is.drach(substr(segment,3,7))) %>%
  ungroup() %>%
  # joining label to each (transcript_id, position)
  merge(x=.,y=labels,by.x=c("tr_id","pos"), by.y=c("transcript_id","transcript_position"),all.x=TRUE) %>%
  select(gene_id, tr_id, pos, segment, everything(), label)


# TO SAVE DF.FINAL AFTER PARSING
# write.csv(df.final, "../data/parsedData.csv", row.names = FALSE)

# cnames <- c("tr_id", "position", "segment", 
#             "dwell_time1", "current_sd1", "current_mean1", 
#             "dwell_time2", "current_sd2", "current_mean2", 
#             "dwell_time3", "current_sd3", "current_mean3")

# duplicated(df.final) # to check for duplicate rows. extremely rare for duplicates
