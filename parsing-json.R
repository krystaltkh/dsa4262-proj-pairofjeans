library(jsonlite)
library(tidyr)
library(dplyr)

# Checks if 5mer follows DRACH motif
is.drach <- function(x){
  if (substr(x,3,3) == "A" & substr(x,4,4) == "C") {
    if (substr(x,1,1) %in% c("A","G","U") & substr(x,2,2) %in% c("A","G") & substr(x,5,5) %in% c("A","C","T")) {
      return(1)
    }
  }
  return(0)
}

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
    rowwise() %>% 
    mutate("rowmean_dwell_time" = mean(c(dwell_time1,dwell_time2,dwell_time3)),
           "rowmean_current_sd" = mean(c(current_sd1,current_sd2,current_sd3)),
           "rowmean_current_mean" = mean(c(current_mean1,current_mean2,current_mean3))) %>%
    mutate("DRACH1" = is.drach(substr(segment,1,5)), 
           "DRACH2"=is.drach(substr(segment,2,6)), 
           "DRACH3"=is.drach(substr(segment,3,7))) %>%
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

# duplicated(df.final) # to check for duplicate rows. extremely rare for duplicates
