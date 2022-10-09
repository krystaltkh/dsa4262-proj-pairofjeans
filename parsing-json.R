library(jsonlite)
library(tidyr)
library(dplyr)

# gtf file (after converting to csv)
gtf_file <- read.csv("../data/hg38_sequins_SIRV_ERCCs_longSIRVs_v5_reformatted.csv")

# prepping the gtf table to join with the data
# this table returns the length of the transcript_id based on gtf file 
gtf.final <- gtf_file %>% 
  filter(feature == "exon") %>% 
  select(start, end, gene_id, transcript_id) %>% 
  mutate(length = end-start) %>% 
  group_by(gene_id, transcript_id) %>% 
  summarise(total_len = sum(length))
colnames(gtf.final)[2] <- "tr_id"

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
  mutate(dwell_time_diff1 = dwell_time2 - dwell_time1,
         dwell_time_diff2 = dwell_time3 - dwell_time2,
         current_mean_diff1 = current_mean2 - current_mean1,
         current_mean_diff2 = current_mean3 - current_mean2,
         weighted_current1 = current_mean1 * dwell_time1,
         weighted_current2 = current_mean2 * dwell_time2,
         weighted_current3 = current_mean3 * dwell_time3) %>%
  group_by(tr_id, pos, segment) %>%
  dplyr::summarize("num_reads" = n(),
                   "mean_dwell_time1" = mean(dwell_time1),
                   #"mean_current_sd1" = mean(current_sd1),
                   "combined_sd1" = sqrt(sum((current_sd1**2 + (current_mean1 - mean(current_mean1))**2))/n()),
                   "mean_current1" = mean(current_mean1),
                   "mean_dwell_time2" = mean(dwell_time2),
                   #"mean_current_sd2" = mean(current_sd2),
                   "combined_sd2" = sqrt(sum((current_sd2**2 + (current_mean2 - mean(current_mean2))**2))/n()),
                   "mean_current2" = mean(current_mean2),
                   "mean_dwell_time3" = mean(dwell_time3),
                   #"mean_current_sd3" = mean(current_sd3),
                   "combined_sd3" = sqrt(sum((current_sd3**2 + (current_mean3 - mean(current_mean3))**2))/n()),
                   "mean_current3" = mean(current_mean3),
                   "mean_dwell_time_diff1" = mean(dwell_time_diff1),
                   "mean_dwell_time_diff2" = mean(dwell_time_diff2),
                   "mean_current_diff1" = mean(current_mean_diff1),
                   "mean_current_diff2" = mean(current_mean_diff2),
                   "weighted_avg_current1" = sum(weighted_current1) / sum(dwell_time1),
                   "weighted_avg_current2" = sum(weighted_current2) / sum(dwell_time2),
                   "weighted_avg_current3" = sum(weighted_current3) / sum(dwell_time3)) %>%
  rowwise() %>%
  mutate("DRACH1" = is.drach(substr(segment,1,5)), 
         "DRACH2"=is.drach(substr(segment,2,6)), 
         "DRACH3"=is.drach(substr(segment,3,7))) %>%
  ungroup() %>%
  # joining label to each (transcript_id, position)
  merge(x=.,y=labels,by.x=c("tr_id","pos"), by.y=c("transcript_id","transcript_position"),all.x=TRUE) %>%
  # find the relative position wrt the max pos of that tr_id
  mutate(pos = as.numeric(pos)) %>%
  group_by(tr_id) %>% 
  mutate(rel_pos_max = pos/max(pos)) %>%
  # final select() to arrange cols and leave label as last col
  select(gene_id, tr_id, pos, segment, everything(), label)

# find the position wrt gtf file
df.final <- merge(df.final, gtf.final[c("tr_id","total_len")], by="tr_id", all.x = TRUE) %>%
  mutate(gtf_rel_len = pos/total_len)

# TO SAVE DF.FINAL AFTER PARSING
# write.csv(df.final, "../data/parsedData.csv", row.names = FALSE)

# cnames <- c("tr_id", "position", "segment", 
#             "dwell_time1", "current_sd1", "current_mean1", 
#             "dwell_time2", "current_sd2", "current_mean2", 
#             "dwell_time3", "current_sd3", "current_mean3")

# duplicated(df.final) # to check for duplicate rows. extremely rare for duplicates
