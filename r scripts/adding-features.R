library(jsonlite)
library(tidyr)
library(dplyr)

labels <- read.csv("../data/data.info")
dataset <- read.csv("../data/imputed_test0.csv")

# Feature engineering
# functions required to find the prop of nucleotide bases in each segment 
find_prop_A <- function(y) {
  total = 0
  input <- y
  for (x in 1:7) {
    if (substr(input,x,x) == "A") {
      total = total + 1
    }
  }
  return(total/7)
}

find_prop_C <- function(y) {
  total = 0
  input <- y
  for (x in 1:7) {
    if (substr(input,x,x) == "C") {
      total = total + 1
    }
  }
  return(total/7)
}

find_prop_G <- function(y) {
  total = 0
  input <- y
  for (x in 1:7) {
    if (substr(input,x,x) == "G") {
      total = total + 1
    }
  }
  return(total/7)
}

find_prop_T <- function(y) {
  total = 0
  input <- y
  for (x in 1:7) {
    if (substr(input,x,x) == "T") {
      total = total + 1
    }
  }
  return(total/7)
}

df.final <- dataset %>% 
  rowwise() %>%
  mutate(prop_A = find_prop_A(segment),
         prop_C = find_prop_C(segment),
         prop_G = find_prop_G(segment),
         prop_T = find_prop_T(segment),
         leftA = ifelse(substr(segment,1,1) == 'A',1,0),
         leftC = ifelse(substr(segment,1,1) == 'C',1,0),
         leftT = ifelse(substr(segment,1,1) == 'T',1,0),
         leftG = ifelse(substr(segment,1,1) == 'G',1,0),
         rightA = ifelse(substr(segment,7,7) == 'A',1,0),
         rightC = ifelse(substr(segment,7,7) == 'C',1,0),
         rightT = ifelse(substr(segment,7,7) == 'T',1,0),
         rightG = ifelse(substr(segment,7,7) == 'G',1,0)) %>% 
  rowwise() %>% # functions will be applied by row
  mutate(five_segment = substr(segment, 2, 6))

# create polymer combination dataframe, count frequency of combination
combi <- df.final %>%
  rowwise() %>%
  select(five_segment) %>%
  group_by(five_segment) %>%
  count()

df.final <- df.final %>%
  # left join df.final with combi, on "five_segment" column
  left_join(combi, by="five_segment") %>%
  # calculate freqRatio of combination
  mutate(freqRatio = n / nrow(df.final)) %>%
  # select out five_segment, and n count
  select(-c(five_segment, n))

########## USE ONLY WITH CORRECT GTF FILE ############
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

# find the position wrt gtf file
df.final <- merge(df.final, gtf.final[c("tr_id","total_len")], by="tr_id", all.x = TRUE) %>%
  mutate(gtf_rel_len = pos/total_len)

#####################################################

# to add labels to dataset
df.final <- df.final %>%
  # joining label to each (transcript_id, position)
  merge(x=.,y=labels,by.x=c("tr_id","pos"), by.y=c("transcript_id","transcript_position"),all.x=TRUE) %>%
  # find the relative position wrt the max pos of that tr_id
  mutate(pos = as.numeric(pos)) %>%
  group_by(tr_id) %>% 
  mutate(rel_pos_max = pos/max(pos)) %>%
  # final select() to arrange cols and leave label as last col
  select(gene_id, tr_id, pos, segment, everything(), label) 

# TO SAVE DF.FINAL AFTER PARSING
write.csv(df.final, "../data/test0_final.csv", row.names = FALSE)
# duplicated(df.final) # to check for duplicate rows. extremely rare for duplicates
