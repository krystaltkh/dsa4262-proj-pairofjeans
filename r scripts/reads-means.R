library(dplyr)

data <- read.csv("../data/parsedData.csv")
data <- data[1:500,]

data %>%
  rowwise() %>%
  mutate("rowmean_dwell_time" = mean(c(dwell_time1,dwell_time2,dwell_time3)),
         "rowmean_current_sd" = mean(c(current_sd1,current_sd2,current_sd3)),
         "rowmean_current_mean" = mean(c(current_mean1,current_mean2,current_mean3))) %>%
  View()

# mean across reads
data %>%
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
                   "mean_current_mean3" = mean(current_mean3)) %>% View()

