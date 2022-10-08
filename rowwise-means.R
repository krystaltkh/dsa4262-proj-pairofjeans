library(dplyr)

data <- read.csv("../data/parsedData.csv")
data <- data[1:100,]

data %>%
  rowwise() %>%
  mutate("rowmean_dwell_time" = mean(c(dwell_time1,dwell_time2,dwell_time3)),
         "rowmean_current_sd" = mean(c(current_sd1,current_sd2,current_sd3)),
         "rowmean_current_mean" = mean(c(current_mean1,current_mean2,current_mean3))) %>%
  View()

# mean across reads
data %>%
  group_by(tr_id, pos, segment) %>%
  dplyr::summarize("tr_mean_dwelltime" = mean(c(dwell_time1)))

