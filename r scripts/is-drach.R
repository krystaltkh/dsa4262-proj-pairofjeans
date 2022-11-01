library(dplyr)

is.drach <- function(x){
  if (substr(x,3,3) == "A" & substr(x,4,4) == "C") {
    if (substr(x,1,1) %in% c("A","G","U") & substr(x,2,2) %in% c("A","G") & substr(x,5,5) %in% c("A","C","T")) {
      return(1)
    }
  }
  return(0)
}

data <- read.csv("../data/parsedData.csv")
data <- data[1:200,]

data %>% 
  rowwise() %>% 
  mutate("DRACH1" = is.drach(substr(segment,1,5)), 
         "DRACH2"=is.drach(substr(segment,2,6)), 
         "DRACH3"=is.drach(substr(segment,3,7))) %>%
  View()
