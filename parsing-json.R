library(jsonlite)

data <- sapply(readLines("../data/data.json"), fromJSON)
