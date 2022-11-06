
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} else if (length(args)==1) {
  stop("Insufficient arguments supplied. Require (model.rds) and (sample data)", call.=FALSE)
} 
# .libPaths("/usr/lib/R/library")
# install.packages("https://cran.r-project.org/src/contrib/Archive/randomForest/randomForest_4.6-14.tar.gz",repos=NULL, type="source")
# library(randomForest)

model <- readRDS(args[1])
df <- read.csv(args[2])
confirm <- args[3]
tryCatch(
  {
    label <- as.data.frame(predict(model, newdata=df[5:28], type='response'))
    preds <- cbind(df[1:4],label)
    write.csv(preds, "predictions.csv")
  },
  error=function(cond) {
    message(cond)
    cat("\n")
  },
  warning=function(cond) {
    message("Here's the original warning message:")
    message(cond)
  }
)
try(
  {
    if (confirm=='y') {
      .libPaths("/usr/lib/R/library")
      install.packages("https://cran.r-project.org/src/contrib/Archive/randomForest/randomForest_4.6-14.tar.gz",repos=NULL, type="source")
      library(randomForest)
      label <- as.data.frame(predict(model, newdata=df[5:28], type='response'))
      preds <- cbind(df[1:4],label)
      write.csv(preds, "predictions.csv")
    }
  },
  error=function(e){
    cat("Specify y/n for third argument to install compatible randomForest package.\n")
  }
)


# preds <- cbind(df[1:4],label)
# write.csv(preds, "predictions.csv")
