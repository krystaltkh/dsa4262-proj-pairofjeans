

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} else if (length(args)==1) {
  stop("Insufficient arguments supplied. Require (model.rds) and (sample data)", call.=FALSE)
} 

model <- readRDS(args[1])
df <- read.csv(args[2])
label <- as.data.frame(predict(model, newdata=df[5:28], type='response'))
preds <- cbind(df[1:4],label)
write.csv(preds, "predictions.csv")


###delete
# testt <- "sample-data.csv"
# d <- read.csv(testt)
# View(d[5:28])
# m <- readRDS("trained_rf.rds")
# label <- predict(m,d[5:28],type = 'response')
# cbind(d[1:4],as.data.frame(label))
