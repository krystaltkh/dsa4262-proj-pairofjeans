install.packages("hash")
library(hash)

drach.map <- hash()

is.drach <- function(x){
  i <- 1
  while(nchar(substr(x,i,i+4)) == 5){
    s <- substr(x,i,i+4)
    if (has.key(key=s, hash=drach.map)){
      i <- i+1 
      next
    }
    letter3 <- substr(s,3,3)
    letter4 <- substr(s,4,4)
    if (letter3 != "A" & letter4 != "C") {
      drach.map[as.character(s)] <- 0
      i <- i+1
      next
    }
    letter1 <- substr(s,1,1)
    letter2 <- substr(s,2,2)
    letter5 <- substr(s,5,5)
    if (letter1 %in% c("A","G","U") & letter2 %in% c("A","G") & letter5 %in% c("A","C","T")) {
      drach.map[as.character(s)] <- 1
    }
    i <- i+1
  }
}
is.drach("AGGACTGAGGACTGUGACC")
drach.map

#keys(h)
#values(h)
#has.key(key = , hash =) 

sapply(unique(df.final$segment), is.drach)

