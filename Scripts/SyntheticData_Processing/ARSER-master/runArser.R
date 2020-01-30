library("gnm")
args <- commandArgs(trailingOnly = TRUE)

source("arser.R")
data <- read.delim(args[2], header=FALSE, stringsAsFactors=FALSE)
timePts <- data[1,2:dim(data)[2]]
data <- data[2:dim(data)[1],]
timePts <- as.numeric(unname(timePts))
output <- runARS(data,timePts)

write.table(x = output, file = args[3])
