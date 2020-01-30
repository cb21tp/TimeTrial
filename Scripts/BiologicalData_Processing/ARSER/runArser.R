library("gnm")
args <- commandArgs(trailingOnly = TRUE)

source("arser.R")
data <- read.delim(args[2],header=FALSE, stringsAsFactors=FALSE)
rownames <- data[2:dim(data)[1],1]

timePts <- data[1,2:(dim(data)[2])]
timePts <- as.numeric(gsub(pattern = "ZT",replacement = "",x = timePts))
data <- data[2:dim(data)[1],]
data <- data.frame(apply(data, 2, function(x) as.numeric(as.character(x))))
data[,1] <- rownames
output <- runARS(indata = data,ARStime = timePts)

write.table(x = output, file = paste0(args[3],"ARSER_Results_",args[1]))
