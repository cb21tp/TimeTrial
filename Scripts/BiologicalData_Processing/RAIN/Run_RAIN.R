
args <- commandArgs(trailingOnly = TRUE)
library("rain")
print(c(args[1],args[2],args[3],args[4],args[5],args[6],args[7]))
project <- args[1]

options(stringsAsFactors=FALSE)
data <- read.delim(args[2],row.names = 1)

results <- rain(t(data), deltat = as.numeric(args[4]), period = as.numeric(args[5]), adjp.method = "rawp",peak.border = c(as.numeric(args[6]),as.numeric(args[7])), verbose = F)
write.table(results,file=paste(args[3],paste0("RAIN_Results_",project),sep = ""),row.names=T,col.names=T,quote=F,sep="\t")

