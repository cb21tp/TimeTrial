
#converts RMA normalized Affy Micro Array Txt files to a list of files in a dataframe
data_fileNames <- list.files(pattern="*.txt")
data <- lapply(data_fileNames, function(x){read.csv(x, sep="",header = T,row.names = 1)})
names(data) <-gsub(pattern = ".data.ann.txt",replacement = "", x = data_fileNames)
rm(data_fileNames)
save.image(file = "~/Desktop/TimeTrial/TimeTrial_Apps/timeTrialProcessTimeSeriesComplete.RData")


   