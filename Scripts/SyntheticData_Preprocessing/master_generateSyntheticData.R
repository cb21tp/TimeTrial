#Source Files For Generating Synthetic Data Set
setwd("~/Desktop/TimeTrial/Scripts/SyntheticData_Preprocessing")

source("SyntheticData.R")
rm(list=ls())
source("CollateReplicateDataFrames.R")
rm(list=ls())
source("AvgReplicateDataFrames.R")
rm(list=ls())
source("CreateSingleDataFrame.R")
rm(list=ls())
source("generateCSVDataFilesForEachExperiment.R")
rm(list=ls())



