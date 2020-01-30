#get Data Ready for Shiny App


## ---------------------------------------- Arser Concate -----------------------------------
#Reformate Arser Concatination Data
setwd("~/Desktop/TimeTrial/Results/RealData/ARSER/")
Arser_fileNames <- list.files(pattern="*.txt")
Arsercate <- lapply(Arser_fileNames, function(x){read.csv(x, sep="")})

head(Arsercate[[1]])

#reduced to just the pvalue
Arser_Reduced <- lapply(Arsercate, function(x){
  df <- x[,c("CycID","fdr_BH")]
  rownames(df) <- df$CycID
  df$CycID <- NULL
  df[is.na(df)] <- 1 ### cant fit FFT (ie. no occilation at all (flat line)), set value to 1
  df[df == 0] <- 2.2e-16 ### percision error, set value to 2.2e-16
  return(df)
})

#remove unwanted patterns from File Names
Arser_fileNames <- gsub(pattern = ".txt",replacement = "",x = Arser_fileNames)
Arser_fileNames <- gsub(pattern = "Results_Concat_",replacement = "",x = Arser_fileNames)
Arser_fileNames <- gsub(pattern = "NoiseLV_",replacement = "",x = Arser_fileNames)
Arser_fileNames <- gsub(pattern = "BioRep_" ,replacement = "",x = Arser_fileNames)
Arser_fileNames <- paste0("Arsercat_",Arser_fileNames)
Arser <- do.call(cbind,Arser_Reduced)
colnames(Arser) <- Arser_fileNames
head(Arser)
head(Arser_fileNames)
#removed unneeded varaibles
rm(Arser_Reduced,Arsercate, Arser_fileNames)
head(Arser)

## ---------------------------------------- BooteJTK Average -----------------------------------

#Reformate Arser Average Data
setwd("~/Desktop/TimeTrial/Results/RealData/BooteJTK/")
IDorder <- read.csv("~/Desktop/TimeTrial/Results/RealData/ARSER/ARSER_Results_Hogenesch2009.all.data.ann.txt",sep = "")
IDorder <- IDorder$CycID
BooteJTK_fileNames <- list.files(pattern="*.txt")
BooteJTKerage <- lapply(BooteJTK_fileNames, function(x){read.csv(x, sep="")})

#reduced to just the pvalue
BooteJTK_Reduced <- lapply(BooteJTKerage, function(x){
  df <- x[,c("ID","GammaBH")]
  rownames(df) <- df$ID
  df <- df[match(IDorder,rownames(df)),]
  df$ID <- NULL
  return(df)
})

#remove unwanted patterns from File Names
BooteJTK_fileNames <- gsub(pattern = ".txt",replacement = "",x = BooteJTK_fileNames)
BooteJTK_fileNames <- gsub(pattern = "_avg_boot25.*",replacement = "",x = BooteJTK_fileNames)
BooteJTK_fileNames <- gsub(pattern = "NoiseLV_",replacement = "",x = BooteJTK_fileNames)
BooteJTK_fileNames <- gsub(pattern = "BioRep_" ,replacement = "",x = BooteJTK_fileNames)
BooteJTK_fileNames <- paste0("BooteJTK_",BooteJTK_fileNames)
BooteJTK <- do.call(cbind,BooteJTK_Reduced)

head(BooteJTK_fileNames)
colnames(BooteJTK) <- BooteJTK_fileNames

#removed unneeded varaibles
rm(BooteJTK_Reduced,BooteJTKerage, BooteJTK_fileNames)

## ---------------------------------------- JTK_Cycle V3 -----------------------------------
#Reformate Arser Concatination Data
setwd("~/Desktop/TimeTrial/Results/RealData/JTK/")
JTK_fileNames <- list.files(pattern="*.txt")
JTK_Cyc <- lapply(JTK_fileNames, function(x){read.csv(x, sep="")})

head(JTK_Cyc[[1]])

#reduced to just the pvalue
JTK_Cyc_Reduced <- lapply(JTK_Cyc, function(x){
  df <- x[,c("annot","BH.Q")]
  rownames(df) <- df$annot
  df$annot <- NULL
  return(df)
})

head(JTK_Cyc_Reduced[[1]])
#remove unwanted patterns from File Names
head(JTK_fileNames)
JTK_fileNames <- gsub(pattern = ".txt",replacement = "",x = JTK_fileNames)
JTK_fileNames <- gsub(pattern = "JTK_Results_",replacement = "",x = JTK_fileNames)
JTK_fileNames <- gsub(pattern = "NoiseLV_",replacement = "",x = JTK_fileNames)
JTK_fileNames <- gsub(pattern = "BioRep_" ,replacement = "",x = JTK_fileNames)
JTK_fileNames <- paste0("JTK_Cycle_",JTK_fileNames)
JTK_Cycle <- do.call(cbind,JTK_Cyc_Reduced)
colnames(JTK_Cycle) <- JTK_fileNames
head(JTK_Cycle)
head(JTK_fileNames)
#removed unneeded varaibles
rm(JTK_Cyc_Reduced,JTK_Cyc, JTK_fileNames)


## ---------------------------------------- RAIN -----------------------------------
setwd("~/Desktop/TimeTrial/Results/RealData/RAIN/")

RAIN_fileNames <- list.files(pattern="RAIN_Results_Adjusted_*")
RAIN_Cyc <- lapply(RAIN_fileNames, function(x){read.csv(x, sep="")})

head(RAIN_Cyc[[1]])

#reduced to just the pvalue
RAIN_Cyc_Reduced <- lapply(RAIN_Cyc, function(x){
  rownames <- rownames(x)
  df <- x[,c("pVal","phase")]
  rownames(df) <- rownames
  df$phase <- NULL
  return(df)
})

#remove unwanted patterns from File Names
RAIN <- do.call(cbind,RAIN_Cyc_Reduced)
colnames(RAIN) <- RAIN_fileNames
head(RAIN)

#removed unneeded varaibles
rm(list=setdiff(ls(), c("RAIN","Arser","BooteJTK_Con","BooteJTK","JTK_Cycle")))

resultsAdj <- list(Arser,BooteJTK,JTK_Cycle,RAIN)
rm(RAIN,Arser,BooteJTK,JTK_Cycle)

save.image("~/Desktop/TimeTrial/TimeTrial_Apps/App_Data/timeTrialAdjRealDataComplete.RData")
#load("~/Desktop/TimeTrial/Scripts/Figures/shinyRealData.RData")


