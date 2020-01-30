#get Data Ready for Shiny App


## ---------------------------------------- Arser Concate -----------------------------------
#Reformate Arser Concatination Data
setwd("~/Desktop/TimeTrial/Results/Arser/con/")
Arser_Con_fileNames <- list.files(pattern="*.txt")
Arser_Concate <- lapply(Arser_Con_fileNames, function(x){read.csv(x, sep="")})

head(Arser_Concate[[1]])

#reduced to just the pvalue
Arser_Con_Reduced <- lapply(Arser_Concate, function(x){
  df <- x[,c("CycID","pvalue")]
  rownames(df) <- df$CycID
  df$CycID <- NULL
  df[is.na(df)] <- 1 ### cant fit FFT (ie. no occilation at all (flat line)), set value to 1
  df[df == 0] <- 2.2e-16 ### percision error, set value to 2.2e-16
  return(df)
})

#remove unwanted patterns from File Names
Arser_Con_fileNames <- gsub(pattern = ".txt",replacement = "",x = Arser_Con_fileNames)
Arser_Con_fileNames <- gsub(pattern = "Results_Concat_",replacement = "",x = Arser_Con_fileNames)
Arser_Con_fileNames <- gsub(pattern = "NoiseLV_",replacement = "",x = Arser_Con_fileNames)
Arser_Con_fileNames <- gsub(pattern = "BioRep_" ,replacement = "",x = Arser_Con_fileNames)
Arser_Con_fileNames <- paste0("Arser_Concat_",Arser_Con_fileNames)
Arser_Con <- do.call(cbind,Arser_Con_Reduced)
colnames(Arser_Con) <- Arser_Con_fileNames
head(Arser_Con)
head(Arser_Con_fileNames)
#removed unneeded varaibles
rm(Arser_Con_Reduced,Arser_Concate, Arser_Con_fileNames)
head(Arser_Con)

## ---------------------------------------- Arser Average -----------------------------------
setwd("~/Desktop/TimeTrial/Results/Arser/avg/")
Arser_Avg_fileNames <- list.files(pattern="*.txt")
Arser_Avgerage <- lapply(Arser_Avg_fileNames, function(x){read.csv(x, sep="")})

head(Arser_Avgerage[[1]])

#reduced to just the pvalue
Arser_Avg_Reduced <- lapply(Arser_Avgerage, function(x){
  df <- x[,c("CycID","pvalue")]
  rownames(df) <- df$CycID
  df$CycID <- NULL
  df[is.na(df)] <- 1 ### cant fit FFT (ie. no occilation at all (flat line)), set value to 1
  df[df == 0] <- 2.2e-16 ### percision error, set value to 2.2e-16
  return(df)
})

head(Arser_Avg_Reduced[[1]])
#remove unwanted patterns from File Names
Arser_Avg_fileNames <- gsub(pattern = ".txt",replacement = "",x = Arser_Avg_fileNames)
Arser_Avg_fileNames <- gsub(pattern = "Results_Avg_",replacement = "",x = Arser_Avg_fileNames)
Arser_Avg_fileNames <- gsub(pattern = "NoiseLV_",replacement = "",x = Arser_Avg_fileNames)
Arser_Avg_fileNames <- gsub(pattern = "BioRep_" ,replacement = "",x = Arser_Avg_fileNames)
Arser_Avg_fileNames <- paste0("Arser_Avg_",Arser_Avg_fileNames)
Arser_Avg <- do.call(cbind,Arser_Avg_Reduced)
head(Arser_Avg)
head(Arser_Avg_fileNames)
colnames(Arser_Avg) <- Arser_Avg_fileNames

#removed unneeded varaibles
rm(Arser_Avg_Reduced,Arser_Avgerage, Arser_Avg_fileNames)


## ---------------------------------------- BooteJTK Average -----------------------------------
setwd("~/Desktop/TimeTrial/Results/BooteJTK/avg/")
IDorder <- read.csv("~/Desktop/TimeTrial/Results/Arser/avg/Results_Avg_2_24_NoiseLV_0_BioRep_1.txt",sep = "")
IDorder <- IDorder$CycID
BooteJTK_Avg_fileNames <- list.files(pattern="*.txt")
BooteJTK_Avgerage <- lapply(BooteJTK_Avg_fileNames, function(x){read.csv(x, sep="")})

#reduced to just the pvalue
BooteJTK_Avg_Reduced <- lapply(BooteJTK_Avgerage, function(x){
  df <- x[,c("ID","GammaP")]
  rownames(df) <- df$ID
  df <- df[match(IDorder,rownames(df)),]
  df$ID <- NULL
  return(df)
})

#remove unwanted patterns from File Names
BooteJTK_Avg_fileNames <- gsub(pattern = ".txt",replacement = "",x = BooteJTK_Avg_fileNames)
BooteJTK_Avg_fileNames <- gsub(pattern = "_avg_boot25.*",replacement = "",x = BooteJTK_Avg_fileNames)
BooteJTK_Avg_fileNames <- gsub(pattern = "NoiseLV_",replacement = "",x = BooteJTK_Avg_fileNames)
BooteJTK_Avg_fileNames <- gsub(pattern = "BioRep_" ,replacement = "",x = BooteJTK_Avg_fileNames)
BooteJTK_Avg_fileNames <- paste0("BooteJTK_Avg_",BooteJTK_Avg_fileNames)
BooteJTK_Avg <- do.call(cbind,BooteJTK_Avg_Reduced)

head(BooteJTK_Avg_fileNames)
colnames(BooteJTK_Avg) <- BooteJTK_Avg_fileNames

#removed unneeded varaibles
rm(BooteJTK_Avg_Reduced,BooteJTK_Avgerage, BooteJTK_Avg_fileNames)


## ---------------------------------------- BooteJTK Concatenation -----------------------------------

#Reformate Arser Average Data
setwd("~/Desktop/TimeTrial/Results/BooteJTK/con/")
BooteJTK_Con_fileNames <- list.files(pattern="*.txt")
BooteJTK_Concate <- lapply(BooteJTK_Con_fileNames, function(x){read.csv(x, sep="")})

#reduced to just the pvalue
BooteJTK_Con_Reduced <- lapply(BooteJTK_Concate, function(x){
  df <- x[,c("ID","GammaP")]
  rownames(df) <- df$ID
  df <- df[match(IDorder,rownames(df)),]
  df$ID <- NULL
  return(df)
})

#remove unwanted patterns from File Names
BooteJTK_Con_fileNames <- gsub(pattern = ".txt",replacement = "",x = BooteJTK_Con_fileNames)
BooteJTK_Con_fileNames <- gsub(pattern = "_output_boot25.*",replacement = "",x = BooteJTK_Con_fileNames)
BooteJTK_Con_fileNames <- gsub(pattern = "NoiseLV_",replacement = "",x = BooteJTK_Con_fileNames)
BooteJTK_Con_fileNames <- gsub(pattern = "BioRep_" ,replacement = "",x = BooteJTK_Con_fileNames)
BooteJTK_Con_fileNames <- paste0("BooteJTK_Concat_",BooteJTK_Con_fileNames)
BooteJTK_Con <- do.call(cbind,BooteJTK_Con_Reduced)

head(BooteJTK_Con_fileNames)
colnames(BooteJTK_Con) <- BooteJTK_Con_fileNames

#removed unneeded varaibles
rm(BooteJTK_Con_Reduced,BooteJTK_Concate, BooteJTK_Con_fileNames)
head(BooteJTK_Con)

## ---------------------------------------- JTK_Cycle V3 -----------------------------------
setwd("~/Desktop/TimeTrial/Results/JTK/")
JTK_fileNames <- list.files(pattern="*.txt")
JTK_Cyc <- lapply(JTK_fileNames, function(x){read.csv(x, sep="")})

head(JTK_Cyc[[1]])

#reduced to just the pvalue
JTK_Cyc_Reduced <- lapply(JTK_Cyc, function(x){
  df <- x[,c("annot","ADJ.P")]
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
setwd("~/Desktop/TimeTrial/Results/Rain/")
load("./RainResults_RawP.Rdata")
rm(df.list,END,RUNTIME,START,choosePeakInterval,hms_span,makeSingleDF)



#reduced to just the pvalue
RAIN <- data.frame(matrix(rep(0,300*11000), ncol = 300, nrow = 11000))
colNamesRain <- rep(0,300)
rownames(RAIN) <- IDorder
count <- 1
for(x in 1:3){
    for(y in 1:5){
      for(z in 1:4){
        for(i in 1:5){
          
          #get the Analysis Results
          data <- RainAnalysis[[x]][[y]][[z]][[i]][[2]]
        
          #get the Name of the Sample
          name <- RainAnalysis[[x]][[y]][[z]][[i]][[1]]
          name <- gsub(pattern = "NoiseLV_",replacement = "",x = name)
          name <- gsub(pattern = "BioRep_" ,replacement = "",x = name)
          name <- paste0("RAIN_",name)
    
          RAIN[,count] <- data[,1]
          colNamesRain[count] <- name
          
          count <- count + 1 
      }
    }
  }
}

colnames(RAIN) <- colNamesRain
head(RAIN)

#removed unneeded varaibles
rm(list=setdiff(ls(), c("RAIN","Arser_Avg","Arser_Con","BooteJTK_Con","BooteJTK_Avg","JTK_Cycle")))

results <- list(Arser_Avg,Arser_Con,BooteJTK_Avg,BooteJTK_Con,JTK_Cycle,RAIN)
rm(RAIN,Arser_Avg,Arser_Con,BooteJTK_Con,BooteJTK_Avg,JTK_Cycle)

save.image("~/Desktop/TimeTrial/TimeTrial_Apps/App_Data/timeTrialSyntheticDataResults.RData")



