#Script Used for Generating the 300 Experiments for use by BooteJTK, ARSER, and Sw1PerS
#Load Rain Data
reps <- list(1,2,3)
lengths <- list(24,36,48,72,96)
splRate <- list(2,4,6,8)

#Create list of subsamples to Use and Run Using Rain
#Data Ordered by
#Reps ie. [[Reps]]
#Lengths ie. [[Reps]][[Length]]
#SampleRate ie. [[Reps]][[Length]][[SampleRate]]
#Percent Noise ie. [[Reps]][[Length]][[SampleRate]][[Noise]][[OutputDataFrame]]

#[[OutputDataFrame]]
#runName
#results
#time
load("~/Desktop/TimeTrial/Data/Raw/RawData_Con.Rdata")

  OutputCONCATE <- 
  lapply(reps, function(rep){
    lapply(lengths, function(lng){
      lapply(splRate, function(smps){
        timePoints <- seq(from = 0, to = lng, by = smps) #get sequence of timePoints
        dfName <- names(df.list) #get Names of Data Frame to Use
        dfTemp <- df.list[which(as.numeric(substr(dfName,nchar(dfName), nchar(dfName))) == rep)] #split by rep
        # print(c(rep, lng,smps))
        
        lapply(seq_along(dfTemp), function(x){
          data <- dfTemp[[x]]
          runName <- paste(smps,lng,names(dfTemp)[[x]],sep = "_") #create a Name for the sample Run
          # print(runName)
          last <- length(timePoints)
          #Select the correct Data to Use for the analysis
          if(rep == 3){
            tpInd <- c(timePoints[-last],timePoints[-last],timePoints)
            repInd <- c(rep(1, length(timePoints)-1),rep(2, length(timePoints)-1),rep(3,length(timePoints)))
          }
          
          if(rep == 2){
            tpInd <- c(timePoints[-last],timePoints)
            repInd <- c(rep(1, length(timePoints)-1),rep(2,length(timePoints)))
          }
          if(rep == 1){
            tpInd <- timePoints
            repInd <- c(rep(1, length(timePoints)))
          }
          
          dataToUse <- data[,paste("ZT_",tpInd,"_",repInd,sep = "")]
          # print(colnames(dataToUse))
          
          if(rep == 2){
            tpInd <- tpInd + c(rep(0,floor(length(tpInd)/2)), rep(max(tpInd),ceiling(length(tpInd)/2)))
          }
          if(rep ==3){
            tpInd <- tpInd + c(rep(0,floor(length(tpInd)/3)), rep(max(tpInd),floor(length(tpInd)/3)),rep(2*(max(tpInd)),ceiling(length(tpInd)/3)))
          }
          #colnames(dataToUse) <- paste("ZT",tpInd,sep = "")
          colnames(dataToUse) <- tpInd
          
          #Add Column for Names of Samples
          dataToUse$'#' <- rownames(dataToUse)
          dataToUse <- dataToUse[c(dim(dataToUse)[2],1:(dim(dataToUse)[2]-1))]
          # print(colnames(dataToUse))
          
          write.table(dataToUse, file = paste("../../Data/Processed/Arser/Concat_Experiments/",runName,".txt", sep = ""),  sep="\t",row.names = FALSE, col.names  = T, quote=FALSE)
        })
      })
    })
  })


  load("~/Desktop/TimeTrial/Data/Raw/RawData_Con.Rdata")
  
  OutputCONCATE <- 
    lapply(reps, function(rep){
      lapply(lengths, function(lng){
        lapply(splRate, function(smps){
          timePoints <- seq(from = 0, to = lng, by = smps) #get sequence of timePoints
          dfName <- names(df.list) #get Names of Data Frame to Use
          dfTemp <- df.list[which(as.numeric(substr(dfName,nchar(dfName), nchar(dfName))) == rep)] #split by rep
          # print(c(rep, lng,smps))
          
          lapply(seq_along(dfTemp), function(x){
            data <- dfTemp[[x]]
            runName <- paste(smps,lng,names(dfTemp)[[x]],sep = "_") #create a Name for the sample Run
            # print(runName)
            last <- length(timePoints)
            #Select the correct Data to Use for the analysis
            if(rep == 3){
              tpInd <- c(timePoints[-last],timePoints[-last],timePoints)
              repInd <- c(rep(1, length(timePoints)-1),rep(2, length(timePoints)-1),rep(3,length(timePoints)))
            }
            
            if(rep == 2){
              tpInd <- c(timePoints[-last],timePoints)
              repInd <- c(rep(1, length(timePoints)-1),rep(2,length(timePoints)))
            }
            if(rep == 1){
              tpInd <- timePoints
              repInd <- c(rep(1, length(timePoints)))
            }
            
            dataToUse <- data[,paste("ZT_",tpInd,"_",repInd,sep = "")]
            # print(colnames(dataToUse))
            
            if(rep == 2){
              tpInd <- tpInd + c(rep(0,floor(length(tpInd)/2)), rep(max(tpInd),ceiling(length(tpInd)/2)))
            }
            if(rep ==3){
              tpInd <- tpInd + c(rep(0,floor(length(tpInd)/3)), rep(max(tpInd),floor(length(tpInd)/3)),rep(2*(max(tpInd)),ceiling(length(tpInd)/3)))
            }
            colnames(dataToUse) <- paste("ZT",tpInd,sep = "")
            
            #Add Column for Names of Samples
            dataToUse$'#' <- rownames(dataToUse)
            dataToUse <- dataToUse[c(dim(dataToUse)[2],1:(dim(dataToUse)[2]-1))]
            # print(colnames(dataToUse))
            
            write.table(dataToUse, file = paste("../../Data/Processed/BooteJTK/Concat_Experiments/",runName,".txt", sep = ""),  sep="\t", row.names = FALSE, col.names  = T, quote=FALSE)
          })
        })
      })
    })
  
  load("~/Desktop/TimeTrial/Data/Raw/RawData_Avg.Rdata")
  OutputAVG <- 
  lapply(reps, function(rep){
    lapply(lengths, function(lng){
      lapply(splRate, function(smps){
        timePoints <- seq(from = 0, to = lng, by = smps) #get sequence of timePoints
        dfName <- names(df.list) #get Names of Data Frame to Use
        dfTemp <- df.list[which(as.numeric(substr(dfName,nchar(dfName), nchar(dfName))) == rep)] #split by rep
        # print(c(rep, lng,smps))
        #lapply(seq_along(dfTemp), function(x){
        lapply(seq_along(dfTemp), function(x){
          data <- dfTemp[[x]] #get the data
          runName <- paste(smps,lng,names(dfTemp)[[x]],sep = "_") #create a Name for the sample Run
          # print(runName)
          
          #Select the correct Data to Use for the analysis
          tpInd <- timePoints
          
          repInd <- rep(1,length(timePoints))
          dataToUse <- data[,paste("ZT_",tpInd,"_",repInd,sep = "")]
          # print(colnames(dataToUse))
          
          colnames(dataToUse) <- paste("ZT",tpInd,sep = "")
          #colnames(dataToUse) <- tpInd
          
          
          #Add Column for Names of Samples
          dataToUse$'#' <- rownames(dataToUse)
          dataToUse <- dataToUse[c(dim(dataToUse)[2],1:(dim(dataToUse)[2]-1))]
          # print(colnames(dataToUse))
          
          write.table(dataToUse, file = paste("../../Data/Processed/BooteJTK/Avg_Experiments/",runName,".txt", sep = ""), sep = "\t", row.names = F, col.names  = T, quote=FALSE)
        })
      })
    })
  })

  load("~/Desktop/TimeTrial/Data/Raw/RawData_Avg.Rdata")
  OutputAVG <- 
    lapply(reps, function(rep){
      lapply(lengths, function(lng){
        lapply(splRate, function(smps){
          timePoints <- seq(from = 0, to = lng, by = smps) #get sequence of timePoints
          dfName <- names(df.list) #get Names of Data Frame to Use
          dfTemp <- df.list[which(as.numeric(substr(dfName,nchar(dfName), nchar(dfName))) == rep)] #split by rep
          # print(c(rep, lng,smps))
          #lapply(seq_along(dfTemp), function(x){
          lapply(seq_along(dfTemp), function(x){
            data <- dfTemp[[x]] #get the data
            runName <- paste(smps,lng,names(dfTemp)[[x]],sep = "_") #create a Name for the sample Run
            # print(runName)
            
            #Select the correct Data to Use for the analysis
            tpInd <- timePoints
            
            repInd <- rep(1,length(timePoints))
            dataToUse <- data[,paste("ZT_",tpInd,"_",repInd,sep = "")]
            # print(colnames(dataToUse))
            
            #colnames(dataToUse) <- paste("ZT",tpInd,sep = "")
            colnames(dataToUse) <- tpInd
            
            
            #Add Column for Names of Samples
            dataToUse$'#' <- rownames(dataToUse)
            dataToUse <- dataToUse[c(dim(dataToUse)[2],1:(dim(dataToUse)[2]-1))]
            # print(colnames(dataToUse))
            
            write.table(dataToUse, file = paste("../../Data/Processed/Arser/Avg_Experiments/",runName,".txt", sep = ""), sep = "\t", row.names = F, col.names  = T, quote=FALSE)
          })
        })
      })
    })


  load("~/Desktop/TimeTrial/Data/Raw/RawData_Con.Rdata")
  OutputAVG <- 
    lapply(reps, function(rep){
      lapply(lengths, function(lng){
        lapply(splRate, function(smps){
          timePoints <- seq(from = 0, to = lng, by = smps) #get sequence of timePoints
          dfName <- names(df.list) #get Names of Data Frame to Use
          dfTemp <- df.list[which(as.numeric(substr(dfName,nchar(dfName), nchar(dfName))) == rep)] #split by rep
          # print(c(rep, lng,smps))
          #lapply(seq_along(dfTemp), function(x){
          lapply(seq_along(dfTemp), function(x){
            data <- dfTemp[[x]] #get the data
            runName <- paste(smps,lng,names(dfTemp)[[x]],sep = "_") #create a Name for the sample Run
            # print(runName)
            
            #Select the correct Data to Use for the analysis
            
            
            if(rep == 3){
              repInd <- rep(c(1,2,3),length(timePoints))
              tpInd <- rbind(timePoints,timePoints,timePoints)
            }
            if(rep == 2){
              repInd <- rep(c(1,2),length(timePoints))
              tpInd <- rbind(timePoints,timePoints)
            }
            if(rep == 1){
              repInd <- rep(1,length(timePoints))
              tpInd <- timePoints
            }
            
            dataToUse <- data[,paste("ZT_",tpInd,"_",repInd,sep = "")]
            # print(colnames(dataToUse))
            
            colnames(dataToUse) <- paste("ZT",tpInd,"_rep",repInd,sep = "")

            
            
            #Add Column for Names of Samples
            dataToUse$probes <- rownames(dataToUse)
            dataToUse <- dataToUse[c(dim(dataToUse)[2],1:(dim(dataToUse)[2]-1))]
            # print(colnames(dataToUse))
            # print(dataToUse[1:5,1:5])
            write.table(dataToUse, file = paste("~/Desktop/TimeTrial/Data/Processed/JTK/",runName,".txt", sep = ""), sep = "\t", row.names = F, col.names  = T, quote=FALSE)
          })
        })
      })
    })
