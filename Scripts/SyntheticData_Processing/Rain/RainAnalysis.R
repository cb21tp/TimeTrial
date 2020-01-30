require(rain)
require(lattice)
require(parallel)

#Load Rain Data
load("/home/emn6548/TimeTrial/Data/Raw/RawData_Con.Rdata")

##################
#General Functions
##################
START <-  Sys.time()
#RunTime Function
hms_span <- function(start, end) {
  dsec <- as.numeric(difftime(end, start, unit = "secs"))
  hours <- floor(dsec / 3600)
  minutes <- floor((dsec - 3600 * hours) / 60)
  seconds <- dsec - 3600*hours - 60*minutes
  paste0(
    sapply(c(hours, minutes, seconds), function(x) {
      formatC(x, width = 2, format = "d", flag = "0")
    }), collapse = ":")
}

#Assuming 24 Hours
#Calculated as follows:
#Sample Rate 2
#24/2 = 12 + 1 (for zero) points to check
#Slope cant exist in 1st and last position
#11/13 = .846 -> Round to Nearest Hole = .8
#return ( c(.2,.8) )
#8 has 4 points, would be c(.5,.5) set instead to c(.4,.6) same as sample Rate of 6
choosePeakInterval <- function(sampleRate){
  if(sampleRate == 2 ){
    return(c(.2,.8))
  } else if(sampleRate == 4 ){
    return(c(.3,.7))
  } else{
    return(c(.4,.6))
  }
}

reps <- list(1,2,3)
lengths <- list(24,36,48,72,96)
splRate <- list(2,4,6,8)

#reps <- list(2)
#lengths <- list(24)
#splRate <- list(2)

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

RainAnalysis <- 
  lapply(reps, function(rep){
    lapply(lengths, function(lng){
      lapply(splRate, function(smps){
        
        timePoints <- seq(from = 0, to = lng, by = smps) #get sequence of timePoints
        dfName <- names(df.list) #get Names of Data Frame to Use
        dfTemp <- df.list[which(as.numeric(substr(dfName,nchar(dfName), nchar(dfName))) == rep)] #split by rep
        print(c(rep, lng,smps))
        #lapply(seq_along(dfTemp), function(x){
        mclapply(seq_along(dfTemp),mc.cores = 20, function(x){
          data <- dfTemp[[x]] #get the data
          runName <- paste(smps,lng,names(dfTemp)[[x]],sep = "_") #create a Name for the sample Run
          print(runName)

          #Select the correct Data to Use for the analysis
          if(rep == 3){
            tpInd <- c(rbind(timePoints,timePoints,timePoints))
            repInd <- rep(c(1:rep),length(timePoints))
          }
          if(rep == 2){
            tpInd <- c(rbind(timePoints,timePoints))
            repInd <- rep(c(1:rep),length(timePoints))
          }
          if(rep == 1){
            tpInd <- timePoints
            repInd <- rep(1,length(timePoints))
          }
          paste("ZT_",tpInd,"_",repInd,sep = "")
          dataToUse <- data[,paste("ZT_",tpInd,"_",repInd,sep = "")]
          print(colnames(dataToUse))
          
          
          #RUN RAIN ON DATA SET
          peakInterval <- choosePeakInterval(smps)
          sTime <- Sys.time()
          if(rep == 1){
            results <- rain(t(dataToUse), deltat = smps, period = 24, peak.border = peakInterval, verbose = F, adjp.method = "rawp")
          } else {
            results <- rain(t(dataToUse), deltat = smps, period = 24, nr.series = rep, peak.border = peakInterval, verbose = F, adjp.method = "rawp")
          }
          eTime <- Sys.time()
          time <- hms_span(sTime, eTime)
          return(list(runName, results, time))
          #return(list(runName, time))
        })
      })
    })
  })

END <- Sys.time()
RUNTIME <- hms_span(START, END)
save.image(file = "RainResults_RawP.Rdata")
