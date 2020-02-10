library(ggplot2)
library(pastecs)
library(tidyr)
library(pROC)

#######################################################
#Cartoon of Concatenation On Signmoid Waveform
#######################################################
sigmoid <- function(x) {
  1 / (1 + exp(-x))
}
plottingColors <- c("#FF9900","#377eb8")
legendNames <- c("Rep 1","Rep 2")
xfig1 <- seq(-5, 15, 0.1)
xfig2 <- seq(-5, 5, 0.1)
col1 <- rep(plottingColors[1],101)
col2 <- rep(plottingColors[2],101)
widthLines <- 6

par(font = 2, 
    font.axis =2, 
    font.lab = 2,
    cex.lab=1.5,
    cex.axis=1, 
    cex.main=1.5, 
    cex.sub=1.5)

layout(mat = matrix(c(1,2,3,4), nrow = 2))
plot(x = xfig1, 
     y = sigmoid(xfig1),
     type = "l",
     xlim = c(-5,15),
     col = col1,
     lwd = widthLines,
     main = "48 Hour Averaged Replicates",
     ylab = "Expression",
     xlab = "Time",
     axes = F)
lines(x = xfig1,
      y = sigmoid(xfig1), 
      type = "l", 
      col = col2,
      lty = "dashed",
      lwd = widthLines)
legend(x = "bottomright",
       legend = legendNames,
       fill = plottingColors,
       box.lwd = 0)
axis(side = 1,at = seq(-5,15,5),labels = seq(0,48,12))
axis(side = 2)
box(lwd = 4)


plot(x = c(5,5),
     y =c(sigmoid(-5),sigmoid(5)),
     xlim = c(-5,15),
     type = "l" ,
     col = "#7F7F7F",
     lwd = widthLines, 
     main = "24 Hour Concatenated Replicates", 
     ylab = "Expression", 
     xlab = "Time", 
     axes = F)
lines(xfig2,sigmoid(xfig2), "l",col = col1, lwd = widthLines)
lines(xfig2+10,sigmoid(xfig2), "l",  col = col2, lwd = widthLines)
legend(x = "bottomright",
       legend = legendNames,
       fill = plottingColors,
       box.lwd = 0)
axis(side = 1,at = seq(-5,15,5),labels = seq(0,48,12))
axis(side = 2)
box(lwd = 4)

#######################################################
#Load the Data For Plotting
#######################################################
load("~/Desktop/TimeTrial/TimeTrial_Apps/App_Data/timeTrialSyntheticDataTimeSeries.RData")
load("~/Desktop/TimeTrial/TimeTrial_Apps/App_Data/timeTrialSyntheticDataResults.RData")
Reviewdf <- data

#Remove Length of 36 Hours
Reviewdf<- Reviewdf[which(Reviewdf$Length != 36),]

#Select Colors for Plotting
#Order of Coloring = 1 ARSER_AVG, 2 ARSER_CONCAT, 3 BOOTEJTK_AVG, 4 BOOTEJTK_CONCAT, 5 JTK_Cycle, 6 RAIN

#######################################################
#Create a Data Frame of the Basic Statitics in Each Run
#######################################################
getSummaryStats <- split(Reviewdf,f = interaction(Reviewdf$Rate,Reviewdf$Length,Reviewdf$Method,Reviewdf$Rep),drop = T)
getSummaryStats <- lapply(getSummaryStats, function(x) {
  vals <- x$AUC
  output <- c(x$Rate[1],x$Length[1],x$Method[1],x$Rep[1], ((x$Length[1]/x$Rate[1]+1)*x$Rep[1]))
  names(output) <- c("Rate","Length","Method","Rep", "NumSmp")
  c(output,stat.desc(vals))
})
SummaryStats <- as.data.frame(t(do.call(cbind.data.frame, getSummaryStats)))
rownames(SummaryStats) <- NULL 
SummaryStats <- SummaryStats[order(-as.numeric(SummaryStats$mean)),]
SummaryStats <- apply(SummaryStats,2,as.vector)
SummaryStats <- as.data.frame(SummaryStats)

##################################################
#High Noise Vs Low Noise - Concatination vs Avg
##################################################
HighLowDfMean <- function(df, method, lng, smps, reps){
  HighLowDf <- df[(df$Noise != 0.2 & df$Rep != reps & df$Length == lng & df$numSmps < smps & df$Method == method),]
  Boot_Concat <- split(HighLowDf,interaction(HighLowDf$Noise < 0.2,HighLowDf$Rate,HighLowDf$Length, HighLowDf$Rep),drop = T)
  names(Boot_Concat)
  df <- as.data.frame(t(data.frame(strsplit(names(Boot_Concat), "[.]"))))
  df$V5 <- unlist(lapply(Boot_Concat,function(x){mean(x$AUC)}))
  df$NumSmp <- as.numeric((as.numeric(as.vector(df$V3))/as.numeric(as.vector(df$V2))+1)*as.numeric(as.vector(df$V4)))
  names(df) <- c("NoiseHL","Rate","Length","Rep","AUC", "NumSmp")
  df$NoiseHL <- as.factor(c("High", "Low")[as.numeric(as.factor(df$NoiseHL))])
  return(df)
}
getMeans <- function(data){
  a<- split(data$AUC,interaction(data$NumSmp,data$NoiseHL))
  df <- as.data.frame(t(data.frame(strsplit(names(a), "[.]"))))
  df$V3 <- unlist(lapply(a,mean))
  df$V1 <- as.numeric(as.vector(df$V1))
  names(df) <- c("numSmp", "NoiseHL", "AUC")
  return(df)
}

unique(Reviewdf$Method)
#Select Only 24 Hours with more than 1 replicates
df_CON <- HighLowDfMean(data, "BooteJTK_Concat", 24, 41, 1)
#Select Only 48 Hours with more less than max sampling of 24 hour appove
df_AVG <- HighLowDfMean(data, "BooteJTK_Avg", 48, 41, 0)

#Get Mean Values of AUC for each Sampling Between High and Low Noise Levels
meanLine_Avg <- getMeans(df_AVG)
meanLine_Con <- getMeans(df_CON)

colsLines <- c("#377eb8", "#377eb8", "#7F7F7F","#7F7F7F")
colsFill <- c("#FFFFFF",alpha("#377eb8",0.8),"#FFFFFF", alpha("#7F7F7F",0.8))


pchShape <- c(21,23,24)
pchLines <- c(21,23)
fitAvg <- glm(formula = AUC ~ numSmp*NoiseHL,data = meanLine_Avg)
fitCon <- glm(formula = AUC ~ numSmp*NoiseHL,data = meanLine_Con)

summary(fitAvg)
summary(fitCon)
lwdCenter <- 5

# par(mfrow = c(2,1), font = 2)
plot(as.numeric(as.vector(meanLine_Avg[meanLine_Avg$NoiseHL=="High",]$numSmp)),
     meanLine_Avg[meanLine_Avg$NoiseHL=="High",]$AUC, col = colsLines[2], 
     type = "l",
     lwd = 0, 
     xlim = c(5,42), 
     ylim = c(0.5,1),
     xlab = "Number of Samples", 
     ylab = "AUC Score", 
     main = "AUC Score in Low Noise Regime")

#Plot Trend Lines
abline(v = 26, lwd = 2, col = "orange")
abline(a = fitAvg$coefficients[1] + fitAvg$coefficients[3], b =fitAvg$coefficients[2]+ fitAvg$coefficients[4], col = colsLines[2], lwd = lwdCenter)
abline(a = fitCon$coefficients[1] + fitCon$coefficients[3], b =fitCon$coefficients[2]+ fitCon$coefficients[4], col = colsLines[4], lwd = lwdCenter)

#Blot Boarder
box(lwd=4)

#Plot Points
toPlot <- which(df_CON$NoiseHL == "Low")
points(df_CON$NumSmp[toPlot],df_CON$AUC[toPlot], col = colsLines[4],  bg  = colsFill[4], pch = pchLines[1], cex = 2, lwd = 3)
points(df_AVG$NumSmp[toPlot],df_AVG$AUC[toPlot], col = colsLines[2], bg  = colsFill[2], pch = pchLines[2], cex = 2, lwd = 3)

#Plot Legend
legend("bottomright", 
       legend= rev(c("Concatenated 24h","Averaged 48h")),
       col = rev(colsLines[c(4,2)]),
       pch = rev(pchLines),
       pt.bg = rev(colsFill[c(4,2)]),
       pt.cex = 2,
       lwd = 2,
       bty = "n",
       box.lwd = 0)

# High Noise Plot
plot(as.numeric(as.vector(meanLine_Avg[meanLine_Avg$NoiseHL=="High",]$numSmp)),
     meanLine_Avg[meanLine_Avg$NoiseHL=="High",]$AUC, col = colsLines[2], 
     type = "l",
     lwd = 0, 
     xlim = c(5,42), 
     ylim = c(0.5,1),
     xlab = "Number of Samples", 
     ylab = "AUC Score", 
     main = "AUC Score in High Noise Regime")

abline(v = 26, lwd = 2, col = "orange")
abline(a = fitAvg$coefficients[1], b =fitAvg$coefficients[2], col = colsLines[1], lwd = lwdCenter)
abline(a = fitCon$coefficients[1], b =fitCon$coefficients[2], col = colsLines[3], lwd = lwdCenter)

#Blot Boarder
box(lwd=4)

toPlot <- which(df_CON$NoiseHL == "High")
points(df_CON$NumSmp[toPlot],df_CON$AUC[toPlot], col = colsLines[4],  bg  = colsFill[4], pch = pchLines[1], cex = 2, lwd = 3)
points(df_AVG$NumSmp[toPlot],df_AVG$AUC[toPlot], col = colsLines[2], bg  = colsFill[2], pch = pchLines[2], cex = 2, lwd = 3)

#Plot Legend
legend("bottomright", 
       legend= rev(c("Concatenated 24h","Averaged 48h")),
       col = rev(colsLines[c(4,2)]),
       pch = rev(pchLines),
       pt.bg = rev(colsFill[c(4,2)]),
       pt.cex = 2,
       lwd = 2,
       bty = "n",
       box.lwd = 0)


###########################################################
#High Noise Vs Low Noise - ROC Concatination vs Avg
###########################################################

catergory <- c(rep(1,7000), rep(0,4000))
x <- c(0,0.1,0.2,0.3,0.4)
par(cex.lab=1.5, cex.axis=1, cex.main=1.5, cex.sub=1.5, font.lab=2, font.axis = 2)
lineColors <- c("grey",'#fff800','#f37200','#cf0000','#000000')
#####################################
#BooteJTK_Concat_2_48_",x,"_1" Plots
######################################
par(font=2)
BooteJTKsamples <- paste0("BooteJTK_Avg_2_48_",x,"_1")
for( i in 5:1){
  ROCplot <- roc(catergory, results[[3]][,BooteJTKsamples[i]])
  
  if(i == 5){
    plot.roc(ROCplot, legacy.axes=T, col = lineColors[i], asp = 1, lwd = 5,type = "l", pch = 16, cex = 1, xlab = "FPR", ylab = "TPR")
  }
  else{
    lines.roc(ROCplot,col = lineColors[i], lwd = 5,type = "l", pch = 16, cex = 1)
  }
}
par(mar=c(4, 4, 2, 2)+.1)
box(lwd=4)

legend(x = "bottomright",legend = x*100,fill = lineColors, inset = c(.1,.1), box.lwd = 0, title = "Noise Level \n (% Amplitude)")

#####################################
#BooteJTK_Concat_2_24_",x,"_2" Plots
#####################################

BooteJTKsamples <- paste0("BooteJTK_Concat_2_24_",x,"_2")
for(i in 5:1){
  ROCplot <- roc(catergory, results[[4]][,BooteJTKsamples[i]])
  
  if(i == 5){
    plot.roc(ROCplot, legacy.axes=T, col = lineColors[i], asp = 1, lwd = 5,type = "l", pch = 16, cex = 1, xlab = "FPR", ylab = "TPR")
  }
  else{
    lines.roc(ROCplot,col = lineColors[i], lwd = 5,type = "l", pch = 16, cex = 1)
  }
}
box(lwd=4)
legend(x = "bottomright",legend = x*100,fill = lineColors, text.font = 2, inset = c(.1,.1), box.lwd = 0, title = "Noise Level \n (% Amplitude)")


###########################################################
#High Noise Vs Low Noise - Histogram Concatination vs Avg
###########################################################

  cycling <- c(rep("Cycling",7000), rep("Non-Cycling",4000))
  waveFormCols <- c("#377eb8","#cccccc")
  waveFormCols <- c("#FF9900",alpha("#cccccc",0.5))
  
  dataNameSchemeA <- "BooteJTK_Avg_2_48_0_1"
  data_A <- results[[3]] # corresponds to BooteJTK_Avg Data
  
  #create Dataframe for plotting
  wave <- gsub(pattern = "_.*",replacement = "",rownames(data_A))
  data_A <- data.frame(-log(as.numeric(data_A[,dataNameSchemeA])),wave,cycling)
  colnames(data_A) <- c('pval',"wave","cycling")
  data_A$cycling  <- with(data_A, reorder(cycling, 11000:1))

  ggplot(data_A, aes(x = pval,fill = cycling, color = cycling)) +
    geom_histogram(boundary = 0, binwidth = 1,position = "identity") + 
    theme(legend.position = "bottom", 
          text = element_text(size = 16, face = "bold"), 
          panel.background=element_blank(),
          panel.grid.major=element_line("black"),
          plot.background=element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=3)) + 
    labs(x="-log(pVal)", y = "Count") +
    scale_fill_manual('',values = waveFormCols)+
    scale_color_manual('',values = c("black","black")) +
    coord_cartesian(xlim =c(0, 50), ylim = c(0,3600))
    
  
  
  dataNameSchemeB <- "BooteJTK_Concat_2_24_0_2"
  data_B <- results[[4]] # corresponds to BooteJTK_Avg Data
  
  #create Dataframe for plotting
  wave <- gsub(pattern = "_.*",replacement = "",rownames(data_B))
  data_B <- data.frame(-log(as.numeric(data_B[,dataNameSchemeB])),wave,cycling)
  colnames(data_B) <- c('pval',"wave", "cycling")
  data_B$cycling  <- with(data_B, reorder(cycling, 11000:1))
  
  ggplot(data_B, aes(x = pval,fill = cycling, color = cycling)) +
    geom_histogram(boundary = 0, binwidth = 1, position="identity") + 
    theme(legend.position = "bottom", 
          text = element_text(size = 16, face = "bold"), 
          panel.background=element_blank(),
          panel.grid.major=element_line("black"),
          plot.background=element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=3)) + 
    scale_fill_manual('',values = waveFormCols)+
    scale_color_manual('',values = c("black","black")) + 
    labs(x="-log(pVal)", y = "Count")  +
    coord_cartesian(xlim =c(0, 50), ylim = c(0,1900))

  