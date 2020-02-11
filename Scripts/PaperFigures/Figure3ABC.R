# code for generating Figure 2

#Load Required Packages
pkg = c('ggplot2', 'plyr','tidyverse', 'gplots','grid','matrixStats', 'shiny', 'shinythemes',"pastecs", "pROC", "shinyjs", "UpSetR", "limma","eulerr","gridExtra")
lapply(pkg,library, character.only = TRUE)

load("~/Desktop/TimeTrial/TimeTrial_Apps/App_Data/timeTrialAdjRealDataComplete.RData")
load("~/Desktop/TimeTrial/TimeTrial_Apps/App_Data/timeTrialRealDataComplete.RData")
load("~/Desktop/TimeTrial/TimeTrial_Apps/App_Data/timeTrialProcessTimeSeriesComplete.RData")


customScatterPlot <- function(x,y, title, xlim = c(0,40), ylim = c(0,40)){
  # , 
  plot(x,y, main = title, xlab = "-log(pVal)", ylab = "-log(pVal)", xlim = xlim, ylim = ylim , cex = .5, pch = 16, col = alpha("black",0.5), font = 2, font.lab = 2, asp = 1, cex.main=2)
  abline(fit <- lm(y ~ x), col='red')
  legend("topleft", bty="n", cex = 2, text.font = 2,legend=paste("Rank Corr:", format(cor(x,y,method = "spearman"), digits=3)))
  points(x[geneID],y[geneID],pch = 21, bg = "orange",col = "black", cex =1.5)
  box(lwd = 3)
}

schemeScatterPlotFormat <- function(dataS1,dataS2){
  
  labels <- c("1","2A","2B","4A","4B","4C","4D","2","4A","4B","2","4A","4B")
  
  par(mar = c(2,2,2,2), oma=c(7,7,1,1))
  customScatterPlot(-log(results[[1]][,dataS1]), -log(results[[1]][,dataS2]), "ARSER")
  mtext(labels[dataS2], side=2, line=7, adj = -0.25, cex=1.5, font = 2)
  mtext("-log(pVal)", side=2, line=4, cex=1.5, font = 2)
  customScatterPlot(-log(results[[2]][,dataS1]), -log(results[[2]][,dataS2]), "BooteJTK")
  customScatterPlot(-log(results[[3]][,dataS1]), -log(results[[3]][,dataS2]), "JTK_Cycle")
  mtext("-log(pVal)", side=2, line=4, cex=1.5, font = 2)
  mtext("-log(pVal)", side=1, line=4, cex=1.5, font = 2)
  customScatterPlot(-log(results[[4]][,dataS1]), -log(results[[4]][,dataS2]), "RAIN")
  mtext(labels[dataS1], side=1, line=7, adj = -0.25, cex=1.5, font = 2)
  mtext("-log(pVal)", side=1, line=4, cex=1.5, font = 2)
  
}

vennPlotsSet <- function(df, method, pval, logFC, colnames){
  methodNames <- c("ARSER", "BooteJTK", "JTK_Cycle", "RAIN")
  p <- getpValGeneList(df[[method]], pval, logFC)
  
  countsVenn <- vennCounts(fromList(p[colnames]))
  #Unique to 2 Counts 
  uniqueTo2h <- which(countsVenn[,1] == 1 & countsVenn[,2] == 0 & countsVenn[,3] == 0 )
  `2Count` <- countsVenn[uniqueTo2h,4]
  
  #Unique to 2 and 4, but not in 4 alone Counts 
  uniqueTo2and4h <- which(countsVenn[,1] == 1 & (countsVenn[,2] == 1 | countsVenn[,3] == 1))
  `24Count` <- sum(countsVenn[uniqueTo2and4h,4])
  
  #Unique only to 4 Counts
  `4Count` <- sum(countsVenn[which(countsVenn[,1] == 0),4])
  
  # Plot Euler Gram
  eulerr_options(font = 2)
  par(mar = c(4,4,4,4))
  v <- euler(c(`2h` = `2Count`,`4h` = `4Count`,"2h&4h"= `24Count`))
  plot(v,
       fills = list(fill = c("grey90","orange","#377eb8"), alpha = 1),
       labels = list(col = "Black", font = 4, fontsize = 40),
       quantities = list(TRUE,col = "Black", font = 2, fontsize = 30),
       lty = 1:2,
       lwd = 6,
       edge = F,
       main = list(label = methodNames[method], font = 2, fontsize = 20))
  
}

getpValGeneList <- function(df, pval,foldC = 0){
  colnames <- c("Hogenesch_1","Hogenesch_2A","Hogenesch_2B","Hogenesch_4A","Hogenesch_4B","Hogenesch_4C","Hogenesch_4D","Hughes_2","Hughes_4A","Hughes_4B","Zhang_2","Zhang_4A","Zhang_4B")

  names <- rownames(df)
  pVals <- apply(df < pval, 2, function(x) names[x])
  foldChanges <- lapply(data,function(x){getFoldChangeGeneList(x,foldC)})
  
  #get Intersection between the pVal and foldChange Gene List  
  reducedGeneList <- sapply(1:length(data), function(i){
    Reduce(intersect,list(pVals[[i]] , foldChanges[[i]]))
  })
  names(reducedGeneList) <- colnames
  return(reducedGeneList)
}

getFoldChangeGeneList <- function(df,fc){
  names <- rownames(df)
  output <- apply(df,1,function(x){
    min_max <- range(x)
    return(abs(min_max[2]-min_max[1]) > fc)
  })
  names[which(output)]
}


## --------------------------------------------- Plot 2A ----------------------------------# 
ds <- 8 # 0 or 3
method <- 1
scheme <- 1
geneID <- 7795
layout(matrix(c(1,1,1,2,2,2,0,5,5,5,
                1,1,1,2,2,2,0,5,5,5,
                1,1,1,2,2,2,0,6,6,6,
                3,3,3,4,4,4,0,6,6,6,
                3,3,3,4,4,4,0,7,7,7,
                3,3,3,4,4,4,0,7,7,7), 6, 10, byrow = TRUE))

schemeScatterPlotFormat(8,9)

#Plot the Time Series Data For Selected Gene
labels <- c("2","4A","4B")
for(i in 0:2){
  plotData <- data[[(ds+i)]][geneID,]
  tp <- as.numeric(gsub(pattern = "ZT",replacement = "",x = colnames(plotData)))
  vals <- as.vector(unlist(plotData))
  plot(tp,vals,"o", cex = 2, lwd = 2, pch = 16, col = "black", font = 2, font.lab = 2,
       main = paste0("Sampling Scheme ",labels[i+1]), cex.main=2)
  points(tp,vals,col = "orange", pch = 16)
  if (i == 1){
  mtext("Expression", side=2, line=4, cex=2, font = 2)
  }
  box(lwd = 3)
}
mtext("ZT Time Point", side=1, line=4, cex=2, font = 2)

## --------------------------------------------- Plot 2B ----------------------------------# 

# GENERATE THE PLOTS
plot1 <- vennPlotsSet(resultsAdj, 1, 0.05, 0, c("Zhang_2","Zhang_4A","Zhang_4B"))
plot2 <- vennPlotsSet(resultsAdj, 2, 0.05, 0, c("Zhang_2","Zhang_4A","Zhang_4B"))
plot3 <- vennPlotsSet(resultsAdj, 3, 0.05, 0, c("Zhang_2","Zhang_4A","Zhang_4B"))
plot4 <- vennPlotsSet(resultsAdj, 4, 0.05, 0, c("Zhang_2","Zhang_4A","Zhang_4B"))
lay <- rbind(c(NA,NA),
              c(1,2),
             c(NA,NA),
             c(3,4))

gridExtra::grid.arrange(plot1, plot2,plot3,plot4, 
                        top= textGrob("Zhang Dataset | FDR < 0.05",
                        gp = gpar(fontsize=30,font=2)), 
                        layout_matrix = lay, 
                        heights = unit(c(10,4,3,4), c("mm", "in","mm","in")))

