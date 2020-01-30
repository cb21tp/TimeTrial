setwd("~/Desktop/TimeTrial/Data/Raw/RawData/")

#Load List of Text Files
fileNames <- list.files(pattern="*1.csv")
data <- lapply(fileNames, read.csv)

#set First Row to Row Names
data <- lapply(data, function(df){
  rownames(df) <- df$X
  df$X <- NULL
  return(df)
})

#set Length of Time Series
timePoints <- seq(0,96,2)

#set Colors For plotting
cols <- c("#e41a1c","#377eb8","#4daf4a","#984ea3", "#ff7f00","#ffff33","#a65628","#cccccc", "#969696", "#636363", "#252525")
NoiseCols <- c("#058ed9","#0b5ea5","#073371","#00093f")


makePlot <- function(row,title,color){

  base <- abs(min(data[[1]][row,]))
  thinkness <- 1 #set thinkness to 0 to remove noise lines
  
  plot(timePoints,data[[5]][row,]+base,
      "l",
      xlab = "Time Points", 
      ylab = "Expression",
      col = 'black',
      cex.lab = 1.25,
      cex.axis = 1.5,
      font.lab = 2,
      font.axis = 2,
      cex.main = 2,
      lwd = thinkness,
      pch = 16,
      axes = F,
      main = title)
  axis(1, at = c(0,24,48,72,96), labels = c(0,24,48,72,96), font = 2, cex = 1.5)
  axis(2, font = 2, cex = 1.5)

  for(i in 5:2){
    lines(timePoints,data[[i]][row,]+base, col = NoiseCols[i-1], lwd = thinkness)
  }
  lines(timePoints,data[[1]][row,]+base, col = color, lwd = 5)
  box(lwd = 3)
}

{
par(mfrow = c(3,3))
makePlot(8,"Sin",cols[1])
makePlot(1008,"Peak",cols[2])
makePlot(2008,"Saw",cols[3])
makePlot(3012,"Linear Trend",cols[4])
makePlot(4008,"Damped",cols[5])
makePlot(5008,"Amplified",cols[6])
plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0, 10), ylim=c(0, 10))
makePlot(6008,"Contractile",cols[7])

par(mfrow = c(3,3))
makePlot(7010,"Flat",cols[8])
makePlot(8011,"Linear",cols[9])
plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(0, 10), ylim=c(0, 10))
makePlot(9012,"Sigmoid",cols[10])
makePlot(10016,"Expenential",cols[11])
}

