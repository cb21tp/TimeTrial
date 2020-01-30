# load the affy library
library(oligo)
library(affy)
# see For reference: 
# http://homer.ucsd.edu/homer/basicTutorial/affymetrix.html
# https://wiki.bits.vib.be/index.php/Analyze_your_own_microarray_data_in_R/Bioconductor

## ================================================== Hogenesch2009 | affy_mouse430_2 | GSE11923 ===================================== 

#Load Data 
setwd("~/Desktop/TimeTrial/Data/Raw/RealData/Hogenesch2009/GSE11923_RAW")
Hogenesch2009data <- ReadAffy()

#initial Plot check
# MAplot(Hogenesch2009data,which = 1)
# image(Hogenesch2009data[,1])

# RMA (Robust Multi-array Average) 
Hogenesch2009eset <- affy::rma(Hogenesch2009data)

# Finally, save the data to an output file to be used by other programs, etc (Data will be log2 transformed and normalized)
write.exprs(Hogenesch2009eset,file="Hogenesch2009data.txt")
H2009_frame <- data.frame(exprs(Hogenesch2009eset))

# BiocManager::install("mouse4302.db")
# Put annotation information in a data frame.
# Merge data frames together
# Write out to a file:
library("mouse4302.db")
Annot <- data.frame(ACCNUM=sapply(contents(mouse4302ACCNUM), paste, collapse=", "), SYMBOL=sapply(contents(mouse4302SYMBOL), paste, collapse=", "), DESC=sapply(contents(mouse4302GENENAME), paste, collapse=", "))
Hogenesch2009all <- merge(Annot, H2009_frame, by.x=0, by.y=0, all=T)
write.table(Hogenesch2009all,file="Hogenesch2009.data.ann.txt",sep="\t")


## ================================================== Hughes2012 | affy_moex_1_0_st_v1 | GSE30411 =====================================

# Read in the CEL files in the directory
setwd("~/Desktop/TimeTrial/Data/Raw/RealData/Hughes2012/GSE30411_RAW")
celFiles <- list.celfiles(listGzipped = T)
affyRaw <- read.celfiles(celFiles)

# RMA (Robust Multi-array Average) 
Hughes2012eset <-  rma(affyRaw)

# Finally, save the data to an output file to be used by other programs, etc (Data will be log2 transformed and normalized)
write.exprs(Hughes2012eset,file="data2.txt")
H2012_frame <- data.frame(exprs(Hughes2012eset))

# BiocManager::install("moex10sttranscriptcluster.db")
# Put annotation information in a data frame
# Merge data frames together
# Write out to a file:
library(moex10sttranscriptcluster.db)
Annot <- data.frame(ACCNUM=sapply(contents(moex10sttranscriptclusterACCNUM), paste, collapse=", "), SYMBOL=sapply(contents(moex10sttranscriptclusterSYMBOL), paste, collapse=", "), DESC=sapply(contents(moex10sttranscriptclusterGENENAME), paste, collapse=", "))
Hughes2012all <- merge(Annot, H2012_frame, by.x=0, by.y=0, all=T)
write.table(Hughes2012all,file="Hughes2012.data.ann2.txt",sep="\t")

## ================================================== Hughes2012 | affy_mogene_1_0_st_v1 | GSE30411 =====================================

# Read in the CEL files in the directory
setwd("~/Desktop/TimeTrial/Data/Raw/RealData/Zhang2014/GSE54650_RAW/")
celFiles <- list.celfiles(listGzipped = T)
affyRaw <- read.celfiles(celFiles)

# RMA (Robust Multi-array Average) 
Zhang2014eset <-  rma(affyRaw)

# Finally, save the data to an output file to be used by other programs, etc (Data will be log2 transformed and normalized)
write.exprs(Zhang2014eset,file="data.txt")
Z2014_frame <- data.frame(exprs(Zhang2014eset))

# BiocManager::install("mogene10sttranscriptcluster.db")
# Put annotation information in a data frame
# Merge data frames together
# Write out to a file:
library(mogene10sttranscriptcluster.db)
Annot <- data.frame(ACCNUM=sapply(contents(mogene10sttranscriptclusterACCNUM), paste, collapse=", "), SYMBOL=sapply(contents(mogene10sttranscriptclusterSYMBOL), paste, collapse=", "), DESC=sapply(contents(mogene10sttranscriptclusterGENENAME), paste, collapse=", "))
Zhang2014all <- merge(Annot, Z2014_frame, by.x=0, by.y=0, all=T)
write.table(Zhang2014all,file="Zhang2014.data.ann.txt",sep="\t")

## ================================================== Get Common List of Probes =====================================

# Get subset of Common Genes to Use for Analysis and Remove NA
commonGenes <- Reduce(intersect, list(Hogenesch2009all$SYMBOL, Hughes2012all$SYMBOL, Zhang2014all$SYMBOL))
commonGenes <- commonGenes[-which(commonGenes == "NA")]
length(commonGenes) #15371

#Create Dataframes with commonGenes (may have no expression)
dataframe1 <- Hogenesch2009all[Hogenesch2009all$SYMBOL %in% commonGenes,] #30285
dataframe2 <- Hughes2012all[Hughes2012all$SYMBOL %in% commonGenes,] #21325
dataframe3 <- Zhang2014all[Zhang2014all$SYMBOL %in% commonGenes,] #16750

#Reorder Dataframes to have the list of genes in the same order
d1 <- dataframe1[order(dataframe1$SYMBOL),c(3,5:dim(dataframe1)[2])] #30285
d2 <- dataframe2[order(dataframe2$SYMBOL),c(3,5:dim(dataframe2)[2])] #21325
d3 <- dataframe3[order(dataframe3$SYMBOL),c(3,5:dim(dataframe3)[2])] #16750

#Aggregate Dataframe by Gene Symbol by taking the mean expression. additionally remove rows with no Expression post aggregation
d1mean <- aggregate(. ~ SYMBOL, data = d1, mean, na.action = na.omit) #15371
d2mean <- aggregate(. ~ SYMBOL, data = d2, mean, na.action = na.omit) #12868
d3mean <- aggregate(. ~ SYMBOL, data = d3, mean, na.action = na.omit) #15371

#Find common List of genes once removing for genes that have no expression
commonExpressedGenes <- Reduce(intersect, list(d1mean$SYMBOL, d2mean$SYMBOL, d3mean$SYMBOL)) #12868

#get Final Dataframe to be used in analysis. Still requires subsetting for different experiments 
Hogenesch2009final <- d1mean[d1mean$SYMBOL %in% commonExpressedGenes,] #12868
Hughes2012final <- d2mean[d2mean$SYMBOL %in% commonExpressedGenes,] #12868
Zhang2014final <- d3mean[d3mean$SYMBOL %in% commonExpressedGenes,] #12868

#save.image("~/Desktop/microArray.Rdata")

## ================================================== Set Row and Column Names for Data =================================

load("~/Desktop/CyclingMethodsReview/DataAnalysis/Data/Processed/RealData/microArray.Rdata")
#rename row names and column names for samples
rownames(Hogenesch2009final) <- Hogenesch2009final$SYMBOL
Hogenesch2009final$SYMBOL <- NULL

rownames(Hughes2012final) <- Hughes2012final$SYMBOL
Hughes2012final$SYMBOL <- NULL

rownames(Zhang2014final) <- Zhang2014final$SYMBOL
Zhang2014final$SYMBOL <- NULL

colnames(Hogenesch2009final) <- paste0("ZT",seq(18,65,1))
colnames(Hughes2012final) <- paste0("ZT",seq(0,46,2))
colnames(Zhang2014final) <- paste0("ZT",seq(18,64,2))

## ================================================== Quality Check of Micro Arrays =====================================

#Generate BoxPlots for checking Expression Across Micro arrays
boxplot(Hogenesch2009final)
boxplot(Hughes2012final)
boxplot(Zhang2014final)

#mean center the data
centerApply <- function(x) {
  apply(x, 2, function(y) y - mean(y))
}

x <- centerApply(Hogenesch2009final)
y <- centerApply(Hughes2012final)
z <- centerApply(Zhang2014final)

boxplot(x)
boxplot(y)
boxplot(z)

#PCAplot <- cbind(Hogenesch2009final,Hughes2012final,Zhang2014final)
PCAplot <- cbind(x,y,z)
color <- c(rep('green',dim(Hogenesch2009final)[2]),rep('red',dim(Hughes2012final)[2]),rep('blue',dim(Zhang2014final)[2]))

data.PC <- prcomp(t(PCAplot),scale=T)
pairs(data.PC$x[,1:6],col=color)


#HeatMaps of Data
# heatmap(as.matrix(Hogenesch2009final), Colv = NA)
# heatmap(as.matrix(Hughes2012final), Colv = NA)
# heatmap(as.matrix(Zhang2014final), Colv = NA)
heatmap(as.matrix(x), Colv = NA, Rowv = NA)
heatmap(as.matrix(y), Colv = NA, Rowv = NA)
heatmap(as.matrix(z), Colv = NA, Rowv = NA)

## ================================================== Save DataFrame for Processing By Cycling Methods =====================================

subsampleEven <- seq(2,24,2)
subsampleOdd <- seq(1,24,2)


Hogenesch2009final <- centerApply(Hogenesch2009final)
Hughes2012final <- centerApply(Hughes2012final)
Zhang2014final <- centerApply(Zhang2014final)

#12868 genes
setwd("~/Desktop/TimeTrial/Data/Processed/RealData/")
write.table(Hogenesch2009final,file="Hogenesch2009.all.data.ann.txt",sep="\t")
write.table(Hughes2012final,file="Hughes2012.all.data.ann.txt",sep="\t")
write.table(Zhang2014final,file="Zhang2014.all.data.ann.txt",sep="\t")

write.table(Hughes2012final[,subsampleEven],file="Hughes2012.subA.data.ann.txt",sep="\t")
write.table(Hughes2012final[,subsampleOdd],file="Hughes2012.subB.data.ann.txt",sep="\t")

write.table(Zhang2014final[,subsampleEven],file="Zhang2014.subA.data.ann.txt",sep="\t")
write.table(Zhang2014final[,subsampleOdd],file="Zhang2014.subB.data.ann.txt",sep="\t")


subsample2A <- seq(1,48,2)
subsample2B <- seq(2,48,2)

subsample4A <- seq(1,48,4)
subsample4B <- seq(2,48,4)
subsample4C <- seq(3,48,4)
subsample4D <- seq(4,48,4)

write.table(Hogenesch2009final[,subsample2A],file="Hogenesch2009.sub2A.data.ann.txt",sep="\t")
write.table(Hogenesch2009final[,subsample2B],file="Hogenesch2009.sub2B.data.ann.txt",sep="\t")

write.table(Hogenesch2009final[,subsample4A],file="Hogenesch2009.sub4A.data.ann.txt",sep="\t")
write.table(Hogenesch2009final[,subsample4B],file="Hogenesch2009.sub4B.data.ann.txt",sep="\t")
write.table(Hogenesch2009final[,subsample4C],file="Hogenesch2009.sub4C.data.ann.txt",sep="\t")
write.table(Hogenesch2009final[,subsample4D],file="Hogenesch2009.sub4D.data.ann.txt",sep="\t")



