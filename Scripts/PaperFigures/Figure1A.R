library("data.tree")
library("networkD3")


setwd("~/Desktop/TimeTrial/Results/SyntheticData/BooteJTK/avg/")
#Load List of Text Files
temp <- list.files(pattern="*.txt")
info <- as.numeric(unlist(regmatches(temp,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",temp))))
info <- info[rep(c(rep(T,4),rep(F,2)),length(info)/6)]
info <- matrix(data = info, ncol = 4,byrow=TRUE)
info <- data.frame(temp,info)
colnames(info) <- c("name","Rate","Length","NoiseLv","Rep")
par(bg=NA)

#define the hierarchy (Session/Room/Speaker)
info$pathString <- paste(" ",info$Rep,info$Length, info$Rate, info$NoiseLv ,sep="|")
#convert to Node
useRtree <- as.Node(info, pathDelimiter = "|")

useRtree

#plot with networkD3
useRtreeList <- ToListExplicit(useRtree, unname = TRUE)

colsRep <- c("red", "white")
colsInt <- c("#E1DAAE", "#848FA2","#FF934F","#058ED9","#2D3142")
cols <- c('#fff800','#f37200','#cf0000','#000000')
colsNoise <-c("grey",'#fff800','#f37200','#cf0000','#000000')

#Initialize Colors for Plots
Rep1ColsFillRow1 <- c("white")
Rep1ColsFillRow2 <- c("white","white","white","white")
Rep1ColsFillRow3 <- c("white","white","white","white")
Rep1ColsFillRow4 <- c("grey",'#fff800','#f37200','#cf0000','#000000')
Rep1ColsLink1 <- c("white")
Rep1ColsLink2 <- c("white","white","white","white")
Rep1ColsLink3 <- c("white","white","white","white")
Rep1ColsLink4  <- c("white","white","white","white","white")
Rep1ColsOutRow1 <- c("white")
Rep1ColsOutRow2 <- c("white","white","white","white")
Rep1ColsOutRow3 <- c("white","white","white","white")
Rep1ColsOutRow4 <- c("grey",'#fff800','#f37200','#cf0000','#000000')

Rep2ColsFillRow1 <- c("white")
Rep2ColsFillRow2 <- c("white","white","white","white")
Rep2ColsFillRow3 <- c("white","white","white","white")
Rep2ColsFillRow4 <- c("grey",'#fff800','#f37200','#cf0000','#000000')
Rep2ColsLink1 <- c("white")
Rep2ColsLink2 <- c("white","white","white","white")
Rep2ColsLink3 <- c("white","white","white","white")
Rep2ColsLink4  <- c("white","white","white","white","white")
Rep2ColsOutRow1 <- c("white")
Rep2ColsOutRow2 <- c("white","white","white","white")
Rep2ColsOutRow3 <- c("white","white","white","white")
Rep2ColsOutRow4 <- c("grey",'#fff800','#f37200','#cf0000','#000000')

Rep3ColsFillRow1 <- c("white")
Rep3ColsFillRow2 <- c("white","white","white","white")
Rep3ColsFillRow3 <- c("white","white","white","white")
Rep3ColsFillRow4 <- c("grey",'#fff800','#f37200','#cf0000','#000000')
Rep3ColsLink1 <- c("white")
Rep3ColsLink2 <- c("white","white","white","white")
Rep3ColsLink3 <- c("white","white","white","white")
Rep3ColsLink4  <- c("white","white","white","white","white")
Rep3ColsOutRow1 <- c("white")
Rep3ColsOutRow2 <- c("white","white","white","white")
Rep3ColsOutRow3 <- c("white","white","white","white")
Rep3ColsOutRow4 <- c("grey",'#fff800','#f37200','#cf0000','#000000')


#Set Colors to Vectors
nodeVector <- c("black", 
                 Rep1ColsFillRow1,Rep2ColsFillRow1,Rep3ColsFillRow1,
                 Rep1ColsFillRow2,Rep2ColsFillRow2,Rep3ColsFillRow2,
                 rep(Rep1ColsFillRow3,4),rep(Rep2ColsFillRow3,4),rep(Rep3ColsFillRow3,4),
                 rep(c(rep(Rep1ColsFillRow4,4*4),rep(Rep2ColsFillRow4,4*4),rep(Rep3ColsFillRow4,4*4)),3))

jsarray <- paste0('["', paste(nodeVector, collapse = '", "'), '"]')
nodeColorJS <- JS(paste0('function(d, i) { return ', jsarray, '[i]; }'))

outlineVector <- c("black", 
                Rep1ColsOutRow1,Rep2ColsOutRow1,Rep3ColsOutRow1,
                Rep1ColsOutRow2,Rep2ColsOutRow2,Rep3ColsOutRow2,
                rep(Rep1ColsOutRow3,4),rep(Rep2ColsOutRow3,4),rep(Rep3ColsOutRow3,4),
                rep(c(rep(Rep1ColsOutRow4,4*4),rep(Rep2ColsOutRow4,4*4),rep(Rep3ColsOutRow4,4*4)),3))

jsarray <- paste0('["', paste(outlineVector, collapse = '", "'), '"]')
outlineColorJS <- JS(paste0('function(d, i) { return ', jsarray, '[i]; }'))

linkVector <- c( 
                Rep1ColsLink1,Rep2ColsLink1,Rep3ColsLink1,
                Rep1ColsLink2,Rep2ColsLink2,Rep3ColsLink2,
                rep(Rep1ColsLink3,4),rep(Rep2ColsLink3,4),rep(Rep3ColsLink3,4),
                rep(c(rep(Rep1ColsLink4,4*4),rep(Rep2ColsLink4,4*4),rep(Rep3ColsLink4,4*4)),3)
                )
linkVector
jsarray <- paste0('["', paste(linkVector, collapse = '", "'), '"]')
linkStrokeJS <- JS(paste0('function(d, i) { return ', jsarray, '[i]; }'))

margins <- c(10,10,10,10)
names(margins) <- c("top", "right", "bottom", "left")
radialNetwork(useRtreeList,
              nodeColour = nodeColorJS,
              nodeStroke = outlineColorJS,
              linkColour = linkStrokeJS,
              #linkColour = 'grey',
              textColour = 'black',
              fontSize = 30
              #opacity = 1
              )
