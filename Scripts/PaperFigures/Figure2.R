##------------Figure 2: Experimental Design for Real Data TimeTrial----------------------##


par(font.lab = 2, cex.main = 2, font.axis = 2, cex.lab = 1.5)
timePoints <- seq(1,48,1)

layout(matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,
                2,2,2,2,2,2,3,3,3,3,3,3,
                4,4,4,5,5,5,6,6,6,7,7,7), 3, 12, byrow = TRUE))



plot(x = timePoints,
     y = rep(0,48), 
     pch = rep(c(21,21,23,23),24), 
     cex = 4,
     xlim = c(0,49),
     ylim = c(-1.5,1.5),
     xlab = "",
     ylab = "",
     # main = "Scheme 1",
     bg = rep(c("#377eb8","orange"),24),
     axes = F)
mtext(side = 1, "Time Points", line = -3, font = 2)
mtext(side = 3, "Scheme 1", line = -4, cex = 1.5, font = 2)

#axis(1,at = seq(0,48,6), labels = seq(0,48,6))
text(x = timePoints, y = rep(0,48), labels = timePoints, col = rep(c("white","black"),24), font = 2)
box(lwd = 3)

plot(x = timePoints[seq(2,48,2)],
     y = rep(0,48)[seq(2,48,2)],
     pch = rep(c(21,23),24),
     cex = 4,
     xlim = c(0,49),
     ylim = c(-1.5,1.5),
     xlab = "",
     ylab = "",
     # main = "Scheme 2A",
     bg = rep(c("orange"),24),
     axes = F)
mtext(side = 1, "Time Points", line = -3, font = 2)
mtext(side = 3, "Scheme 2A", line = -4, cex = 1.5, font = 2)

#axis(1,at = seq(0,48,6), labels = seq(0,48,6))
text(x = timePoints[seq(2,48,2)], y = rep(0,48)[seq(2,48,2)], labels = timePoints[seq(2,48,2)], font = 2)
box(lwd = 3)

plot(x = timePoints[seq(1,48,2)],
     y = rep(0,48)[seq(1,48,2)], 
     pch = rep(c(21,23),24), 
     cex = 4, 
     xlim = c(0,49),
     ylim = c(-1.5,1.5),
     xlab = "",
     ylab = "",
     # main = "Scheme 2B",
     bg = rep(c("#377eb8"),24),
     axes = F)
mtext(side = 1, "Time Points", line = -3, font = 2)
mtext(side = 3, "Scheme 2B", line = -4, cex = 1.5, font = 2)

#axis(1,at = seq(0,48,6), labels = seq(0,48,6))
text(x = timePoints[seq(1,48,2)], y = rep(0,48)[seq(1,48,2)], labels = timePoints[seq(1,48,2)], col = "white", font = 2)
box(lwd = 3)

plot(x = timePoints[c(seq(2,48,2))[seq(1,24,2)]],
     y = rep(0,48)[c(seq(2,48,2))[seq(1,24,2)]], 
     pch = 21, 
     cex = 4,
     xlim = c(0,49),
     ylim = c(-1.5,1.5),
     xlab = "",
     ylab = "",
     # main = "Scheme 4A",
     bg = rep("orange",12),
     axes = F)
mtext(side = 1, "Time Points", line = -3, font = 2)
mtext(side = 3, "Scheme 4A", line = -4, cex = 1.5, font = 2)

#axis(1,at = seq(0,48,6), labels = seq(0,48,6))
text(x = timePoints[c(seq(2,48,2))[seq(1,24,2)]], y = rep(0,48)[c(seq(2,48,2))[seq(1,24,2)]], labels = timePoints[c(seq(2,48,2))[seq(1,24,2)]], font = 2)
box(lwd = 3)

plot(x = timePoints[c(seq(2,48,2))[seq(2,24,2)]],
     y = rep(0,48)[c(seq(2,48,2))[seq(2,24,2)]],
     pch = 23,
     cex = 4,
     xlim = c(0,49),
     ylim = c(-1.5,1.5),
     xlab = "",
     ylab = "",
     # main = "Scheme 4B"
     bg = rep("orange",12),
     axes = F)
mtext(side = 1, "Time Points", line = -3, font = 2)
mtext(side = 3, "Scheme 4B", line = -4, cex = 1.5, font = 2)

#axis(1,at = seq(0,48,6), labels = seq(0,48,6))
text(x = timePoints[c(seq(2,48,2))[seq(2,24,2)]], y = rep(0,48)[c(seq(2,48,2))[seq(2,24,2)]], labels = timePoints[c(seq(2,48,2))[seq(2,24,2)]], font = 2)
box(lwd = 3)


plot(x = timePoints[c(seq(1,48,2))[seq(1,24,2)]],
     y = rep(0,48)[c(seq(1,48,2))[seq(1,24,2)]], 
     pch = 21, 
     cex = 4,
     xlim = c(0,49),
     ylim = c(-1.5,1.5),
     xlab = "",
     ylab = "",
     # main = "Scheme 4C"
     bg = rep("#377eb8",12),
     axes = F)
mtext(side = 1, "Time Points", line = -3, font = 2)
mtext(side = 3, "Scheme 4C", line = -4, cex = 1.5, font = 2)

#axis(1,at = seq(0,48,6), labels = seq(0,48,6))
text(x = timePoints[c(seq(1,48,2))[seq(1,24,2)]], y = rep(0,48)[c(seq(1,48,2))[seq(1,24,2)]], labels = timePoints[c(seq(1,48,2))[seq(1,24,2)]], col = "white", font = 2)
box(lwd = 3)

plot(x = timePoints[c(seq(1,48,2))[seq(2,24,2)]],
     y = rep(0,48)[c(seq(1,48,2))[seq(2,24,2)]],
     pch = 23,
     cex = 4,
     xlim = c(0,49),
     ylim = c(-1.5,1.5),
     xlab = "",
     ylab = "",
     # main = "Scheme 4D"
     bg = rep("#377eb8",12),
     axes = F)
mtext(side = 1, "Time Points", line = -3, font = 2)
mtext(side = 3, "Scheme 4D", line = -4, cex = 1.5, font = 2)
#axis(1,at = seq(0,48,6), labels = seq(0,48,6))
text(x = timePoints[c(seq(1,48,2))[seq(2,24,2)]], y = rep(0,48)[c(seq(1,48,2))[seq(2,24,2)]], labels = timePoints[c(seq(1,48,2))[seq(2,24,2)]], col = "white", font = 2)
box(lwd = 3)

