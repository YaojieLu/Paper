
# Data
dataS1 <- read.csv("Data/Scenario 1 - gs(w).csv")
dataS2 <- read.csv("Data/Scenario 2 - gs(w).csv")
dataS3 <- read.csv("Data/Scenario 3 - gs(w).csv")
dataS4 <- read.csv("Data/Scenario 4 - gs(w).csv")

# Figures
Cols <- c("purple", "orange", "forestgreen", "black")
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.5, 3.5, 1, 1), mfrow=c(1, 1))
plot(0, 0, type="n", xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(0, 1), ylim=c(0, 0.4), cex.lab=1.3)

points(subset(dataS2, pkx==0.5 & h3==25, select=c("w", "gs")), type="l", col=Cols[2])
points(subset(dataS2, pkx==0.5 & h3==100, select=c("w", "gs")), type="l", lty=2, col=Cols[2])
points(subset(dataS2, pkx==0.25, select=c("w", "gs")), type="l", lwd=1, col=Cols[2])
points(subset(dataS2, pkx==0.75, select=c("w", "gs")), type="l", lwd=4, col=Cols[2])
points(dataS1, type="l", col=Cols[1])
points(dataS3, type="l", col=Cols[3])
points(dataS4, type="l", col=Cols[4])

text(1.01, 0.1867, "Complete & immediate refilling", col=Cols[1], cex=1, pos=2)
text(1.01, 0.2236, "Partial & immediate refilling", col=Cols[2], cex=1, pos=2)
text(1.01, 0.27, "No refilling", col=Cols[3], cex=1, pos=2)
text(1.01, 0.3815, "Complete & delayed refilling", col=Cols[4], cex=1, pos=2)

axis(1, xlim=c(0, 1), pos=0, lwd=2)
mtext(expression(italic(s)),side=1,line=2, cex=1.3)
axis(2, ylim=c(0, 0.4), pos=0, lwd=2)
mtext(expression(italic(g[s])~(mol~m^-2~s^-1)),side=2,line=1.8, cex=1.3)
legend("bottomleft", legend=c("I", "II", "III", "IV"), title="Scenario", lty=1, col=Cols[1:4])
legend("bottom", legend=c("25", "100"), title=expression(beta), lty=c(1, 2), col=Cols[2])
legend("bottomright", legend=c("25%", "50%", "75%"), title=expression(italic(p[k[x]])), lty=1, lwd=c(1, 2, 4), col=Cols[2], bg="white")

dev.copy2pdf(file = "Figures/gs(w).pdf")
