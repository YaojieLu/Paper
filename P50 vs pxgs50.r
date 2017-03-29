
# Data
dataS1 <- read.csv("Data/Scenario 1.csv")
dataS2 <- read.csv("Data/Scenario 2.csv")
dataS3 <- read.csv("Data/Scenario 3.csv")

# Figures
Cols <- c("lightblue", "lightpink", "purple", "forestgreen", "orange")
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.5, 3.5, 1, 0.5), mfrow=c(1, 1))
plot(0, 0, type="n",
     xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(-10, 0), ylim=c(-10, 0), cex.lab=1.3)

points(subset(dataS1, h3==25 & d<=10, select=c("P50", "pxgs50")), type="l", col=Cols[3])
points(subset(dataS1, h3==100 & d<=10, select=c("P50", "pxgs50")), type="l", lty=2, col=Cols[3])
points(subset(dataS3, k==0.05 & MAP==1825 & pkx==0.5 & h3==25 & d<=10, select=c("P50", "pxgs50")), type="l", col=Cols[5])
points(subset(dataS3, k==0.05 & MAP==1825 & pkx==0.5 & h3==100 & d<=10, select=c("P50", "pxgs50")), type="l", lty=2, col=Cols[5])
points(subset(dataS3, pkx==0.25 & d<=10, select=c("P50", "pxgs50")), type="l", lwd=1, col=Cols[5])
points(subset(dataS3, pkx==0.75 & d<=10, select=c("P50", "pxgs50")), type="l", lwd=4, col=Cols[5])

curve(0.49*x-0.42, -7, -1, lty=3, add=T, lwd=8)

axis(1, xlim=c(-10, 0), pos=-10, lwd=2)
mtext(expression(psi[x50]~(MPa)),side=1,line=2.4, cex=1.3)
axis(2, ylim=c(-10, 0), pos=-10, lwd=2)
mtext(expression(psi[x50*", "*italic(g[s])]~(MPa)),side=2,line=1.8, cex=1.3)
abline(a=0, b=1, lwd=1, lty=3)
legend("topleft", c("Klein 2014"), lty=3, col="black")
legend("bottomleft", legend=c("I", "III"), title="Scenario", lty=c(1), col=c(Cols[3], Cols[5]), bg="white")
legend("bottom", legend=c("25", "100"), title=expression(beta), lty=c(1, 2), col="black")
legend("bottomright", legend=c("25%", "50%", "75%"), title=expression(italic(p[k[x]])), lty=1, lwd=c(1, 2, 4), col=Cols[5])

dev.copy2pdf(file = "Figures/P50 vs pxgs50.pdf")
