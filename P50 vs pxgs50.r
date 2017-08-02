#
## Data
#dataS1 <- read.csv("Data/Scenario 1.csv")
#dataS2 <- read.csv("Data/Scenario 2.csv")
#dataS3 <- read.csv("Data/Scenario 3.csv")
#
## Figures
#Cols <- c("lightblue", "lightpink", "purple", "orange", "forestgreen")
#windows(8, 6)
#par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.5, 3.5, 1, 1), mfrow=c(1, 1))
plot(0, 0, type="n",
     xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(-10, 0), ylim=c(-10, 0), cex.lab=1.3)

curve(0.49*x-0.42, -7, -1, lty=2, add=T, lwd=6)

points(subset(dataS1, h3==25 & d<=10, select=c("P50", "pxgs50")), type="l", col=Cols[3])
#points(subset(dataS1, h3==100 & d<=10, select=c("P50", "pxgs50")), type="l", lty=2, col=Cols[3])
points(subset(dataS2, k==0.05 & MAP==1825 & pkx==0.5 & h3==25 & d<=10, select=c("P50", "pxgs50")), type="l", col=Cols[4])
points(subset(dataS2, k==0.05 & MAP==1825 & pkx==0.5 & h3==100 & d<=10, select=c("P50", "pxgs50")), type="l", lty=2, col=Cols[4])
points(subset(dataS2, pkx==0.25 & d<=10, select=c("P50", "pxgs50")), type="l", lwd=1, col=Cols[4])
points(subset(dataS2, pkx==0.75 & d<=10, select=c("P50", "pxgs50")), type="l", lwd=4, col=Cols[4])
points(subset(dataS3, k==0.05 & MAP==1825, select=c("P50", "pxmin")), type="l", col=Cols[5])

text(-10*0.965, -10*0.035, "b)", cex=1.3)
axis(1, xlim=c(-10, 0), pos=-10, lwd=2, at=c(-10, -5, 0))
mtext(expression(psi[x50]~(MPa)),side=1,line=3.1, cex=1.3)
axis(2, ylim=c(-10, 0), pos=-10, lwd=2, at=c(-10, -5, 0))
mtext(expression(psi[x*", "*italic(g[s]*50)]~(MPa)),side=2,line=1.8, cex=1.3)
abline(a=0, b=1, lwd=1, lty=3)
legend("bottomright", c("Klein 2014"), lty=2, lwd=6, col="black")
#legend("bottomleft", legend=c("I", "II", "III"), title="Scenario", lty=c(1), col=Cols[3:5], bg="white")
#legend("bottom", legend=c("25", "100"), title=expression(beta), lty=c(1, 2), col=Cols[4])
#legend("bottomright", legend=c("25%", "50%", "75%"), title=expression(italic(p[k[x]])), lty=1, lwd=c(1, 2, 4), col=Cols[4])
box()

#dev.copy2pdf(file = "Figures/P50 vs pxgs50.pdf")
