
# Data
datam <- read.csv("Data/P50 vs pxmin.csv")
datao <- read.csv("Data/Choat2012.csv")
dataAng <- subset(datao, Type=="Angiosperm", select=c("Psi50", "Psimin"))
dataAng <- dataAng[order(dataAng$Psi50), ]
dataGym <- subset(datao, Type=="Gymnosperm", select=c("Psi50", "Psimin"))
dataGym <- dataGym[order(dataGym$Psi50), ]

# Regression
fitAng <- nls(Psimin ~ -a*Psi50+b, data=dataAng, start=list(a=-0.4, b=-1),
              control=c(minFactor=1e-5))
fitGym <- nls(Psimin ~ -a*Psi50+b, data=dataGym, start=list(a=-0.4, b=-1),
              control=c(minFactor=1e-5))

# Figures
Cols <- c("lightblue", "lightpink", "purple", "forestgreen", "orange")
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.5, 3.5, 0.5, 0.5), mfrow=c(1, 1))
plot(0, 0, type="n",
     xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(-15, 0), ylim=c(-15, 0),
     cex.lab=1.3, col=Cols[1])

axis(1, xlim=c(-15, 0), pos=-15, lwd=2, at=c(-15, -10, -5, 0))
mtext(expression(italic(psi[x50])~(MPa)),side=1,line=2.4, cex=1.3)
axis(2, ylim=c(-15, 0), pos=-15, lwd=2, at=c(-15, -10, -5, 0))
mtext(expression(italic(psi[xmin])~(MPa)),side=2,line=1.8, cex=1.3)
abline(a=0, b=1, lwd=1, lty=3)
legend("topleft", legend=c("I", "II", "III"), title="Scenario", lty=c(1), col=Cols[3:5])
legend("bottomright", c("Angiosperm", "Gymnosperm"), pch=c(1, 2), col=Cols[1:2])

points(dataAng, type="p", col=Cols[1], pch=1)
lines(dataAng$Psi50, predict(fitAng), col=Cols[1])
points(dataGym, type="p", col=Cols[2], pch=2)
lines(dataGym$Psi50, predict(fitGym), col=Cols[2])

points(subset(datam, pkx=="100%" & h3=="25", select=c("P50", "pxmin")), type="l", col=Cols[3])
points(subset(datam, pkx=="100%" & h3=="100", select=c("P50", "pxmin")), type="l", lty=2, col=Cols[3])
points(subset(datam, pkx=="0%", select=c("P50", "pxmin")), type="l", col=Cols[4])
points(subset(datam, pkx=="50%" & h3=="25", select=c("P50", "pxmin")), type="l", col=Cols[5])
points(subset(datam, pkx=="50%" & h3=="100", select=c("P50", "pxmin")), type="l", lty=2, col=Cols[5])

dev.copy2pdf(file = "Figures/Psimin - Psi50.pdf")
