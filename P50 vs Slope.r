
# Data
dataS1 <- read.csv("Data/Scenario 1.csv")
dataS2 <- read.csv("Data/Scenario 2.csv")
dataS3 <- read.csv("Data/Scenario 3.csv")
rawdata <- read.csv("Data/Martinez-vilalta 2014.csv")
datao <- data.frame(P50=rawdata$P50.Mpa, sigma=rawdata$sigma)
datao <- datao[order(datao$P50), ]

# Martinez-vilalta 2014
fit <- nls(sigma ~ a*(-P50)^b+c, data=datao, start=list(a=0.8459, b=0.1261, c=-0.1320),
           control=c(minFactor=1e-5))

# Figures
Cols <- c("lightblue", "lightpink", "purple", "orange", "forestgreen")
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(3.5, 3.5, 1, 1), mfrow=c(1, 1))
plot(datao$P50, datao$sigma,
     type="p", xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(-10, 0), ylim=c(0.2, 1.4), cex.lab=1.3)

lines(datao$P50, predict(fit), lty=3)

points(subset(dataS1, h3==25 & d<=10, select=c("P50", "slope")), type="l", col=Cols[3])
#points(subset(dataS1, h3==100 & d<=10, select=c("P50", "slope")), type="l", lty=2, col=Cols[3])
points(subset(dataS2, k==0.05 & MAP==1825 & pkx==0.5 & h3==25 & d<=10, select=c("P50", "slope")), type="l", col=Cols[4])
points(subset(dataS2, k==0.05 & MAP==1825 & pkx==0.5 & h3==100 & d<=10, select=c("P50", "slope")), type="l", lty=2, col=Cols[4])
points(subset(dataS2, pkx==0.25 & d<=10, select=c("P50", "slope")), type="l", lwd=1, col=Cols[4])
points(subset(dataS2, pkx==0.75 & d<=10, select=c("P50", "slope")), type="l", lwd=4, col=Cols[4])

axis(1, xlim=c(-10, 0), pos=0.2, lwd=2)
mtext(expression(psi[x50]~(MPa)),side=1,line=2.5, cex=1.3)
axis(2, ylim=c(0.2, 1.4), pos=-10, lwd=2)
mtext(expression(Slope~of~psi[x]*(psi[s])~(MPa~MPa^-1)), side=2, line=1.8, cex=1.3)
legend("bottomleft", legend=expression(Martinez-vilalta~italic(et~al.)~2014), lty=3, pch=1)
legend("topleft", legend=c("I", "II"), title="Scenario", lty=c(1), col=Cols[3:4])
legend("top", legend=c("25", "100"), title=expression(beta), lty=c(1, 2), col=Cols[4], bg="white")
legend("topright", legend=c("25%", "50%", "75%"), title=expression(italic(p[k[x]])), lty=1, lwd=c(1, 2, 4), col=Cols[4])

dev.copy2pdf(file = "Figures/P50 vs Slope.pdf")
