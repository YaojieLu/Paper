
# Data
dataS1 <- read.csv("Data/Scenario 1 - gs(w).csv")
dataS2 <- read.csv("Data/Scenario 2 - gs(w).csv")
dataS3 <- read.csv("Data/Scenario 3 - gs(w).csv")
dataS4 <- read.csv("Data/Scenario 4 - gs(w).csv")

# Figures
Cols <- c("purple", "orange", "forestgreen", "black")
windows(16, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(2.8, 3.5, 0.7, 0.7), mfrow=c(1, 2))
# gs(w)
plot(0, 0, type="n", xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(0, 1), ylim=c(0, 0.4), cex.lab=1.3)

points(subset(dataS2, pkx==0.5 & h3==25, select=c("w", "gs")), type="l", col=Cols[2])
points(subset(dataS2, pkx==0.5 & h3==100, select=c("w", "gs")), type="l", lty=2, col=Cols[2])
points(subset(dataS2, pkx==0.25, select=c("w", "gs")), type="l", lwd=1, col=Cols[2])
points(subset(dataS2, pkx==0.75, select=c("w", "gs")), type="l", lwd=4, col=Cols[2])
points(dataS1, type="l", col=Cols[1])
points(dataS3, type="l", col=Cols[3])
points(dataS4, type="l", col=Cols[4])

text(1*0.965, 0.4*0.965, "a)", cex=1.3)
text(1.01, 0.182, "Complete & immediate refilling", col=Cols[1], cex=1, pos=2)
text(1.01, 0.2255, "Partial & immediate refilling", col=Cols[2], cex=1, pos=2)
text(1.01, 0.2724, "No refilling", col=Cols[3], cex=1, pos=2)
text(1.01, 0.3542, "Complete & delayed refilling", col=Cols[4], cex=1, pos=2)

axis(1, xlim=c(0, 1), pos=0, lwd=2)
mtext(expression(italic(s)),side=1, line=1.85, cex=1.3)
axis(2, ylim=c(0, 0.4), pos=0, lwd=2)
mtext(expression(italic(g[s])~(mol~m^-2~s^-1)),side=2,line=1.8, cex=1.3)
legend("topleft", legend=c("I", "II", "III", "IV"), title="Scenario", lty=1, col=Cols[1:4])
legend("bottom", legend=c("25", "100"), title=expression(beta), lty=c(1, 2), col=Cols[2])
legend("bottomright", legend=c("25%", "50%", "75%"), title=expression(italic(p[k[x]])), lty=1, lwd=c(1, 2, 4), col=Cols[2], bg="white")
box()

# B(w)
plot(0, 0, type="n", xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(0, 1), ylim=c(0, 15), cex.lab=1.3)

points(subset(dataS2, pkx==0.5 & h3==25, select=c("w", "B")), type="l", col=Cols[2])
points(subset(dataS2, pkx==0.5 & h3==100, select=c("w", "B")), type="l", lty=2, col=Cols[2])
points(subset(dataS2, pkx==0.25, select=c("w", "B")), type="l", lwd=1, col=Cols[2])
points(subset(dataS2, pkx==0.75, select=c("w", "B")), type="l", lwd=4, col=Cols[2])
points(subset(dataS1, select=c("w", "B")), type="l", col=Cols[1])
points(subset(dataS3, select=c("w", "A")), type="l", col=Cols[3])

text(1*0.035, 15*0.965, "b)", cex=1.3)
axis(1, xlim=c(0, 1), pos=0, lwd=2)
mtext(expression(italic(s)),side=1, line=1.85, cex=1.3)
axis(2, ylim=c(0, 15), pos=0, lwd=2, at=c(0, 5, 10, 15))
mtext(expression(italic(B)~(mu*mol~m^-2~s^-1)), side=2, line=1.87, cex=1.3)
box()

dev.copy2pdf(file = "Figures/Figure 2.pdf")
