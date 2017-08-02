
# Data
dataS2 <- read.csv("Data/Scenario 2 - gs(w).csv")
a <- 1.6
nZ <- 0.5
p <- 43200
l <- 1.8e-5
LAI <- 1
h <- l*a*LAI/nZ*p
VPD <- 0.02
h3 <- 0.01
dataS2$E <- h*VPD*dataS2$gs*nZ*1000*365
f <- function(w)h3*w*nZ*1000*365

# Figures
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(2.8, 3.5, 1, 0.7), mfrow=c(1, 1))
# gs(w)
plot(0, 0, type="n", xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(0, 1), ylim=c(0, 2000), cex.lab=1.3)

points(subset(dataS2, pkx==0.5 & h3==25, select=c("w", "E")), type="l")
curve(f, 0, 1, lty=2, add=T)

axis(1, xlim=c(0, 1), pos=0, lwd=2)
mtext(expression(italic(s)),side=1, line=1.85, cex=1.3)
axis(2, ylim=c(0, 100), pos=0, lwd=2)
mtext(expression(italic(E)~or~italic(L)~(mm~year^-1)),side=2,line=1.8, cex=1.3)
legend("topleft", legend=c("Transpiration", "Other loss terms"), lty=1:2)
box()

dev.copy2pdf(file = "Figures/Figure S2.pdf")
