
library(nlme)
library(doBy)

# Data
dataS1 <- read.csv("Data/Scenario 1 - g1(ps).csv")
dataS2 <- read.csv("Data/Scenario 2 - g1(ps).csv")
dataS3 <- read.csv("Data/Scenario 3 - g1(ps).csv")
zhou <- read.csv("Data/Zhou2013_g1data.csv")

# Zhou 2013
fits <- nlsList(g1new ~ a*exp(b*LWP)|Species, start=list(a=10, b=0.5), data=zhou)
rangefun <- function(x)setNames(range(x), c("min", "max"))
ran <- summaryBy(LWP ~ Species, FUN=rangefun, data=zhou)
pars <- cbind(ran, as.data.frame(coef(fits)))

# Figures
Cols <- c("lightblue", "lightpink", "purple", "forestgreen", "orange")
windows(8, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", lwd=2, mar=c(3.5, 3.5, 1, 0.5), mfrow=c(1, 1))
plot(0, 0, type="n", xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(-6, 0), ylim=c(0, 15), cex.lab=1.3)

for(i in 1:nrow(pars))curve(pars[i, "a"]*exp(pars[i, "b"]*x), from=pars[i, "LWP.min"], to=0, add=TRUE, lwd=1, lty=3)

points(subset(dataS1, h3==25, select=c("ps", "g1")), type="l", col=Cols[3])
points(subset(dataS1, h3==100, select=c("ps", "g1")), type="l", lty=2, col=Cols[3])
points(dataS2, type="l", col=Cols[4])
points(subset(dataS3, pkx==0.5 & h3==25, select=c("ps", "g1")), type="l", col=Cols[5])
points(subset(dataS3, pkx==0.5 & h3==100, select=c("ps", "g1")), type="l", lty=2, col=Cols[5])
points(subset(dataS3, pkx==0.25, select=c("ps", "g1")), type="l", lwd=1, col=Cols[5])
points(subset(dataS3, pkx==0.75, select=c("ps", "g1")), type="l", lwd=4, col=Cols[5])

axis(1, xlim=c(-6, 0), pos=-15*0.04, lwd=2)
mtext(expression(psi[s]~(MPa)),side=1,line=2.4, cex=1.3)
axis(2, ylim=c(0, 15), pos=-6, lwd=2, at=c(0, 5, 10, 15))
mtext(expression(italic(g[1])~(kPa^-0.5)),side=2,line=1.8, cex=1.3)
legend("left", c("Zhou et al. 2013"), lwd=1, lty=3, col="black")
legend("topleft", legend=c("I", "II", "III"), title="Scenario", lty=1, col=Cols[3:5])
legend("top", legend=c("25", "100"), title=expression(beta), lty=c(1, 2), col="black")
legend("topright", legend=c("25%", "50%", "75%"), title=expression(italic(p[k[x]])), lty=1, lwd=c(1, 2, 4), col=Cols[5], bg="white")

dev.copy2pdf(file = "Figures/g1(ps).pdf")
