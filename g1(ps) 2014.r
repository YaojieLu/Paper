#
#library(nlme)
#library(doBy)
#
## Data
#dataS1g1 <- read.csv("Data/Scenario 1 - g1(ps).csv")
#dataS2g1 <- read.csv("Data/Scenario 2 - g1(ps).csv")
#dataS3g1 <- read.csv("Data/Scenario 3 - g1(ps).csv")
#zhou <- read.csv("Data/Zhou 2014.csv")
#
## Zhou 2013
#fits <- nlsList(g1 ~ a*exp(b*LWP)|Species, start=list(a=10, b=0.5), data=zhou)
#rangefun <- function(x)setNames(range(x), c("min", "max"))
#ran <- summaryBy(LWP ~ Species, FUN=rangefun, data=zhou)
#pars <- cbind(ran, as.data.frame(coef(fits)))
#
## Figures
#Cols <- c("lightblue", "lightpink", "purple", "orange", "forestgreen")
#windows(8, 6)
#par(mgp=c(2.2, 1, 0), xaxs="i", lwd=2, mar=c(3.5, 3.5, 1, 1), mfrow=c(1, 1))
plot(0, 0, type="n", xaxt="n", yaxt="n", xlab=NA, ylab=NA,
     xlim=c(-6, 0), ylim=c(0, 8), cex.lab=1.3)

for(i in 1:nrow(pars))curve(pars[i, "a"]*exp(pars[i, "b"]*x), from=pars[i, "LWP.min"], to=0, add=TRUE, lwd=1, lty=3)

points(subset(dataS1g1, h3==25, select=c("ps", "g1")), type="l", col=Cols[3])
#points(subset(dataS1g1, h3==100, select=c("ps", "g1")), type="l", lty=2, col=Cols[3])
points(subset(dataS2g1, pkx==0.5 & h3==25, select=c("ps", "g1")), type="l", col=Cols[4])
points(subset(dataS2g1, pkx==0.5 & h3==100, select=c("ps", "g1")), type="l", lty=2, col=Cols[4])
points(subset(dataS2g1, pkx==0.25, select=c("ps", "g1")), type="l", lwd=1, col=Cols[4])
points(subset(dataS2g1, pkx==0.75, select=c("ps", "g1")), type="l", lwd=4, col=Cols[4])
points(dataS3g1, type="l", col=Cols[5])

text(-6*0.965, 8*1.04*0.965, "d)", cex=1.3)
axis(1, xlim=c(-6, 0), pos=-8*0.04, lwd=2, at=c(-6, -3, 0))
mtext(expression(psi[s]~(MPa)),side=1,line=3.1, cex=1.3)
axis(2, ylim=c(0, 8), pos=-6, lwd=2, at=c(0, 4, 8))
mtext(expression(italic(g[1])~(kPa^-0.5)),side=2,line=1.8, cex=1.3)
legend("topright", c("Zhou et al. 2014"), lwd=1, lty=3, col="black")
#legend("topleft", legend=c("I", "II", "III"), title="Scenario", lty=1, col=Cols[3:5])
#legend("top", legend=c("25", "100"), title=expression(beta), lty=c(1, 2), col=Cols[4])
#legend("topright", legend=c("25%", "50%", "75%"), title=expression(italic(p[k[x]])), lty=1, lwd=c(1, 2, 4), col=Cols[4], bg="white")
box()

dev.copy2pdf(file = "Figures/g1(ps).pdf")
