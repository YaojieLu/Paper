
library(nlme)
library(doBy)

# Data
dataS1 <- read.csv("Data/Scenario 1.csv")
dataS2 <- read.csv("Data/Scenario 2.csv")
dataS3 <- read.csv("Data/Scenario 3.csv")
dataS1g1 <- read.csv("Data/Scenario 1 - g1(ps).csv")
dataS2g1 <- read.csv("Data/Scenario 2 - g1(ps).csv")
dataS3g1 <- read.csv("Data/Scenario 3 - g1(ps).csv")
# a
datao <- read.csv("Data/Choat2012.csv")
dataAng <- subset(datao, Type=="Angiosperm", select=c("Psi50", "Psimin"))
dataAng <- dataAng[order(dataAng$Psi50), ]
dataGym <- subset(datao, Type=="Gymnosperm", select=c("Psi50", "Psimin"))
dataGym <- dataGym[order(dataGym$Psi50), ]
# c
rawdata <- read.csv("Data/Martinez-vilalta 2014.csv")
datao <- data.frame(P50=rawdata$P50.Mpa, sigma=rawdata$sigma)
datao <- datao[order(datao$P50), ]
# d
zhou <- read.csv("Data/Zhou 2014.csv")

# Regression
# a
fitAng <- nls(Psimin ~ -a*Psi50+b, data=dataAng, start=list(a=-0.4, b=-1),
              control=c(minFactor=1e-5))
fitGym <- nls(Psimin ~ -a*Psi50+b, data=dataGym, start=list(a=-0.4, b=-1),
              control=c(minFactor=1e-5))
# c
fit <- nls(sigma ~ a*(-P50)^b+c, data=datao, start=list(a=0.8459, b=0.1261, c=-0.1320),
           control=c(minFactor=1e-5))
# d
fits <- nlsList(g1 ~ a*exp(b*LWP)|Species, start=list(a=10, b=0.5), data=zhou)
rangefun <- function(x)setNames(range(x), c("min", "max"))
ran <- summaryBy(LWP ~ Species, FUN=rangefun, data=zhou)
pars <- cbind(ran, as.data.frame(coef(fits)))

# Figure
Cols <- c("lightblue", "lightpink", "purple", "orange", "forestgreen")
windows(8*7/4, 6*7/4)
par(mgp=c(2.2, 1, 0), xaxs="i", yaxs="i", lwd=2, mar=c(4, 4.2, 0.8, 0.5))
layout(mat=matrix(c(1, 2, 5, 3, 4, 5), nrow=2, ncol=3, byrow=TRUE), widths=c(0.45, 0.45, 0.08))

source("P50 vs pxmin.r")
source("P50 vs pxgs50.r")
source("P50 vs Slope.r")
par(yaxs="r")
source("g1(ps) 2014.r")

par(mar=c(12, 0, 12, 0))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend("top", legend=c("I", "II", "III"), title="Scenario", lty=c(1), col=Cols[3:5])
legend("center", legend=c("25", "100"), title=expression(beta), lty=c(1, 2), col=Cols[4])
legend("bottom", legend=c("25%", "50%", "75%"), title=expression(italic(p[k[x]])), lty=1, lwd=c(1, 2, 4), col=Cols[4])

dev.copy2pdf(file = "Figures/Figure 3.pdf")
