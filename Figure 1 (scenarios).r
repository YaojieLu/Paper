
source("Functions.r")

# Initialize
pe <- -1.58*10^-3
psL <- -4
pkx <- 0.6

PLCf1 <- function(px)PLCf(px)*100
PLCmax <- PLCf1(psL)
PLCmf <- function(px)PLCf1(x)-(PLCf1(x)-PLCf1(px))*pkx

# Figure
Cols <- c("purple", "orange", "darkgreen")
windows(24, 6)
par(mgp=c(2.2, 1, 0), xaxs="i", lwd=2, mar=c(3, 4, 0.5, 0.5), mfrow=c(1, 3))
# 1
x <- pe
curve(PLCf1, -10, x,
      xlim=c(-10, 0), ylim=c(0, 100), xaxt="n", yaxt="n",
      xlab=NA, ylab=NA,
      cex.lab=1.3, lty=2)
curve(PLCf1, x, pe, col=Cols[1], add=T)
segments(x, PLCf1(x), pe, PLCf1(x), col=Cols[2])
curve(PLCmf, x, pe, col=Cols[3], add=T)

axis(1, pos=-4, lwd=2, at=c(0))
axis(2, pos=-10, lwd=2, at=c(0, 50, 100))
mtext(expression(psi[x]~(MPa)), side=1, line=2, cex=1.3)
mtext("PLC (%)", side=2, line=2.1, cex=1.3)

box()
# 2
x <- -2
curve(PLCf1, -10, x,
      xlim=c(-10, 0), ylim=c(0, 100), xaxt="n", yaxt="n",
      xlab=NA, ylab=NA,
      cex.lab=1.3, lty=2)
curve(PLCf1, x, pe, col=Cols[1], add=T)
curve(PLCmf, x, pe, col=Cols[2], add=T)
segments(x, PLCf1(x), pe, PLCf1(x), col=Cols[3])

axis(1, pos=-4, lwd=2, at=c(0))
axis(2, pos=-10, lwd=2, at=c(0, 50, 100))
mtext(expression(psi[x]~(MPa)), side=1, line=2, cex=1.3)
mtext("PLC (%)", side=2, line=2.1, cex=1.3)

box()
# 3
x <- -4
curve(PLCf1, -10, x,
      xlim=c(-10, 0), ylim=c(0, 100), xaxt="n", yaxt="n",
      xlab=NA, ylab=NA,
      cex.lab=1.3, lty=2)
curve(PLCf1, x, pe, col=Cols[1], add=T)
curve(PLCmf, x, pe, col=Cols[2], add=T)
segments(x, PLCf1(x), pe, PLCf1(x), col=Cols[3])

axis(1, pos=-4, lwd=2, at=c(0))
axis(2, pos=-10, lwd=2, at=c(0, 50, 100))
mtext(expression(psi[x]~(MPa)), side=1, line=2, cex=1.3)
mtext("PLC (%)", side=2, line=2.1, cex=1.3)

arrows(-1, PLCmf(-1)*1.1, -1, PLCmax, lwd=1, col=Cols[2])
arrows(-1, PLCmf(-1)*0.9, -1, PLCf1(-1), lwd=1, col=Cols[2])
arrows(psL, PLCmax, psL, -4, lty=2, lwd=1)
arrows(psL, PLCmax, -10, PLCmax, lty=2, lwd=1)
text(psL-0.49, PLCmax/2, expression(psi[xL]), cex=1.3)
text((-10-psL)/2+psL, PLCmax+6.5, expression(PLC[italic(max)]), cex=1.3)

legend("bottomleft", c("Scenario I", "Scenario II", "Scenario III"), lty=c(1, 1, 1), col=Cols)
box()
dev.copy2pdf(file = "Figures/Figure 1.pdf")
