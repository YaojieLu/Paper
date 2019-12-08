
# ps(w)
psf <- function(w, pe=-1.58*10^-3, b=4.38)pe*w^(-b)

# the original PLC(px)
PLCf <- function(px, c=2.64, d=3.54)1-exp(-(-px/d)^c)
