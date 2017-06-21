
# ps(w)
psf <- function(w, pe=-1.58*10^-3, b=4.38)pe*w^(-b)

# PLC(px)
PLCf <- function(px, kxmax=5, c=2.64, d=3.54){
  
  # xylem conductance function
  kxf <- function(x)kxmax*exp(-(-x/d)^c)
  
  kx <- kxf(px)
  res <- 1-kx/kxmax
  return(res)
}

