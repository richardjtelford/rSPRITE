#' Check GRIM
#' @description  See if a mean is GRIM-consistent. If not, return the nearest mean that is.
#' @param N Number of observations
#' @param tMean Reported mean
#' @param dp Number of decimal places
#' @param rSprite.dust Very small number



rSprite.checkGrim <- function (N, tMean, dp, rSprite.dust = sqrt(.Machine$double.eps)) {
  gMean <- tMean
  int <- round(tMean * N)           # nearest integer; doesn't matter if this rounds up or down
  frac <- int / N
  dif <- abs(tMean - frac)
  granule <- ((0.1 ^ dp) / 2) + rSprite.dust   # allow for rounding errors
  if (dif > granule) {
    gMean <- round(frac, dp)
    dpformat <- paste("%.", dp, "f", sep="")
    s <- paste("Mean ", sprintf(dpformat, tMean), " fails GRIM test - using ", sprintf(dpformat, gMean), sep="")
    rSprite.message(s, shinyType="warning")
  }
  
  return(gMean)
}
