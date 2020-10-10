# Determine minimum and maximum SDs for given scale ranges, N, and mean.
#'@importFrom stats sd
rSprite.sdLimits <- function (N, tMean, scaleMin, scaleMax, dp, rSprite.huge) {
  result <- c(rSprite.huge, -rSprite.huge)        # impossible values
  
  aMax <- scaleMin                                # "aMax" means "value of a to produce the max SD"
  aMin <- floor(tMean)
  bMax <- max(scaleMax, scaleMin + 1, aMin + 1)   # sanity check (just scaleMax would normally be ok)
  bMin <- aMin + 1
  total <- round(tMean * N)
  for (abm in list(c(aMin, bMin, 1), c(aMax, bMax, 2))) {
    a <- abm[1]
    b <- abm[2]
    m <- abm[3]
    
    k <- round((total - (N * b)) / (a - b))
    k <- min(max(k, 1), N - 1)               # ensure there is at least one of each of two numbers
    vec <- c(rep(a, k), rep(b, N - k))
    diff <- sum(vec) - total
    if ((diff < 0) && (k > 1)) {
      vec <- c(rep(a, k - 1), abs(diff), rep(b, N - k))
    }
    else if ((diff > 0) && ((N - k) > 1)) {
      vec <- c(rep(a, k), diff, rep(b, N - k - 1))
    }
    result[m] <- round(sd(vec), dp)
  }
  
  return(result)
}
