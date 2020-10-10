# Find a single vector of responses that matches the target mean and SD.
# Assumes that the mean has been checked for GRIM consistency (see rSprite.getSample).
#' @importFrom stats runif sd

rSprite.seekVector <- function (
  N, tMean, tSD, scaleMin, scaleMax, dp=2, fixed=c(), label, 
  rSprite.dust = sqrt(.Machine$double.eps), 
  rSprite.huge, 
  rSprite.maxDeltaLoopsLower = 10000,
  rSprite.maxDeltaLoopsUpper = 1000000) {
  # Generate some random starting data.
  rN <- N - length(fixed)
  scaleMinZB <- 0
  scaleMaxZB <- scaleMax - scaleMin
  tMeanZB <- tMean - scaleMin
  vec <- pmax(pmin(as.integer(runif(rN) * (2 * tMean + 1)), scaleMax), scaleMin)
  result <- c()
  
  if (length(fixed) > 0) {         # replace any of the fixed numbers with a random non-fixed number
    whichFixed <- which(vec %in% fixed)
    notFixed <- sample(setdiff(scaleMin:scaleMax, fixed), length(whichFixed), replace=TRUE)
    vec[whichFixed] <- notFixed
  }
  
  # Adjust mean of starting data.
  granule <- ((0.1 ^ dp) / 2) + rSprite.dust   # allow for rounding errors
  meanOK <- FALSE
  maxStartLoops <- N * (scaleMax - scaleMin)
  
  for (i in 1:maxStartLoops) {
    fullVec <- c(vec, fixed)
    cMean <- mean(fullVec)
    dif <- abs(cMean - tMean)
    if (dif < granule) {
      meanOK <- TRUE
      break;
    }
    
    # Identify numbers that we can increment or decrement.
    # This should exclude numbers that would become one of the fixed values.
    deltaMean <- 1
    if (    (length(fixed) > 0)
            && (runif(1) < 0.2)
    ) {
      deltaMean <- 2       # This allows us to "jump over" the fixed values, if they are not at the extremities.
    }
    
    increaseMean <- (cMean < tMean)
    if (increaseMean) {
      filter <- (vec < (scaleMax - deltaMean + 1)) & (!(vec %in% (fixed - deltaMean)))
    }
    else {
      filter <- (vec > (scaleMin + deltaMean - 1)) & (!(vec %in% (fixed + deltaMean)))
    }
    
    canBumpMean <- which(filter)
    bumpMean <- canBumpMean[as.integer(runif(1) * length(canBumpMean)) + 1]   # select a changeable number
    vec[bumpMean] <- vec[bumpMean] + (if (increaseMean) deltaMean else -deltaMean)
  }
  
  if (!meanOK) {
    s <- "Couldn't initialize data with correct mean"  # this probably indicates a coding error, if the mean is in range
    rSprite.message(s, shinyType="error")
    return(result)
  }
  
  maxLoops <- min(max(round(N * ((scaleMax - scaleMin) ^ 2)), rSprite.maxDeltaLoopsLower), rSprite.maxDeltaLoopsUpper)
  found <- FALSE
  
  for (i in 1:maxLoops) {
    cSD <- sd(c(vec, fixed))
    if (abs(cSD - tSD) <= granule) {
      result <- vec
      break
    }
    
    vec <- rSprite.delta(vec, tMean, tSD, scaleMin, scaleMax, dp, fixed, 
                         rSprite.huge = rSprite.huge)
  }
  
  return(result)
}