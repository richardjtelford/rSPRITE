# Generate a sample of one or more unique SPRITE solutions.
#'@importFrom utils tail
rSprite.getSample <- function (
  maxCases, N, tMean, tSD, scaleMin, scaleMax, dp=2, fixed=c(), 
  rSprite.dust = sqrt(.Machine$double.eps),
  rSprite.huge, 
  rSprite.maxDupLoops = 8) {
  result <- list(rows=c(), label="")
  
  # Check mean is possible with GRIM; if not, identify the nearest valid mean.
  tMean <- rSprite.checkGrim(N, tMean, dp, rSprite.dust = rSprite.dust)
  
  # Determine minimum and maximum SDs.
  sdLimits <- rSprite.sdLimits(
    N, tMean, scaleMin, scaleMax, dp, 
    rSprite.huge = rSprite.huge)
  
  for (m in 1:2) {
    mSD <- sdLimits[m]
    s <- ""
    if ((m == 1) && (mSD > tSD)) {
      s <- "small; minimum="
    }
    else if ((m == 2) && (mSD < tSD)) {
      s <- "large; maximum="
    }
    
    if (s != "") {
      dpformat <- paste("%.", dp, "f", sep="")
      s <- paste("Target SD ", sprintf(dpformat, tSD), " is too ", s, sprintf(dpformat, mSD), sep="")
      rSprite.message(s, shinyType="warning")
      return(result)
    }
  }
  
  if (scaleMin >= scaleMax) {
    s <- paste("Scale minimum should be smaller than maximum")
    rSprite.message(s, shinyType="warning")
    return(result)
  }
  
  result$rows <- c()
  nCases <- 0
  result$label <- rSprite.chartLabel(N, tMean, tSD, scaleMin, scaleMax, dp, (maxCases > 9))
  for (i in 1:(maxCases * rSprite.maxDupLoops)) {
    vec <- rSprite.seekVector(N, tMean, tSD, scaleMin, scaleMax, dp, fixed, result$label, rSprite.dust = rSprite.dust, rSprite.huge = rSprite.huge)
    if (length(vec) == 0) {
      break                                 # we failed to find a case despite many tries
    }
    
    fullVec <- sort(c(vec, fixed))          # sorting lets us find duplicates more easily
    if (length(result$rows) == 0) {
      result$rows <- matrix(fullVec, nrow=1)
    }
    else {
      newRows <- rbind(result$rows, fullVec)
      if (tail(duplicated(newRows), 1)) {   # the solution we just found is a duplicate
        dups <- dups + 1
        if (dups > maxDups) {
          break
        }
        else {
          next
        }
      }
      
      result$rows <- newRows
    }
    
    nCases <- nrow(result$rows)
    s <- paste("Found ", nCases, " unique solution", (if (nCases == 1) "" else "s"), sep="")
    rSprite.message(s, showNow=TRUE)    # progress counter
    
    if (nCases >= maxCases) {           # we have enough cases now
      break
    }
    
    # Calculate the maximum number of consecutive duplicates we will accept before deciding to give up.
    # The value of 0.00001 below is our nominal acceptable chance of missing a valid solution;
    #  however, it's extremely likely that all possible solutions are not all equally likely to be found.
    # So we also set a floor of 100 attempts.
    maxDups <- max(round(log(0.00001) / log(nCases / (nCases + 1))), 100)
    dups <- 0
  }
  
  if (nCases < maxCases) {
    if (nCases == 0) {
      s <- paste("No solution found for ", paste(result$label, collapse=" "), sep="")
    }
    else {
      was <- if (nCases == 1) "was" else "were"
      s <- paste(maxCases, " unique solutions were requested, but only ", nrow(result$rows), " ", was, " found", sep="")
    }
    
    rSprite.message(s, shinyType="warning")
  }
  
  return(result)
}
