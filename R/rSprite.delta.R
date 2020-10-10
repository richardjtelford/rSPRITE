# Make a single tweak to the data to try and move the SD in the desired direction.
# This will usually subtract 1 (or 2) from one element and add the same to another,
#  thus preserving the mean, but occasionally it will just do one of those things,
#  if the resulting mean is still GRIM-consistent.
#' @importFrom stats runif sd

rSprite.delta <- function (
  vec, tMean, tSD, scaleMin, scaleMax, dp=2, fixed=c(),
  rSprite.huge) {
  originalVec <- vec
  
  # Decide if we want to increment or decrement first.
  incFirst <- (runif(1) < 0.5)
  
  # Decide if we need to increase or decrease the SD.
  fullVec <- c(vec, fixed)
  increaseSD <- (sd(fullVec) < tSD)
  
  # Most of the time we change a pair of numbers by +/- 1, but changing by +/- 2 allows us to jump over fixed numbers.
  absDelta <- 1
  if (    (length(fixed) > 0)
          && (runif(1) < 0.2)
  ) {
    # Check there is at least one number that we can increment or decrement by 2!
    TwoFromEnd <- if (incFirst) (vec < (scaleMax - 1)) else (vec > (scaleMin + 1))
    if ((length(vec[TwoFromEnd])) > 0) {
      absDelta <- 2
    }
  }
  
  maxToInc <- scaleMax - absDelta                       # maximum value that we can increment
  minToDec <- scaleMin + absDelta                       # minimum value that we can decrement
  
  # Select an element to increment or decrement.
  # For better performance, we select from unique elements only; this means that any number that appears in the vector is
  #  equally likely to be chosen regardless of how often it appears. I'm not sure if this is good or bad.
  uniqueCanBump1 <- !duplicated(vec)
  delta1 <- if (incFirst) absDelta else -absDelta     # actual value we will add when bumping first number
  
  # The element that we change should be less than the maximum (increment) or greater than the minimum (decrement).
  # It should also not be <delta> less/greater than a fixed value (because adding delta would give us the fixed value).
  notFixed1 <- if (length(fixed) > 0) !(vec %in% (fixed - delta1)) else TRUE
  notEdge1 <- if (delta1 > 0) (vec <= maxToInc) else (vec >= minToDec)
  indexCanBump1 <- uniqueCanBump1 & notFixed1 & notEdge1
  
  # If we can't find an element to change, just return the original vector and let our caller sort it out.
  if (sum(indexCanBump1) == 0) {
    return(originalVec)
  }
  
  # Unless we have no other choice:
  # - If we want to make the SD larger, there is no point in incrementing the smallest element, or decrementing the largest
  # - If we want to make the SD smaller, there is no point in decrementing the smallest element, or incrementing the largest
  if (increaseSD) {
    noPoint1 <- if (incFirst) (vec == min(vec)) else (vec == max(vec))
  }
  else {
    noPoint1 <- if (incFirst) (vec == maxToInc) else (vec == minToDec)
  }
  indexCanBump1Try <- indexCanBump1 & (! noPoint1)
  if (sum(indexCanBump1Try) > 0) {
    indexCanBump1 <- indexCanBump1Try
  }
  
  whichCanBump1 <- which(indexCanBump1)
  whichWillBump1 <- whichCanBump1[as.integer(runif(1) * length(whichCanBump1)) + 1];
  willBump1 <- vec[whichWillBump1]
  vec[whichWillBump1] <- vec[whichWillBump1] + delta1
  
  # At this point we can decide to only change one of the elements (decrement one without incrementing another, or vice versa).
  # This enables us to explore different means that still round to the same target value.
  # So here we perform the first increment or decrement first, and see if the mean is still GRIM-consistent with the target mean.
  # If it is, then in a proportion of cases we don't adjust the other cell.
  newFullVec <- c(vec, fixed)
  newMean <- mean(newFullVec)
  meanChanged <- (round(newMean, dp) != tMean) # new mean is no longer GRIM-consistent
  
  if (meanChanged || (runif(1) < 0.4)) {
    delta2 <- -delta1                          # apply the opposite delta to a different element
    vecBump2 <- vec                            # make a scratch copy of the input vector so we can change it
    vecBump2[whichWillBump1] <- rSprite.huge   # remove the element chosen in part 1...
    uniqueCanBump2 <- !duplicated(vecBump2)    # ... but if there was more than one copy of that, it's still a candidate
    notFixed2 <- if (length(fixed) > 0) !(vec %in% (fixed - delta2)) else TRUE
    notEdge2 <- if (delta2 > 0) (vec <= maxToInc) else (vec >= minToDec)
    indexCanBump2 <- uniqueCanBump2 & notFixed2 & notEdge2 & (vecBump2 != rSprite.huge)
    
    # If we can't find an element to change in the opposite direction to the first, then if the mean with the first change is still OK,
    #  we return either the vector with that change. Otherwise we return the original vector and let our caller sort it out.
    if (sum(indexCanBump2) == 0) {
      return(if (meanChanged) originalVec else vec)
    }
    
    # Unless we have no other choice:
    # - If we want to make the SD larger:
    #   - If in step 1 we chose an element to increment, there is no point in now changing (decrementing) a larger one
    #   - If in step 1 we chose an element to decrement, there is no point in now changing (incrementing) a smaller one
    # - If we want to make the SD smaller:
    #   - If in step 1 we chose an element to increment, there is no point in now changing (decrementing) a smaller one
    #   - If in step 1 we chose an element to decrement, there is no point in now changing (incrementing) a larger one
    # There is also no point in incrementing an element that is equal to the new value of the one that we have already chosen.
    noPoint2 <- (   (if (increaseSD == incFirst) (vec > willBump1) else (vec < willBump1))
                    | (vec == (willBump1 + delta1))
    )
    indexCanBump2Try <- indexCanBump2 & (! noPoint2)
    if (sum(indexCanBump2Try) > 0) {
      indexCanBump2 <- indexCanBump2Try
    }
    
    whichCanBump2 <- which(indexCanBump2)
    whichWillBump2 <- whichCanBump2[as.integer(runif(1) * length(whichCanBump2)) + 1];
    vec[whichWillBump2] <- vec[whichWillBump2] + delta2
  }
  
  return(vec)
}