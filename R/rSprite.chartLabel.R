# Build the label to go across the top of each results chart.
rSprite.chartLabel <- function (N, tMean, tSD, scaleMin, scaleMax, dp, splitLine) {
  dpformat <- paste("%.", dp, "f", sep="")
  label <- paste("N=", N
                 , " (", scaleMin, "-", scaleMax, ")%%"
                 , "M=", sprintf(dpformat, tMean)
                 , " SD=", sprintf(dpformat, tSD)
                 , sep=""
  )
  
  if (splitLine) {
    label <- unlist(strsplit(label, "%%"))
  }
  else {
    label <- gsub("%%", " ", label)
  }
  
  return (label)
}
