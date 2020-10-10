#' @importFrom gridExtra grid.arrange
#' @importFrom moments skewness

# Build a grid containing all the results charts.
rSprite.buildCharts <- function (sample, scaleMin, scaleMax, gridSize) {
  rows <- sample$rows
  
  nCases <- nrow(rows)
  s <- paste("Plotting ", nCases, " unique solution", (if (nCases == 1) "" else "s"), "...", sep="")
  rSprite.message(s, showNow=TRUE)
  
  if (nCases > 1) {
    rows <- rows[order(apply(rows, 1, skewness)),]
  }
  
  xMax <- max(rows)
  yMax <- max(unlist(apply(rows, 1, table)))
  grobs <- apply(rows, 1, function (x) {
    rSprite.buildOneChart(x, scaleMin, scaleMax, gridSize, xMax, yMax, sample$label)
  })
  layoutMatrix <- matrix(1:(gridSize ^ 2), nrow=gridSize, ncol=gridSize, byrow=TRUE)
  grid.arrange(grobs=grobs, layout_matrix=layoutMatrix)
}
