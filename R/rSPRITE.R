# Written by Nick Brown (nicholasjlbrown@gmail.com), 2018-2020.
# This work is licensed under a Creative Commons Attribution 4.0 International License (CC-BY).
#  See http://creativecommons.org/licenses/by/4.0/
# Thanks to Cédric Batailler for help with the X-axis.


# To do (some of these could be hard):
# Check when to turn X-axis numbers sideways, eg 13-77 N=345 M=26 SD=12, one pane.
# Allow zero as a number of a fixed value (i.e., that value explicitly does not appear).
# Idea: If no solution is found, print SD of nearest solution.

# Parameters that trade off speed versus completeness.
# maxDeltaLoopsLower controls how many times we tweak pairs of numbers before giving up hope of finding any solution;
#  it is the lower bound on a formula that includes the sample size and range.
# maxDeltaLoopsUpper is the absolute upper bound on that formula (a sanity check, in effect).
# maxDupLoops controls how many times we try to find another unique solution, when we know that at least one exists.
#' Run rSPRITE app
#'@importFrom shiny fluidPage titlePanel sidebarPanel numericInput selectInput column checkboxInput fluidRow conditionalPanel actionButton br downloadLink mainPanel plotOutput absolutePanel textOutput htmlOutput tags HTML shinyApp textInput sidebarLayout
#'@importFrom shinycssloaders withSpinner
#' @export

rSprite.app <- function(){



  
  N <- 45
  tMean <- 3.532
  tSD <- 1.561
  dp <- 2
  scaleMin <- 1
  scaleMax <- 7
  fixedValue <- ""
  fixedCount <- ""
  fixedSeed <- 0
  
  #tMean <- 19.4
  #tSD <- 19.9
  #dp <- 1
  #scaleMin <- 0
  #scaleMax <- 41
  #fixedValue <- 0
  #fixedCount <- 21
  #fixedSeed <- 1
  
  dstep <- c(0.1, 0.01, 0.001)[dp]
  
  ui <- fluidPage(
    titlePanel("rSPRITE beta 0.16")
  , sidebarLayout(
      position="left"
    , sidebarPanel(
        width=2
      , numericInput(inputId="scaleMin", label="Minimum scale value", value=scaleMin, min=-20, max=1, step=1)
      , numericInput(inputId="scaleMax", label="Maximum scale value", value=scaleMax, min=2, max=50, step=1)
      , numericInput(inputId="N", label="Sample size", value=N, min=2, max=10000, step=1)
      , numericInput(inputId="tMean", label="Target mean", value=round(tMean, dp), min=scaleMin, max=scaleMax, step=dstep)
  #limits    , numericInput(inputId="tMean", label="Target mean", value=round(tMean, dp), step=dstep)  #limits
      , numericInput(inputId="tSD", label="Target SD", value=round(tSD, dp), min=0, max=(((scaleMax - scaleMin) / 2) + 1), step=dstep)
  #limits    , numericInput(inputId="tSD", label="Target SD", value=round(tSD, dp), step=dstep)  #limits
      , numericInput(inputId="dp", label="Decimal places", value=dp, min=1, max=3, step=1)
      , selectInput(inputId="gridSize", label="Number of results", choices=(c(1:10) ^ 2), selected=9)
      , fluidRow(
          column(
            6
          , textInput(inputId="fixedResponse", label="Fixed value", value=fixedValue)
        )
        , column(
            6
          , textInput(inputId="fixedCount", label="Fixed count", value=fixedCount)
        )
      )
      , checkboxInput(inputId="fixedSeed", label="Use fixed seed", value=fixedSeed)
      , fluidRow(
          column(
            6
          , actionButton(inputId="go", label="Go!")
          )
        , column(
            6
          , actionButton(inputId="help", label="Help")
          )
        )
      , conditionalPanel(
          condition="output.plotDisplayed"
        , br()
        , downloadLink(outputId="downloadData", label="Download data")
      )
    )
    , mainPanel(
        withSpinner(
          plotOutput(outputId="plot", width="100%"), type=5
        )
      , absolutePanel(        # allows help panel to overlay plot panel
          top="50"
        , textOutput(outputId="dummy")
        , htmlOutput(outputId="help")
        )
      )
    )
  , tags$head(
      tags$style(
        HTML(".shiny-notification { position:relative; bottom:2px; left:-200px; width:125% }")
      )
    )
  )
  

  shinyApp(ui=ui, server=server)
}
