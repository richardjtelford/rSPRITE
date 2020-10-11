#' @importFrom shiny reactive eventReactive renderText updateNumericInput isolate downloadHandler outputOptions renderPlot renderUI
#' @importFrom utils write.table

server <- function (input, output, session) {
  debounced_tMean <- debounce(input$tMean, 1000)
  
  rSprite.huge <- 1e15
  rSprite.dust <- 1e-12
  rSprite.prevHelp <- 0
  rSprite.prevGo <- 0
  rSprite.plotData <- c()
  rSprite.notifIdList <- list()
  rSprite.messageList <- list()
  
  debounced_tSD <- debounce(input$tSD, 1000)
  
  fixedCount <- reactive({
    result <- 0
    sn <- gsub(" ", "", input$fixedCount)
    if (sn != "") {
      result <- rSprite.huge
      if (grepl("^[0-9]+$", sn)) {
        f <- as.numeric(sn)
        if ((f > 0) && (f < input$N)) {
          result <- f
        }
      }
    }
    
    if (result == rSprite.huge) {
      s <- paste("Fixed count must be an integer from 1 to ", (input$N - 1)
                 , "; input |", input$fixedCount
                 , "| ignored"
                 , sep=""
      )
      rSprite.messageList <<- append(rSprite.messageList, rSprite.message(s, shinyType="warning"))
      result <- 0
    }
    
    result
  })
  
  fixedResponse <- reactive({
    result <- 0
    sn <- gsub(" ", "", input$fixedResponse)
    if (sn != "") {
      result <- rSprite.huge
      if (grepl("^-?[0-9]+$", sn)) {
        result <- as.numeric(sn)
      }
    }
    
    if (result == rSprite.huge) {
      s <- paste("Fixed value must be an integer from ", input$scaleMin
                 , " to ", input$scaleMax
                 , "; input |", input$fixedResponse
                 , "| ignored"
                 , sep=""
      )
      rSprite.messageList <<- append(rSprite.messageList, rSprite.message(s, shinyType="warning"))
      result <- 0
    }
    
    result
  })
  
  reactiveSample <- eventReactive(input$go, {
    rSprite.message("Calculating...", showNow=TRUE)
    set.seed(if (input$fixedSeed) 1 else as.numeric(Sys.time()))
    fixed <- rep(fixedResponse(), fixedCount())
    gridSize <- sqrt(as.numeric(input$gridSize))

    rSprite.getSample(
      gridSize ^ 2
      , input$N
      , input$tMean
      , input$tSD
      , input$scaleMin
      , input$scaleMax
      , input$dp
      , fixed,
      rSprite.dust = rSprite.dust,
      rSprite.huge = rSprite.huge
    )
  })
  
  rSprite.helpText <- c(
    "rSPRITE is an implementation by Nick Brown of SPRITE, an idea by James Heathers."
    , "<br/><br/>"
    , "rSPRITE simulates data from an integer (e.g., Likert-type) scale in the form of bar charts."
    , "<br/><br/>"
    , "You can request up to 100 samples to be presented on a square grid."
    , " You need to specify the minimum and maximum item values of the scale,"
    , " and the mean, standard deviation, and size of the sample."
    , " The charts are presented in increasing order of skewness, from top left to bottom right."
    , "<br/><br/>"
    , "Optionally, you can provide a fixed value and a count;"
    , " this forces every sample to contain exactly that many occurrences of that value,"
    , " which may be outside the scale range."
    , "<br/><br/>"
    , "You can also download the individual values that make up the bar charts to a CSV file."
    , "<br/><br/>"
    , "If you check the box labeled 'Use fixed seed', you will get the same results on every run;"
    , " this can be useful when reporting problems, but otherwise, leave this box unchecked."
    , "<br/><br/>"
    , "rSPRITE may not always find every solution when there are only a few to be found."
    , " If you get a message saying that fewer results were found than you hoped for,"
    , " please try a couple more times to see if one or two more solutions show up."
    , "<br/><br/>"
    , "A general observation: rSPRITE is a tool and not a complete system."
    , " Like any tool, it has the potential to be used incorrectly."
    , " If you ask it do something silly, it will do it, very probably without warning you."
    , "<br/><br/>"
    , "For more information on SPRITE in general, see <a href=https://peerj.com/preprints/26968v1/>here</a>."
    , "<br/><br/>"
    , "Please report bugs to nicholasjlbrown@gmail.com"
    , "<br/><br/>"
    , "Privacy policy: rSPRITE does not collect any information about you whatsoever."
    , " If you are using this code in a web browser at shinyapps.io, you can find the RStudio"
    , " terms of use <a href='https://www.rstudio.com/about/rstudio-service-terms-of-use/'>here</a>."
  )
  
  
  
  # This element is just a placeholder to catch and handle changes in the input controls and their relations to each other.
  # We never actually output anything to a text box.
  output$dummy <- renderText({
    N <- input$N
    #bounce    tMean <- input$tMean
    tMean <- debounced_tMean()    #bounce
    #bounce    tSD <- input$tSD
    tSD <- debounced_tSD()        #bounce
    scaleMin <- input$scaleMin
    scaleMax <- input$scaleMax
    dp <- input$dp
    dstep <- c(0.1, 0.01, 0.001)[dp]
    
    updateNumericInput(session, inputId="scaleMin", max=(scaleMax - 1))
    updateNumericInput(session, inputId="scaleMax", min=(scaleMin + 1))
    updateNumericInput(session, inputId="tMean", min=scaleMin, max=scaleMax, step=dstep)  #limits
    updateNumericInput(session, inputId="tSD", min=0, max=(((scaleMax - scaleMin) / 2) + 1), step=dstep)  #limits
    
    # It is tempting to force the mean value to a GRIM-consistent one here
    #  (cf. what we do for the SD below), but this would be an error,
    #  as we would be unable to "scroll" from one valid mean to another using the
    #  input spinners if there were any invalid intermediate values
    #  (we would constantly be forced back).
    # However, we do force the mean to be between scaleMin and scaleMax.
    if (!is.na(tMean)) {
      newMean <- max(min(round(tMean, dp), scaleMax), scaleMin)
      if (newMean != tMean) {
        updateNumericInput(session, inputId="tMean", value=newMean) #limits
      }
    }
    
    # Similarly, it would be nice to have the range for the SD limited by the current mean,
    #  but this leads to all sorts of complications. So we allow the user to enter an SD
    #  that is too small or large, and tell them later.
    if (!is.na(tSD)) {
      newSD <- max(min(round(tSD, dp), scaleMax), 0)
      if (newSD != tSD) {
        updateNumericInput(session, inputId="tSD", value=newSD)
      }
    }
    
    return()      # just falling out at the end gives an Shiny error message the first time we come here
  })
  
  output$plotDisplayed <- reactive({
    input$go
    input$help
    
    (length(rSprite.plotData) > 0)
  })
  outputOptions(output, "plotDisplayed", suspendWhenHidden=FALSE, priority=-1)
  
  output$downloadData <- downloadHandler(
    filename=function () {
      "spritedata.csv"
    }
    , content=function (file) {
      write.table(rSprite.plotData, file, row.names=FALSE, col.names=FALSE, sep=",")
    }
  )
  
  output$help <- renderUI({
    input$go                  # creates a dependency on the Go button
    
    helpText <- ""            # Unless the user clicked Help, we will clear any existing help text.
    if (input$help > rSprite.prevHelp) {    # user clicked the Help button
      rSprite.prevHelp <<- input$help
      helpText <- HTML(paste(rSprite.helpText, collapse=""))
    }
    
    isolate({
      helpText
    })
  })
  
  output$plot <- renderPlot({
    input$help                          # creates a dependency on the Help button
    rSprite.plotData <<- c()
    
    if (input$go > rSprite.prevGo) {    # user clicked the Go button
      rSprite.prevGo <<- input$go
    }
    else {
      return()           # this clears the plot area (which conveniently allows the help text to show)
    }
    
    isolate({
      N <- input$N
      tMean <- input$tMean
      tSD <- input$tSD
      scaleMin <- input$scaleMin
      scaleMax <- input$scaleMax
      dp <- input$dp
      gridSize <- sqrt(as.numeric(input$gridSize))
      
      sample <- reactiveSample()
      if (length(sample$rows) > 0) {
        if (    (gridSize == 10)
                && (session$clientData$url_hostname == "127.0.0.1")        # On developer's local screen...
        ) {                                                          # ... don't show 10x10 grid...
          rSprite.message("Skipping rendering", showNow=TRUE)           # ... to speed up generation of test data.
        }
        else {
          nCases <- nrow(sample$rows)
          s <- paste0("Plotting ", nCases, " unique solution", (if (nCases == 1) "" else "s"), "...")
          rSprite.message(s, showNow=TRUE)

          rSprite.buildCharts(sample, scaleMin, scaleMax)
          rSprite.message("Rendering...", showNow=TRUE)
        }
        
        rSprite.plotData <<- sample$rows
      }
      
      rSprite.notifIdList  <<- rSprite.shinyMessages(rSprite.messageList)
      rSprite.messageList <<- list()
    })
  }, height=function () {
    min(session$clientData$output_plot_width, 780)
  }, width=1200)
}
