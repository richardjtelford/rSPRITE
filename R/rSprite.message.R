# Store a message for future display (unless otherwise specified).
# If we have unloaded Shiny for debugging, show the message immediately.


rSprite.message <- function (s, shinyType="default", showNow=FALSE) {
  if (!exists("shinyUI")) {
    cat("rSPRITE message: |", s, "| (shinyType=", shinyType, ")", "\n", sep="")
    return()
  }
  
  message <- paste(shinyType, s, sep="%%")
  if (showNow) {
    rSprite.shinyMessages(list(message))
  }
  else {
    rSprite.messageList <<- append(rSprite.messageList, message)
  }
}