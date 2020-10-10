# Function to display one or more notification messages.
#' @importFrom shiny showNotification removeNotification
rSprite.shinyMessages <- function (messageList) {
  lapply(rSprite.notifIdList, function (x) {
    removeNotification(x)
  })
  rSprite.notifIdList <<- list()
  
  uniqueMessages <- unique(unlist(messageList))
  sapply(uniqueMessages, function (x) {
    split <- unlist(strsplit(x, "%%"))
    messageType <- split[1]
    messageText <- split[2]
    id <- showNotification(messageText, type=messageType, duration=NULL, closeButton=FALSE)
    rSprite.notifIdList <<- append(rSprite.notifIdList, id)
  })
}
