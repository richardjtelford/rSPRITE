# Function to display one or more notification messages.
#' @importFrom shiny showNotification
rSprite.shinyMessages <- function (messageList) {

  uniqueMessages <- unique(unlist(messageList))
  notifIdList <- lapply(uniqueMessages, function (x) {
    split <- unlist(strsplit(x, "%%"))
    messageType <- split[1]
    messageText <- split[2]
    id <- showNotification(messageText, type=messageType)
    id
  })
}
