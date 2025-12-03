carryingcapacityUI <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(NS(id,"Kval"))
  )
}

carryingcapacityServer <- function(id, K) {
  moduleServer(
    id,
    function(input, output, session) {
      output$Kval<- renderText(K())
    }
  )
}