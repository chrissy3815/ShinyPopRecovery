matrixUI <- function(id) {
  ns <- NS(id)
  tagList(value_box(
             title='Selected matrix population model',
             div(tableOutput(NS(id,"Amat")), style='font-size:80%')
           ),
           card(div(
             h3('Description of selected matrix and associated study'),
             uiOutput(NS(id,"metadata"))
           )
           )
  )
}

matrixServer <- function(id, matrixID) {
  
  moduleServer(
    id,
    function(input, output, session) {
      output$Amat<- renderTable({
        eval(parse(text=paste0("MPMs_preloaded$",matrixID(),"$Amat")))
      })
      output$metadata<- renderUI({
        HTML(eval(parse(text=paste0("MPMs_preloaded$",matrixID(),"$metadata"))))
      })
    }
  )
}