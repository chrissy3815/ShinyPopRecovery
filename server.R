server <- function(input, output) {
  
  warning_message<- reactiveVal(NULL)
  
  observeEvent(
    {
      DDparams$matrixID()
      DDparams$DDfunc()
      DDparams$K()
      DDparams$b()
      DDparams$x0()
      DDparams$x1()
      DDparams$theta()
      DDparams$DDmethod()
      DDparams$DDcalcN()
      DDparams$ismemberN()
      DDparams$tmax()
      DDparams$vectorMethod()
      DDparams$DDelem()
      DDparams$n_adults()
    },
    {
      warning_message(NULL)
    }
  )
  
  DDparams<- paramsServer("side1")
  
  matinfo<- matrixServer('mat1', DDparams$matrixID)
  
  carryingcapacityServer("tab1", DDparams$K)
  
  proj_result<- projectionServer("plot1", DDparams, warning_message)
  
  DDpanelServer("DDplot", DDparams)
  
}