paramsUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(NS(id,"matrixID"), label='Example population matrix', 
                choices=c("White-tailed deer" = 'whitedeer',
                          "Gray wolves" = 'graywolves',
                          "Crocodiles" = 'crocs',
                          "Peregrine falcon" = 'falcons',
                          "Broad-leafed forest (mixed species)" = 'ogawa')
    ),
    
    selectInput(NS(id,'DDfunc'),label='Functional form of density dependence',
                choices=c("Logistic" = 'logistic',
                          "Ricker (negative exponential)" = 'ricker',
                          "Beverton-Holt" = 'beverton-holt',
                          "Theta-logistic" = 'theta-logistic',
                          "Logistic probability of becoming a breeder" = 'log_pb')
    ),
    
    conditionalPanel(condition = "input.DDfunc == 'logistic' | input.DDfunc =='theta-logistic' ",
                     numericInput(NS(id, "K"), label = "Carrying capacity, K",
                                  value=110, min=10, max=200000, step=1),
                     ns=NS(id)
    ),
    
    conditionalPanel(condition = "input.DDfunc == 'ricker' | input.DDfunc == 'beverton-holt'",
                     numericInput(NS(id, "b"), label='Scaling parameter, b',
                                  value=1, min=1e-4, max=10),
                     ns=NS(id)
    ),
    
    conditionalPanel(condition = "input.DDfunc == 'theta-logistic' ",
                     numericInput(NS(id, "theta"), label='Shape parameter, theta',
                                  value=1, min=1e-4, max=10),
                     ns=NS(id)
    ),
    
    conditionalPanel(condition = "input.DDfunc == 'log_pb' ",
                     numericInput(NS(id,"x0"), label='Intercept',
                                  value=2.36, min=0, max=1000),
                     ns=NS(id)
    ),
    conditionalPanel(condition = "input.DDfunc == 'log_pb' ",
                     numericInput(NS(id, "x1"), label='Slope',
                                  value=-0.014, min=-10, max=10),
                     ns=NS(id)
    ),
    
    conditionalPanel(condition = "input.DDfunc != 'log_pb' ",
                     selectInput(NS(id,'DDmethod'), 
                                 label='How do you want to apply density dependence?',
                                 choices=c("The entire matrix"='matrix',
                                           "The fertility matrix"='fertility',
                                           "The survival matrix"='survival',
                                           #"Transitions into reproductive classes"= 'reprotrans',
                                           "Specify a single matrix element"= 'element')
                     ),
                     ns=NS(id)
    ),
    
    conditionalPanel(condition="input.DDmethod == 'element'",
                     textInput(NS(id,'DDelem'),
                               label='Identify the matrix element to which density dependence should apply as the [row, column].',
                               value="[1,4]"),
                     ns=NS(id)
    ),
    
    selectInput(NS(id, 'DDcalcN'), label='How do you want to calculate population density?',
                choices=c("Sum of all age/stage/size classes" = 'all',
                          "Sum of reproductively active size classes" = 'repro_classes',
                          "I want to specify which classes" = 'specify')
    ),
    
    conditionalPanel(condition="input.DDcalcN == 'specify'",
                     textInput(NS(id, 'ismemberN'),
                               label='Provide a comma-separated list of the size/stage/age
                                                                 classes which should be summed to calculate population density
                                                                 for the density-dependent function.',
                               value="1,3,5"),
                     ns=NS(id)
    ),
    
    numericInput(NS(id, 'tmax'), label='How many years into the future do you want to project?',
                 value=20, min=1, max=1000
    ),
    
    radioButtons(NS(id,'vectorMethod'), label='How do you want to specify the initial population vector?',
                 choices=c('Published initial vector' = 'pubvec',
                           'Random vector, user-specified initial number of adults' = 'randvec',
                           'Stable distribution, user-specified initial number of adults' = 'ssd')
    ),
    
    conditionalPanel(condition = "input.vectorMethod == 'randvec' | input.vectorMethod == 'ssd'",
                     numericInput(NS(id,'n_adults'),
                                  label='Initial number of adults (sum of individuals in reproductively active stages):',
                                  value=25, min=1, max=10000, step=10),
                     ns=NS(id)
    )
  )
}

paramsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Case-specific defaults:
    observeEvent(input$matrixID, {
      if (input$matrixID == 'whitedeer'){
        updateSelectInput(inputId = 'DDfunc', selected = 'logistic')
        updateSelectInput(inputId = 'DDmethod', selected = 'matrix')
        updateNumericInput(inputId = 'K', value=110)
        updateNumericInput(inputId = 'tmax', value=20)
        updateSelectInput(inputId = 'DDcalcN', selected = 'all')
      } else if (input$matrixID == 'graywolves'){
        updateSelectInput(inputId = 'DDfunc', selected = 'logistic')
        updateSelectInput(inputId = 'DDmethod', selected = 'matrix')
        updateNumericInput(inputId = 'K', value=969)
        updateNumericInput(inputId = 'tmax', value=20)
        updateSelectInput(inputId = 'DDcalcN', selected = 'all')
      } else if (input$matrixID == 'falcons'){
        updateSelectInput(inputId = 'DDfunc', selected='log_pb')
        updateNumericInput(inputId = 'x0', value=2.36)
        updateNumericInput(inputId = 'x1', value=-0.014)
        updateNumericInput(inputId = 'tmax', value=40)
        updateSelectInput(inputId = 'DDcalcN', selected='specify')
        updateTextInput(inputId = 'ismemberN', value="4")
      } else if (input$matrixID == 'crocs'){
        updateSelectInput(inputId = 'DDfunc', selected='logistic')
        updateSelectInput(inputId = 'DDmethod', selected = 'matrix')
        updateNumericInput(inputId = 'K', value=100000)
        updateNumericInput(inputId = 'tmax', value=20)
        updateSelectInput(inputId = 'DDcalcN', selected = 'all')
      } else if (input$matrixID == 'ogawa'){
        updateSelectInput(inputId = 'DDfunc', selected='ricker')
        updateSelectInput(inputId = 'DDmethod', selected = 'fertility')
        updateNumericInput(inputId = 'b', value=0.218)
        updateNumericInput(inputId = 'tmax', value=400)
        updateSelectInput(inputId = 'DDcalcN', selected='repro_classes')
      }
    })
    
    output<- list(
      matrixID = reactive(input$matrixID),
      DDfunc = reactive(input$DDfunc),
      K = reactive(input$K),
      b = reactive(input$b),
      x0 = reactive(input$x0),
      x1 = reactive(input$x1),
      theta = reactive(input$theta),
      DDmethod = reactive(input$DDmethod),
      DDcalcN = reactive(input$DDcalcN),
      ismemberN = reactive(input$ismemberN),
      tmax = reactive(input$tmax),
      vectorMethod = reactive(input$vectorMethod),
      DDelem = reactive(input$DDelem),
      n_adults = reactive(input$n_adults)
    )
    return(output)
  }
  )
}