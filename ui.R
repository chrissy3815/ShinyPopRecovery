
# Define UI for app that does density-dependent population trajectories ----
ui <- fluidPage(
  
  page_sidebar(
    # App title ----
    title = "Density dependent recovery projections for data-poor species",
    # Sidebar panel for inputs ----
    sidebar = sidebar(
      selectInput("matrixID", label='Population matrix', 
                  choices=c("White-tailed deer" = 'whitedeer',
                            "Gray wolves" = 'graywolves',
                            "Crocodiles" = 'crocs',
                            "Peregrine falcon" = 'falcons')
      ),
      
      selectInput('DDfunc',label='Functional form of density dependence',
                  choices=c("Logistic" = 'logistic',
                            "Ricker (negative exponential)" = 'ricker',
                            "Beverton-Holt" = 'beverton-holt',
                            "Theta-logistic" = 'theta-logistic',
                            "Logistic probability of becoming a breeder" = 'log_pb')
      ),
      
      conditionalPanel(id="K", 
                       condition = "input.DDfunc == 'logistic' | input.DDfunc =='theta-logistic' ",
                       numericInput("K", label = "Carrying capacity, K",
                                    value=110, min=10, max=200000, step=1)
      ),
      
      conditionalPanel(id='b',
                       condition = "input.DDfunc == 'ricker' | input.DDfunc == 'beverton-holt'",
                       numericInput("b", label='Scaling parameter, b',
                                    value=1, min=1e-4, max=10)
      ),
      
      conditionalPanel(id='theta',
                       condition = "input.DDfunc == 'theta-logistic' ",
                       numericInput("theta", label='Shape parameter, theta',
                                    value=1, min=1e-4, max=10)
      ),
      
      conditionalPanel(id='x0',
                       condition = "input.DDfunc == 'log_pb' ",
                       numericInput("x0", label='Intercept', 
                                    value=2.36, min=0, max=1000)),
      conditionalPanel(id='x1',
                       condition = "input.DDfunc == 'log_pb' ",
                       numericInput("x1", label='Slope', 
                                    value=-0.014, min=-10, max=10)),
      conditionalPanel(id='DDmethod',
                       condition = "input.DDfunc != 'log_pb' ",
                       selectInput('DDmethod', label='How do you want to apply density dependence?',
                                   choices=c("The entire matrix"='matrix',
                                             "The fertility matrix"='fertility',
                                             "The survival matrix"='survival',
                                             #"Transitions into reproductive classes"= 'reprotrans',
                                             "Specify a single matrix element"= 'userelem')
                       )),
      
      conditionalPanel(id="DDelem", 
                       condition="input.DDmethod == 'userelem'",
                       textInput(inputId = 'DDelem', 
                                 label='Identify the matrix element to which density dependence should apply as the [row, column].',
                                 value="[1,4]")
      ),
      
      numericInput('tmax', label='How many years into the future do you want to project?',
                   value=20, min=1, max=1000
      ),
      
      radioButtons('vectorMethod', label='How do you want to specify the starting matrix?',
                   choices=c('Published initial vector' = 'pubvec',
                             'Random vector, user-specified initial number of adults' = 'randvec',
                             'Stable distribution, user-specified initial number of adults' = 'ssd')
      ),
      
      conditionalPanel(id='n_adults', condition = "input.vectorMethod == 'randvec' | input.vectorMethod == 'ssd'",
                       numericInput(inputId = 'n_adults', 
                                    label='Initial number of adults (sum of individuals in reproductively active stages):',
                                    value=25, min=1, max=10000, step=10)
      ),
      
      width=400
    ),
    
    # Output: population projection ----
    card(
      card_header('Population projection plot'),
      plotOutput('projplot')
    ),
    
    card(
      card_header('Selected functional form of density dependence'),
      plotOutput('DDfuncplot')
    ),
    
    value_box(
      title='Selected matrix population model',
      div(tableOutput("mpm"), style='font-size:80%')
    ),
    
    card(
      card_header('Description of selected matrix and associated study'),
      htmlOutput("metadata")
    ),
    
    value_box(
      title='Carrying capacity',
      value=textOutput("K"),
      theme='teal',
      max_height='100px'
    )
    
  )
)