library(shiny)
library(bslib)
source('/Users/Chrissy/MSCA/ddRecoveryProjections/code/projectDD_functions.R')
load('data/ExampleMPMs_forShiny.RDS')

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

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Case-specific defaults:
  observeEvent(input$matrixID, {
    if (input$matrixID == 'whitedeer'){
      updateSelectInput(inputId = 'DDfunc', selected = 'logistic')
      updateSelectInput(inputId = 'DDmethod', selected = 'matrix')
      updateNumericInput(inputId = 'K', value=110)
    } else if (input$matrixID == 'graywolves'){
      updateSelectInput(inputId = 'DDfunc', selected = 'logistic')
      updateSelectInput(inputId = 'DDmethod', selected = 'matrix')
      updateNumericInput(inputId = 'K', value=969)
    } else if (input$matrixID == 'falcons'){
      updateSelectInput(inputId = 'DDfunc', selected='log_pb')
      updateNumericInput(inputId = 'x0', value=2.36)
      updateNumericInput(inputId = 'x1', value=-0.014)
    } else if (input$matrixID == 'crocs'){
      updateSelectInput(inputId = 'DDfunc', selected='logistic')
      updateNumericInput(inputId = 'K', value=100000)
    }
  })
  
  output$mpm<- renderTable({
    eval(parse(text=paste0("MPMs_preloaded$",input$matrixID,"$Amat")))
  }, rownames=TRUE, colnames=TRUE, align='c', spacing='s', digits=3, na="0.000")
  
  output$metadata<- renderText({
    eval(parse(text=paste0("MPMs_preloaded$",input$matrixID,"$metadata")))
  })
  
  output$K<- renderText({
    if (input$DDfunc=="logistic" | input$DDfunc=="theta-logistic"){
      input$K
    } else {
      "Not defined"
    }
  })
  
  output$DDfuncplot<- renderPlot({
    if (input$DDfunc=='logistic'){
      Nvals<- seq(from=0, to=input$K, length.out=200)
      D_factor<- calc_Dfactor(Nvals, params=list(K=input$K), DDfunc=input$DDfunc)
    } else if (input$DDfunc == "beverton-holt"){
      max_N<- ceiling(99/input$b)
      Nvals<- seq(from=0, to=max_N, length.out=200)
      D_factor<- calc_Dfactor(Nvals, params=list(b=input$b), DDfunc=input$DDfunc)
    } else if (input$DDfunc == "ricker"){
      max_N<- ceiling(log(0.01)/-input$b)
      Nvals<- seq(from=0, to=max_N, length.out=200)
      D_factor<- calc_Dfactor(Nvals, params=list(b=input$b), DDfunc=input$DDfunc)
    } else if (input$DDfunc == 'theta-logistic'){
      Nvals<- seq(from=0, to=input$K, length.out=200)
      D_factor<- calc_Dfactor(Nvals, params=list(K=input$K, theta=input$theta), DDfunc=input$DDfunc)
    } else if (input$DDfunc == 'log_pb'){
      Nvals<- seq(from=0, to=500, by=1)
      D_factor<- calc_Dfactor(Nvals, params=list(x0=input$x0, x1=input$x1), DDfunc = "log_pb")
    }
    # Get rid of all the negative values?
    D_factor[D_factor<0]<- NA
    
    plot(Nvals, D_factor, type='l', cex.lab=1.5, cex.axis=1.5,
         xlab='Population size', ylab='Scaling factor', lwd=2)
  })
  
  output$projplot<- renderPlot({
    
    Fmat<- eval(parse(text=paste0("MPMs_preloaded$",input$matrixID,"$Fmat")))
    Umat<- eval(parse(text=paste0("MPMs_preloaded$",input$matrixID,"$Umat")))
    
    if (input$vectorMethod=='pubvec'){
      N0<- eval(parse(text=paste0("MPMs_preloaded$",input$matrixID,"$N0")))
    } else if (input$vectorMethod=="randvec"){
      N0<- calc_initvec(Fmat, Umat, n_adults=input$n_adults, method='random')
    } else if (input$vectorMethod=='ssd'){
      N0<- calc_initvec(Fmat, Umat, n_adults=input$n_adults, method='ssd')
    }
    
    if(input$DDfunc=='logistic'){
      DDparams<- list(K=input$K)
    } else if (input$DDfunc=='ricker' | input$DDfunc=='beverton-holt'){
      DDparams<- list(b=input$b)
    } else if (input$DDfunc=='theta-logistic'){
      DDparams<- list(theta=input$theta, K=input$K)
    } else if (input$DDfunc=='log_pb'){
      DDparams<- list(x0=input$x0, x1=input$x1)
    }
    
    if (input$DDfunc=='log_pb'){
      proj_out<- project_Dfactor(Fmat, Umat, vector=N0, time=input$tmax, return.vec=TRUE,
                                 DDfunc=input$DDfunc, DDparams=DDparams,
                                 DDmethod='reprotrans')
    } else{
      proj_out<- project_Dfactor(Fmat, Umat, vector=N0, time=input$tmax, return.vec=TRUE,
                                 DDfunc=input$DDfunc, DDparams=DDparams,
                                 DDmethod=input$DDmethod)
    }
  
    plot(proj_out$pop[-1], type='l', lwd=4, lty=2, cex.lab=1.5, cex.axis=1.5,
         xlab='Years since recovery initiated', ylab='Population size', main='')
  })
  
}

shinyApp(ui = ui, server = server)