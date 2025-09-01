
# Define UI for app that does density-dependent population trajectories ----
ui <- fluidPage(
  
  theme = bslib::bs_theme(preset = "minty"),
  shinyFeedback::useShinyFeedback(),
  
  # Application title
  titlePanel("ShinyPopRecovery"),
  
  
  tabsetPanel(selected="Density-Dependent Recovery Trajectories",
              # User guide
              tabPanel("USER GUIDE",
                       h3("USER GUIDE"),
                       HTML("This user guide assumes some basic knowledge about MPMs: 
                 if you lack it, the MPM 'bible'
                 <i>Caswell (2001) Matrix Projection Models: Construction, 
                 analysis and interpretation (Sinauer)</i> is a good place 
                 to start."
                       ),
                       br(), br(), br(),
                       h3("CHOOSING AN MPM"),
                       HTML("Currently, the app contains four example matrices."),
                       br(),
                       br(),
                       h4("MATRICES"),
                       h4("POPULATION STRUCTURES AND INITIAL VECTORS"),
                       HTML("The population structure is the proportion of each 
                      life history stage in the population. The population vector
                      contains the number/density of individuals in each stage at any 
                      given point in time.
                 For the recovery projections, we need to specify the initial 
                 population vector: the number of individuals in each age/size/stage class.
                 Different initial population vectors yield different population dynamics during 
                 the recovery process.
                 The app currently allows three options for an initial population vector:
                 <ul>
                     <li>Use the starting population vector that goes along with the 
                     <b>published</b> example. </li>
                     <li>A <b>random</b> population structure can be generated, 
                     and the user can specify the number of adults in the starting
                     population. </li>
                     <li> We can use the <b>stable population structure</b> as implied 
                     by the low-density MPM, and the user can specify the number
                     of adults in the starting population. </li>
                 </ul>"
                       ),
                       br(), br(), 
                       h3("CONTACT ME"),
                       HTML("This app and the associated R package are currently 
                           under construction, and I would really appreciate any
                           feedback that you have!
                           <ul>
                             <li> Did you find an error or bug?</li>
                             <li> Do you have a suggestion of a feature to add? </li>
                             <li> Do you have another good example that I should add? 
                             (e-mails of this type would make me particularly happy)</li>
                           </ul>
                           "),
                       br(),
                       HTML("To save us all some time, here are a few features I
                           am planning to add:
                           <ul>
                              <li> Allow users to enter their own MPM. </li>
                              <li> Allow users to select MPMs from the COMADRE and
                              COMPADRE databases. </li>
                              <li> Display the trajectories of different age/size/stage 
                              classes, not only the full population size.</li>
                           </ul>
                           "),
                       br(), br(),
                       HTML("If you'd like to get in touch, please e-mail me, at
                      chrissy.hernandez[at]biology.ox.ac.uk
                           "),
                       br(), br(), br(), br(),
              ), # close USER GUIDE tabPanel
              tabPanel("Density-Dependent Recovery Trajectories",
                       sidebarLayout(
                         sidebarPanel(width=4,
                                      h4("Model specification"),
                                      
                                      selectInput("matrixID", label='Example population matrix', 
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
                                      
                                      radioButtons('vectorMethod', label='How do you want to specify the initial population vector?',
                                                   choices=c('Published initial vector' = 'pubvec',
                                                             'Random vector, user-specified initial number of adults' = 'randvec',
                                                             'Stable distribution, user-specified initial number of adults' = 'ssd')
                                      ),
                                      
                                      conditionalPanel(id='n_adults', condition = "input.vectorMethod == 'randvec' | input.vectorMethod == 'ssd'",
                                                       numericInput(inputId = 'n_adults', 
                                                                    label='Initial number of adults (sum of individuals in reproductively active stages):',
                                                                    value=25, min=1, max=10000, step=10)
                                      ),
                         ), # close sidebarPanel
                         mainPanel(
                           tabsetPanel(
                             selected='QUICK-START GUIDE',
                             type='tabs',
                             # Quick-start guide tab
                             tabPanel("QUICK-START GUIDE",
                                      br(),
                                      h3("Welcome to ShinyPopRecovery!"),
                                      HTML("If you're not sure where to start, let me suggest:
                                   <ol>
                                   <li> First, <b>select an example population matrix.</b> The app will 
                                   auto-populate with the default settings for this population. 
                                   You can see the results in the RECOVERY SIMULATION tab. </li>
                                   <li> Next, <b>try changing the density-dependence 
                                   parameter</b> (<i>e.g.</i> K or b). See how it changes the RECOVERY SIMULATION, 
                                   and the functional form in the DENSITY DEPENDENCE tab. </li>
                                   <li> Next, you could <b>change which elements of the matrix are
                                   affected by density dependence</b> (<i>i.e.</i> which elements 
                                   are multiplied by the density-dependent scaling factor between
                                   ~1 at low density and ~0 at high density). What happens to the 
                                   RECOVERY SIMULATION if density dependence affects only 
                                   survival? only fertility?</li>
                                   <li> You could also <b>try changing the funciontal form of
                                   density dependence</b>. The parameters you can set may change 
                                   (<i>e.g.</i>, K in the case of the logistic function, b in the 
                                   case of the Ricker and Beverton-Holt models). If the results don't 
                                   look the way you expect, try changing this parameter again.</li>
                                   <li> Finally, you might see how much the initial conditions matter
                                   by <b>changing the initial vector</b>. What happens to the RECOVERY 
                                   SIMULATION if you change the starting population structure to a 
                                   random vector? What happens as you make the starting population 
                                   larger or smaller? </li>
                                   </ol>"),
                                      br()
                             ),
                             
                             # Output: population projection ----
                             tabPanel("RECOVERY SIMULATION",
                                      card(
                                        card_header('Population projection plot'),
                                        plotOutput('projplot')
                                      ),
                                      
                                      uiOutput("warning_card"),
                                      
                                      value_box(
                                        title='Carrying capacity',
                                        value=textOutput("K"),
                                        theme='teal',
                                        max_height='100px'
                                      ),
                                      
                                      card(
                                        card_header('Population structure across time steps'),
                                        HTML("<p>Age or stage classes are the rows, time steps are the columns.</p>"),
                                        div(tableOutput("projtable"), style='font-size:80%')
                                     )
                             ),
                             
                             tabPanel("DENSITY DEPENDENCE",
                                      card(
                                        card_header('Selected functional form of density dependence'),
                                        plotOutput('DDfuncplot')
                                      ),
                             ),
                             
                             tabPanel("MATRIX",
                                      value_box(
                                        title='Selected matrix population model',
                                        div(tableOutput("mpm"), style='font-size:80%')
                                      ),
                                      
                                      card(div(
                                          h3('Description of selected matrix and associated study'),
                                          uiOutput("metadata")
                                      )),
                             ) # close MATRIX tabPanel
                             
                           ) # close tabsetPanel
                         ) # close mainPanel 
                       ) # close sidebarLayout
              ) # close navbarPage
  ) # close fluidPage
) # close UI