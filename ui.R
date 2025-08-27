
# Define UI for app that does density-dependent population trajectories ----
ui <- fluidPage(
  
  theme = bslib::bs_theme(preset = "minty"),
  
  # Application title
  titlePanel("ShinyPopRecovery"),
  

  tabsetPanel(selected="DENSITY-DEPENDENT RECOVERY TRAJECTORIES",
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
                     <li>Use the starting population vector that goes along with the published example.
                     </li>
                     <li>A <b>random</b> population structure can be generated, 
                     and the user can specify the number of adults in the starting
                     population.
                     </li>
                     <li> We can use the stable population structure as implied 
                     by the low-density MPM, and the user can specify the number
                     of adults in the starting population.
                     </li>
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
                      chrissy[dot]hernandez[at]biology.ox.ac.uk
                           "),
                      br(), br(), br(), br(),
             ),
             tabPanel("DENSITY-DEPENDENT RECOVERY TRAJECTORIES",
                      sidebarLayout(
                        sidebarPanel(width=4,
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
                        ),
                        mainPanel(
                          tabsetPanel(
                            selected='QUICK-START GUIDE',
                            type='tabs',
                            # Quick-start guide tab
                            tabPanel("QUICK-START GUIDE",
                              h3("Title"),
                              HTML("If you're not sure where to start, let me suggest:
                                   <ol>
                                   <li> Select an example matrix. The app will 
                                   auto-populate with the default settings 
                                   for this population. </li>
                                   <li> First, try changing the density-dependence 
                                   parameter (carrying capacity, scaling parameter, etc.). 
                                   See how it changes the population trajectory, 
                                   and the functional form in the DENSITY DEPENDENCE tab. </li>
                                   </ol>
                                   ")
                            ),
                            
                            # Output: population projection ----
                            tabPanel("RECOVERY SIMULATION",
                                     card(
                                       card_header('Population projection plot'),
                                       plotOutput('projplot')
                                     ),
                                     value_box(
                                       title='Carrying capacity',
                                       value=textOutput("K"),
                                       theme='teal',
                                       max_height='100px'
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
                                     
                                     card(
                                       card_header('Description of selected matrix and associated study'),
                                       htmlOutput("metadata")
                                     ),
                            )
                            
                          )
                        ),
                      )
             ) # close navbarPage
  ) # close fluidPage
) # close UI