ui <- fluidPage(
  
  theme = bslib::bs_theme(preset = "minty"),
  shinyFeedback::useShinyFeedback(),
  
  # Application title
  titlePanel("ShinyPopRecovery"),
  
  tabsetPanel(selected="Density-Dependent Recovery Trajectories",
              userguidecontents("userguide"), # close USER GUIDE tabPanel
              tabPanel("Density-Dependent Recovery Trajectories",
                       sidebarLayout(
                         sidebarPanel(width=4,
                                      h4("Model specification"),
                                      
                                      paramsUI('side1')
                         ), # close sidebarPanel
                         mainPanel(
                           tabsetPanel(
                             selected='QUICK-START GUIDE',
                             type='tabs',
                             # Quick-start guide tab
                             quickstartcontents("quickstart"),
                             
                             tabPanel("RECOVERY SIMULATION",
                                      # Output: population projection ----
                                      projectionUI("plot1"),
                                      
                                      value_box(
                                        title='Carrying capacity',
                                        value=carryingcapacityUI("tab1"),
                                        theme='teal',
                                        max_height='100px'
                                      )
                             ),
                             tabPanel("DENSITY DEPENDENCE",
                                      DDpanelUI('DDplot')),
                             tabPanel("MATRIX",
                                      matrixUI('mat1')
                             )
                           )
                         )
                       )
              )
  )
)