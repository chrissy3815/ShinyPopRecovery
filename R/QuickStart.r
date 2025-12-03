quickstartcontents<- function(id){
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
                                   <li> You could also <b>try changing the functional form of
                                   density dependence</b>. The parameters you can set may change 
                                   (<i>e.g.</i>, K in the case of the logistic function, b in the 
                                   case of the Ricker and Beverton-Holt models). If the results don't 
                                   look the way you expect, try changing the density dependence parameter(s) 
                                   again.</li>
                                   <li> Finally, you might see how much the initial conditions matter
                                   by <b>changing the initial vector</b>. What happens to the RECOVERY 
                                   SIMULATION if you change the starting population structure to a 
                                   random vector? What happens as you make the starting population 
                                   larger or smaller? </li>
                                   </ol>"),
           br()
  )
}