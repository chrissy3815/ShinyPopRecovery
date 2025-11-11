userguidecontents<- function(id){
  # User guide
  tabPanel("USER GUIDE",
           h3("USER GUIDE -- UNDER CONSTRUCTION"),
           HTML("This user guide assumes some basic knowledge about MPMs: 
                 if you lack it, the MPM 'bible'
                 <i>Caswell (2001) Matrix Projection Models: Construction, 
                 analysis and interpretation (Sinauer)</i> is a good place 
                 to start."
           ),
           br(), br(), br(),
           h3("CHOOSING AN MPM"),
           HTML("Currently, the app contains four example matrices. 
                            This section is under construction, but there is some 
                            basic information available in the MATRIX tab on the 
                            right-hand side once you switch over to the 'Density-Dependent
                            Recovery Trajectories'."),
           br(),
           br(),
           h4("MATRICES"),
           HTML("THIS SECTION UNDER CONSTRUCTION"),
           br(),
           br(),
           h4("DENSITY-DEPENDENT FUNCTIONAL FORMS"),
           HTML("THIS SECTION UNDER CONSTRUCTION"),
           br(),
           br(),
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
  )
}