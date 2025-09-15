################################################################################
## Prepare input data for DD Shiny app
## 
## Chrissy Hernandez
## August 2025

# Jensen white deer:
whitedeerF<- matrix(0, nrow=7, ncol=7, dimnames=list(1:7, 1:7))
whitedeerF[1,]<- c(1.426, 1.290, 1.296, 1.120, 1.126, 1.554, 0)
whitedeerU<- matrix(0, nrow=7, ncol=7, dimnames=list(1:7, 1:7))
diag(whitedeerU[-1,-ncol(whitedeerU)])<- c(0.713, 0.645, 0.648, 0.560, 0.563, 0.777)
whitedeerA<- whitedeerU+whitedeerF
whitedeerN0<- c(0,4, rep(0,5))
whitedeer<- list("Amat"=whitedeerA,
                 "Fmat"=whitedeerF,
                 "Umat"=whitedeerU,
                 "N0"=whitedeerN0,
                 "metadata"="The white-tailed deer example matrix comes from a 
                 study in southeastern Michigan (<a href='https://doi.org/10.1016/0304-3800(93)E0081-D'>Jensen 1995 <i>Ecological 
                 Modelling</i></a>). 
                 <br><br>
                 The published model and its default settings for ShinyPopRecovery: 
                 <ul>
                     <li>a prebreeding Leslie (age-based) matrix for parameters at low density </li>
                     <li>logistic density dependence</li>
                     <li>applied to the entire matrix</li>
                     <li>carrying capacity, K=110 female deer.</li>
                 </ul>
                 <br>
                 <h5>Brief description of the study background and aims:</h5>
                 In 1927, an enclosure was built around a 464-ha 
                 area in a nature reserve, and 2 male and 4 female deer were 
                 introduced into the enclosure. Pulling together evidence about 
                 the carrying capacity of the enclosure (220 total individuals, 
                 110 females), a life table, and a fecundity schedule for 
                 white-tailed deer at low population densities, Jensen built a 
                 prebreeding Leslie matrix and explored its behavior under 
                 logistic density dependence which operates on the entire 
                 projection matrix.")

# Jensen2 gray wolves:
graywolvesU<- matrix(0, nrow=10, ncol=10, dimnames=list(1:10, 1:10))
diag(graywolvesU[-1,-ncol(graywolvesU)])<- c(0.61, rep(0.87,8))
graywolvesF<- matrix(0, nrow=10, ncol=10, dimnames=list(1:10, 1:10))
graywolvesF[1,]<- c(0, rep(1.07,9))
graywolvesA<- graywolvesF+graywolvesU
graywolvesN0<- c(8,5,0,4,rep(0,6))
graywolves<- list("Amat"=graywolvesA,
                  "Fmat"=graywolvesF,
                  "Umat"=graywolvesU,
                  "N0"=graywolvesN0,
                  "metadata"="The gray wolves example matrix comes from a study 
                  in the Upper Peninsula of Michigan, USA 
                  (<a href='https://doi.org/10.1016/S0304-3800(01)00493-8'>
                  Miller et al. 2002 <i>Ecological Modelling</i></a>). 
                 <br><br>
                 The published model and its default settings for ShinyPopRecovery: 
                 <ul>
                     <li>a prebreeding Leslie (age-based) matrix for parameters at low density </li>
                     <li>logistic density dependence</li>
                     <li>applied to the entire matrix</li>
                     <li>carrying capacity, K=969 wolves.</li>
                 </ul>
                 <br>
                 <h5>Brief description of the study background and aims:</h5>
                  Gray wolves (<i>Canis lupus</i>) disappeared from the Upper 
                  Peninsula of Michigan in ca. 1960, and the species was listed 
                  as federally endangered in the lower 48 
                  states in 1974. Thanks to legal protections, gray wolves 
                  recovered in the upper midwest and recolonized the Upper 
                  Peninsula of Michigan. The Michigan Dept. of Natural Resources 
                  performed a winter census starting in 1991, when the estimated
                  population size was 17 individuals. <br>
                  Using data from a variety of sources, Miller et al. 
                  (2002) built a density-dependent model of gray wolf population
                  dynamics in the Upper Peninsula. They evaluated model 
                  performance by comparing it with the winter census data from 
                  1991-2000 and used the model to project when the population 
                  would reach its carrying capacity. Their model had
                  good performance when compared with winter census data.")

# Crocodiles:
stages<- c("Eggs","Hatchlings","Juveniles","Adults")
crocsF<- matrix(0, nrow=4, ncol=4, dimnames=list(stages,stages))
crocsF[1,]<- c(0,0,0,25.70)
crocsU<- matrix(0, nrow=4, ncol=4, dimnames=list(stages,stages))
diag(crocsU)<- c(0,0,0.63, 0.97)
diag(crocsU[-1,-ncol(crocsU)])<-c(0.25, 0.54, 0.09)
crocsA<- crocsU+crocsF
crocsN0<- c(19171.9, 3733.5, 3044.3, 955.7)
crocs<- list("Amat"=crocsA,
             "Fmat"=crocsF,
             "Umat"=crocsU,
             "N0"=crocsN0,
             "metadata"="The crocodile example comes from a study in the Northern
             Territory, Australia, on saltwater crocodiles <i>Crocodylus porosus</i> 
             (<a href='https://doi.org/10.1071/WR20033'>
             Fukuda et al. 2020 <i>Wildlife Research</i></a>). 
             <br><br>
             This model contains highly stage-specific density-dependent processes, 
             so we plan to build a vignette that compares the full model with a 
             'data-poor' scenario that could be explored using ShinyPopRecovery. 
             <br>
             For now, the matrix model and default settings for the app are:
             <ul>
               <li>a postbreeding stage-based matrix (4 stages) with parameters at low density </li>
               <li>logistic density dependence</li>
               <li>applied to the entire matrix</li>
               <li>carrying capacity, K=100,000 crocodiles total (including eggs).</li>
             </ul>
             <br>
             <h5>Brief description of the study background and aims:</h5>
             The authors of this study had access to a long term dataset (1971-2019) covering a period
             of recovery for the population, so they were able to incorporate 
             highly specific effects of density dependence. <br>
             They built a <b>four-stage postbreeding matrix model</b> and estimated
             the following density-dependent effects:
             <ul>
                 <li> Hatchling survival follows a Ricker function based on the density of hatchlings </li>
                 <li> Juvenile survival follows a Ricker function based on the combined density of juveniles and adults </li>
                 <li> Adult survival follows a Ricker function based on the density of adults </li>
                 <li> Adult reproduction follows a Beverton-Holt function based on the density of adult females </li>
             </ul>
             <p> The authors used their full model to evaluate the impact of egg 
             harvesting on the population dynamics and viability.</p>
             <br>
             Here, we provide the population projection matrix corresponding to low density,
             and allow users to play around with different density-dependent functions and methods.
             Eventually, we would like to have a more complete example where users 
             could turn on and off the different density-dependent parameters for 
             this crocodile model.")

# Falcons:
stages<- c("Juv (wild)", "Juv (captive)", "Adult (non-breeding)", "Adult (breeding)")
pb_0<- exp(2.36)/(1+exp(2.36))
falconsF<- matrix(0, nrow=4, ncol=4, dimnames=list(stages,stages))
falconsF[1,]<- c(0,0, 0.86*1.75*pb_0, 0.86*1.75)
falconsU<- matrix(0, nrow=4, ncol=4, dimnames=list(stages,stages))
falconsU[3,]<- c(0.38, 0.24, 0.86*(1-pb_0), 0)
falconsU[4,]<- c(0, 0, 0.86*pb_0, 0.86)
falconsA<- falconsU+falconsF
falconsN0<- c(12,0,0,14)
falcons<- list("Amat"=falconsA,
               "Fmat"=falconsF,
               "Umat"=falconsU,
               "N0"=falconsN0,
               "metadata"="The peregrine falcon (<i> Falco peregrinus anatum</i>)
               example comes from a study in California, USA.
               (<a href='https://doi.org/10.1111/1365-2664.12142'>
               Schipper et al. 2013 <i>Journal of Applied Ecology</i></a>). 
               <br><br>
               The published model and its default settings for ShinyPopRecovery: 
               <ul>
                  <li>a postbreeding stage-based matrix (four stages) for parameters at low density </li>
                  <li>density-dependence in the form of competition for a breeding site:
                  non-breeders have a logistic probability of becoming a breeder, depending
                  on how many breeders there currently are,</li>
                  <li>applied only to matrix elements that contain the probability of breeding
                  (<i>i.e.</i>, stasis in non-breeder stage, progression from non-breeder to
                  breeder stage, and first-year reproduction by non-breeders that progress),</li>
                  <li>logistic probability function intercept of 2.36, </li>
                  <li> logistic probability function slope of -0.014.</li>
               </ul>
               <br>
               <h5>Brief description of the study background and aims:</h5>
               Management of peregrine falcons included a captive breeding program 
               and population monitoring starting from 1975. A previous study 
               (<a https://doi.org/10.1086/424763> Kauffman et al. 2004</a>) used
               long-term monitoring data and built a four-stage matrix model with
               multiple subpopulations connected via dispersal to look at 
               spatially-structured demography across California. The model includes
               four stages:
               <ul>
                  <li> First-year wild-fledged birds</li>
                  <li> First-year hacked birds ('hacked' refers to various management 
                       interventions for fledglings) </li>
                  <li> Non-breeding adults</li>
                  <li> Breeding adults </li>
               </ul>
               Schipper et al. (2013) published a simplified version of the model without
               the spatial component, representing the entire California region
               as a single, closed population. Their goals was to understand the
               interacting effects of toxicants and density dependence on the falcons.")

# Ogawa forest (Takada and Nakashizuka 1996)
stages<- c("4cm<DBH<8cm", "8cm<DBH<16cm", "16cm<DBH<32cm", "32cm<DBH<64cm", "64cm<DBH")
ogawaF<- matrix(0, nrow=5, ncol=5, dimnames=list(stages,stages))
ogawaF[1,]<- c(0,0,0, exp(0.463), exp(0.463))
ogawaU<- matrix(0, nrow=5, ncol=5, dimnames=list(stages,stages))
diag(ogawaU)<- c(0.879, 0.919, 0.908, 0.967, 0.963)
diag(ogawaU[-1,])<- c(0.078, 0.028, 0.040, 0.013)
diag(ogawaU[,-1])<- c(0.007, 0.006, 0.002, 0)
ogawaA<- ogawaU+ogawaF
ogawaN0<- c(13.83, 10.8, 5.77, 3.47, 0.72)
ogawa<- list("Amat"=ogawaA,
               "Fmat"=ogawaF,
               "Umat"=ogawaU,
               "N0"=ogawaN0,
               "metadata"="The broad-leafed forest (mixed species) example comes 
               from a study in the Ogawa Forest Reserve in eastern Japan
               (<a href='https://doi.org/10.1007/BF00045495'>
               Takada and Nakashizuka 1996 <i>Vegetatio</i></a>). 
               <br><br>
               The published model and its default settings for ShinyPopRecovery: 
               <ul>
                  <li>a prebreeding size-based matrix (five size classes based on 
                  the diameter at breast height, DBH, for individual trees) for parameters at low density </li>
                  <li>density-dependence affects reproduction following a Ricker model</li>
                  <li>Ricker model strength parameter is 0.281 </li>
                  <li>NB: This model has a four year time step! </li>
               </ul>
               Also, in this study, the density-dependent reproduction was based 
               on the density of reproductive individuals (DBH of 32 cm and higher).
               Although this functionality has not yet been fully added to ShinyPopRecovery, 
               we control this example behind the scenes so that the default settings 
               will reproduce Figure 2 from the original publication. However, if you 
               switch the way that density dependence is applied (e.g. to the full matrix
               or survival only), then the density will be calculated as the sum of 
               densities across all classes.
               <br><br>
               <h5>Brief description of the study background and aims:</h5>
               TO BE ADDED")


MPMs_preloaded<- list("whitedeer"=whitedeer,
                      "graywolves"=graywolves,
                      "crocs"=crocs,
                      "falcons"=falcons, 
                      "ogawa"=ogawa)

save(MPMs_preloaded, file="data/ExampleMPMs_forShiny.RDS")


