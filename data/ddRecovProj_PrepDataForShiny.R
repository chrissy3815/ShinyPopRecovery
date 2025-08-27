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
                 study in southeastern Michigan (Jensen 1995 <i>Ecological 
                 Modelling</i>). <br>
                 In 1927, an enclosure was built around a 464-ha 
                 area in a nature reserve, and 2 male and 4 female deer were 
                 introduced into the enclosure. Pulling together evidence about 
                 the carrying capacity of the enclosure (220 total individuals, 
                 110 females), a life table, and a fecundity schedule for 
                 white-tailed deer at low population densities, Jensen built a 
                 <b>prebreeding Leslie matrix</b> and explored its behavior under 
                 <b>logistic density dependence</b> which operates on the entire 
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
                  in the Upper Peninsula of Michigan, USA (Miller et al. 2002 
                  <i>Ecological Modelling</i>). Gray wolves (<i>Canis lupus</i>) 
                  disappeared from the Upper Peninsula in ca. 1960, and gray 
                  wolves were listed as federally endangered in the lower 48 
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
                  would reach its carrying capacity. Their model, which had
                  good performance when compared with winter census data, is a
                  <b>prebreeding Leslie matrix</b> where <b>logistic density dependence</b> 
                  (carrying capacity = 969 wolves) operates on the whole matrix.")

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
             (Fukuda et al. 2021 <i>Wildlife Research</i>). The authors of this 
             study had access to a long term dataset (1971-2019) covering a period
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
             <p> Here, we provide the population projection matrix corresponding to low density,
             and allow users to play around with different density-dependent functions and methods.</p>
             Eventually, we would like to have a more complete example where users could turn on and off the different 
             density-dependent parameters for this crocodile model, but that does not exist in our code base yet.")

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
               "metadata"="The peregrine falcon (<i>Falco peregrinus anatum</i>)
               example comes from a study in California, USA (Schipper et al. 
               2013 <i>Journal of Applied Ecology</i>). Management of peregrine
               falcons included a captive breeding program and population monitoring
               starting from 1975. <br>
               MORE TO BE ADDED HERE.")


MPMs_preloaded<- list("whitedeer"=whitedeer,
                      "graywolves"=graywolves,
                      "crocs"=crocs,
                      "falcons"=falcons)

save(MPMs_preloaded, file="data/ExampleMPMs_forShiny.RDS")


