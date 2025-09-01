################################################################################
## Density-dependent versions of popdemo::project()
## 
## Chrissy Hernandez
## November 2024

#Current call in Darren's code: project(pop_mat_100_low, vector = pop_n, Aseq = 1:ntimes)

# The goal is to build our own version of popdemo::project() that can take into
# account density dependence. But popdemo::project() is actually extremely
# complicated, because it can do a bunch of other things that we don't care
# about. So let's start simple.

project_simple<- function(A, vector=NULL, time=100, return.vec=TRUE){
  # This function will reproduce the simple version of popdemo::project() where
  # we simply want to project the population forwards according to a series of
  # matrices.
  if (is.list(A) & length(A) == 1) {
    A <- A[[1]] }
  if (is.matrix(A)) {
    M1 <- A
    dim(A) <- c(dim(A), 1)
    dimnames(A)[[1]] <- dimnames(M1)[[1]]
    dimnames(A)[[2]] <- dimnames(M1)[[2]]
    dimnames(A)[[3]] <- NULL
  }
  if (is.list(A) & length(A) > 1) {
    numA <- length(A)
    alldim <- sapply(A, dim)
    if (!diff(range(alldim)) == 0) {
      stop("all matrices in A must be square and have the same dimension as each other")
    }
    dimA <- mean(alldim)
    L <- A
    A <- numeric(dimA * dimA * numA)
    dim(A) <- c(dimA, dimA, numA)
    for (i in 1:numA) {
      A[, , i] <- L[[i]]
    }
    dimnames(A)[[1]] <- dimnames(L[[1]])[[1]]
    dimnames(A)[[2]] <- dimnames(L[[1]])[[2]]
    dimnames(A)[[3]] <- names(L)
  }
  
  order <- dim(A)[1]
  stagenames <- dimnames(A)[[2]]
  if (is.null(stagenames)) {
    stagenames <- paste("S", as.character(1:order), sep = "") }
  nmat <- dim(A)[3]

  # Initialize an output object:
  out<- list(pop=vector(), vec=matrix(), mat=matrix())
  
  # Initial vector:
  if (is.null(vector)){
    n0<- rep(1/order, order)
  } else{
    n0 <- vector
  }
  
  # Set the order that the matrices will be applied:
  if (dim(A)[3]==1){
    MC<- rep(1, time)
  } else if (dim(A)[3]>=time){
    MC<- 1:time
  } else { # if A has fewer matrices than the requested number of timesteps, 
    # then loop through them until you reach the number of requested timesteps
    MC<- rep(1:nmat, ceiling(time/nmat))[1:time]
  }
  
  # Initialize the output for population vector:
  Vec <- matrix(0, ncol = order, nrow = time + 1)
  Pop <- numeric(time + 1)
  dimnames(Vec)[[2]] <- stagenames
  Vec[1, ] <- vector
  Pop[1] <- sum(vector)
  for (i in 1:time) {
    Vec[(i + 1), ] <- A[, , MC[i]] %*% Vec[i, ]
    Pop[i + 1] <- sum(Vec[(i + 1), ])
  }
  if (is.null(dim(Pop))) 
    dim(Pop) <- time + 1
  out$pop <- Pop
  if (return.vec) 
    out$vec <- Vec
  out$mat <- A
  
  return(out)

}

project_threshold<- function(A, vector=NULL, time=100, return.vec=TRUE, threshold=NULL, A_DD=NULL){
  # This function will project forward according to the sequence of matrices in
  # A until reaching a threshold population size, at which point it will switch
  # to a specified high-density population matrix
  
  if (is.list(A) & length(A) == 1) {
    A <- A[[1]] }
  if (is.matrix(A)) {
    M1 <- A
    dim(A) <- c(dim(A), 1)
    dimnames(A)[[1]] <- dimnames(M1)[[1]]
    dimnames(A)[[2]] <- dimnames(M1)[[2]]
    dimnames(A)[[3]] <- NULL
  }
  if (is.list(A) & length(A) > 1) {
    numA <- length(A)
    alldim <- sapply(A, dim)
    if (!diff(range(alldim)) == 0) {
      stop("all matrices in A must be square and have the same dimension as each other")
    }
    dimA <- mean(alldim)
    L <- A
    A <- numeric(dimA * dimA * numA)
    dim(A) <- c(dimA, dimA, numA)
    for (i in 1:numA) {
      A[, , i] <- L[[i]]
    }
    dimnames(A)[[1]] <- dimnames(L[[1]])[[1]]
    dimnames(A)[[2]] <- dimnames(L[[1]])[[2]]
    dimnames(A)[[3]] <- names(L)
  }
  
  order <- dim(A)[1]
  stagenames <- dimnames(A)[[2]]
  if (is.null(stagenames)) {
    stagenames <- paste("S", as.character(1:order), sep = "") }
  nmat <- dim(A)[3]
  
  # If we're using the DD part of this function:
  if (!is.null(threshold)){
    if (is.null(A_DD)){
      stop("If you supply a population threshold, then you must supply a high-density projection matrix.") }
    # For now, assume that we'll supply a single DD matrix - can adjust to work
    # with a sequence of matrices to enable stochastic simulations
    if (is.list(A_DD) & length(A_DD) == 1) {
      A_DD <- A_DD[[1]] }
    if (is.matrix(A_DD)) {
      M1 <- A_DD
      dim(A_DD) <- c(dim(A_DD), 1)
      dimnames(A_DD)[[1]] <- dimnames(M1)[[1]]
      dimnames(A_DD)[[2]] <- dimnames(M1)[[2]]
      dimnames(A_DD)[[3]] <- NULL
    }
    if (!is.list(A_DD)){
      if (dim(A_DD)[1]!=dim(A_DD)[2]){
        stop("The high-density projection matrix must be square.")
      }
      order_DD<- dim(A_DD)[1]
      if (order_DD!=order){
        stop("The high-density projection matrix must be the same order as the matrices in A.")
      }
    } else {
      numA <- length(A_DD)
      alldim <- sapply(A_DD, dim)
      if (!diff(range(alldim)) == 0) {
        stop("all matrices in A_DD must be square and have the same dimension as each other")
      }
      dimA <- mean(alldim)
      L <- A_DD
      A_DD <- numeric(dimA * dimA * numA)
      dim(A_DD) <- c(dimA, dimA, numA)
      for (i in 1:numA) {
        A_DD[, , i] <- L[[i]]
      }
      dimnames(A_DD)[[1]] <- dimnames(L[[1]])[[1]]
      dimnames(A_DD)[[2]] <- dimnames(L[[1]])[[2]]
      dimnames(A_DD)[[3]] <- names(L)
    }
    
    
  }
  
  # Initialize an output object:
  out<- list(pop=vector(), vec=matrix(), mat=matrix())
  
  # Initial vector:
  if (is.null(vector)){
    n0<- rep(1/order, order)
  } else{
    n0 <- vector
  }
  
  # Set the order that the matrices will be applied:
  if (dim(A)[3]==1){
    MC<- rep(1, time)
  } else if (dim(A)[3]>=time){
    MC<- 1:time
  } else { # if A has fewer matrices than the requested number of timesteps, 
    # then loop through them until you reach the number of requested timesteps
    MC<- rep(1:nmat, ceiling(time/nmat))[1:time]
  }
  
  # Set the order that the DD matrices will be applied:
  if (dim(A_DD)[3]==1){
    MC_DD<- rep(1, time)
  } else if (dim(A_DD)[3]>=time){
    MC_DD<- 1:time
  } else { # if A has fewer matrices than the requested number of timesteps, 
    # then loop through them until you reach the number of requested timesteps
    nmat_DD<- dim(A_DD)[3]
    MC_DD<- rep(1:nmat_DD, ceiling(time/nmat_DD))[1:time]
  }
  
  # Initialize the output for population vector:
  Vec <- matrix(0, ncol = order, nrow = time + 1)
  Pop <- numeric(time + 1)
  dimnames(Vec)[[2]] <- stagenames
  Vec[1, ] <- vector
  Pop[1] <- sum(vector)
  for (i in 1:time) {
    if (Pop[i]>threshold & dim(A_DD)[3]==1){
      Vec[(i + 1), ] <- A_DD[,,1] %*% Vec[i, ]
      Pop[i + 1] <- sum(Vec[(i + 1), ])
    } else if (Pop[i]>threshold & dim(A_DD)[3]>1){
      Vec[(i+1), ]<- A_DD[,,MC_DD[i]] %*% Vec[i, ]
      Pop[i + 1] <- sum(Vec[(i + 1), ])
    }  else {
      Vec[(i + 1), ] <- A[, , MC[i]] %*% Vec[i, ]
      Pop[i + 1] <- sum(Vec[(i + 1), ])
    }
  }
  if (is.null(dim(Pop))) 
    dim(Pop) <- time + 1
  out$pop <- Pop
  if (return.vec) 
    out$vec <- Vec
  out$mat <- A
  
  return(out)
  
}

repro_DD<- function(N_t, params, model='Exponential'){
  if (is.null(model)){
    warning("No density-dependent model specified. Assuming density-independent reproduction.")
    percapita_repro<- params$fmax
  }
  
  if (model=="Logistic" | model=="logistic"){
    percapita_repro<- params$fmax * (1- (1-(1-params$sa_max)/(params$fmax*params$sj_max))*(N_t/params$repro_K) )
  }
  
  if (model=="Beverton-Holt" | model=="beverton-holt"){
    percapita_repro<- params$fmax / (1+ (params$fmax*params$sj_max/(1-params$sa_max) -1)*N_t/params$repro_K)
  }
  
  if (model %in% c("Exponential", "exponential", "Ricker", "ricker")){
    percapita_repro<- params$fmax * exp(-params$b*N_t)
  }
  
  return(percapita_repro)
  
}

project_DD<- function(A, vector=NULL, time=100, return.vec=TRUE, 
                      DD="Exponential", DD_params=NULL){
  # This function can take a list of population matrices (where they differ in
  # adult survival), but now reproductive output is a density-dependent
  # function.
  
  if (is.list(A) & length(A) == 1) {
    A <- A[[1]] }
  if (is.matrix(A)) {
    M1 <- A
    dim(A) <- c(dim(A), 1)
    dimnames(A)[[1]] <- dimnames(M1)[[1]]
    dimnames(A)[[2]] <- dimnames(M1)[[2]]
    dimnames(A)[[3]] <- NULL
  }
  if (is.list(A) & length(A) > 1) {
    numA <- length(A)
    alldim <- sapply(A, dim)
    if (!diff(range(alldim)) == 0) {
      stop("all matrices in A must be square and have the same dimension as each other")
    }
    dimA <- mean(alldim)
    L <- A
    A <- numeric(dimA * dimA * numA)
    dim(A) <- c(dimA, dimA, numA)
    for (i in 1:numA) {
      A[, , i] <- L[[i]]
    }
    dimnames(A)[[1]] <- dimnames(L[[1]])[[1]]
    dimnames(A)[[2]] <- dimnames(L[[1]])[[2]]
    dimnames(A)[[3]] <- names(L)
  }
  
  order <- dim(A)[1]
  stagenames <- dimnames(A)[[2]]
  if (is.null(stagenames)) {
    stagenames <- paste("S", as.character(1:order), sep = "") }
  nmat <- dim(A)[3]
  
  # For the DD part of this function:
  if (is.null(DD_params)){
    stop("You must provide the parameters that match your selected function for density-dependent reproduction.")
  }
  if (DD=="Exponential" | DD=="exponential"){
    if (is.null(DD_params$fmax)){
      stop("You must provide DD_params$fmax for an exponential decay DD function.") }
    if (is.null(DD_params$b)){
      b<- 1
    }
  }
  ## Note: We can change this to be Beverton-Holt, etc. See function above.
  ## Exponential has the fewest number of parameters, so I'm starting with that
  ## one.
  
  # Initialize an output object:
  out<- list(pop=vector(), vec=matrix(), mat=matrix())
  
  # Initial vector:
  if (is.null(vector)){
    n0<- rep(1/order, order)
  } else{
    n0 <- vector
  }
  
  # Set the order that the matrices will be applied:
  if (dim(A)[3]==1){
    MC<- rep(1, time)
  } else if (dim(A)[3]>=time){
    MC<- 1:time
  } else { # if A has fewer matrices than the requested number of timesteps, 
    # then loop through them until you reach the number of requested timesteps
    MC<- rep(1:nmat, ceiling(time/nmat))[1:time]
  }
  
  # Initialize the output for population vector:
  Vec <- matrix(0, ncol = order, nrow = time + 1)
  Pop <- numeric(time + 1)
  dimnames(Vec)[[2]] <- stagenames
  Vec[1, ] <- vector
  Pop[1] <- sum(vector)
  for (i in 1:time) {
    thisAmat<- A[, , MC[i]]
    thisrepro<- repro_DD(Pop[i], DD_params)
    thisAmat[1,order]<- thisrepro
    
    Vec[(i + 1), ] <- thisAmat %*% Vec[i, ]
    Pop[i + 1] <- sum(Vec[(i + 1), ])
  }
  if (is.null(dim(Pop))) 
    dim(Pop) <- time + 1
  out$pop <- Pop
  if (return.vec) 
    out$vec <- Vec
  out$mat <- A
  
  return(out)
  
}

## Function to calculate a starting vector with a specified number of adults
calc_initvec<- function(Fmat, Umat, n_adults, method='ssd'){
  # Two possible methods: random and stable stage distribution.
  
  # Check that Fmat and Umat are the same dimensions and both square
  if (dim(Fmat)[1] != dim(Umat)[1] | dim(Fmat)[2] != dim(Umat)[2]){
    stop("Fmat and Umat must have the same dimensions.")
  }
  if (dim(Fmat)[1]!=dim(Fmat)[2] | dim(Umat)[1]!=dim(Umat)[2]){
    stop("Fmat and Umat must both be square matrices.")
  }
  
  # For stable stage distribution:
  if (method=='ssd'){
    Amat<- Fmat + Umat
    eigz<- eigen(Amat)
    I<- which(Re(eigz$values) == max(Re(eigz$values)))
    distvec<- Re(eigz$vectors[,I])
    distvec<- distvec/sum(distvec)
  } else if (method=='random'){
    mdim<- dim(Fmat)[1]
    distvec<- matrix(rgamma(mdim, rep(1,mdim)), ncol=1)
    distvec<- distvec/sum(distvec)
  } else {
    stop('You have not specified a valid method to calculate the initial vector. 
         The function is defined only for the stable stage distribution 
         (method="ssd") or a random distribution (method="random")')
  }
  
  # Identify the stages that correspond to adults (reproductively active):
  repro_active<- (colSums(Fmat)>0)
  scale_val<- sum(distvec[repro_active])
  
  initVec<- distvec*(n_adults/scale_val)
  return(initVec)
}

## Function for the D factor that we can apply to the full matrix or a particular part of the matrix:
calc_Dfactor<- function(N, params=NULL, DDfunc="Ricker"){
  if (is.null(params)){
    stop("No parameters provided for density-dependent scaling.")
  }
  
  # Exponential:
  if (DDfunc %in% c("Exponential", "exponential", "Ricker", "ricker")){
    if (is.null(params$b)){
      warning("No b parameter provided for Ricker function - assuming D_factor=exp(-N).")
      params$b<-1
    }
    D_factor<- exp(-params$b*N)
  }
  
  # Beverton-Holt
  if (DDfunc %in% c("Beverton-Holt", "beverton-holt")){
    if (is.null(params$b)){
      warning("No b parameter provided for Beverton-Holt function - assuming D_factor=1/(1+N).")
      params$b<-1
    }
    D_factor<- 1/(1+params$b*N)
  }
  
  # Logistic
  if (DDfunc %in% c("Logistic", "logistic")){
    if (is.null(params$K)){
      warning("No K parameter provided for logistic equation - assuming D_factor=1-N/100.")
      params$K<- 100
    }
    D_factor<- 1-N/params$K
  }
  
  # Theta-logistic
  if (DDfunc %in% c("Theta-logistic", "theta-logistic")){
    if (is.null(params$K)){
      warning("No K parameter provided for logistic equation - assuming K=100.")
      params$K<- 100
    }
    if (is.null(params$theta)){
      warning("No theta parameter provided for logistic equation - assuming theta=1.")
      params$theta<- 1
    }
    D_factor<- 1-(N/params$K)^params$theta
  }
  
  # Logistic function for the probability of transitioning into the reproductive class
  # (generalization of the peregrine falcon example)
  if (DDfunc == "log_pb"){
    D_factor<- exp(params$x0 + params$x1*N)/(1+exp(params$x0 + params$x1*N))
  }
  
  return(D_factor)
}

## Function to apply the D_factor to a separable MPM. This function takes in
## both the Fmat and Umat, and returns Amat(N).
apply_Dfactor<- function(Fmat, Umat, N, DDfunc, DDparams, DDmethod='matrix', 
                         matelem=NULL, env.params=NULL, env.method=NULL){
  
  Amat<- Fmat + Umat
  Dfactor<- calc_Dfactor(N, DDparams, DDfunc)
  # if (Dfactor<0){
  #   Dfactor<- 0
  # }
  
  if (is.null(env.params) & is.null(env.method)){
    # Apply the DD parameter to the whole matrix:
    if (DDmethod=="matrix"){ 
      eye<- diag(1, nrow=nrow(Amat), ncol=ncol(Amat))
      Amat_N<- Dfactor*(Amat-eye)+eye
    } else if (DDmethod=='survival'){
      Umat_N<- Dfactor*(Umat)
      Amat_N<- Fmat + Umat_N
    } else if (DDmethod=='fertility'){
      Fmat_N<- Dfactor*Fmat
      Amat_N<- Fmat_N + Umat
    } else if (DDmethod=='falcon'){
      Amat_N<- Amat
      Amat_N[c(1,4),3]<- Dfactor*Amat[c(1,4),3]
      Amat_N[3,3]<- (1-Dfactor)*Amat[3,3]
    } else if (DDmethod=='reprotrans'){ # for now, this is identical to the falcons. Need to generalize!
      Amat_N<- Amat
      Dfactor_0<- calc_Dfactor(0, DDparams, DDfunc)
      Amat_N[c(1,4),3]<- (Dfactor/Dfactor_0)*Amat[c(1,4),3]
      Amat_N[3,3]<- (1-Dfactor)/(1-Dfactor_0)*Amat[3,3]
    } else if (DDmethod=='element'){
      if (is.null(matelem)){
        stop("You have specificed that density dependence should apply only to a 
           specific matrix element, but you have not specified which one. 
           Provide the [i,j] row and column indices to scale particular matrix 
           elements, or choose a different method.")
      }
      Amat_N<- Amat
      Amat_N[matelem]<- Dfactor*Amat_N[matelem]
    }
  } else {
    if (is.null(env.params) | is.null(env.method)){
      stop("You have specified only environmental parameters or an environmental 
           method, but both are needed.")
    }
    if (is.null(env.params$sd)){
      warning("No standard deviation specified for the environmental effect, assuming sd=0.1.")
      env.params$sd<- 0
    }
    if (is.null(env.params$mean)){
      warning("No mean value specified for the environmental effect, assuming mean=0.")
      env.params$mean<- 0
    }
    if (env.method=='matrix'){
      eye<- diag(1, nrow=nrow(Amat), ncol=ncol(Amat))
      this_env<- rnorm(1, mean=env.params$mean, sd=env.params$sd)
      # Apply the DD parameter and environmental stochasticity to the whole matrix:
      if (DDmethod=="matrix"){ 
        Amat_N<- Dfactor*(Amat-eye)+eye
      } else if (DDmethod=='survival'){ 
        # Apply DD only to survival, and then environmental stochasticity to the
        # whole matrix.
        Umat_N<- Dfactor*(Umat)
        Amat_N<- Fmat + Umat_N 
      } else if (DDmethod=='fertility'){
        Fmat_N<- Dfactor*Fmat
        Amat_N<- Fmat_N + Umat
      } else if (DDmethod=='element'){
        if (is.null(matelem)){
          stop("You have specificed that density dependence should apply only to a 
           specific matrix element, but you have not specified which one. 
           Provide the [i,j] row and column indices to scale particular matrix 
           elements, or choose a different method.")
        }
        Amat_N<- Amat
        Amat_N[matelem]<- Dfactor*Amat_N[matelem]
      }
      
      # Add the environmental stochasticity:
      Amat_N<- Amat_N + this_env*eye
      
    } else {
      stop("The only method currently defined for applying environmental stochasticity is 'matrix'.")
    }
  }
  
  return(Amat_N)
  
}

## Function to project a population using our very general approach to density
## dependence using the Dfactor
project_Dfactor<- function(Fmat, Umat, vector=NULL, time=100, return.vec=TRUE, 
                           DDfunc="Ricker", DDparams=NULL, DDmethod='matrix', 
                           matelem=NULL){
  # This function takes a single set of low-density vital rates in the form of a
  # reproduction matrix (Fmatrix) and transition matrix (Umatrix), and then
  # projects using the chosen DD function and DD method
  
  # Check that the Fmatrix and Umatrix are the same dimensions
  if (sum(dim(Fmat)==dim(Umat))!=2){
    stop("Reproductive (Fmat) and survival/growth transition (Umat) matrices 
         must have the same dimensions.")
  }
  
  # Make the A matrix:
  Amat<- Fmat+Umat
  order <- dim(Amat)[1]
  
  # For the DD part of this function:
  if (is.null(DDparams)){
    stop("You must provide the parameters that match your selected function for density-dependent reproduction.")
  }
  
  # Stage names:
  stagenames <- dimnames(Fmat)[[2]]
  if (is.null(stagenames)){
    stagenames <- paste("S", as.character(1:order), sep = "")
  }
  
  # Initialize an output object:
  out<- list(pop=vector(), vec=matrix(), mat=matrix())
  
  # Initial vector:
  if (is.null(vector)){
    n0<- rep(1/order, order)
  } else{
    n0 <- vector
  }
  
  # Initialize the output for population vector:
  Vec <- matrix(0, ncol = order, nrow = time + 1)
  Pop <- rep(NA, (time + 1))
  dimnames(Vec)[[2]] <- stagenames
  dimnames(Vec)[[1]] <- 1:(time+1)
  Vec[1, ] <- n0
  Pop[1] <- sum(n0)
  for (i in 1:time) {
    if (DDfunc=="log_pb"){
      thisAmat<- apply_Dfactor(Fmat, Umat, N=Vec[i,4], DDfunc, DDparams, 
                               DDmethod, matelem)
    } else {
      thisAmat<- apply_Dfactor(Fmat, Umat, Pop[i], DDfunc, DDparams, DDmethod, 
                               matelem)
    }
    
    # If the projection matrix has any negative values in it, stop iterating but
    # return the projection up until this point.
    if (sum(thisAmat<0)>0){
      warning(paste("Projection stopped at time step", i, "because the density-dependent projection matrix has negative values."))
      break
    }
    
    Vec[(i + 1), ] <- thisAmat %*% Vec[i, ]
    Pop[i + 1] <- sum(Vec[(i + 1), ])
    
  }
    
  if (is.null(dim(Pop))) 
    dim(Pop) <- time + 1
  out$pop <- Pop
  if (return.vec) 
    out$vec <- Vec
  out$mat <- thisAmat
  
  return(out)
  
}

## Function to project a population using our very general approach to density
## dependence using the Dfactor
project_Dfactor_stochenv<- function(Fmat, Umat, vector=NULL, time=100, return.vec=TRUE,
                                    DDfunc="Ricker", DDparams=NULL, DDmethod='matrix',
                                    matelem=NULL, env.params=NULL, env.method='matrix'){
  # This function takes a single set of low-density vital rates in the form of a
  # reproduction matrix (Fmatrix) and transition matrix (Umatrix), and then
  # projects using the chosen DD function and DD method, and including simple
  # environmental stochasticity.
  
  # Check that the Fmatrix and Umatrix are the same dimensions
  if (sum(dim(Fmat)==dim(Umat))!=2){
    stop("Reproductive (Fmat) and survival/growth transition (Umat) matrices 
         must have the same dimensions.")
  }
  
  # Make the A matrix:
  Amat<- Fmat+Umat
  order <- dim(Amat)[1]
  
  # For the DD part of this function:
  if (is.null(DDparams)){
    stop("You must provide the parameters that match your selected function for density-dependent reproduction.")
  }
  
  # Stage names:
  stagenames <- dimnames(Fmat)[[2]]
  if (is.null(stagenames)){
    stagenames <- paste("S", as.character(1:order), sep = "")
  }
  
  # Initialize an output object:
  out<- list(pop=vector(), vec=matrix(), mat=matrix())
  
  # Initial vector:
  if (is.null(vector)){
    n0<- rep(1/order, order)
  } else{
    n0 <- vector
  }
  
  # Initialize the output for population vector:
  Vec <- matrix(0, ncol = order, nrow = time + 1)
  Pop <- numeric(time + 1)
  dimnames(Vec)[[2]] <- stagenames
  Vec[1, ] <- vector
  Pop[1] <- sum(vector)
  for (i in 1:time) {
    thisAmat<- apply_Dfactor(Fmat, Umat, Pop[i], DDfunc, DDparams, DDmethod, 
                             env.params = env.params, env.method=env.method)
    
    Vec[(i + 1), ] <- thisAmat %*% Vec[i, ]
    Pop[i + 1] <- sum(Vec[(i + 1), ])
  }
  if (is.null(dim(Pop))) 
    dim(Pop) <- time + 1
  out$pop <- Pop
  if (return.vec) 
    out$vec <- Vec
  out$mat <- thisAmat
  
  return(out)
  
}


# project<- function (A, vector = "n", time = 100, standard.A = FALSE, standard.vec = FALSE, 
#                     return.vec = TRUE, Aseq = "unif", Astart = NULL, draws = 1000, 
#                     alpha.draws = "unif", PREcheck = TRUE) 
# {
#   if (!any(is.matrix(A), (is.list(A) & all(sapply(A, is.matrix))), 
#            (is.array(A) & length(dim(A)) == 3))) {
#     stop("A must be a matrix or list of matrices")
#   }
#   if (is.list(A) & length(A) == 1) 
#     A <- A[[1]]
#   if (is.matrix(A)) {
#     M1 <- A
#     dim(A) <- c(dim(A), 1)
#     dimnames(A)[[1]] <- dimnames(M1)[[1]]
#     dimnames(A)[[2]] <- dimnames(M1)[[2]]
#     dimnames(A)[[3]] <- NULL
#   }
#   if (is.list(A) & length(A) > 1) {
#     numA <- length(A)
#     alldim <- sapply(A, dim)
#     if (!diff(range(alldim)) == 0) {
#       stop("all matrices in A must be square and have the same dimension as each other")
#     }
#     dimA <- mean(alldim)
#     L <- A
#     A <- numeric(dimA * dimA * numA)
#     dim(A) <- c(dimA, dimA, numA)
#     for (i in 1:numA) {
#       A[, , i] <- L[[i]]
#     }
#     dimnames(A)[[1]] <- dimnames(L[[1]])[[1]]
#     dimnames(A)[[2]] <- dimnames(L[[1]])[[2]]
#     dimnames(A)[[3]] <- names(L)
#   }
#   if (is.array(A) & dim(A)[3] == 1) {
#     if (dim(A)[1] != dim(A)[2]) 
#       stop("A must be a square matrix")
#     if (PREcheck) {
#       if (!isIrreducible(A[, , 1])) {
#         warning("Matrix is reducible")
#       }
#       else {
#         if (!isPrimitive(A[, , 1])) {
#           warning("Matrix is imprimitive")
#         }
#       }
#     }
#   }
#   if (is.array(A) & dim(A)[3] > 1) {
#     if (dim(A)[1] != dim(A)[2]) 
#       stop("all matrices in A must be square")
#     if (PREcheck) {
#       red <- numeric(0)
#       imp <- numeric(0)
#       for (i in 1:dim(A)[3]) {
#         if (!isIrreducible(A[, , i])) {
#           red <- c(red, i)
#         }
#         else {
#           if (!isPrimitive(A[, , i])) {
#             imp <- c(imp, i)
#           }
#         }
#       }
#       if (length(red) > 0) {
#         if (is.null(dimnames(A)[[3]])) 
#           red <- as.character(red)
#         if (!is.null(dimnames(A)[[3]])) 
#           red <- dimnames(A)[[3]][red]
#         red <- paste(red, collapse = ", ")
#         warning(paste(c("One or more matrices are reducible (", 
#                         red, ")"), collapse = ""))
#       }
#       if (length(imp) > 0) {
#         if (is.null(dimnames(A)[[3]])) 
#           imp <- as.character(imp)
#         if (!is.null(dimnames(A)[[3]])) 
#           imp <- dimnames(A)[[3]][imp]
#         imp <- paste(imp, collapse = ", ")
#         warning(paste(c("One or more matrices are imprimitive (", 
#                         imp, ")"), collapse = ""))
#       }
#     }
#   }
#   order <- dim(A)[1]
#   stagenames <- dimnames(A)[[2]]
#   if (is.null(stagenames)) 
#     stagenames <- paste("S", as.character(1:order), sep = "")
#   nmat <- dim(A)[3]
#   matrixnames <- dimnames(A)[[3]]
#   if (standard.A == TRUE) {
#     if (nmat != 1) 
#       warning("Ignoring standard.A = TRUE: only valid when A is a single matrix.")
#     if (nmat == 1) {
#       MS <- A
#       lambda <- apply(MS, 3, eigs, what = "lambda")
#       A <- numeric(order * order * nmat)
#       dim(A) <- c(order, order, nmat)
#       for (i in 1:nmat) {
#         A[, , i] <- MS[, , i]/lambda[i]
#       }
#     }
#   }
#   if (nmat == 1) {
#     if (Aseq != "unif") 
#       warning("Ignoring Aseq: only 1 matrix in A")
#     MC <- rep(1L, time)
#     names(MC) <- matrixnames[MC]
#   }
#   if (nmat > 1) {
#     if (!any(Aseq[1] == "unif", is.matrix(Aseq), is.numeric(Aseq) & 
#              is.null(dim(Aseq)), is.character(Aseq) & is.null(dim(Aseq)))) {
#       stop("Aseq must take \"unif\", a numeric matrix, a numeric vector, or a character vector")
#     }
#     if (!any(is.numeric(Astart), is.character(Astart), is.null(Astart))) 
#       stop("Astart should be numeric, character or NULL")
#     if (is.numeric(Astart)) {
#       if (length(Astart) > 1) 
#         stop("Astart should be length 1")
#       if (Astart < 1) 
#         stop("Astart must be greater than 0")
#       if (Astart > nmat) 
#         stop("Astart cannot be greater than the number of matrices in A")
#       if (Astart%%1 != 0) 
#         stop("Astart must be an integer")
#       MCstart <- Astart
#     }
#     if (is.character(Astart)) {
#       if (length(Astart) > 1) 
#         stop("Astart should be length 1")
#       if (!(Astart %in% matrixnames)) 
#         stop("Astart is not found in names of matrices in A")
#       MCstart <- match(Astart, matrixnames)
#     }
#     if (is.null(Astart)) 
#       MCstart <- NULL
#     if (any(Aseq[1] == "unif", is.matrix(Aseq))) {
#       if (Aseq[1] == "unif") 
#         MCtm <- matrix(rep(1/nmat, nmat^2), nmat, nmat)
#       if (is.matrix(Aseq)) 
#         MCtm <- Aseq
#       if (dim(MCtm)[1] != dim(MCtm)[2]) 
#         stop("Aseq is not a square matrix")
#       if (dim(MCtm)[1] != nmat) {
#         stop("Dimensions of Aseq must be equal to number of matrices in A")
#       }
#       if (!all(colSums(MCtm) == 1)) 
#         stop("Columns of Aseq do not sum to 1")
#       MC <- .rmc(MCtm, time, MCstart)
#       names(MC) <- matrixnames[MC]
#     }
#     if (is.numeric(Aseq) & is.null(dim(Aseq))) {
#       if (min(Aseq) < 1) 
#         stop("Entries in Aseq are not all greater than 0")
#       if (max(Aseq) > nmat) 
#         stop("One or more entries in Aseq are greater than the number of matrices in A")
#       if (!all(Aseq%%1 == 0)) 
#         stop("One or more entries in Aseq are not integers")
#       if (length(Aseq) != time) 
#         time <- length(Aseq)
#       MC <- Aseq
#       names(MC) <- matrixnames[MC]
#     }
#     if (Aseq[1] != "unif" & is.character(Aseq) & is.null(dim(Aseq))) {
#       if (!all(Aseq %in% matrixnames)) 
#         stop("Names of Aseq aren't all found in A")
#       if (length(Aseq) != time) 
#         time <- length(Aseq)
#       MC <- match(Aseq, matrixnames)
#       names(MC) <- matrixnames[MC]
#     }
#   }
#   out <- Projection()
#   if (nmat == 1 | vector[1] == "n") {
#     VecBias <- numeric((time + 1) * order * order)
#     dim(VecBias) <- c(time + 1, order, order)
#     dimnames(VecBias)[[2]] <- stagenames
#     dimnames(VecBias)[[3]] <- paste("bias", stagenames, sep = "")
#     PopBias <- numeric((time + 1) * order)
#     dim(PopBias) <- c(time + 1, order)
#     dimnames(PopBias)[[2]] <- paste("bias", stagenames, sep = "")
#     I <- diag(order)
#     VecBias[1, , ] <- I
#     PopBias[1, ] <- 1
#     for (i in 1:order) {
#       for (j in 1:time) {
#         VecBias[j + 1, , i] <- A[, , MC[j]] %*% VecBias[j, 
#                                                         , i]
#         PopBias[j + 1, i] <- sum(VecBias[j + 1, , i])
#       }
#     }
#     if (nmat == 1) {
#       bounds <- t(apply(PopBias, 1, range))
#     }
#     if (vector[1] == "n") {
#       if (is.null(dim(PopBias))) 
#         dim(PopBias) <- time + 1
#       out@.Data <- PopBias
#       if (return.vec) 
#         out@vec <- VecBias
#       out@mat <- A
#       out@Aseq <- MC
#       if (nmat == 1) {
#         out@bounds <- bounds
#         out@projtype <- "deterministic"
#       }
#       if (nmat > 1) {
#         out@projtype <- "stochastic"
#       }
#       out@vectype <- "bias"
#       return(out)
#     }
#   }
#   if (vector[1] == "diri") {
#     if (alpha.draws[1] == "unif") 
#       alpha.draws <- rep(1, order)
#     if (length(alpha.draws) != order) 
#       stop("length of alpha.draws must be equal to matrix dimension")
#     VecDraws <- numeric((time + 1) * order * draws)
#     dim(VecDraws) <- c(time + 1, order, draws)
#     dimnames(VecDraws)[[2]] <- stagenames
#     dimnames(VecDraws)[[3]] <- paste("draw", as.character(1:draws), 
#                                      sep = "")
#     PopDraws <- numeric((time + 1) * draws)
#     dim(PopDraws) <- c(time + 1, draws)
#     dimnames(PopDraws)[[2]] <- paste("draw", as.character(1:draws), 
#                                      sep = "")
#     VecDraws[1, , ] <- t(MCMCpack::rdirichlet(draws, alpha.draws))
#     PopDraws[1, ] <- 1
#     for (i in 1:draws) {
#       for (j in 1:time) {
#         VecDraws[j + 1, , i] <- A[, , MC[j]] %*% VecDraws[j, 
#                                                           , i]
#         PopDraws[j + 1, i] <- sum(VecDraws[j + 1, , i])
#       }
#     }
#     if (is.null(dim(PopDraws))) 
#       dim(PopDraws) <- time + 1
#     out@.Data <- PopDraws
#     if (return.vec) 
#       out@vec <- VecDraws
#     out@mat <- A
#     out@Aseq <- MC
#     if (nmat == 1) {
#       out@bounds <- bounds
#       out@projtype <- "deterministic"
#     }
#     if (nmat > 1) {
#       out@projtype <- "stochastic"
#     }
#     out@vectype <- "diri"
#     return(out)
#   }
#   if (vector[1] != "n" & vector[1] != "diri" & (length(vector)%%order) != 
#       0) 
#     stop("vector has the wrong dimension(s)")
#   if (length(vector) > order) {
#     nvec <- dim(vector)[2]
#     vectornames <- dimnames(vector)[[2]]
#     if (is.null(vectornames)) 
#       vectornames <- paste("V", as.character(1:nvec), sep = "")
#     n0 <- vector
#     if (standard.vec) 
#       vector <- apply(n0, 2, function(x) {
#         x/sum(x)
#       })
#     Vec <- numeric((time + 1) * order * nvec)
#     dim(Vec) <- c(time + 1, order, nvec)
#     dimnames(Vec)[[2]] <- stagenames
#     dimnames(Vec)[[3]] <- vectornames
#     Pop <- numeric((time + 1) * nvec)
#     dim(Pop) <- c(time + 1, nvec)
#     dimnames(Pop)[[2]] <- vectornames
#     Vec[1, , ] <- vector
#     Pop[1, ] <- colSums(vector)
#     for (i in 1:nvec) {
#       for (j in 1:time) {
#         Vec[j + 1, , i] <- A[, , MC[j]] %*% Vec[j, , 
#                                                 i]
#         Pop[j + 1, i] <- sum(Vec[j + 1, , i])
#       }
#     }
#     if (is.null(dim(Pop))) 
#       dim(Pop) <- time + 1
#     out@.Data <- Pop
#     if (return.vec) 
#       out@vec <- Vec
#     out@mat <- A
#     out@Aseq <- MC
#     if (nmat == 1) {
#       out@bounds <- bounds
#       out@projtype <- "deterministic"
#     }
#     if (nmat > 1) {
#       out@projtype <- "stochastic"
#     }
#     out@vectype <- "multiple"
#     return(out)
#   }
#   if (length(vector) == order) {
#     n0 <- vector
#     if (standard.vec) 
#       vector <- n0/sum(n0)
#     Vec <- matrix(0, ncol = order, nrow = time + 1)
#     Pop <- numeric(time + 1)
#     dimnames(Vec)[[2]] <- stagenames
#     Vec[1, ] <- vector
#     Pop[1] <- sum(vector)
#     for (i in 1:time) {
#       Vec[(i + 1), ] <- A[, , MC[i]] %*% Vec[i, ]
#       Pop[i + 1] <- sum(Vec[(i + 1), ])
#     }
#     if (is.null(dim(Pop))) 
#       dim(Pop) <- time + 1
#     out@.Data <- Pop
#     if (return.vec) 
#       out@vec <- Vec
#     out@mat <- A
#     out@Aseq <- MC
#     if (nmat == 1) {
#       out@bounds <- bounds
#       out@projtype <- "deterministic"
#     }
#     if (nmat > 1) {
#       out@projtype <- "stochastic"
#     }
#     out@vectype <- "single"
#     return(out)
#   }
# }