projectionUI <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header('Population projection plot'),
      plotOutput(NS(id,'projplot'))
    ),
    uiOutput(NS(id,"warning_card")),
    card(
      card_header('Population structure across time steps'),
      HTML("<p>Age or stage classes are the rows, time steps are the columns.</p>"),
      div(tableOutput(NS(id,"projtable")), style='font-size:80%')
    )
  )
  
}

projectionServer <- function(id, params, warning_message) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      proj_out<- reactive({
        
        Fmat<- eval(parse(text=paste0("MPMs_preloaded$",params$matrixID(),"$Fmat")))
        Umat<- eval(parse(text=paste0("MPMs_preloaded$",params$matrixID(),"$Umat")))
        
        N0<- eval(parse(text=paste0("MPMs_preloaded$",params$matrixID(),"$N0")))
        
        if (params$vectorMethod()=='pubvec'){
          N0<- eval(parse(text=paste0("MPMs_preloaded$",params$matrixID(),"$N0")))
        } else if (params$vectorMethod()=="randvec"){
          N0<- calc_initvec(Fmat, Umat, n_adults=params$n_adults(), method='random')
        } else if (params$vectorMethod()=='ssd'){
          N0<- calc_initvec(Fmat, Umat, n_adults=params$n_adults(), method='ssd')
        }
        
        if(params$DDfunc()=='logistic'){
          projparams<- list(K=params$K())
        } else if (params$DDfunc()=='ricker' | params$DDfunc()=='beverton-holt'){
          projparams<- list(b=params$b())
        } else if (params$DDfunc()=='theta-logistic'){
          projparams<- list(theta=params$theta(), K=params$K())
        } else if (params$DDfunc()=='log_pb'){
          projparams<- list(x0=params$x0(), x1=params$x1())
        }
        
        if(params$DDcalcN()=='repro_classes'){
          I<- which(colSums(Fmat)>0)
          Nmembers<- I
        } else if (params$DDcalcN()=='all'){
          Nmembers<- 1:dim(Fmat)[1]
        } else if (params$DDcalcN()=='specify'){
          Nmembers<- as.numeric(unlist(strsplit(params$ismemberN(),",")))
        }
        
        if(params$matrixID()=='ogawa'){
          tmax<- params$tmax()/4
        } else {
          tmax<- params$tmax()
        }
        
        if(params$DDmethod()=='element'){
          string<- params$DDelem()
          string<- strsplit(string, split=',')[[1]]
          irow<- as.numeric(substr(string[1], 2, 1000))
          icol<- as.numeric(substr(string[2], 1, (nchar(string[2])-1)))
          matelem<- c(irow, icol)
        } else {
          matelem<- NULL
        }
        
        # proj_out<- project_Dfactor(Fmat, Umat, vector=N0, time=tmax, return.vec=TRUE,
        #                            DDfunc=params$DDfunc(), DDmethod=params$DDmethod(), 
        #                            DDparams=projparams)
        if (params$DDfunc()=='log_pb'){
          proj_out<- project_Dfactor(Fmat, Umat, vector=N0, time=tmax, return.vec=TRUE,
                                     DDfunc=params$DDfunc(), DDparams=projparams,
                                     DDmethod='reprotrans', ismemberN=Nmembers)
        } else{
          proj_out<- tryCatch({
            withCallingHandlers({project_Dfactor(Fmat, Umat, vector=N0, time=tmax, return.vec=TRUE,
                                                 DDfunc=params$DDfunc(), DDparams=projparams,
                                                 DDmethod=params$DDmethod(), matelem = matelem,
                                                 ismemberN = Nmembers)},
                                warning = function(w) {
                                  warning_message(conditionMessage(w))  # Capture warning
                                  invokeRestart("muffleWarning")  # Prevent it from printing
                                })
          })
        }
        return(proj_out)
      })
      
      output$projplot<- renderPlot({
        
        if(sum(!is.na(proj_out()$pop))<2){
          plot(NULL, cex.lab=1.5, cex.axis=1.5, xlim=c(0, params$tmax()), ylim=c(0,100),
               xlab='Years since recovery initiated', ylab='Population size', main='')
        } else if (params$matrixID()=='ogawa'){
          years<- seq(0, params$tmax(), by=4)
          plot(years, proj_out()$pop, type='l', lwd=4, lty=2, cex.lab=1.5, cex.axis=1.5,
               xlab='Years since recovery initiated', ylab='Population size', main='')
        }
        else{
          plot(0:params$tmax(), proj_out()$pop, type='l', lwd=4, lty=2, cex.lab=1.5, cex.axis=1.5,
               xlab='Years since recovery initiated', ylab='Population size', main='')
        }
        
      })
      
      output$projtable<- renderTable(
        rbind(t(proj_out()$vec), total=proj_out()$pop), rownames=TRUE, colnames=TRUE, align='c', spacing='s', digits=3, na="0.000"
      )
      
      output$warning_card <- renderUI({
        req(warning_message())  # Show only if there's a warning
        
        # Display a card with the warning
        tags$div(
          class = "card border-warning mb-3",
          style = "max-width: 100%;",
          tags$div(class = "card-header bg-warning text-white", "⚠️ Warning"),
          tags$div(class = "card-body",
                   tags$p(class = "card-text", warning_message()),
                   tags$p("Negative values in the population projection matrix or population vector
                      are biologically unrealistic - we cannot have a negative number of adults!"),
                   tags$hr(),
                   tags$p("💡 Suggestion: Try using a different functional form of density-dependence
                      or a different way of applying density dependence."),
                   tags$p("The logistic function, applied to the entire matrix, is particularly prone 
                      to generating negative values in the matrix or vector.")
          )
        )
      })
    }
  )
}