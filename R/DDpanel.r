DDpanelUI <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header('Selected functional form of density dependence'),
      plotOutput(NS(id,'DDfuncplot'))
    )
  )
}

DDpanelServer <- function(id, params) {
  moduleServer(
    id,
    function(input, output, session) {
      output$DDfuncplot<- renderPlot({
        if (params$DDfunc()=='logistic'){
          Nvals<- seq(from=0, to=params$K(), length.out=200)
          D_factor<- calc_Dfactor(Nvals, params=list(K=params$K()), DDfunc=params$DDfunc())
        } else if (params$DDfunc() == "beverton-holt"){
          max_N<- ceiling(99/params$b())
          Nvals<- seq(from=0, to=max_N, length.out=200)
          D_factor<- calc_Dfactor(Nvals, params=list(b=params$b()), DDfunc=params$DDfunc())
        } else if (params$DDfunc() == "ricker"){
          max_N<- ceiling(log(0.01)/-params$b())
          Nvals<- seq(from=0, to=max_N, length.out=200)
          D_factor<- calc_Dfactor(Nvals, params=list(b=params$b()), DDfunc=params$DDfunc())
        } else if (params$DDfunc() == 'theta-logistic'){
          Nvals<- seq(from=0, to=params$K(), length.out=200)
          D_factor<- calc_Dfactor(Nvals, params=list(K=params$K(), theta=params$theta()), 
                                  DDfunc=params$DDfunc())
        } else if (params$DDfunc() == 'log_pb'){
          Nvals<- seq(from=0, to=500, by=1)
          D_factor<- calc_Dfactor(Nvals, params=list(x0=params$x0(), x1=params$x1()), 
                                  DDfunc = "log_pb")
        }
        # Get rid of all the negative values?
        D_factor[D_factor<0]<- NA
        
        plot(Nvals, D_factor, type='l', cex.lab=1.5, cex.axis=1.5,
             xlab='Population density', ylab='Scaling factor', lwd=2)
      })
    }
  )
}