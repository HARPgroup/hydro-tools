# imp_utils.R
# Supporting functions for analyzing impoundments.


# 
#' fn_plot_impoundment_flux Function: Plot water fluxes and balance info for an impoundment
#' @description Generates a plot of Qin, Qout, percent storage remaining and demand
#' @param dat a zoo series with all relevant columns
#' @param pur_col Percent use remaining column
#' @param Qin_col Inflow column
#' @param Qout_col Outflow column
#' @param wd_col Withdrawal column
#' @return plot object
#' @export fn_plot_impoundment_flux
fn_plot_impoundment_flux <- function(
    dat, pur_col = 'pct_use_remain', 
    Qin_col = 'Qin', Qout_col = 'Qout',
    wd_col = 'demand'
    ) {
  ymn <- 1
  ymx <- 100
  par(mar = c(5,5,2,5))
  plot(
    dat[,pur_col] * 100.0,
    ylim=c(ymn,ymx),
    ylab="Reservoir Storage (%)",
    xlab=paste("Model Flow Period",min(index(dat)),"to",max(index(dat)))
  )
  par(new = TRUE)
  plot(dat[,Qin_col],col='blue', axes=FALSE, xlab="", ylab="")
  lines(dat[,Qout_col],col='green')
  lines(dat[,wd_col] * 1.547,col='red')
  graphics::axis(side = 4)
  mtext(side = 4, line = 3, 'Flow/Demand (cfs)')
}

