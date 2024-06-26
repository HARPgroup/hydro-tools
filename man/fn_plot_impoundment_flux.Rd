% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imp_utils.R
\name{fn_plot_impoundment_flux}
\alias{fn_plot_impoundment_flux}
\title{fn_plot_impoundment_flux Function: Plot water fluxes and balance info for an impoundment}
\usage{
fn_plot_impoundment_flux(
  dat,
  pur_col = "pct_use_remain",
  Qin_col = "Qin",
  Qout_col = "Qout",
  wd_col = "demand",
  legend_on = TRUE
)
}
\arguments{
\item{dat}{a zoo series with all relevant columns}

\item{pur_col}{Percent use remaining column}

\item{Qin_col}{Inflow column}

\item{Qout_col}{Outflow column}

\item{wd_col}{Withdrawal column}

\item{legend_on}{Should legend be included on plot?}
}
\value{
plot object
}
\description{
Generates a plot of Qin, Qout, percent storage remaining and demand
}
