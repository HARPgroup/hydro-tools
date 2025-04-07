# helper plotting and dfata access functions

pothab_plot <- function (
  wua_dat, all_dat, nat_col, alt_col,
  flow_pct, ifim_da_sqmi,
  site_name, scenario
) {
  # format the input data
  udat <- all_dat[c('Date', nat_col)]
  names(udat) <- c('Date', 'Flow')
  adat <- all_dat[c('Date', alt_col)]
  names(adat) <- c('Date', 'Flow')
  # just look at the box plot
  ifim_icprb_maxwd_lf <- ifim_wua_change_plot(
    udat,
    adat,
    wua_dat, flow_pct,
    "ifim_da_sqmi" = ifim_da_sqmi,
    runid_a = scenario,
    metric_a = "Qbaseline",
    runid_b = "6",metric_b = "Qout"
  )
  ifim_icprb_maxwd_lf +
    labs(
      title = paste("Habitat Change,", site_name,",",flow_pct,"%ile")
    ) + ylim(c(-100,100))
  ifim_icprb_maxwd_lf$labels$subtitle <- str_split(ifim_icprb_maxwd_lf$labels$subtitle, "\n")[[1]][[1]]
  return(ifim_icprb_maxwd_lf)
}

hab_alt_tbl <- function(yrplot) {

  yrplot$data$pctchg <- round(yrplot$data$pctchg, 2)
  yrplot$data[is.na(yrplot$data$pctchg),]$pctchg <- 0.0
  ifim_sumdata_yr <- xtabs(pctchg ~ metric + flow, data = yrplot$data)
  ifim_mat <- as.data.frame.matrix(ifim_sumdata_yr)
  ifim_mat <- cbind(MAF = ifim_mat[,"MAF"], ifim_mat[,month.abb])
  tbls5pct <-  cbind(MAF = ifim_mat[,"MAF"], ifim_mat[,month.abb])
  tbls5pct <- as.data.frame(tbls5pct)
  return(tbls5pct)
}
