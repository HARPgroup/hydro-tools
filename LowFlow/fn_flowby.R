htFlowByGroup2 <- function(loflows, flowby = 0.0) {
  
  loflows$nowd90 <- loflows['90 Day Min'] < flowby
  loflows$nowd30 <- loflows['30 Day Min'] < flowby
  loflows$nowd7 <- loflows['7 Day Min'] < flowby
  loflows$flowby <- flowby;
  return(loflows)
}

# hydrotools - flowby charts
htFlowByBox <- function(
  loflows, 
  flowby = 0.0,
  main = 'Multi Day Low Flow Distribution'
) {
  # loflows = output of IHA group2 function
  noms = c('90 Day Min','30 Day Min','7 Day Min')
  boxplot(
    loflows$`90 Day Min`, 
    loflows$`30 Day Min`, 
    loflows$`7 Day Min`,
    names = noms,
    main = main
  )
  abline(h = flowby, lwd = 2, col = "red")
  par(pch=22, col="red") # plotting symbol and color 
}

htFlowByBar <- function(
  loflows, 
  flowby = 0.0,
  metric = '90 Day Min',
  main = FALSE
) {
  if (main == FALSE) {
    main = paste("Annual", metric)
  }
  # loflows = output of IHA group2 function
  lfb <- barplot(
    loflows[,metric], 
    loflows$year, 
    names.arg=loflows$year, 
    las=2,
    main = main
  )
  par(pch=22, col="red") # plotting symbol and color 
  lines(x = lfb, y = loflows$flowby)
}

htDOR <- function(loflows) {
  if (!is.null(loflows)) {
    # this is the 90 day low flow, better for Drought of Record?
    l90 <- loflows["90 Day Min"];
    ndx = which.min(as.numeric(l90[,"90 Day Min"]));
    dor_flow = round(loflows[ndx,]$"90 Day Min",1);
    dor_year = loflows[ndx,]$"year";
    
    #moflows <- aggregate(flows, function(tt) as.Date(as.yearmon(tt), na.rm = TRUE), mean);
    #ndx = which.min(moflows);
    #x2a <- aggregate(flows, as.Date(as.yearmon(flows), na.rm = TRUE), mean);
    #dor_flow = round(moflows[ndx],2);
    #dor_year = index(moflows[ndx]);
  } else {
    dor_flow = 'na';
    dor_year = 1776;
  }
  return(data.frame('dor_flow' = dor_flow, 'dor_year' = dor_year))
}

htIndieFlows <- function(flows) {
  
  if (!is.null(flows)) {
    x7q10 = round(fn_iha_7q10(flows),2);
    alf = round(fn_iha_mlf(flows, 8),2);
  } else {
    x7q10 = 'na';
    alf = 'na';
  }
  return(data.frame('x7q10' = x7q10, 'alf' = alf))
}
