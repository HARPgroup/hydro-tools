# Facility Utilities
om_flow_table <- function(df2sum, q_col = "Qout", mo_col = "month", rdigits = 1) {
  # Expects a data frame, df2sum with Qout, month columns
  if (is.zoo(df2sum)) {
    df2sum <- as.data.frame(df2sum)
  }
  intake_summary_tbl = data.frame(
    "Month" = character(), 
    'Min' = numeric(),
    '5%' = numeric(),
    '10%' = numeric(),
    '25%' = numeric(), 
    '30%' = numeric(),
    '50%' = numeric(),
    'Mean' = numeric(),
    stringsAsFactors = FALSE) ;
  if (rdigits > 1) {
    mrdigits = rdigits - 1
  } else {
    mrdigits = rdigits
  }
  for (i in index(month.abb)) {
    moname <- month.abb[i]
    drows <- sqldf(paste("select * from df2sum where ", mo_col, " = ", i))
    q_drows <- quantile(drows[,q_col], probs=c(0,0.05,0.1,0.25, 0.3, 0.5), na.rm=TRUE)
    q_mean <- mean(drows[,q_col])
    newline = data.frame(
      "Month" = moname,
      'Min' = round(as.numeric(q_drows["0%"]),rdigits),
      '5%' = round(as.numeric(q_drows["5%"]),rdigits),
      '10%' = round(as.numeric(q_drows["10%"]),rdigits),
      '25%' = round(as.numeric(q_drows["25%"]),rdigits), 
      '30%' = round(as.numeric(q_drows["30%"]),rdigits),
      '50%' = round(as.numeric(q_drows["50%"]),rdigits),
      'Mean' = round(q_mean,mrdigits),
      stringsAsFactors = FALSE
    )
    intake_summary_tbl <- rbind(intake_summary_tbl, newline)
  }
  names(intake_summary_tbl) <- c('Month', 'Min', '5%', '10%', '25%', '30%', '50%', 'Mean')
  return(intake_summary_tbl)
}

om_quantile_table <- function(rundata_df, metrics = c("OldDuck_use_remain_mg"), quantiles = c(0,0.1,0.25,0.5,0.75,0.9,1.0), rdigits = 2) {
  quantile_df <- data.frame(matrix(ncol = length(quantiles), nrow = 0))
  colnames(quantile_df) <- paste0(quantiles*100, "%")
  for (i in 1:length(metrics)){
    if (length(grep(metrics[i],names(rundata_df),value=TRUE)) == 0){
      quantile <- data.frame(rep(NA,length(quantiles)))
      colnames(quantile) <- metrics[i]
      quantile <- t(quantile)
      colnames(quantile) <- paste0(quantiles*100, "%")
      quantile_df <- rbind(quantile_df, quantile)
    } else {
      quantile <- data.frame(quantile(rundata_df[,metrics[i]], probs=quantiles))
      colnames(quantile) <- metrics[i]
      quantile_df <- rbind(quantile_df, t(quantile))
    }
  }
  quantile_df <- round(quantile_df,rdigits)
  return(quantile_df)
}
