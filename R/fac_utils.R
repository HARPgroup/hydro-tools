# Facility Utilities
om_flow_table <- function(df2sum, q_col = "Qout", mo_col = "month") {
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
    stringsAsFactors = FALSE) ;
  for (i in index(month.abb)) {
    moname <- month.abb[i]
    drows <- sqldf(paste("select * from df2sum where ", mo_col, " = ", i))
    q_drows <- quantile(drows[,q_col], probs=c(0,0.05,0.1,0.25, 0.3, 0.5), na.rm=TRUE)
    newline = data.frame(
      "Month" = moname,
      'Min' = round(as.numeric(q_drows["0%"]),1),
      '5%' = round(as.numeric(q_drows["5%"]),1),
      '10%' = round(as.numeric(q_drows["10%"]),1),
      '25%' = round(as.numeric(q_drows["25%"]),1), 
      '30%' = round(as.numeric(q_drows["30%"]),1),
      '50%' = round(as.numeric(q_drows["50%"]),1),
      stringsAsFactors = FALSE
    )
    intake_summary_tbl <- rbind(intake_summary_tbl, newline)
  }
  names(intake_summary_tbl) <- c('Month', 'Min', '5%', '10%', '25%', '30%', '50%')
  return(intake_summary_tbl)
}