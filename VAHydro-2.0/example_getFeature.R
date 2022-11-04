#-----------------------------------------------------------------------------

base_url <- "http://deq1.bse.vt.edu/d.dh/"

#Set file locations dynamically 
path <- substr(getwd(),1,nchar(getwd())-11)
source(paste(path,"/VAHydro-2.0/rest_functions.R", sep = ""))
source(paste(path,"auth.private", sep = "")); 
token <- rest_token(base_url, token, rest_uname, rest_pw);

#inputs 
xxxinputs <- list(
  varkey = "erom_q0001e_mean",
  featureid = 199720,
  entity_type = "dh_feature"
)
# pid = 3575849
inputs <- list (
  hydrocode = 'vahydrosw_wshed_YP3_6470_6690',
  bundle = 'watershed',
  ftype = 'vahydro'
)
#property dataframe returned
dataframe <- getFeature(inputs, token, base_url, feature)
print(dataframe)



