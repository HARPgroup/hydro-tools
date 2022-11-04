#-----------------------------------------------------------------------------

base_url <- "http://deq1.bse.vt.edu/d.dh/"

#Set file locations dynamically 
path <- substr(getwd(),1,nchar(getwd())-11)
source(paste(path,"/VAHydro-2.0/rest_functions.R", sep = ""))
source(paste(path,"auth.private", sep = "")); 
token <- rest_token(base_url, token, rest_uname, rest_pw);


#inputs 
inputs <- list(
  varkey = "erom_q0001e_mean", 
  featureid = 199720,
  entity_type = "dh_feature",
  proptext = NULL,
  propvalue = 10.017,
  propcode = NULL,
  startdate = NULL,
  enddate = NULL
)

#Search for existing property matching supplied *varkey, *featureid, *entity_type 
#--If none exist, proeprty is created 
#--If one exists, property is updated 
#--If more than one exist, execution is halted 
prop_created <- postProperty(inputs,fxn_locations,base_url,prop)
print(prop_created)
