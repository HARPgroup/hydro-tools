rm(list = ls())  #clear variables

#----------------------------------------------
site <- "http://deq1.bse.vt.edu/d.bet"    #Specify the site of interest, either d.bet OR d.dh
hydro_tools <- 'C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\hydro-tools\\' #location of hydro-tools repo
save_directory <- 'C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\plots\\' #Location to output images

#----------------------------------------------

#Generate REST token              
rest_uname = FALSE
rest_pw = FALSE
source(paste(hydro_tools,"auth.private", sep = "\\")); #load rest username and password, contained in auth.private file
source(paste(hydro_tools,"VAHydro-2.0","rest_functions.R", sep = "\\")) #load REST functions
token <- rest_token(site, token, rest_uname, rest_pw)

#--------------------------------------------------------------------------------------------
# Retrieve Feature From VAHydro 
#-------------------------------------------------------------------------------------------- 
  inputs <- list (
    bundle = 'transfer',
    ftype = 'outfall',
    hydrocode = 'echo_VA0000248006'
  )


  inputs <- list (
    bundle = 'facility',
    hydrocode = 'echo_VA0086355'
  )


  dataframe <- getFeature(inputs, token, site)
  #print(dataframe)

  geom <- as.character(dataframe$geom)

#-------------------------------------------------------------------------------------------- 
#--------------------------------------------------------------------------------------------
  prop_inputs <- list (
    featureid = '418259'#,
    #varkey = 'reachcode_rad'
  )
  
  #prop <- getProperty(prop_inputs, site, prop)
  prop <- getProperty(prop_inputs, token, site, prop)
#-------------------------------------------------------------------------------------------- 
#--------------------------------------------------------------------------------------------
  ts_inputs <- list (
    featureid = as.character(dataframe$hydroid)
  )
  
  
  timeseries.df <- getTimeseries(ts_inputs, token, site, ts)
  
  pbody = list(
    #bundle = 'dh_timeseries',
    featureid = inputs$featureid,
    varid = varid,
    entity_type = inputs$entity_type
  );
  