# These functions have been removed from WCHO_VAHydro_Import.R

############################################################################################
# RETRIEVE/CREATE/UPDATE FACILITY DH FEATURE
############################################################################################  

#--------------------------------------------------------#

facility_import<- function(facility_inputs,iteration){
  old <- Sys.time() # get start time
  facility.dataframe <- data.frame()
  
  for (i in iteration:length(facility_inputs$hydrocode)){
    print(paste("Processing Facility ",i, "(",facility_inputs$hydrocode[i],")","  of ", length(facility_inputs$dh_link_admin_location)))
    facility.dataframe_i <- getFeature(facility_inputs[i,], token, site, feature) #need token now for access
    
    if(facility.dataframe_i[1]==FALSE){
      facility.dataframe_ii <- postFeature(facility_inputs[i,], site, feature)
      print(facility.dataframe_ii)
      facility.dataframe_i <- getFeature(facility_inputs[i,], token, site, feature)
      
    }else{
      print("This Facility Feature already exists")
    }
    
    facility.dataframe<-rbind(facility.dataframe,facility.dataframe_i)
    
  }
  
  facility.hydroid<-data.frame(hydrocode=facility.dataframe$hydrocode,hydroid=facility.dataframe$hydroid)
  
  assign("facility.dataframe",facility.dataframe,envir = .GlobalEnv)
  assign("facility.hydroid",facility.hydroid,envir = .GlobalEnv)
  
  write.table(facility.dataframe,file="C:/Users/maf95834/Documents/ECHO_VAHydro_Import/ECHO_NPDES/Documentation/Echo_VAHydro_Imports/facility.dataframe.txt",sep="\t",row.names=F)
  new <- Sys.time() - old # calculate difference
  print <- Sys.time()
  print(new) # print in nice format
}
facility_import(facility_inputs,1)
#start at iteration == 12576
############################################################################################
# RETRIEVE/CREATE/UPDATE FACILITY METADATA PROPERTIES
# Make sure hydrocodes are in same order as in the facility data frame as well
############################################################################################   
# Waterbody Name (GNIS): Name of waterbody from the Geographic Names Information System database where the facility is permitted to discharge directly

waterbody_import<- function(wb_gnis_name,iteration){
  property.dataframe<-data.frame()
  
  prop_inputs <-data.frame(
    bundle = rep(paste0('dh_properties'),length(wb_gnis_name$hydrocode)),
    featureid = facility.hydroid$hydroid[match(facility.hydroid$hydrocode,wb_gnis_name$hydrocode)],
    varkey = as.character(wb_gnis_name$varkey),
    entity_type = rep(paste0('dh_feature'),length(wb_gnis_name$hydrocode)),
    propname = as.character(wb_gnis_name$propname),
    #propvalue = rep(NA,length(wb_gnis_name$hydrocode)),
    #proptext = rep(NA,length(wb_gnis_name$hydrocode)),
    propcode = as.character(wb_gnis_name$propcode),
    #startdate = rep(NA,length(wb_gnis_name$hydrocode)),
    #enddate = rep(NA,length(wb_gnis_name$hydrocode)),
    stringsAsFactors = F
  )
  
  for (i in iteration:length(prop_inputs$featureid)){
    print(paste("Processing Hydrocode ",i," of ", length(wb_gnis_name$hydrocode)))
    
    property.dataframe_i <- getProperty(prop_inputs[i,], site, prop) #"http://deq1.bse.vt.edu/d.alpha" 
    
    if(property.dataframe_i[1]==FALSE){
      property.dataframe_ii <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
      print(property.dataframe_ii)
      if(property.dataframe_ii=="Status 406, Error: Property Not Created Successfully"){
        break
      }
      property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
    }else{
      print("This Property already exists")
    }
    property.dataframe<-rbind(property.dataframe,property.dataframe_i) #store all properties in same large dataframe to double check quality of script
  }
  
  assign("wbgnis.dataframe",property.dataframe,envir=.GlobalEnv)
  
}
waterbody_import(wb_gnis_name,1)

############################################################################################ 
#Combined Sewer System (css): The discharge from a combined sewer system at a point prior to a treatment plant

css_import<- function(css,iteration){
  property.dataframe<-data.frame()
  
  prop_inputs <-data.frame(
    bundle = rep(paste0('dh_properties'),length(css$hydrocode)),
    featureid = facility.hydroid$hydroid[match(facility.dataframe$hydrocode,css$hydrocode)],
    varkey = as.character(css$varkey),
    entity_type = rep(paste0('dh_feature'),length(css$hydrocode)),
    propname = as.character(css$propname),
    #propvalue = rep(NA,length(css$hydrocode)),
    #proptext = rep(NA,length(css$hydrocode)),
    propcode = as.character(css$propcode),
    #startdate = rep(NA,length(css$hydrocode)),
    #enddate = rep(NA,length(css$hydrocode)),
    stringsAsFactors = F
  )
  
  for (i in iteration:length(prop_inputs$featureid)){
    print(paste("Processing Hydrocode ",i," of ", length(css$hydrocode)))
    property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
    
    if(property.dataframe_i[1]==FALSE){
      property.dataframe_ii <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
      print(property.dataframe_ii)
      if(property.dataframe_ii=="Status 406, Error: Property Not Created Successfully"){
        break
      }
      property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
    }else{
      print("This Property already exists")
    }
    property.dataframe<-rbind(property.dataframe,property.dataframe_i) #store all properties in same large dataframe to double check quality of script
  }
  
  assign("css.dataframe",property.dataframe,envir=.GlobalEnv)
  
}
css_import(css,1)

############################################################################################ 
#Number of Discharge Outfalls Prior to the Treatment Plant (CWP_CSO_Outfalls)

cso_outfall_import<- function(cwp_cso_outfalls,iteration){
  property.dataframe<-data.frame()
  
  prop_inputs <-data.frame(
    bundle = rep(paste0('dh_properties'),length(cwp_cso_outfalls$hydrocode)),
    featureid = facility.hydroid$hydroid[match(facility.dataframe$hydrocode,cwp_cso_outfalls$hydrocode)],
    varkey = as.character(cwp_cso_outfalls$varkey),
    entity_type = rep(paste0('dh_feature'),length(cwp_cso_outfalls$hydrocode)),
    propname = as.character(cwp_cso_outfalls$propname),
    propvalue = as.character(cwp_cso_outfalls$propvalue),
    #proptext = rep(NA,length(cwp_cso_outfalls$hydrocode)),
    #propcode = rep(NA,length(cwp_cso_outfalls$hydrocode)),
    #startdate = rep(NA,length(cwp_cso_outfalls$hydrocode)),
    #enddate = rep(NA,length(cwp_cso_outfalls$hydrocode)),
    stringsAsFactors = F
  )
  
  prop_inputs<-subset(prop_inputs,!is.na(prop_inputs$propvalue))
  
  for (i in iteration:length(prop_inputs$featureid)){
    print(paste("Processing Hydrocode ",i," of ", length(cwp_cso_outfalls$hydrocode)))
    property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
    
    if(property.dataframe_i[1]==FALSE){
      property.dataframe_ii <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
      print(property.dataframe_ii)
      if(property.dataframe_ii=="Status 406, Error: Property Not Created Successfully"){
        break
      }
      property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
      
    }else{
      print("This Property already exists")
    }
    property.dataframe<-rbind(property.dataframe,property.dataframe_i) #store all properties in same large dataframe to double check quality of script
  }
  
  assign("csooutfall.dataframe",property.dataframe,envir=.GlobalEnv)
  
}

#replace NA values in the properties dataframes with 0
cwp_cso_outfalls$propvalue <- 0

cso_outfall_import(cwp_cso_outfalls,1)

############################################################################################ 
#Impairment Class or Category of the Waterbody (impair_cause)

impair_import<- function(impair_cause,iteration){
  
  property.dataframe<-data.frame()
  
  prop_inputs <-data.frame(
    bundle = rep(paste0('dh_properties'),length(impair_cause$hydrocode)),
    featureid = facility.hydroid$hydroid[match(facility.dataframe$hydrocode,impair_cause$hydrocode)],
    varkey = as.character(impair_cause$varkey),
    entity_type = rep(paste0('dh_feature'),length(impair_cause$hydrocode)),
    propname = as.character(impair_cause$propname),
    #propvalue = rep(NA,length(impair_cause$hydrocode)),
    proptext = as.character(impair_cause$proptext),
    #propcode = rep(NA,length(impair_cause$hydrocode)),
    #startdate = rep(NA,length(impair_cause$hydrocode)),
    #enddate = rep(NA,length(impair_cause$hydrocode)),
    stringsAsFactors = F
  )
  
  prop_inputs<-subset(prop_inputs,!prop_inputs$proptext=="")
  
  for (i in iteration:length(prop_inputs$featureid)){
    print(paste("Processing Hydrocode ",i," of ", length(impair_cause$hydrocode)))
    property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
    
    if(property.dataframe_i[1]==FALSE){
      property.dataframe_ii <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
      print(property.dataframe_ii)
      
      if(property.dataframe_ii=="Status 406, Error: Property Not Created Successfully"){
        break
      }
      property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
    }else{
      print("This Property already exists")
    }
    property.dataframe<-rbind(property.dataframe,property.dataframe_i) 
  }
  
  assign("impair.dataframe",property.dataframe,envir=.GlobalEnv)
  
}
impair_import(impair_cause,1)

############################################################################################ 
#Date of most recent inspection of the facility (last_inspect)

#last_inspect_import function does not like the last_inspect$startdate
last_inspect$startdate <- c('2018/12/20', '2018/08/29')


last_inspect_import<- function(last_inspect,iteration){
  property.dataframe<-data.frame()
  
  prop_inputs <-data.frame(
    bundle = rep(paste0('dh_properties'),length(last_inspect$hydrocode)),
    featureid = as.character(facility.hydroid$hydroid[match(facility.dataframe$hydrocode,last_inspect$hydrocode)]),
    varkey = as.character(last_inspect$varkey),
    entity_type = rep(paste0('dh_feature'),length(last_inspect$hydrocode)),
    propname = as.character(last_inspect$propname),
    #propvalue = rep(NA,length(last_inspect$hydrocode)),
    #proptext = rep(NA,length(last_inspect$hydrocode)),
    #propcode = as.character(last_inspect$propcode),
    startdate = as.character(last_inspect$startdate),
    #startdate = as.PosixCT(as.character(last_inspect$startdate),
    #enddate = rep(NA,length(last_inspect$hydrocode)),
    stringsAsFactors = F
  )
  
  prop_inputs<-subset(prop_inputs,!prop_inputs$startdate=="")
  
  for (i in iteration:length(prop_inputs$featureid)){
    print(paste("Processing Hydrocode ",i," of ", length(last_inspect$hydrocode)))
    
    property.dataframe_i <- getProperty(prop_inputs[1,], site, prop)
    
    if(property.dataframe_i==FALSE){
      property.dataframe_ii <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
      print(property.dataframe_ii)
      if(property.dataframe_ii=="Status 406, Error: Property Not Created Successfully"){
        break
      }
      property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
      
    }else{
      print("This Property already exists")
    }
    property.dataframe<-rbind(property.dataframe,property.dataframe_i) 
  }
  
  assign("inspect.dataframe",property.dataframe,envir = .GlobalEnv)
}
last_inspect_import(last_inspect,1)

############################################################################################ 
#Unique ID for Waterbody (reachcode_rad)

reachcode_import<- function(reachcode_rad,iteration){
  property.dataframe<-data.frame()
  
  prop_inputs <-data.frame(
    bundle = rep(paste0('dh_properties'),length(reachcode_rad$hydrocode)),
    featureid = facility.hydroid$hydroid[match(facility.dataframe$hydrocode,reachcode_rad$hydrocode)],
    varkey = as.character(reachcode_rad$varkey),
    entity_type = rep(paste0('dh_feature'),length(reachcode_rad$hydrocode)),
    propname = as.character(reachcode_rad$propname),
    #propvalue = rep(NA,length(reachcode_rad$hydrocode)),
    #proptext = rep(NA,length(reachcode_rad$hydrocode)),
    propcode = as.character(reachcode_rad$propcode),
    #startdate = rep(NA,length(reachcode_rad$hydrocode)),
    #enddate = rep(NA,length(reachcode_rad$hydrocode)),
    stringsAsFactors = F
  )
  
  prop_inputs<-subset(prop_inputs,!is.na(prop_inputs$propcode))
  
  for (i in iteration:length(prop_inputs$featureid)){
    print(paste("Processing Hydrocode ",i," of ", length(reachcode_rad$hydrocode)))
    property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
    
    if(property.dataframe_i[1]==F){
      property.dataframe_ii <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
      print(property.dataframe_ii)
      if(property.dataframe_ii=="Status 406, Error: Property Not Created Successfully"){
        break
      }
      property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
    }else{
      print("This Property already exists")
    }
    property.dataframe<-rbind(property.dataframe,property.dataframe_i) 
  }
  
  assign("reachcode.dataframe",property.dataframe,envir = .GlobalEnv)
}
reachcode_import(reachcode_rad,10265)

############################################################################################ 
# Facility Design Flow in MGD (design_flow)

df_import<- function(design_flow,iteration){
  property.dataframe<-data.frame()
  
  prop_inputs <-data.frame(
    bundle = rep(paste0('dh_properties'),length(design_flow$hydrocode)),
    featureid = facility.hydroid$hydroid[match(facility.dataframe$hydrocode,design_flow$hydrocode)],
    varkey = as.character(design_flow$varkey),
    entity_type = rep(paste0('dh_feature'),length(design_flow$hydrocode)),
    propname = as.character(design_flow$propname),
    propvalue = as.character(design_flow$propvalue[match(design_flow$hydrocode,facility.dataframe$hydrocode)]),
    #proptext = rep(NA,length(design_flow$hydrocode)),
    #propcode = rep(NA,length(design_flow$hydrocode)),
    #startdate = rep(NA,length(design_flow$hydrocode)),
    #enddate = rep(NA,length(design_flow$hydrocode)),
    stringsAsFactors = F
  )
  
  for (i in iteration:length(prop_inputs$featureid)){
    print(paste("Processing Hydrocode ",i," of ", length(design_flow$hydrocode)))
    property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
    
    if(property.dataframe_i[1]==FALSE){
      property.dataframe_ii <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
      print(property.dataframe_ii)
      if(property.dataframe_ii=="Status 406, Error: Property Not Created Successfully"){
        break
      }
      property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
    }else{
      print("This Property already exists")
    }
    property.dataframe<-rbind(property.dataframe,property.dataframe_i) 
  }
  
  assign("df.dataframe",property.dataframe,envir = .GlobalEnv)
}
df_import(design_flow,12331)

############################################################################################
# RETRIEVE/CREATE/UPDATE RELEASE DH FEATURE
############################################################################################  

release_import<- function(releasepoint,iteration){
  release.dataframe<-data.frame()
  release.dataframe_ii<-data.frame()
  
  release_inputs <- data.frame(
    bundle = as.character(releasepoint$bundle),
    ftype = as.character(releasepoint$ftype),
    hydrocode = as.character(releasepoint$hydrocode),
    name = as.character(releasepoint$name),
    fstatus = as.character(releasepoint$fstatus),
    dh_link_facility_mps =  facility.hydroid$hydroid[match(releasepoint$dh_link_facility_mps,facility.hydroid$hydrocode)], #dependent on facility import
    dh_geofield = as.character(releasepoint$dh_geofield),
    stringsAsFactors=FALSE
  ) 
  
  for (i in iteration:length(release_inputs$hydrocode)){
    print(paste("Processing Hydrocode ",i," of ", length(release_inputs$hydrocode)))
    release.dataframe_i <- getFeature(release_inputs[i,], token, site, feature)
    
    if(release.dataframe_i[1]==FALSE){
      release.dataframe_ii <- postFeature(release_inputs[i,],site, feature)
      print(release.dataframe_ii)
      
      release.dataframe_i <- getFeature(release_inputs[i,], token, site, feature)
      
    }else{
      print("This Feature already exists")
    }
    release.dataframe<-rbind(release.dataframe,release.dataframe_i)
    
  }
  
  release.hydroid<-data.frame(hydrocode=release.dataframe$hydrocode, hydroid=release.dataframe$hydroid)
  
  assign("release.dataframe",release.dataframe,env=.GlobalEnv)
  assign("release.hydroid",release.hydroid,env=.GlobalEnv)
  
  write.table(release.dataframe,file="C:/Users/maf95834/Documents/ECHO_VAHydro_Import/ECHO_NPDES/Documentation/Echo_VAHydro_Imports/release.dataframe.txt",sep="\t",row.names=F)
  
}
release_import(releasepoint,1)

############################################################################################
# RETRIEVE/CREATE/UPDATE OUTFALL DH FEATURE
############################################################################################   

outfall_import<- function(outfalls,iteration){
  
  outfall.dataframe<-data.frame()
  outfall.dataframe_ii<-data.frame()
  
  outfall_inputs <- data.frame(
    bundle = as.character(outfalls$bundle),
    ftype = as.character(outfalls$ftype),
    hydrocode = as.character(outfalls$hydrocode),
    name = as.character(outfalls$name),
    fstatus = as.character(outfalls$fstatus),
    dh_link_facility_mps =  facility.hydroid$hydroid[match(outfalls$dh_link_facility_mps,facility.hydroid$hydrocode)], #dependent on facility import
    dh_geofield = as.character(outfalls$dh_geofield),
    stringsAsFactors=FALSE
  ) 
  
  for (i in iteration:length(outfalls$hydrocode)){
    print(paste("Processing Hydrocode ",i," of ", length(outfalls$hydrocode)))
    outfall.dataframe_i <- getFeature(outfall_inputs[i,], token, site, feature)
    
    if(outfall.dataframe_i[1]==FALSE){
      outfall.dataframe_ii <- postFeature(outfall_inputs[i,], site, feature)
      print(outfall.dataframe_ii)
      outfall.dataframe_i <- getFeature(outfall_inputs[i,], token, site, feature)
      
    }else{
      print("This Feature already exists")
    }
    outfall.dataframe<-rbind(outfall.dataframe,outfall.dataframe_i)
  }
  
  outfall.hydroid<-data.frame(hydrocode=outfall.dataframe$hydrocode,hydroid=outfall.dataframe$hydroid)
  
  assign("outfall.dataframe",outfall.hydroid,env=.GlobalEnv)
  assign("outfall.hydroid",outfall.hydroid,env=.GlobalEnv)
  
  write.table(outfall.dataframe,file="C:/Users/maf95834/Documents/ECHO_VAHydro_Import/ECHO_NPDES/Documentation/Echo_VAHydro_Imports/outfall.dataframe.txt",sep="\t",row.names=F)
}
outfall_import(outfalls,1)

outfall_type_import<- function(outfall_props,iteration){
  
  property.dataframe<-data.frame()
  
  prop_inputs <-data.frame(
    bundle = rep(paste0('dh_properties'),length(outfall_props$hydrocode)),
    featureid = outfall.dataframe$hydroid[match(outfall_props$hydrocode,outfall.dataframe$hydrocode)],
    varkey = as.character(outfall_props$varkey),
    entity_type = rep(paste0('dh_feature'),length(outfall_props$hydrocode)),
    propname = as.character(outfall_props$propname),
    proptext = as.character(outfall_props$proptext),
    propcode = as.character(outfall_props$propcode),
    stringsAsFactors = F
  )
  
  for (i in iteration:length(prop_inputs$featureid)){
    print(paste("Processing Hydrocode ",i," of ", length(prop_inputs$featureid)))
    property.dataframe_i <- getProperty(prop_inputs[i,], site, prop) #"http://deq1.bse.vt.edu/d.alpha" 
    
    if(property.dataframe_i[1]==FALSE){
      property.dataframe_ii <- postProperty(prop_inputs[i,], fxn_locations, site, prop)
      print(property.dataframe_ii)
      if(property.dataframe_ii=="Status 406, Error: Property Not Created Successfully"){
        break
      }
      property.dataframe_i <- getProperty(prop_inputs[i,], site, prop)
    }else{
      print("This Property already exists")
    }
    property.dataframe<-rbind(property.dataframe,property.dataframe_i) #store all properties in same large dataframe to double check quality of script
  }
  
  assign("outfall.type.dataframe",property.dataframe,envir=.GlobalEnv)
}
outfall_type_import(outfall_props,1)

############################################################################################
# RETRIEVE/CREATE/UPDATE CONVEYANCE DH FEATURE
############################################################################################  

conveyance_import<- function(conveyance,iteration){
  
  # Format conveyance geom from release and outfall geoms 
  release.geofield <- substring(release.dataframe$dh_geofield, 8)
  release.geofield <-substr(release.geofield, 1, nchar(release.geofield)-1) 
  
  outfall.geofield <- substring(outfall.dataframe$dh_geofield, 8)
  outfall.geofield <-substr(outfall.geofield, 1, nchar(outfall.geofield)-1) 
  conveyance.geofield <- paste('LINESTRING (',release.geofield,', ',outfall.geofield,')',sep="")
  
  conveyance.dataframe<-data.frame()
  conveyance.dataframe_ii<-data.frame()
  
  conveyance_inputs <- data.frame(
    bundle = as.character(conveyance$bundle),
    ftype = as.character(conveyance$ftype),
    hydrocode = as.character(conveyance$hydrocode),
    name = as.character(conveyance$name),
    fstatus = as.character(conveyance$fstatus),
    field_dh_from_entity =  release.hydroid$hydroid[match(release.hydroid$hydrocode,conveyance$field_dh_from_entity)], 
    field_dh_to_entity =  outfall.hydroid$hydroid[match(outfall.hydroid$hydrocode,conveyance$field_dh_to_entity)],
    dh_geofield = conveyance.geofield,
    stringsAsFactors=FALSE
  ) 
  
  for (i in 1:length(conveyance_inputs$hydrocode)){
    print(paste("Processing Hydrocode ",i," of ", length(conveyance_inputs$hydrocode)))
    conveyance.dataframe_i <- getFeature(conveyance_inputs[i,], token, site, feature)
    
    if(conveyance.dataframe_i[1]==FALSE){
      conveyance.dataframe_ii <- postFeature(conveyance_inputs[i,], site, feature)
      print(conveyance.dataframe_ii)
      conveyance.dataframe_i <- getFeature(conveyance_inputs[i,], token, site, feature)
      
    }else{
      print("This Feature already exists")
    }
    conveyance.dataframe<- rbind(conveyance.dataframe, conveyance.dataframe_i)
    
  }
  
  conveyance.hydroid<-data.frame(hydrocode=conveyance.dataframe$hydrocode,hydroid=conveyance.dataframe$hydroid)
  
  assign("conveyance.dataframe",conveyance.dataframe,env=.GlobalEnv)
  assign("conveyance.hydroid",conveyance.hydroid,env=.GlobalEnv)
  
  write.table(conveyance.dataframe,file="conveyance.dataframe.txt",sep="\t",row.names=F)
  
}
conveyance_import(conveyance,1)


##################################################################################################################################
###########################################3 Release Point Generation#############################################################

# Generate basic list of outfalls
ECHO_Outfalls <- vahydro_echo_outfalls(ECHO_Facilities, VPDES_Outfalls, timeseries)

# Generation of the release point imports. In essence, this portion just formats various data from both the facility
# and the outfall list (VPDES_Outfalls and timeseries) to create the release point attributes and geometry.
releasepoint <- release_generation(ECHO_Facilities,ECHO_Outfalls,timeseries)
write.table(releasepoint,"releasepoint.txt",sep="\t",row.names = F)

design_flow <- facility_properties(ECHO_Facilities)
write.table(design_flow,"design_flow.txt",sep="\t",row.names = F)


##################################################################################################################################
###########################################4 Conveyance and Outfall Formatting ####################################################

# Generates the conveyance import using the outfall list in 'ECHO_Outfalls' which is dependent on reporting outfalls in timeseries
# Important to save ECHO_Outfalls from release_generation function

outfalls <- outfall_formatted(ECHO_Outfalls)
write.table(outfalls,paste0("outfalls.txt"),sep="\t",row.names = F)
# conveyances connect releae points to outfalls
conveyance <- conveyance_generation(ECHO_Outfalls)
write.table(conveyance,paste0("conveyance.txt"),sep="\t",row.names = F)

# Known Virginia outfall list for isolating only true external outfalls
outfall_types<-read.table(file="https://raw.githubusercontent.com/HARPgroup/USGS_Consumptive_Use/master/Code/Data%20Cleaning/Outfall_Types.txt", header=T, sep="|")

outfall_props <- outfall_properties(outfall_types)
##################################################################################################################################
###########################################5 Facility Properties Generation####################################################

#Facility dH Property Mapping

#hydrocode, varkey, propname, propvalue, proptext, propcode, startdate, enddate

#Facility Metadata/Properties 
#-Waterbody Name (GNIS)
#-Combined Sewer System Flag (CWPCsoFlag)
#-Number of Discharge Outfalls Prior to the Treatment Plant (CWP_CSO_Outfalls)
#-Impairment Class or Category of the Waterbody (impair_cause)
#-Date of most recent inspection of the facility (last_inspect)
#-Unique ID for Waterbody (reachcode_rad)
#-Facility Design Flow in MGD (design_flow)


############################################################################################
# CREATE/UPDATE FLAGGING PROPERTIES OF TIMESERIES
############################################################################################   

tsflag_import<- function(flag_dataframe,iteration){
  
  flag.dataframe_ii<-data.frame()
  
  for (i in iteration:length(flag_dataframe$featureid)){
    print(paste("Processing Property ",i," of ", length(flag_dataframe$featureid)))
    flag.dataframe_i <- getProperty(flag_dataframe[i,], basepath, prop)
    if(flag.dataframe_i[1]==FALSE){
      flag.dataframe_ii <- postProperty(flag_dataframe[i,], fxn_locations, basepath, prop)
      print(flag.dataframe_ii)
      if(flag.dataframe_ii=="Status 200, Error: Property Not Created Successfully"){
        break
      }
    }else{
      print("Flag Already Exists")
    }
    
  }
  
}

for(i in 1:length(flag_inputs_echo_flag$featureid)){
  if(flag_inputs_echo_flag$propcode[i]=="E90"){
    flag_inputs_echo_flag$proptext[i]<-"Effluent Violation"
  }else if(flag_inputs_echo_flag$propcode[i]=="D90"){
    flag_inputs_echo_flag$proptext[i]<-"DMR Overdue, with a numeric limit"
  }else if(flag_inputs_echo_flag$propcode[i]=="D80"){
    flag_inputs_echo_flag$proptext[i]<-"DMR Overdue, monitoring only required"
  }
}
tsflag_import(flag_inputs_echo_flag,1)
tsflag_import(flag_inputs_dmr_flag_desflow,1)
tsflag_import(flag_inputs_dmr_flag_units_100,1)
tsflag_import(flag_inputs_1000000,1)

