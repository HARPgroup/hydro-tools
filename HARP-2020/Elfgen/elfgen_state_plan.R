#----------------------------------------------
site <- "http://deq2.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
#----------------------------------------------
# Load Libraries
basepath='/var/www/R';
source(paste(basepath,'config.R',sep='/'))
library(stringr)
library(sqldf)
library(elfgen)
# dirs/URLs
save_directory <- "/var/www/html/data/proj3/out"
save_url <- paste(str_remove(site, 'd.dh'), "data/proj3/out", sep='');

#Add in all riv seg list
riv_seg <- 'PS3_5990_6161' #random example for practice
runid<-11
pid <- get.overall.vahydro.prop(riv_seg, site = site, token = token)

inputs <- list(
hydrocode = paste0('vahydrosw_wshed_', riv_seg))
feature <- getFeature(inputs, token, site)

hydroid<-as.character(feature$hydroid)

# Read Args
# argst <- commandArgs(trailingOnly=T)
# pid <- as.integer(argst[1])
# elid <- as.integer(argst[2])
# runid <- as.integer(argst[3])

scen.propname<-paste0('runid_', runid)

# GETTING SCENARIO PROPERTY FROM VA HYDRO
sceninfo <- list(
  varkey = 'om_scenario',
  propname = scen.propname,
  featureid = pid,
  entity_type = "dh_properties"
)
scenprop <- getProperty(sceninfo, site, scenprop)
# POST PROPERTY IF IT IS NOT YET CREATED
if (identical(scenprop, FALSE)) {
  # create
  sceninfo$pid = NULL
} else {
  sceninfo$pid = scenprop$pid
}
#scenprop = postProperty(inputs=sceninfo,base_url=base_url,prop)
scenprop <- getProperty(sceninfo, site, scenprop)

# Create an algorithm that finds the outlet point for the watershed 

nhdplus_views <- paste(site,'dh-feature-containing-export', hydroid, 'watershed/nhdplus/nhdp_drainage_sqmi',  sep = '/')
nhdplus_df <- read.csv(file=nhdplus_views, header=TRUE, sep=",")

hydroid_out <-sqldf("SELECT hydroid, max(propvalue)
                  FROM nhdplus_df ")

inputs <- list(
  varkey = 'om_class_Constant',
  propname = 'consumptive_use_frac',
  entity_type = 'dh_properties',
  featureid = scenprop$pid
)
prop <- getProperty(inputs, site)
cuf <- prop$propvalue


site_comparison <- paste(site,'dh-feature-contained-within-export', hydroid_out$hydroid, 'watershed', sep = '/')
containing_watersheds <- read.csv(file=site_comparison, header=TRUE, sep=",")


nhd_huc8_code <- sqldf("SELECT hydrocode 
             FROM containing_watersheds 
             WHERE ftype = 'nhd_huc8'")
             

nhd_huc10_code <- sqldf("SELECT hydrocode 
             FROM containing_watersheds 
             WHERE ftype = 'nhd_huc10'")










hydroid2 <- sqldf("SELECT hydroid 
                  FROM containing_watersheds 
                  WHERE ftype 
                  LIKE '%nhdplus%'")













# Load the model result data for this scenario for "consumptive_use_frac" property

# Calculate richness change here
elfgen_richness_change_8 = made_up_elfgen_function()
elfgen_richness_change_10 = made_up_elfgen_function()

vahydro_post_metric_to_scenprop(scenprop$pid, 'om_class_Constant', NULL, 'mne9_10', sept_10, site, token)
