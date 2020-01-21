require(sqldf)
#require(rgdal)
require(httr)

#prevents scientific notation
options(scipen = 20)
#QA Check for demand projections (for Aquaveo export and SWRP update)

#load in exports
export_path <- "U:\\OWS\\Data Development_Management\\Data Requests\\Aquaveo\\QA_check_2019\\"
#use either sys.date OR date of when export was downloaded and is in title of export
export_date <- Sys.Date()
#export_date <- "2019-12-26"
# gwma_path <- "U:\\OWS\\GIS\\GWMA\\gwma_all-2015"
# gwma_layer_name <- "gwma_all-2015"

###Load in allocation view export
#load downloaded export
#Active, well, GWMA counties, prop_name
#wsp2020_2020_mgy
# wsp2020_load <- read.csv(file = paste0(export_path, paste0(export_date,"_wsp2020_2020_mgy_allocation_export.csv")), header=TRUE, sep=",")
# wsp2020 <- wsp2020_load
# #wsp2020_2040_mgy
# wsp2040_load<- read.csv(file = paste0(export_path, paste0(export_date,"_wsp2020_2040_mgy_allocation_export.csv")), header=TRUE, sep=",")
# wsp2040 <- wsp2040_load
# #wd_current_mgy
# wdcurrent_load <- read.csv(file = paste0(export_path, paste0("2020-01-07_wd_current_mgy_allocation_export.csv")), header=TRUE, sep=",")
# wdcurrent <- wdcurrent_load
# #load in sql_base_snapshot data_base .csv file 
 data_base_load <- read.csv(file = paste0(export_path, paste0("2020-01-07_data_base.csv")), header = T, sep = ",")
 data_base <- data_base_load

#pull directly from VAHydro export url
#filters used: active, well, prop_name
#wsp2020_2020_mgy
localpath <- tempdir()
filename <- paste("data.all.csv",sep="")
destfile <- paste(localpath,filename,sep="\\")
download.file(paste("https://deq1.bse.vt.edu/d.dh/facility_mp_frac_value_export?bundle%5B0%5D=well&hydroid=&propcode_op=%3D&propcode=&fstatus_op=in&fstatus=active&propname_op=%3D&propname=wsp2020_2020_mgy&hydroid_1_op=%3D&hydroid_1%5Bvalue%5D=&hydroid_1%5Bmin%5D=&hydroid_1%5Bmax%5D=&dh_link_admin_fa_usafips_target_id_op=in&ftype_op=contains&ftype=",sep=""), destfile = destfile, method = "libcurl")
wsp2020_load <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")

wsp2020 <- wsp2020_load
#wsp2020_2040_mgy
localpath <- tempdir()
filename <- paste("data.all.csv",sep="")
destfile <- paste(localpath,filename,sep="\\")
download.file(paste("https://deq1.bse.vt.edu/d.dh/facility_mp_frac_value_export?bundle%5B0%5D=well&hydroid=&propcode_op=%3D&propcode=&fstatus_op=in&fstatus=active&propname_op=%3D&propname=wsp2020_2040_mgy&hydroid_1_op=%3D&hydroid_1%5Bvalue%5D=&hydroid_1%5Bmin%5D=&hydroid_1%5Bmax%5D=&dh_link_admin_fa_usafips_target_id_op=in&ftype_op=contains&ftype=",sep=""), destfile = destfile, method = "libcurl")
wsp2040_load <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")

wsp2040 <- wsp2040_load
#wd_current_mgy
localpath <- tempdir()
filename <- paste("data.all.csv",sep="")
destfile <- paste(localpath,filename,sep="\\")
download.file(paste("https://deq1.bse.vt.edu/d.dh/facility_mp_frac_value_export?bundle%5B0%5D=well&hydroid=&propcode_op=%3D&propcode=&fstatus_op=in&fstatus=active&propname_op=%3D&propname=wd_current_mgy&hydroid_1_op=%3D&hydroid_1%5Bvalue%5D=&hydroid_1%5Bmin%5D=&hydroid_1%5Bmax%5D=&dh_link_admin_fa_usafips_target_id_op=in&ftype_op=contains&ftype=",sep=""), destfile = destfile, method = "libcurl")
wdcurrent_load <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")

wdcurrent <- wdcurrent_load

#extract only GWMA counties for Aquaveo
# Accomack 51001
# Caroline 51033
# Charles City 51036
# Chesapeake 51550
# Chesterfield 51041
# Essex 51057
# Fairfax County 51059
# Franklin city 51620
# Gloucester 51073
# Hampton 51650
# Hanover 51085
# Henrico 51087
# Hopewell 51670
# Isle of Wight 51093
# James City 51095
# King and Queen 51097
# King George 51099
# King William 51101
# Lancaster 51103
# Mathews 51115
# Middlesex 51119
# New Kent 51127
# Newport news 51700
# Northampton 51131
# Northumberland 51133
# Poquoson 51735
# Portsmouth 51740
# PrinceGeorge 51149
# PRinceWilliam 51153
# Richmond COunty 51159
# Southampton 51175
# Spotsylvania 51177
# Stafford 51179
# Suffolk 51800
# Surry 51181
# Sussex 51183
# Virginia Beach 51810
# Westmoreland 51193
# Williamsburg 51830
# York 51199

# GWMA_county_fips <- c(51001, 51033, 51036, 51550, 51041, 51057, 51059, 51620, 51073, 51650, 51085, 51087, 51670, 51093, 51095, 51097, 51099, 51101, 51103, 51115, 51119, 51127, 51700, 51131, 51133, 51735, 51740, 51149, 51153, 51159, 51175, 51177, 51179, 51800, 51181, 51183, 51810, 51193, 51830, 51199)


#Check the use types for these MPs
types_2020 <- sqldf("SELECT distinct ftype
               from wsp2020_load")
types_2040 <- sqldf("SELECT distinct ftype
               from wsp2040_load")
types_current <- sqldf("SELECT distinct ftype
               from wdcurrent_load")

#use the sp_contain function in the r_function.r scrupt in the echo import vahydro folder on github
# #### sp_contain function requires 2 fields (must have "Name" and "Code" attributes)
# #use sp_contain function written for echo import script to clip the points only within the GWMA boundaries
# 
# print(paste("Number of MPs Before Spatial Containment", length(wsp2020[,1])))
# 
# coordinates(wsp2020) <- c("FacLong", "FacLat") # add col of coordinates, convert dataframe to Large SpatialPointsDataFrame
# wsp2020 <- sp_contain(gwma_path,gwma_layer_name,wsp2020)
# #------------------------------------------------------------
# #ECHO_Facilities_original <- ECHO_Facilities 
# wsp2020 <- wsp2020[-which(is.na(wsp2020$Poly_Code)),]

# print(paste("Number of MPs After Spatial Containment", length(wsp2020[,1])))

#split & apply

#group by facility_hydroid
#filter by fips_code - could change this to join on GWMA_fips table
###not filtering out incorrect lat/lon because each facility has a fips code --- (Latitude between 34 and 40 OR Longitude between -74 and -84) 
###filtering out ftype = %power
#when sending to Aquaveo, DO NOT filter out %power 
### sum the mp_share column to get the GW allocation for each of the 3 variables

wsp2020_facility <- sqldf("SELECT 
                          Facility_hydroid,
                          Latitude,
                          Longitude,
                          fips_code,
                          fac_value,
                          ftype,
                          sum(facility_use_fraction * fac_value) as GW_share
                     FROM wsp2020
                     WHERE 
                     fips_code in (51001, 51033, 51036, 51550, 51041, 51057, 51059, 51620, 51073, 51650, 51085, 51087, 51670, 51093, 51095, 51097, 51099, 51101, 51103, 51115, 51119, 51127, 51700, 51131, 51133, 51735, 51740, 51149, 51153, 51159, 51175, 51177, 51179, 51800, 51181, 51183, 51810, 51193, 51830, 51199)
                     AND
                     ftype NOT LIKE '%power'
                     GROUP BY Facility_hydroid
                     ORDER BY GW_share DESC")

wsp2040_facility <- sqldf("SELECT 
                          Facility_hydroid,
                          Latitude,
                          Longitude,
                          fac_value,
                          ftype,
                          sum(facility_use_fraction * fac_value) as GW_share
                     FROM wsp2040
                     WHERE 
                     fips_code in (51001, 51033, 51036, 51550, 51041, 51057, 51059, 51620, 51073, 51650, 51085, 51087, 51670, 51093, 51095, 51097, 51099, 51101, 51103, 51115, 51119, 51127, 51700, 51131, 51133, 51735, 51740, 51149, 51153, 51159, 51175, 51177, 51179, 51800, 51181, 51183, 51810, 51193, 51830, 51199)
                     AND ftype NOT LIKE '%power'
                     GROUP BY Facility_hydroid
                     ORDER BY GW_share DESC")

wdcurrent_facility <- sqldf("SELECT 
                          Facility_hydroid,
                          Latitude,
                          Longitude,
                          fac_value,
                          ftype,
                          sum(facility_use_fraction * fac_value) as GW_share
                     FROM wdcurrent
                     WHERE 
                     fips_code in (51001, 51033, 51036, 51550, 51041, 51057, 51059, 51620, 51073, 51650, 51085, 51087, 51670, 51093, 51095, 51097, 51099, 51101, 51103, 51115, 51119, 51127, 51700, 51131, 51133, 51735, 51740, 51149, 51153, 51159, 51175, 51177, 51179, 51800, 51181, 51183, 51810, 51193, 51830, 51199)
                     AND ftype NOT LIKE '%power'
                     GROUP BY Facility_hydroid
                     ORDER BY GW_share DESC")
#sum GW_share
wsp2020_total <- sqldf("SELECT  sum(GW_share)/365
                       FROM wsp2020_facility")
wsp2040_total <- sqldf("SELECT  sum(GW_share)/365
                       FROM wsp2040_facility")
wdcurrent_total <- sqldf("SELECT  sum(GW_share)/365
                       FROM wdcurrent_facility")
#colSums(wsp2020_facility[11])

#combine
#join on facility_hydroid to compare the 3 values (2020, 2040, current)
facility_join <- sqldf("SELECT a.Facility_hydroid, 
                       b.GW_share as wsp_2020_mgy, 
                       c.GW_share as wsp_2040_mgy, 
                       a.GW_share as wd_current_mgy
                       FROM wdcurrent_facility as a
                       LEFT JOIN wsp2020_facility as b
                       ON a.Facility_hydroid = b.Facility_hydroid
                       LEFT JOIN wsp2040_facility as c
                       ON a.Facility_hydroid = c.Facility_hydroid
                       ORDER BY wd_current_mgy DESC")

#includes both linked and unlinked facilities
all_facility_total <- sqldf("SELECT  sum(wsp_2020_mgy) as wsp_2020_mgy, 
                        sum(wsp_2020_mgy)/365 as wsp_2020_mgd,
                        sum(wsp_2040_mgy) as wsp_2040_mgy, 
                        sum(wsp_2040_mgy)/365 as wsp_2040_mgd, 
                        sum(wd_current_mgy) as wd_current_mgy, 
                        sum(wd_current_mgy)/365 as wd_current_mgd 
                        FROM facility_join")
#only linked facilities (facility with a system linked to it which gives it a wsp2020/2040 value)
linked_facility_total <- sqldf("SELECT  sum(wsp_2020_mgy) as wsp_2020_mgy, 
                        sum(wsp_2020_mgy)/365 as wsp_2020_mgd,
                        sum(wsp_2040_mgy) as wsp_2040_mgy, 
                        sum(wsp_2040_mgy)/365 as wsp_2040_mgd, 
                        sum(wd_current_mgy) as wd_current_mgy, 
                        sum(wd_current_mgy)/365 as wd_current_mgd 
                        FROM facility_join
                        WHERE wsp_2020_mgy IS NOT NULL 
                          ")
#only unlinked facilities (both permitted (GWP) and unpermitted (VWUDS), hence why there isn't a wsp sum total)
unlinked_facility_total <- sqldf("SELECT  sum(wsp_2020_mgy) as wsp_2020_mgy, 
                        sum(wsp_2020_mgy)/365 as wsp_2020_mgd,
                        sum(wsp_2040_mgy) as wsp_2040_mgy, 
                        sum(wsp_2040_mgy)/365 as wsp_2040_mgd, 
                        sum(wd_current_mgy) as wd_current_mgy, 
                        sum(wd_current_mgy)/365 as wd_current_mgd 
                        FROM facility_join
                        WHERE wsp_2020_mgy IS NULL 
                          ")

#returns a list of hydroids for facility that does not have a WSP 2020 or 2040 value (but does have a wd_current value) 
#
no_wsp_value <- sqldf("select Facility_hydroid, wd_current_mgy
                      FROM facility_join
                      WHERE NOT(wsp_2020_mgy > 0)
                      AND wd_current_mgy > 0")

#joins in has_permit list from sql_base_snapshot R query
data_base_facility <- sqldf("SELECT Facility_hydroid, GWP_permit, VWP_permit
                            FROM data_base
                            GROUP BY Facility_hydroid")

#returns a list of facility hydroids that don't have a wsp2020/2040 value which means they do not have a wsp system linked to them - use this to see which permitted facility needs a system linked to it
no_wsp_value_with_permit <- sqldf("Select a.Facility_hydroid, a.wd_current_mgy, b.GWP_permit
                                  FROM no_wsp_value as a
                                  LEFT OUTER JOIN data_base_facility as b
                                  ON a.Facility_hydroid = b.Facility_hydroid
                                  ORDER BY wd_current_mgy DESC")

write.csv(no_wsp_value_with_permit, file = paste0(export_path, paste0(export_date,"_no_wsp_value.csv")), row.names=FALSE)
#returns a count of GWP facilities that do NOT have a WSP value (indicating a facility without a system link)
count_GWP_without_wsp_value <- sqldf("Select count(Facility_hydroid) as 'GWPs without WSP Value'
                      from no_wsp_value_with_permit
                      where GWP_permit IS NOT NULL")

#returns a sum of GWP facilities that do NOT have a WSP value (indicating a facility without a system link)
top_20_mgd <- sqldf("select sum(wd_current_mgy)/365 as top_20_MGD
                    from 
                    (select *
                    from no_wsp_value_with_permit
                    where GWP_permit is not null
                    ORDER BY wd_current_mgy DESC
                    limit 20) as top_table")

top_10_mgd <- sqldf("select sum(wd_current_mgy)/365 as top_10_MGD
                    from 
                    (select *
                    from no_wsp_value_with_permit
                    where GWP_permit is not null
                    ORDER BY wd_current_mgy DESC
                    limit 10) as top_table")

all_mgd <- sqldf("select sum(wd_current_mgy)/365 as all_MGD
                    from 
                    (select *
                    from no_wsp_value_with_permit
                    where GWP_permit is not null
                    ORDER BY wd_current_mgy DESC) as top_table")

unlinked_mgd <- c(all_mgd,top_20_mgd,top_10_mgd)
write.csv(unlinked_mgd, file = paste0(export_path, paste0(export_date,"_unlinked_mgd.csv")), row.names=FALSE)


################# Virtual Facilities Data Summary ####################################
#virtual facilities represent county-wide estimates - each county has a wsp_category facility and 2 MPs (SW & GW) (except SSU has only GW MP) 
localpath <- tempdir()
filename <- paste("data_vf2020.csv",sep="")
destfile <- paste(localpath,filename,sep="\\")
download.file(paste("https://deq1.bse.vt.edu/d.dh/facility_mp_frac_value_export?bundle%5B0%5D=well&hydroid=&propcode_op=%3D&propcode=&fstatus_op=in&fstatus=All&propname_op=%3D&propname=wsp2020_2020_mgy&hydroid_1_op=%3D&hydroid_1%5Bvalue%5D=&hydroid_1%5Bmin%5D=&hydroid_1%5Bmax%5D=&dh_link_admin_fa_usafips_target_id_op=in&ftype_op=contains&ftype=wsp",sep=""), destfile = destfile, method = "libcurl")
vf_wsp2020_load <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")
vf_wsp2020 <- vf_wsp2020_load

vf_wsp2020_type <- sqldf("SELECT
                          ftype,
                          sum(facility_use_fraction * fac_value) as GW_wsp2020_MGY, (sum(facility_use_fraction * fac_value))/365 as GW_wsp2020_MGD
                     FROM vf_wsp2020
                     GROUP BY ftype
                     ORDER BY GW_wsp2020_MGY DESC")

localpath <- tempdir()
filename <- paste("data_vf2040.csv",sep="")
destfile <- paste(localpath,filename,sep="\\")
download.file(paste("https://deq1.bse.vt.edu/d.dh/facility_mp_frac_value_export?bundle%5B0%5D=well&hydroid=&propcode_op=%3D&propcode=&fstatus_op=in&fstatus=&propname_op=%3D&propname=wsp2020_2040_mgy&hydroid_1_op=%3D&hydroid_1%5Bvalue%5D=&hydroid_1%5Bmin%5D=&hydroid_1%5Bmax%5D=&dh_link_admin_fa_usafips_target_id_op=in&ftype_op=contains&ftype=wsp_",sep=""), destfile = destfile, method = "libcurl")
vf_wsp2040_load <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")
vf_wsp2040 <- vf_wsp2040_load

vf_wsp2040_type <- sqldf("SELECT
                          ftype,
                          sum(facility_use_fraction * fac_value) as GW_wsp2040_MGY, (sum(facility_use_fraction * fac_value))/365 as GW_wsp2040_MGD
                     FROM vf_wsp2040
                     GROUP BY ftype
                     ORDER BY GW_wsp2040_MGY DESC")

###############################################################################################

all_facility_total
linked_facility_total
no_wsp_value_with_permit
count_GWP_without_wsp_value
unlinked_mgd
vf_wsp2020_type
vf_wsp2040_type


#pull facility hydroid where difference is greater than 20%
on_the_fly <- sqldf("SELECT *
                    FROM facility_join
                    WHERE (wsp_2020_mgy/wd_current_mgy) >1.2
                    order by wsp_2020_mgy")
#pull facility hydroid where difference is greater than 100 MGY
on_the_fly2 <- sqldf("SELECT *
                    FROM facility_join
                    WHERE abs(wsp_2020_mgy-wd_current_mgy) >100
                    order by wsp_2020_mgy")

#################################################################################################
#SEND TO AQUAVEO
wsp2020_MP_share <- sqldf("SELECT 
                          MP_hydroid,
                          Facility_hydroid,
                          Latitude,
                          Longitude,
                          fips_code,
                          ftype,
                          (facility_use_fraction * fac_value) as wsp_2020_GW
                     FROM wsp2020
                     WHERE 
                     fips_code in (51001, 51033, 51036, 51550, 51041, 51057, 51059, 51620, 51073, 51650, 51085, 51087, 51670, 51093, 51095, 51097, 51099, 51101, 51103, 51115, 51119, 51127, 51700, 51131, 51133, 51735, 51740, 51149, 51153, 51159, 51175, 51177, 51179, 51800, 51181, 51183, 51810, 51193, 51830, 51199)
                     ORDER BY wsp_2020_GW DESC")

wsp2020_permitted_MP <- sqldf("SELECT a.*, b.GWP_permit
                                    FROM wsp2020_MP_share as a
                                    left outer join data_base_facility as b
                                    ON a.Facility_hydroid = b.Facility_hydroid
                                    where b.GWP_permit = 'GWP'")

write.csv(wsp2020_permitted_MP,file = paste0(export_path,"wsp_current_demand_permitted_well_",export_date, ".csv"), row.names=FALSE)

sqldf(" select sum(wsp_2020_GW)/365
      from wsp2020_MP_share")
wsp2020_county_wide_estimate<- sqldf("SELECT
                          ftype,
                          (facility_use_fraction * fac_value) as GW_wsp2020_MGY, (facility_use_fraction * fac_value)/365 as GW_wsp2020_MGD
                     FROM vf_wsp2020
                     ORDER BY GW_wsp2020_MGY DESC")

write.csv(wsp2020_county_wide_estimate,file = paste0(export_path,"_wsp_current_demand_county_wide_estimate",export_date, ".csv"), row.names=FALSE)

