library(devtools)
install_github("HARPgroup/hydro-tools")
#devtools::install_github("HARPgroup/hydro-tools")

library(hydrotools)

#EXAMPLE FUNCTION DOCUMENTATION
??om_vahydro_metric_grid
#??om_vahydro_get_token

# ??rom_getObject
# 
# #EXAMPLE
# facility_obj <- rom_getObject('dh_feature', 154)  
# # show a graph of the selected varkeys, builds a ggplot that we can customize to our hearts content 
# facility_obj->plot(c('wd_mgy', 'ps_mgy'), '1985-01-01', '2020-12-31')
#
library("httr")
library("sqldf")
library("stringr")

# Load Libraries
basepath='/var/www/R';
site <- "http://deq2.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
#source("/var/www/R/config.local.private"); 
source(paste(basepath,'config.R',sep='/'))
# source(paste(hydro_tools_location,'/R/om_vahydro_metric_grid.R', sep = ''));
folder <- "C:/Workspace/tmp/"


df <- data.frame(
  'model_version' = c('vahydro-1.0',  'vahydro-1.0',  'vahydro-1.0'),
  'runid' = c('runid_11', '0.%20River%20Channel', 'local_channel'),
  'runlabel' = c('QBaseline_2020', 'comp_da', 'subcomp_da'),
  'metric' = c('Qbaseline', 'drainage_area', 'drainage_area')
)

# df <- data.frame(
#   'model_version' = c('vahydro-1.0'),
#   'runid' = c('runid_13'),
#   'metric' = c('consumptive_use_frac'),
#   'runlabel' = c('CU')
# )

da_data <- om_vahydro_metric_grid(metric = 'wd_mgy',df)


da_data <- sqldf(
  "select pid, comp_da, subcomp_da,
   CASE
    WHEN comp_da is null then subcomp_da
    ELSE comp_da
    END as da
   from da_data
  ")