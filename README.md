# hydrotools

# Overview
 
The hydro-tools repository is currently undergoing maintenance in order to be converted into the 'hydrotools' R package. 

# Installation

``` r
install.packages("devtools")
library("devtools")
install_github("HARPgroup/hydro-tools")
# alternate, install a development branch for testing:
install_github("HARPgroup/hydro-tools", ref = "odbc")

# hydrotools uses IHA for some metrics.
# Install IHA as:
devtools::install_github("jasonelaw/iha")
#HAVE ALSO HAD LUCK INSTALLING WITH THE FOLLOWING
install.packages("IHA", repos="http://R-Forge.R-project.org")

#EXAMPLE FUNCTION DOCUMENTATION
??om_vahydro_metric_grid
```

# Troubleshooting Installation
```
# Early install tests have failed when installing R6, 
# which is due o a "Error converted from Warning" (see: https://github.com/r-lib/remotes/issues/403)
# Was with R6 version 2.5, so we ran:
install_version("R6", version = "2.4.1")
# todo: sort that out.

```

# Example
## This is for testing purposes at this point.

``` r
library(hydrotools)
library(httr)
library(sqldf)
library(stringr)

basepath='/var/www/R';
site <- "http://deq2.bse.vt.edu/d.dh"
source(paste(basepath,'config.R',sep='/'))
folder <- "C:/Workspace/tmp/"

df <- data.frame(
  'model_version' = c('vahydro-1.0',  'vahydro-1.0',  'vahydro-1.0'),
  'runid' = c('runid_11', '0.%20River%20Channel', 'local_channel'),
  'runlabel' = c('QBaseline_2020', 'comp_da', 'subcomp_da'),
  'metric' = c('Qbaseline', 'drainage_area', 'drainage_area')
)

da_data <- om_vahydro_metric_grid(metric = 'wd_mgy',df)

da_data <- sqldf(
  "select pid, comp_da, subcomp_da,
   CASE
    WHEN comp_da is null then subcomp_da
    ELSE comp_da
    END as da
   from da_data
  ")
```
