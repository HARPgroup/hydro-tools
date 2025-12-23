basepath="/var/www/R"
source("/var/www/R/config.R")
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(sqldf))

nldas_root=Sys.getenv(c('NLDAS_ROOT'))[1]
nldas_url_base <- paste0(ext_url_base,'/met/out/lseg_csv')
source(paste0(nldas_root,"/R/nldas_feature_dataset_prop.R"))


RomMetData <- R6Class(
  "RomMetData",
  public = list(
    #' @field base for uri_info
    base = "",
    #' @field prc_uri uri for precip dataset
    prc_uri = "",
    #' @field tmp_uri TEMPERATURE file URI
    tmp_uri = "",
    #' @field pet_uri POTENTIAL EVAPOTRANSPIRATION file URI
    pet_uri = "",
    #' @field dsns list of data sources
    dsns = c('PRC', 'TMP', 'PET', 'WND', 'RAD'),
    #' @field prc precipitation time series
    prc = FALSE,
    #' @field tmp temperature time series
    tmp = FALSE,
    #' @field pet potential ET time series
    pet = FALSE,
    #' @field temp_uri TEMPERATURE file URI
    #' @param dataname met data con set name 
    #' @param hcode alpha identifier of coverage
    #' @param site Base URI for the web site
    #' @return object instance
    initialize = function(ds, dataname, hcode, site, base='auto') {
      if (base == 'auto') {
        self$base <- paste0(site, "/met/", dataname, "/lseg_csv/", hcode)
      } else {
        self$base <- base
      }
      self$prc_uri <- paste0(self$base, ".PRC")
      self$tmp_uri <- paste0(self$base, ".TMP")
    },
    #' @param uri file loc - this allows more flexi connections
    #' @return csv
    read_csv = function(uri) {
      rawdata <- RCurl::getURL(uri, ssl.verifypeer = FALSE)
      parsed_data <- read.csv(textConnection(rawdata), col.names = c('yr', 'mo', 'da', 'hr', 'value'))
      return(parsed_data)
    },
    #' @param dsn which dataset? Default is all
    #' @return NULL
    get_data = function(dsn='all') {
      if (dsn != 'all') {
        dsns = c(dsn)
      } else (
        dsns = self$dsns
      )
      for (i in dsns) {
        if (i == "PRC") {
          self$prc <- self$read_csv(self$prc_uri)
        } else if (i == "TMP") {
          self$tmp <- self$read_csv(self$tmp_uri)
        } else if (i == "PET") {
          self$pet <- self$read_csv(self$pet_uri)
        }
      }
    }
  )
)

ds1 <- "nldas2rst"
ds2 <- "prismrst"
ds3 <- "prismret"
hcode <- "N24001"
met1 <- RomMetData$new(ds, ds1, hcode, site=omsite)
met1$get_data('PRC')

# Example: Use static URI instead of letting system guess from scenario name
# met2 <- RomMetData$new(ds, ds2, hcode, site=omsite, base=paste0("https://deq1.bse.vt.edu:81","/met/nldas2root/meteorology/out/lseg_csv/nldas2rst/",hcode))
# met2$get_data('PRC')
met2v <- RomMetData$new(ds, ds2, "N51001", site=omsite)
met2v$get_data('PRC')


met2 <- RomMetData$new(ds, ds2, hcode, site=omsite)
met2$get_data('PRC')
met3 <- RomMetData$new(ds, ds3, hcode, site=omsite)
met3$get_data('PRC')
met4 <- RomMetData$new(ds, ds3, hcode, site=omsite)
met4$prc_uri <- paste0("https://deq1.bse.vt.edu:81","/p6/vadeq/input/scenario/climate/prad/prismrst/prad_",hcode,"_2000.csv")
met4$get_data('PRC')

met1prc <- met1$prc
met2prc <- met2$prc
met3prc <- met3$prc
met2vprc <- met2v$prc
ann2vprc <- sqldf("select yr, sum(value) from met2vprc group by yr order by yr")
quantile(ann2vprc$`sum(value)`)

comp_yrs <- sqldf(
  "select a.yr, sum(a.value) as pa, sum(b.value) as pb, sum(c.value) as pc, sum(d.value) as pd
   from met1prc as a
   left outer join met2prc as b 
   on (a.yr = b.yr AND a.mo = b.mo AND a.da = b.da AND a.hr = b.hr)
   left outer join met2prc as c 
   on (a.yr = c.yr AND a.mo = c.mo AND a.da = c.da AND a.hr = c.hr)
   left outer join met2prc as d 
   on (a.yr = d.yr AND a.mo = d.mo AND a.da = d.da AND a.hr = d.hr)
   group by a.yr
   order by a.yr
  "
)
