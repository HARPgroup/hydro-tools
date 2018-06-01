library(lubridate)
library(ggplot2)
library(scales)

#update to location of config.local.private file
config_file <- "C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\hydro-tools\\"

#----------------------------------------------------------------------------------------
#load functions
source(paste(config_file,'config.local.private',sep='/'))
save_directory <- paste(repo_location,"plots",sep="")
dir.create(save_directory, showWarnings = FALSE) #create "plots" directory if doesn't exist 
source(paste(repo_location,"hydro-tools\\USGS\\usgs_gage_functions.R", sep = ""))
#----------------------------------------------------------------------------------------

# SPECIFY GAGE AND MODEL SEGMENT OF INTEREST
gageID <- '03171000'
model_segment <- 'OD2_8840_9020_0111'

#RETRIEVE MODEL DATA
URL_model_hourly <-paste("http://deq2.bse.vt.edu/p532c-sova/wdm/river/p532cal_062211/stream/",model_segment,".csv",sep="")
model_hourly <- read.csv(URL_model_hourly, header = FALSE, sep = ",", stringsAsFactors = FALSE);
model_hourly <- model_hourly[-1,]
colnames(model_hourly) <- c("year","month","day","hour","flow")
model_hourly$date <- with(model_hourly, ymd_h(paste(year, month, day, hour, sep= ' ')))
model_hourly <- data.frame(model_hourly$date,model_hourly$flow)
colnames(model_hourly) <- c("date","flow")

#RETRIEVE GAGE DATA
gage_data <- streamgage_historic(gageID)
gage_data <- clean_historic(gage_data)

#PLOT DATA
plt <- ggplot(data = model_hourly,aes(date, flow))+
              geom_point(data = model_hourly, aes(date, flow))


filename <- paste(gageID,"__",model_segment,".png", sep="")
ggsave(file=filename, path = save_directory, width=8, height=6)

