#The following code is used to convert hourly data from the model to daily data and compare the daily data of the model with the daily USGS data
#The majority of the code below was provided by Joey, Hailey made adjustments starting below the gg2plot command
#The only portions of the code that need to be changed to compare the model to USGS is the gageID, model segment, gageplot range and the ylim range

library(lubridate)
library(ggplot2)
library(scales)

#update to location of config.local.private file
config_file <- "C:\\Users\\HaileyMae\\Documents\\GitHub\\hydro-tools"

#----------------------------------------------------------------------------------------
#load functions
source(paste(config_file,'config.local.private',sep='/'))
save_directory <- paste(repo_location,"plots",sep="")
dir.create(save_directory, showWarnings = FALSE) #create "plots" directory if doesn't exist 
source(paste(repo_location,"hydro-tools\\USGS\\usgs_gage_functions.R", sep = ""))
#----------------------------------------------------------------------------------------

# SPECIFY GAGE AND MODEL SEGMENT OF INTEREST
gageID <- '02077500'
model_segment <- 'OD2_8830_8710_0111'

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

#PLOT DATA using ggplot
#plt <- ggplot(data = model_hourly,aes(date, flow))+
              #geom_point(data = model_hourly, aes(date, flow))


#filename <- paste(gageID,"__",model_segment,".png", sep="")
#ggsave(file=filename, path = save_directory, width=8, height=6)


#-----------------------------------------------------------------------------------------------
#Hailey's modifications start below

#Shorten total gage data to just data that the model has access to 1984-01-01 to 2005-12-31
#This number must be altered by hand. Click on gage_data and search 1984-01 copy the row number (probably in the 1000s) into the number before the :
#Then click on gage_data again and serach 2005-12-31 and copy and paste that row number into the place behind the :
gageplot<-gage_data[8128:14721,]

#Counters/Initalizing Variables
j<-1      #Start at the first hour (0:00)
k<-24     #Stop after 1 day has completed (23:00)
h<-1      #Counter for the date
i<-1      #Counter for the loop
modelplot<-data.frame(matrix(nrow=1,ncol=1))     #Create empty vector for daily model data to go

#For loop to do daily data calculations
for (i in 1:length(gageplot$Date)){         #Run the loop for as many dates as the gage data has
  dailyflow<-sum(model_hourly[j:k,2])       #Calculate the model daily flow by summing the the 24 rows of the second column of the model_hourly data
      modelplot[i,2]<-gageplot$Date[h]      #Recreate the date column for model data (Used gageplot data for ease of formatting model_hourly could have been used but extra steps would be required to remove the hour from the date column)
      modelplot[i,3]<-dailyflow             #Assign the daily flow in ac-ft/day to one column
      modelplot[i,4]<-dailyflow*0.5041655   #Multiply the model data by 0.5041655 to go from ac-ft/day to cfs 
      j<-j+24                               #Add 24 hours to the first time value
      k<-k+24                               #Add 24 hours to the last time value
      h=h+1                                 #Add 1 day to the date
      i=i+1                                 #Move the loop forward 1 count
}
names(modelplot)<-c('Purposely Blank', 'Date', 'DailyFlow(ac-ft/day)', 'DailyFlow(cfs)')


#Plot To Compare USGS and Model Data
par(mfrow=c(1,1))                                                                                                    #Both data sets on one graph
par(cex=1.1)
plot(gageplot$Date, gageplot$Flow, type='l', col="red", xlab='Date', ylab='Flow (cfs)', main='Daily Discharge Data', lwd=1.5) #USGS gage data
lines(modelplot$Date, modelplot$`DailyFlow(cfs)`, col="blue", type='l', lwd=1.5)                                                              #Model data
legend ("topright", legend=c("USGS Gage", "Model Output"), col=c('red','blue'), lty=1, bty='n')


par(mfrow=c(1,2))                                                                                                                              #Two seperate graphs side by side ylim will need to be adjusted accordingly but to better compare should be the same for both graphs
plot(gageplot$Date, gageplot$Flow, type='l', col="red", ylim= c(0,5000), xlab='Date', ylab='Flow (cfs)', main='Daily Discharge of USGS Gage')  #USGS gage data
plot(modelplot$Date, modelplot$`DailyFlow(cfs)`, col="blue", type='l', ylim= c(0,5000), xlab='Date', ylab='Flow (cfs)', main='Daily Discharge of Model Data')  #Model data
