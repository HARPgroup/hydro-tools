#####Created by JDK 8.11.16
#####Updated 6.29.18

rm(list = ls())  #clear variables
library(waterData)
library(dataRetrieval)

#Specify Area of Interest			#Example Inputs
bundle <- 'watershed'				#watershed, hwi_region, ecoregion
ws_ftype <- 'nhd_huc6'				#nhd_huc6, nhd_huc2, hwi_region, ecoregion_iii
ws_hydrocode <- '020802'			#020802, 2, atl_coastal_plain_usgs

save_location <- "C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\plots"

gage_url <- paste("http://deq2.bse.vt.edu/d.bet/export-usgs-gages/",bundle,"/",ws_ftype,"/",ws_hydrocode,sep="")
gage_list <- read.csv(gage_url, header = TRUE, sep = ",")
watershed_name <- gage_list$Watershed.Name[1]
gages <- gage_list$USGS.Gage.ID
gages <- paste0("0",gages)
USGS_GAGE_ID <- gages
num_gages <- length(USGS_GAGE_ID)


month = c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec')
dataframe <- data.frame(month)

for (i in 1:length(USGS_GAGE_ID)) {
  
  site <- readNWISsite(USGS_GAGE_ID[i])
  da_sqmi <- site$drain_area_va
  da_sqkm <- da_sqmi*2.58999
  
  x <- readNWISstat(siteNumbers=c(USGS_GAGE_ID[i]),
                    parameterCd=c("00060"), #code for discharge in cubic feet per second
                    statReportType="monthly")
  
  jan_rows <- which(x$month_nu == 1)
  jan_data <- x[jan_rows,]
  jan_flows <- jan_data$mean_va
  jan_mean <- mean(jan_flows)
  
  feb_rows <- which(x$month_nu == 2)
  feb_data <- x[feb_rows,]
  feb_flows <- feb_data$mean_va
  feb_mean <- mean(feb_flows)
  
  mar_rows <- which(x$month_nu == 3)
  mar_data <- x[mar_rows,]
  mar_flows <- mar_data$mean_va
  mar_mean <- mean(mar_flows)
  
  apr_rows <- which(x$month_nu == 4)
  apr_data <- x[apr_rows,]
  apr_flows <- apr_data$mean_va
  apr_mean <- mean(apr_flows)
  
  may_rows <- which(x$month_nu == 5)
  may_data <- x[may_rows,]
  may_flows <- may_data$mean_va
  may_mean <- mean(may_flows)
  
  jun_rows <- which(x$month_nu == 6)
  jun_data <- x[jun_rows,]
  jun_flows <- jun_data$mean_va
  jun_mean <- mean(jun_flows)
  
  jul_rows <- which(x$month_nu == 7)
  jul_data <- x[jul_rows,]
  jul_flows <- jul_data$mean_va
  jul_mean <- mean(jul_flows)
  
  aug_rows <- which(x$month_nu == 8)
  aug_data <- x[aug_rows,]
  aug_flows <- aug_data$mean_va
  aug_mean <- mean(aug_flows)
  
  sep_rows <- which(x$month_nu == 9)
  sep_data <- x[sep_rows,]
  sep_flows <- sep_data$mean_va
  sep_mean <- mean(sep_flows)
  
  oct_rows <- which(x$month_nu == 10)
  oct_data <- x[oct_rows,]
  oct_flows <- oct_data$mean_va
  oct_mean <- mean(oct_flows)
  
  nov_rows <- which(x$month_nu == 11)
  nov_data <- x[nov_rows,]
  nov_flows <- nov_data$mean_va
  nov_mean <- mean(nov_flows)
  
  dec_rows <- which(x$month_nu == 12)
  dec_data <- x[dec_rows,]
  dec_flows <- dec_data$mean_va
  dec_mean <- mean(dec_flows)
  
  
  monthly_means = matrix(c(jan_mean,
                           feb_mean,
                           mar_mean,
                           apr_mean,
                           may_mean,
                           jun_mean,
                           jul_mean,
                           aug_mean,
                           sep_mean,
                           oct_mean,
                           nov_mean,
                           dec_mean)/da_sqkm) 
  monthly_means <- round(monthly_means, digits=5)
  
  dataframe[,USGS_GAGE_ID[i]] <- monthly_means
} #closes gage for loop

dataframe$month <- NULL

jan <- dataframe[1,]
jan <- as.vector(as.matrix(jan))

feb <- dataframe[2,]
feb <- as.vector(as.matrix(feb))

mar <- dataframe[3,]
mar <- as.vector(as.matrix(mar))

apr <- dataframe[4,]
apr <- as.vector(as.matrix(apr))

may <- dataframe[5,]
may <- as.vector(as.matrix(may))

jun <- dataframe[6,]
jun <- as.vector(as.matrix(jun))

jul <- dataframe[7,]
jul <- as.vector(as.matrix(jul))

aug <- dataframe[8,]
aug <- as.vector(as.matrix(aug))

sep <- dataframe[9,]
sep <- as.vector(as.matrix(sep))

oct <- dataframe[10,]
oct <- as.vector(as.matrix(oct))

nov <- dataframe[11,]
nov <- as.vector(as.matrix(nov))

dec <- dataframe[12,]
dec <- as.vector(as.matrix(dec))


REGION <- paste(watershed_name," (", ws_hydrocode,")",sep="")
plot_title <- paste("All ",num_gages," USGS Gages Within ", REGION, "\nHistoric Monthly Mean Flow per Unit Drainage Area",sep="")

png(paste(save_location,'\\',REGION,'_histoic_boxplots.png',sep=""),width = 1000, height = 600)

boxplot(jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec, 
        names=c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"),
        xlab="Month", 
        ylab="Mean Flow per Unit Area (cfs/sqkm)",
        main= plot_title )

dev.off()
#