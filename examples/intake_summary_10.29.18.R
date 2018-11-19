basepath <- 'C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\r-dh-ecohydro'
save_directory <-  'C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\plots\\' #Location to output images


######intakes <- read.csv(file=paste(basepath,"ELFGEN\\internal\\surface_water_intake_summary.csv",sep="\\"), header=TRUE, sep=",")
#intakes <- read.table(file=paste(basepath,"ELFGEN\\internal\\surface_water_intake_summary.txt",sep="\\"), header=TRUE, sep="\t")
#intakes <- read.table('http://deq2.bse.vt.edu/files/hwi/intake_nhd_attributes.txt', header=TRUE, sep="\t")
#intakes <- read.csv(file=paste(basepath,"ELFGEN\\internal\\intake_nhd_attributes.txt",sep="\\"), header=TRUE, sep="\t")
intakes <- read.csv(file='http://deq2.bse.vt.edu/files/hwi/intake_nhd_attributes.txt', header=TRUE, sep="\t")

intakes$pct_wd <- ((intakes$dh_timeseries_dh_feature_tsvalue*1.547)/365)/(intakes$dh_properties_dh_feature_propvalue)

sum(intakes$dh_timeseries_dh_feature_tsvalue)/365

######intakes <- read.csv("http://deq2.bse.vt.edu/d.alpha/surface-water-intake-summary-export-nhdplus", header=TRUE, sep=",")

#library(RCurl) 
#intakes <- getURL("http://deq2.bse.vt.edu/d.alpha/surface-water-intake-summary-export-nhdplus", timeout = 20000)


#intakes$tsvalue_numeric <- as.numeric(as.character(intakes$tsvalue))

#MAF or MAF
df.0_10 <- intakes[which((as.numeric(as.character(intakes$dh_properties_dh_feature_propvalue)) >= 0.0) & (as.numeric(as.character(intakes$dh_properties_dh_feature_propvalue)) <= 10.0)),]
df.10_50 <- intakes[which((as.numeric(as.character(intakes$dh_properties_dh_feature_propvalue)) > 10.0) & (as.numeric(as.character(intakes$dh_properties_dh_feature_propvalue)) <= 50.0)),]
df.50_100 <- intakes[which((as.numeric(as.character(intakes$dh_properties_dh_feature_propvalue)) > 50.0) & (as.numeric(as.character(intakes$dh_properties_dh_feature_propvalue)) <= 100.0)),]
df.100_500 <- intakes[which((as.numeric(as.character(intakes$dh_properties_dh_feature_propvalue)) > 100.0) & (as.numeric(as.character(intakes$dh_properties_dh_feature_propvalue)) <= 500.0)),]
df.500_plus <- intakes[which((as.numeric(as.character(intakes$dh_properties_dh_feature_propvalue)) > 500.0)),]

intake_summary <- data.frame("df.0_10"=nrow(df.0_10),
                             "df.10_50"=nrow(df.10_50),
                             "df.50_100"=nrow(df.50_100),
                             "df.100_500"=nrow(df.100_500),
                             "df.500_plus"=nrow(df.500_plus))

count <- c(nrow(df.0_10),nrow(df.10_50),nrow(df.50_100),nrow(df.100_500),nrow(df.500_plus))
range <- c("df.0_10","df.10_50","df.50_100","df.100_500","df.500_plus")
# sum <- c(sum(as.numeric(as.character(df.0_10$tsvalue))),
#         sum(as.numeric(as.character(df.10_50$tsvalue))),
#         sum(as.numeric(as.character(df.50_100$tsvalue))),
#         sum(as.numeric(as.character(df.100_500$tsvalue))),
#         sum(as.numeric(as.character(df.500_plus$tsvalue))))

#sum <- c(sum(df.0_10$dh_timeseries_dh_feature_tsvalue),
#         sum(df.10_50$dh_timeseries_dh_feature_tsvalue),
#         sum(df.50_100$dh_timeseries_dh_feature_tsvalue),
#         sum(df.100_500$dh_timeseries_dh_feature_tsvalue),
#         sum(df.500_plus$dh_timeseries_dh_feature_tsvalue))/365

sum <- c(mean(df.0_10$pct_wd),
         mean(df.10_50$pct_wd),
         mean(df.50_100$pct_wd),
         mean(df.100_500$pct_wd),
         mean(df.500_plus$pct_wd))
sum <- sum*100

# sum <- c(mean(df.0_10$dh_timeseries_dh_feature_tsvalue),
#          mean(df.10_50$dh_timeseries_dh_feature_tsvalue),
#          mean(df.50_100$dh_timeseries_dh_feature_tsvalue),
#          mean(df.100_500$dh_timeseries_dh_feature_tsvalue),
#          mean(df.500_plus$dh_timeseries_dh_feature_tsvalue))

#sum <- sum/365

#sum(intakes$tsvalue)
#sum(as.numeric(df.0_10$tsvalue))

intake_summary <- data.frame(range,count,sum)


library(ggplot2)
p <- ggplot(data=intake_summary, aes(x=range, y=count)) +
      geom_bar(stat="identity")+
      geom_text(label = paste(round(sum,1)," cfs (pct_wd)",sep=""), vjust = -0.5)+
      scale_x_discrete(labels = c("0-10", "10-50", "50-100", "100-500", "500+"))+
  
      ylim(0, 500)+
  
      ggtitle(paste("Summary of Surface Water Intakes by Stream Size","\n",sep=""))+
      xlab(paste("\n","Mean Annual Flow (cfs)",sep=""))+
      ylab(paste("Number of Intakes","\n",sep=""))



filename <- paste("intake_summary.png", sep="_")
ggsave(file=filename, path = save_directory, width=8, height=5)


#################################################################################

df.0_10$dh_timeseries_dh_feature_tsvalue <- (df.0_10$dh_timeseries_dh_feature_tsvalue/365)*1.547

median(df.0_10$pct_wd)
mean(df.0_10$pct_wd)

library(ggplot2)
p <- ggplot(data=df.0_10, aes(x=dh_properties_dh_feature_propvalue, y=dh_timeseries_dh_feature_tsvalue)) +
  geom_point(aes(x=dh_properties_dh_feature_propvalue, y=dh_timeseries_dh_feature_tsvalue))




filename <- paste("0_intake_summary.png", sep="_")
ggsave(file=filename, path = save_directory, width=8, height=5)

