basepath <- 'C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\r-dh-ecohydro'
save_directory <-  'C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\plots\\' #Location to output images

#intakes <- read.csv(file='http://deq2.bse.vt.edu/files/hwi/intake_nhd_attributes.txt', header=TRUE, sep="\t")
intakes <- read.csv(file='http://deq2.bse.vt.edu/files/hwi/intake_nhd_attributes-named.txt', header=TRUE, sep="\t")
####################################################################################################
# RENAME DATAFRAME COLUMNS
colnames(intakes)[colnames(intakes)=="hydroid"] <- "mp_hydroid"
colnames(intakes)[colnames(intakes)=="dh_feature_name"] <- "mp_name"
colnames(intakes)[colnames(intakes)=="wd_ts_tstime"] <- "tstime"
colnames(intakes)[colnames(intakes)=="wd_ts_tsvalue"] <- "tsvalue"
colnames(intakes)[colnames(intakes)=="wshed_name"] <- "nhdplus_name"
colnames(intakes)[colnames(intakes)=="wshed_maf"] <- "nhdplus_MAF"
colnames(intakes)[colnames(intakes)=="wshed_da"] <- "nhdplus_DA"
colnames(intakes)[colnames(intakes)=="facility_hydroid"] <- "facility_hydroid"
colnames(intakes)[colnames(intakes)=="facility_name"] <- "facility_name"
colnames(intakes)[colnames(intakes)=="facility_ftype"] <- "facility_ftype"
####################################################################################################

print(paste("Sum of all withdrawals in mgd: ",sum(intakes$tsvalue)/365,sep=""))



#ADD PERCENT WITHDRAWAL COLUMN (convert to cfs)
intakes$pct_wd <- ((intakes$tsvalue*1.547)/365)/(intakes$nhdplus_MAF)

#nhdplus_MAF or nhdplus_DA
df.0_10 <- intakes[which((as.numeric(as.character(intakes$nhdplus_MAF)) >= 0.0) & (as.numeric(as.character(intakes$nhdplus_MAF)) <= 10.0)),]
df.10_50 <- intakes[which((as.numeric(as.character(intakes$nhdplus_MAF)) > 10.0) & (as.numeric(as.character(intakes$nhdplus_MAF)) <= 50.0)),]
df.50_100 <- intakes[which((as.numeric(as.character(intakes$nhdplus_MAF)) > 50.0) & (as.numeric(as.character(intakes$nhdplus_MAF)) <= 100.0)),]
df.100_500 <- intakes[which((as.numeric(as.character(intakes$nhdplus_MAF)) > 100.0) & (as.numeric(as.character(intakes$nhdplus_MAF)) <= 500.0)),]
df.500_plus <- intakes[which((as.numeric(as.character(intakes$nhdplus_MAF)) > 500.0)),]

intake_summary <- data.frame("df.0_10"=nrow(df.0_10),
                             "df.10_50"=nrow(df.10_50),
                             "df.50_100"=nrow(df.50_100),
                             "df.100_500"=nrow(df.100_500),
                             "df.500_plus"=nrow(df.500_plus))

count <- c(nrow(df.0_10),nrow(df.10_50),nrow(df.50_100),nrow(df.100_500),nrow(df.500_plus))
range <- c("df.0_10","df.10_50","df.50_100","df.100_500","df.500_plus")

sum.mgd <- c(sum(df.0_10$tsvalue),
              sum(df.10_50$tsvalue),
              sum(df.50_100$tsvalue),
              sum(df.100_500$tsvalue),
              sum(df.500_plus$tsvalue))/365

median.mgd <- c(median(df.0_10$tsvalue),
             median(df.10_50$tsvalue),
             median(df.50_100$tsvalue),
             median(df.100_500$tsvalue),
             median(df.500_plus$tsvalue))/365

mean.mgd <- c(mean(df.0_10$tsvalue),
                median(df.10_50$tsvalue),
                mean(df.50_100$tsvalue),
                mean(df.100_500$tsvalue),
                mean(df.500_plus$tsvalue))/365

#--------------------------------------------------
mean.pct <- c(mean(df.0_10$pct_wd),
              mean(df.10_50$pct_wd),
              mean(df.50_100$pct_wd),
              mean(df.100_500$pct_wd),
              mean(df.500_plus$pct_wd))
mean.pct <- mean.pct*100


#--------------------------------------------------
plt.var <- "sum.mgd"

if (plt.var == "sum.mgd") {bar.var <- sum.mgd}
if (plt.var == "median.mgd") {bar.var <- median.mgd}
if (plt.var == "mean.mgd") {bar.var <- mean.mgd}
if (plt.var == "mean.pct") {bar.var <- mean.pct}
  
intake_summary <- data.frame(range,count,bar.var)
#--------------------------------------------------

library(ggplot2)
p <- ggplot(data=intake_summary, aes(x=range, y=count)) +
      geom_bar(stat="identity")+
      scale_x_discrete(labels = c("0-10", "10-50", "50-100", "100-500", "500+"))+
  
      ylim(0, 500)+
  
      ggtitle(paste("Summary of Surface Water Intakes by Stream Size","\n",sep=""))+
      #xlab(paste("\n","Drainage Area (mi^2)",sep=""))+
      xlab(paste("\n","Mean Annual Flow (cfs)",sep=""))+
      ylab(paste("Number of Intakes","\n",sep=""))



if (plt.var == "mean.pct") {p <- p + geom_text(label = paste(round(bar.var,2),"% Withdrawal",sep=""), vjust = -0.5)}
if (plt.var == "sum.mgd") {pp <- p + geom_text(label = paste(round(bar.var,2)," mgd (sum)",sep=""), vjust = -0.5)}
if (plt.var == "median.mgd") {p <- p + geom_text(label = paste(round(bar.var,2)," mgd (median)",sep=""), vjust = -0.5)}
if (plt.var == "mean.mgd") {p <- p + geom_text(label = paste(round(bar.var,2)," mgd (mean)",sep=""), vjust = -0.5)}

filename <- paste("intake_summary",plt.var,".png", sep="_")
ggsave(file=filename, path = save_directory, width=8, height=5)



#################################################################################
df.0_10_QA <- df.0_10
df.0_10_QA$tsvalue <- (df.0_10_QA$tsvalue/365)*1.547

median(df.0_10$pct_wd)
mean(df.0_10$pct_wd)

library(ggplot2)
p <- ggplot(data=df.0_10_QA, aes(x=nhdplus_MAF, y=tsvalue)) +
  geom_point(aes(x=nhdplus_MAF, y=tsvalue))+
  geom_abline(intercept = 0, slope = .10)

filename <- paste("intake_summary_wd_vs_maf.png", sep="_")
ggsave(file=filename, path = save_directory, width=8, height=5)

#################################################################################

intakes_QA <- intakes
intakes_QA$one <- 'one'

#p <- ggplot(intakes_QA, aes(x=one, y=pct_wd)) + 
#  geom_boxplot()+
#  ylim(0,0.0000175)

boxplot(pct_wd~one,data=intakes_QA, main="", 
        xlab="", ylab="", ylim=c(0,0.08))


quantile(intakes_QA$pct_wd,.9)
quantile(intakes_QA$pct_wd,0.5)
quantile(intakes_QA$pct_wd,0.1)

quantile(intakes_QA$pct_wd,  probs = c(10, 25, 50, 75, 90, 100)/100)



