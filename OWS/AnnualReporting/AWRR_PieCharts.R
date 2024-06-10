library(sqldf)
library(dplyr)
#site <- "http://deq2.bse.vt.edu/d.dh/"
site <- "https://deq1.bse.vt.edu/d.dh/"

syear <- 2019
eyear <- 2023
#remember to change legend year in figure setup section

basepath <- "/var/www/R/"

source(paste(basepath,"config.local.private",sep = '/'))
export_path <- paste0(onedrive_location,"/OWS/Report Development/Annual Water Resources Report/October ",eyear+1," Report/overleaf/") ##BB Put this line after 12 since it overwrites export_path
## Needs to read in table1 without power

Table1 <- read.csv(paste0(onedrive_location,"/OWS/foundation_datasets/awrr/",eyear+1,"/Table1_",eyear-4,"-",eyear,".csv"))

## This line removes power from Table1 (with power)
Table1 <- Table1[-c(7:8,15:16,23:24,28:31),]  

# AWRR Pie Charts ################################################################

#Change for easier string substitution and clear pie chart 
Table1$Source.Type <- recode(Table1$Source.Type, "Total (GW + SW)" = "Total") 

#labeling 
source_type <- c("Groundwater", "Surface Water", "Total")

for (s in 1:length(source_type)) {
  
  wd.df <- sqldf(paste0('
    SELECT *,
    CASE
      WHEN "Category" = "Agriculture" THEN "dodgerblue4"
      WHEN "Category" = "Commercial" THEN "firebrick4"
      WHEN "Category" = "Irrigation" THEN "darkolivegreen3"
      WHEN "Category" = "Manufacturing" THEN "blueviolet"
      WHEN "Category" = "Mining" THEN "deepskyblue3" 
      WHEN "Category" = "Public Water Supply" THEN "darkorange2" 
      ELSE "white"
    END AS col
   FROM Table1
   WHERE "Source.Type" = "',source_type[s],'"'))
  
  # FIVE YEAR AVERAGE:
  wd.df["avg.percent"] <- round((100*(wd.df$multi_yr_avg/sum(wd.df$multi_yr_avg))),digits=2)
  # MOST RECENT YEAR:
  wd.df["year.percent"] <- round((100*(wd.df[paste0("X",eyear)]/sum(wd.df[paste0("X",eyear)]))),digits=2)
  
  #############################################################################################
  # FIGURE SETUP
  cols <- wd.df$col
  legend.avg <-paste(wd.df$Category," (",wd.df$multi_yr_avg," MGD)",sep="")
  legend.year <-paste(wd.df$Category," (",wd.df[paste0('X',eyear)][,1]," MGD)",sep="")  #A bit ugly but updates automatically
  
  filename <- paste(export_path,"AWRR_",source_type[s],"_Withdrawals_Pie.png", sep="")
  png(filename=paste(filename,sep='/'),width=10,height=6,units="in",res=1000)
  par(mfrow=c(1,2),mai=c(1.4,0.5,0.4,0.4),oma = c(0,0,0,0),xpd=TRUE,cex = 1.0499)
  #--------------------------------------------------------------------
  # LEFT PIE
  if (source_type[s] == "Total") {
    left_title_text <- paste0("(a) ",syear,"-",eyear," Average Withdrawals")
  } else {
    left_title_text <- paste0("(a) ",syear,"-",eyear," Average ",source_type[s]," Withdrawals")
  }
  pie(wd.df$multi_yr_avg, labels = paste(wd.df$avg.percent,"%",sep=""), 
      main= left_title_text,
      cex = 1.1,cex.main = 1.1,col=cols
  )
  legend('bottom',legend.avg,cex=1.0, fill=cols, inset=-0.3) 
  #--------------------------------------------------------------------
  # RIGHT PIE
  if (source_type[s] == "Total") {
    right_title_text <- paste0("(b) ",eyear," Total Withdrawals")
  } else {
    right_title_text <- paste0("(b) ",eyear," Total ",source_type[s]," Withdrawals")
  }
  ## Need a paste statement to select the year column dynamically
  pie(wd.df[,paste0('X',eyear)], labels = paste(wd.df$year.percent,"%",sep=""), 
      main= right_title_text,
      cex = 1.1,cex.main = 1.1,col=cols
  )
  legend('bottom',legend.year,cex=1.0, fill=cols, inset=-0.3)
  #--------------------------------------------------------------------
  dev.off()
  print(paste0(source_type[s]," Pie Chart: COMPLETE"))
}
