library(sqldf)
library(dplyr)
#site <- "http://deq2.bse.vt.edu/d.dh/"
site <- "https://deq1.bse.vt.edu/d.dh/"

syear <- 2018
eyear <- 2022
#remember to change legend year in figure setup section

basepath <- "/var/www/R/"

source(paste(basepath,"config.local.private",sep = '/'))
export_path <- paste0("U:/OWS/Report Development/Annual Water Resources Report/October ",eyear+1," Report/overleaf") ##BB Put this line after 12 since it overwrites export_path
Table1 <- read.csv(paste0("U:/OWS/foundation_datasets/awrr/",eyear+1,"/Table1_",syear,"-",eyear,".csv"))

# AWRR Pie Charts ################################################################
Table1$Source.Type <- recode(Table1$Source.Type, "Total (GW + SW)" = "Total") #Change for easier string substitution and clear pie chart 
Table1$Category <- recode(Table1$Category, "Municipal" = "Public Water Supply")
#labeling 
source_type <- c("Groundwater", "Surface Water", "Total")
#s = 1
for (s in 1:length(source_type)) {

wd.sql <- paste('SELECT *,
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
                 WHERE "Source.Type" = "',source_type[s],'"',sep='')
wd.df <- sqldf(wd.sql)

# FIVE YEAR AVERAGE:
wd.df  ["avg.percent"] <- round((100*(wd.df$multi_yr_avg/sum(wd.df$multi_yr_avg))),digits=2)
# MOST RECENT YEAR:
wd.df  ["year.percent"] <- round((100*(wd.df[paste0("X",eyear)]/sum(wd.df[paste0("X",eyear)]))),digits=2)

#############################################################################################
# FIGURE SETUP
cols <- wd.df$col
legend.avg <-paste(wd.df$Category," (",wd.df$multi_yr_avg," MGD)",sep="")
legend.year <-paste(wd.df$Category," (",wd.df$X2022," MGD)",sep="")  #update to eyear

filename <- paste("AWRR",source_type[s],"Withdrawals_Pie.png", sep="_")
png(filename=paste(export_path,filename,sep='/'),width=10,height=6,units="in",res=1000)
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
pie(wd.df$X2020, labels = paste(wd.df$year.percent,"%",sep=""), 
    main= right_title_text,
    cex = 1.1,cex.main = 1.1,col=cols
)
legend('bottom',legend.year,cex=1.0, fill=cols, inset=-0.3)
#--------------------------------------------------------------------
dev.off()
print(paste0(source_type[s]," Pie Chart: COMPLETE"))
}
