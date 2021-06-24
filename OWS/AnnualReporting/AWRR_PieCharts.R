library(sqldf)
library(dplyr)
#site <- "http://deq2.bse.vt.edu/d.dh/"
site <- "https://deq1.bse.vt.edu/d.dh/"

basepath <- "/var/www/R/"
source(paste(basepath,"config.local.private",sep = '/'))
Table1 <- read.csv("U:/OWS/foundation_datasets/awrr/2021/Table1_2016-2020.csv")

# AWRR Pie Charts ################################################################
Table1$Source.Type <- recode(Table1$Source.Type, "Total (GW + SW)" = "Total") #Change for easier string substitution and clear pie chart labeling 
source_type <- c("Groundwater", "Surface Water", "Total")
#s = 1
for (s in 1:length(source_type)) {

wd.sql <- paste('SELECT *,
                  CASE
                    WHEN "Category" = "Agricultural" THEN "dodgerblue4"
                    WHEN "Category" = "Commercial" THEN "firebrick4"
                    WHEN "Category" = "Irrigation" THEN "darkolivegreen3"
                    WHEN "Category" = "Manufacturing" THEN "blueviolet"
                    WHEN "Category" = "Mining" THEN "deepskyblue3" 
                    WHEN "Category" = "Municipal" THEN "darkorange2" 
                    ELSE "white"
                  END AS col
                 FROM Table1
                 WHERE "Source.Type" = "',source_type[s],'"',sep='')
wd.df <- sqldf(wd.sql)

# FIVE YEAR AVERAGE:
wd.df  ["avg.percent"] <- round((100*(wd.df$multi_yr_avg/sum(wd.df$multi_yr_avg))),digits=2)
# MOST RECENT YEAR:
wd.df  ["year.percent"] <- round((100*(wd.df$X2020/sum(wd.df$X2020))),digits=2)
#############################################################################################
# FIGURE SETUP
cols <- wd.df$col
legend.avg <-paste(wd.df$Category," (",wd.df$multi_yr_avg," MGD)",sep="")
legend.year <-paste(wd.df$Category," (",wd.df$X2020," MGD)",sep="")

filename <- paste("AWRR",source_type[s],"Withdrawals_Pie.png", sep="_")
png(filename=paste(export_path,filename,sep='/'),width=10,height=6,units="in",res=1000)
par(mfrow=c(1,2),mai=c(1.4,0.5,0.4,0.4),oma = c(0,0,0,0),xpd=TRUE,cex = 1.0499)
#--------------------------------------------------------------------
# LEFT PIE
if (source_type[s] == "Total") {
  left_title_text <- paste0("(a) 2016-2020 Average Withdrawals")
} else {
  left_title_text <- paste0("(a) 2016-2020 Average ",source_type[s]," Withdrawals")
}
pie(wd.df$multi_yr_avg, labels = paste(wd.df$avg.percent,"%",sep=""), 
    main= left_title_text,
    cex = 1.1,cex.main = 1.1,col=cols
)
legend('bottom',legend.avg,cex=1.0, fill=cols, inset=-0.3) 
#--------------------------------------------------------------------
# RIGHT PIE
if (source_type[s] == "Total") {
  right_title_text <- "(b) 2020 Total Withdrawals"
} else {
  right_title_text <- paste0("(b) 2020 Total ",source_type[s]," Withdrawals")
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
