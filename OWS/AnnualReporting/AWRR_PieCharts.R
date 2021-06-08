library(sqldf)

#site <- "http://deq2.bse.vt.edu/d.dh/"
site <- "https://deq1.bse.vt.edu/d.dh/"

basepath <- "/var/www/R/"
source(paste(basepath,"config.local.private",sep = '/'))
Table1 <- read.csv("U:/OWS/foundation_datasets/awrr/2021/Table1_2016-2020.csv")

#############################################################################################
gw.sql <- paste('SELECT *,
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
                 WHERE "Source.Type" = "Groundwater"',sep='')
gw.df <- sqldf(gw.sql)

# FIVE YEAR AVERAGE:
gw.df  ["avg.percent"] <- round((100*(gw.df$multi_yr_avg/sum(gw.df$multi_yr_avg))),digits=2)
# MOST RECENT YEAR:
gw.df  ["year.percent"] <- round((100*(gw.df$X2020/sum(gw.df$X2020))),digits=2)
#############################################################################################
# FIGURE SETUP
cols <- gw.df$col
legend.avg <-paste(gw.df$Category," (",gw.df$multi_yr_avg," MGD)",sep="")
legend.year <-paste(gw.df$Category," (",gw.df$X2020," MGD)",sep="")

filename <- paste("AWRR_Groundwater_Withdrawals_Pie.png", sep="_")
png(filename=paste(export_path,filename,sep='/'),width=10,height=6,units="in",res=1000)
par(mfrow=c(1,2),mai=c(1.4,0.5,0.4,0.4),oma = c(0,0,0,0),xpd=TRUE,cex = 1.0499)
#--------------------------------------------------------------------
# LEFT PIE
pie(gw.df$multi_yr_avg, labels = paste(gw.df$avg.percent,"%",sep=""), 
    main="(a) 2016-2020 Average Groundwater Withdrawals",
    cex = 1.1,cex.main = 1.1,col=cols
)
legend('bottom',legend.avg,cex=1.0, fill=cols, inset=-0.3) 
#--------------------------------------------------------------------
# RIGHT PIE
pie(gw.df$X2020, labels = paste(gw.df$year.percent,"%",sep=""), 
    main="(b) 2020 Total Groundwater Withdrawals",
    cex = 1.1,cex.main = 1.1,col=cols
)
legend('bottom',legend.year,cex=1.0, fill=cols, inset=-0.3)
#--------------------------------------------------------------------
dev.off()
#############################################################################################