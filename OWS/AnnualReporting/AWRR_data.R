library("readxl")
library("dplyr") # neeeded for distinct()

startdate <- "2018-01-01"
enddate <- "2018-12-31"

#data.vwuds <- read.csv(file=paste("https://deq1.bse.vt.edu/d.dh/ows-awrr-map-export/wd_mgy?ftype_op=not&ftype=power&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=77498",sep=""), header=TRUE, sep=",")
data.all <- read.csv(file=paste("https://deq1.bse.vt.edu/d.dh/ows-awrr-map-export/wd_mgy?ftype_op=not&ftype=power&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=65668&dh_link_admin_reg_issuer_target_id%5B1%5D=91200&dh_link_admin_reg_issuer_target_id%5B2%5D=77498",sep=""), header=TRUE, sep=",")

data <- data.all

agriculture <- data[which(data$Use.Type=='agriculture'), ]
commercial <- data[which(data$Use.Type=='commercial'), ]
irrigation <- data[which(data$Use.Type=='irrigation'), ]
manufacturing <- data[which(data$Use.Type=='manufacturing'), ]
mining <- data[which(data$Use.Type=='mining'), ]
  Municipal <- data[which(data$Use.Type=='Municipal'), ]
  municipal <- data[which(data$Use.Type=='municipal'), ]
muni <- rbind(Municipal,municipal)


agriculture.mgd <- sum(agriculture$Water.Use..MGD)
commercial.mgd <- sum(commercial$Water.Use..MGD)
irrigation.mgd <- sum(irrigation$Water.Use..MGD)
manufacturing.mgd <- sum(manufacturing$Water.Use..MGD)
mining.mgd <- sum(mining$Water.Use..MGD)
muni.mgd <- sum(muni$Water.Use..MGD)



