basepath <- 'C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\r-dh-ecohydro'
save_directory <-  'C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\plots\\' #Location to output images


intakes <- read.table(file=paste(basepath,"ELFGEN\\internal\\surface_water_intake_summary.csv",sep="\\"), header=TRUE, sep=",")

#MAF or SQMI
df.0_10 <- intakes[which((as.numeric(as.character(intakes$SQMI)) >= 0.0) & (as.numeric(as.character(intakes$SQMI)) <= 10.0)),]
df.10_50 <- intakes[which((as.numeric(as.character(intakes$SQMI)) >= 10.0) & (as.numeric(as.character(intakes$SQMI)) <= 50.0)),]
df.50_100 <- intakes[which((as.numeric(as.character(intakes$SQMI)) >= 50.0) & (as.numeric(as.character(intakes$SQMI)) <= 100.0)),]
df.100_500 <- intakes[which((as.numeric(as.character(intakes$SQMI)) >= 100.0) & (as.numeric(as.character(intakes$SQMI)) <= 500.0)),]
df.500_plus <- intakes[which((as.numeric(as.character(intakes$SQMI)) >= 500.0)),]

intake_summary <- data.frame("df.0_10"=nrow(df.0_10),
                             "df.10_50"=nrow(df.10_50),
                             "df.50_100"=nrow(df.50_100),
                             "df.100_500"=nrow(df.100_500),
                             "df.500_plus"=nrow(df.500_plus))

count <- c(nrow(df.0_10),nrow(df.10_50),nrow(df.50_100),nrow(df.100_500),nrow(df.500_plus))
range <- c("df.0_10","df.10_50","df.50_100","df.100_500","df.500_plus")
sum <- c(sum(as.numeric(as.character(df.0_10$MAF))),
        sum(as.numeric(as.character(df.10_50$MAF))),
        sum(as.numeric(as.character(df.50_100$MAF))),
        sum(as.numeric(as.character(df.100_500$MAF))),
        sum(as.numeric(as.character(df.500_plus$MAF))))
         
intake_summary <- data.frame(range,count,sum)


library(ggplot2)
p <- ggplot(data=intake_summary, aes(x=range, y=count)) +
      geom_bar(stat="identity")+
      geom_text(label = paste("MAF = ",round(sum,1)," mgy",sep=""), vjust = -0.5)+
      scale_x_discrete(labels = c("0-10", "10-50", "50-100", "100-500", "500+"))
p


filename <- paste("intake_summary.png", sep="_")
ggsave(file=filename, path = save_directory, width=8, height=5)



