#library("readxl")
library("dplyr")
library('httr')
library('stringr')
library("kableExtra")

a <- c(
  'agricultural', 
  'commercial', 
  'irrigation',
  'manufacturing',  
  'mining', 
  'municipal'
)
b <- c('Groundwater', 'Surface Water', 'Total (GW + SW)')
cat_table<- data.frame(expand.grid(a,b))

colnames(cat_table) <- c('Use_Type', 'Source_Type')
cat_table <- arrange(cat_table, Source_Type, Use_Type )
#cat_table = FALSE
syear = 2015
eyear = 2019
year.range <- syear:eyear

for (y in year.range) {
  
  print(y)
  startdate <- paste(y, "-01-01",sep='')
  enddate <- paste(y, "-12-31", sep='')
  
  localpath <- tempdir()
  filename <- paste("data.all_",y,".csv",sep="")
  destfile <- paste(localpath,filename,sep="\\")  
  download.file(paste("http://deq2.bse.vt.edu/d.dh/ows-awrr-map-export/wd_mgy?ftype_op=not&ftype=power&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=65668&dh_link_admin_reg_issuer_target_id%5B1%5D=91200&dh_link_admin_reg_issuer_target_id%5B2%5D=77498",sep=""), destfile = destfile, method = "libcurl")  
  data.all <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")
  
  #has 3 issuing authorities, does not include power
#  data.all <- read.csv(file=paste("http://deq2.bse.vt.edu/d.dh/ows-awrr-map-export/wd_mgy?ftype_op=not&ftype=power&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=65668&dh_link_admin_reg_issuer_target_id%5B1%5D=91200&dh_link_admin_reg_issuer_target_id%5B2%5D=77498",sep=""), header=TRUE, sep=",")
  
  data <- data.all
  
  #remove duplicates (keeps one row)
  data <- distinct(data, MP_hydroid, .keep_all = TRUE)
  #exclude dalecarlia
  data <- data[-which(data$Facility=='DALECARLIA WTP'),]
  
  if (length(which(data$Use.Type=='facility')) > 0) {
    data <- data[-which(data$Use.Type=='facility'),]
  }
  #rename columns
  colnames(data) <- c('HydroID', 'Hydrocode', 'Source_Type',
                      'MP_Name','Facility_hydroid','Facility', 'Use_Type', 'Year',
                      'mgy', 'mgd', 'lat', 'lon', 'fips','locality')
  
  data$mgd <- data$mgy/365
  #make use type values lowercase
  data$Use_Type <- str_to_lower(data$Use_Type)
  #change 'Well' and 'Surface Water Intake' values in source_type column to match report headers
  levels(data$Source_Type) <- c(levels(data$Source_Type), "Groundwater", "Surface Water")
  data$Source_Type[data$Source_Type == 'Well'] <- 'Groundwater'
  data$Source_Type[data$Source_Type == 'Surface Water Intake'] <- 'Surface Water'
  

  data$Use_Type[data$Use_Type == 'industrial'] <- 'manufacturing'
  
  
  catsourcesum <- data %>% group_by(Use_Type, Source_Type)
  
  catsourcesum <- catsourcesum %>% summarise(
    mgd = sum(mgd),
    mgy = sum(mgy)
  )
  
  catsourcesum$mgd = round(catsourcesum$mgy / 365.0,2)
  catsourcesum <- arrange(catsourcesum, Source_Type, Use_Type)
  
  
  catsum <- catsourcesum
  catsum$Source_Type <- "Total (GW + SW)"
  catsum <- catsum %>% group_by(Use_Type, Source_Type)
  
  catsum <- catsum %>% summarise(
    mgd = sum(mgd),
    mgy = sum(mgy)
  )
  catsum <- arrange(catsum, Source_Type, Use_Type)
  
  
  year_table <- rbind(catsourcesum, catsum)
  year_table <- arrange(year_table, Source_Type, Use_Type)
  assign(paste("y", y, sep=''), year_table)
  if (is.logical(cat_table)) {
    cat_table = year_table[,1:3]
  } else {
    cat_table <- cbind(cat_table, year_table[,3])
  }
  

}

#cat_table_raw <- cat_table <- cat_table_raw

cat_table <- data.frame(cat_table[2],cat_table[1],cat_table[3:(length(year.range)+2)])
names(cat_table) <- c('Source Type', 'Category', year.range)

multi_yr_avg <- round((rowMeans(cat_table[3:(length(year.range)+2)], na.rm = FALSE, dims = 1)),2)
#names(multi_yr_avg) <- paste(length(year.range)," Year Avg.",sep="")
cat_table <- cbind(cat_table,multi_yr_avg)



##Groundwater Total##
gw_table <- cat_table[cat_table$"Source Type" == 'Groundwater',]
gw_sums <- data.frame(Source_Type="",
                      Category="Total Groundwater",
                      mgd=sum(gw_table[3]),
                      mgd=sum(gw_table[4]),
                      mgd=sum(gw_table[5]),
                      mgd=sum(gw_table[6]),
                      mgd=sum(gw_table[7]),
                      mgd=sum(gw_table[8])
)
colnames(gw_sums) <- c('Source Type', 'Category',year.range,'multi_yr_avg')
##Surface Water Total##
sw_table <- cat_table[cat_table$"Source Type" == 'Surface Water',]
sw_sums <- data.frame(Source_Type="",
                      Category="Total Surface Water",
                      mgd=sum(sw_table[3]),
                      mgd=sum(sw_table[4]),
                      mgd=sum(sw_table[5]),
                      mgd=sum(sw_table[6]),
                      mgd=sum(sw_table[7]),
                      mgd=sum(sw_table[8])
)
colnames(sw_sums) <- c('Source Type', 'Category',year.range,'multi_yr_avg')
cat_table <- rbind(cat_table,gw_sums, sw_sums)


pct_chg <- round(((cat_table[paste(eyear)]-cat_table["multi_yr_avg"])/cat_table["multi_yr_avg"])*100, 1)
names(pct_chg) <- paste('% Change',eyear,'to Avg.')
cat_table <- cbind(cat_table,'pct_chg' = pct_chg)

##############################################################
# ADD BOTTOM ROW OF TOTALS TO TABLE
cat_table.total <- cat_table[c(13:18),]
multi_yr_avg.sums <- mean(c(sum(cat_table.total[3]),
                            sum(cat_table.total[4]),
                            sum(cat_table.total[5]),
                            sum(cat_table.total[6]),
                            sum(cat_table.total[7])))

total_pct_chg <- round(((sum(cat_table.total[7])-multi_yr_avg.sums)/multi_yr_avg.sums)*100, 1)


#cat_table.total <- cat_table[c(13:18),]
catsum.sums <- data.frame(Source_Type="",
                          Category="Total (GW + SW)",
                          mgd=sum(cat_table.total[3]),
                          mgd=sum(cat_table.total[4]),
                          mgd=sum(cat_table.total[5]),
                          mgd=sum(cat_table.total[6]),
                          mgd=sum(cat_table.total[7]),
                          mgd=multi_yr_avg.sums,
                          mgd=total_pct_chg 
)


colnames(catsum.sums) <- c('Source Type', 'Category',year.range,'multi_yr_avg',paste('% Change',eyear,'to Avg.'))
cat_table <- rbind(cat_table,catsum.sums)
##############################################################

print(cat_table)


kable(cat_table, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(8, width = "5em") %>%
  column_spec(9, width = "5em")


##################################################################################
###bySourceType - tables 5,7,9,11,13,16,19
#change avg column name 
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(wesanderson)
colnames(cat_table)[8] <- paste((eyear-syear)+1,"Year Avg.")

#ag
agtable5 <- cat_table[c(1,7,13),-2]
rownames(agtable5) <- c()

kable(agtable5, "latex", booktabs = T, align = c('l','c','c','c','c','c','c','c')) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  row_spec(row = 3, bold = TRUE)

################################################################################################
#transform wide to long table
agtable5 <- agtable5[-3,-8]
colnames(agtable5)[colnames(agtable5)=="Source Type"] <- "Source"
colnames(agtable5)[colnames(agtable5)==paste((eyear-syear)+1,"Year Avg.")] <- "Average"
agtable5 <- gather(agtable5,Year, MGD, paste(syear):paste(eyear), factor_key = TRUE)

#plot bar graph
ggplot(data=agtable5, aes(x=Year, y=MGD, fill = Source)) +
  geom_col(position=position_dodge(), colour = "gray") + 
  geom_hline(yintercept = agtable5$Average, size = .4, colour = "black",linetype = "dashed") +
  labs( y="Million Gallons per Day", fill = "Source Type") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "light gray", 
                                          size=.3),
        legend.position="bottom", 
        legend.box = "horizontal",
        axis.title.x=element_text(size=14),  # X axis title
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size=11, vjust = 1)) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  geom_text(aes(label=MGD),
            position=position_dodge(width=0.9), 
            vjust = -.88)
#+ annotate("text", y=agtable5$Average-1.8, x=.79, label ="5 Year Avg.") 
#+ annotate("text", y=agtable5$Average-3, x=.79, label = paste('=',agtable5$Average, " MGD"))
  

filename <- paste("Agriculture",paste(syear,"-",eyear, sep = ""),"Bar_Graph.pdf", sep="_")
ggsave(file=filename, path = paste("U:/OWS/Report Development/Annual Water Resources Report/October",eyear+1,"Report/Maps/Bar Graphs/",sep = " "), width=12, height=6)

#####################################################################################################
#irrig
irrigtable7 <- cat_table[c(3,9,15),-2]
rownames(irrigtable7) <- c()

kable(irrigtable7, "latex", booktabs = T, align = c('l','c','c','c','c','c','c','c')) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  row_spec(row = 3, bold = TRUE)

################################################################################################
#transform wide to long table
irrigtable7 <- irrigtable7[-3,-8]
colnames(irrigtable7)[colnames(irrigtable7)=="Source Type"] <- "Source"
colnames(irrigtable7)[colnames(irrigtable7)=="5 Year Avg."] <- "Average"
irrigtable7 <- gather(irrigtable7,Year, MGD, "2014":'2018', factor_key = TRUE)

#plot bar graph
ggplot(data=irrigtable7, aes(x=Year, y=MGD, fill = Source)) +
  geom_col(position=position_dodge(), colour = "gray") + 
  geom_hline(yintercept = irrigtable7$Average, size = .4, colour = "black",linetype = "dashed") +
  labs( y="Million Gallons per Day", fill = "Source Type") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "light gray", 
                                          size=.3),
        legend.position="bottom", 
        legend.box = "horizontal",
        axis.title.x=element_text(size=14),  # X axis title
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size=11, vjust = 1)) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  geom_text(aes(label=MGD),
            position=position_dodge(width=0.9), 
            vjust = -.88)
#+ annotate("text", y=irrigtable7$Average-1.8, x=.79, label ="5 Year Avg.") 
#+ annotate("text", y=irrigtable7$Average-3, x=.79, label = paste('=',irrigtable7$Average, " MGD"))


filename <- paste("Irrigation 2014-2018 Bar Graph",".pdf", sep="_")
ggsave(file=filename, path = paste("U:/OWS/Report Development/Annual Water Resources Report/October",eyear+1,"Report/Maps/Bar Graphs/",sep = " "), width=12, height=6)

#####################################################################################################

#commercial
commtable9 <- cat_table[c(2,8,14),-2]
rownames(commtable9) <- c()

kable(commtable9, "latex", booktabs = T, align = c('l','c','c','c','c','c','c','c')) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  row_spec(row = 3, bold = TRUE)

################################################################################################
#transform wide to long table
commtable9 <- commtable9[-3,-8]
colnames(commtable9)[colnames(commtable9)=="Source Type"] <- "Source"
colnames(commtable9)[colnames(commtable9)=="5 Year Avg."] <- "Average"
commtable9 <- gather(commtable9,Year, MGD, "2014":'2018', factor_key = TRUE)

#plot bar graph
ggplot(data=commtable9, aes(x=Year, y=MGD, fill = Source)) +
  geom_col(position=position_dodge(), colour = "gray") + 
  geom_hline(yintercept = commtable9$Average, size = .4, colour = "black",linetype = "dashed") +
  labs( y="Million Gallons per Day", fill = "Source Type") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "light gray", 
                                          size=.3),
        legend.position="bottom", 
        legend.box = "horizontal",
        axis.title.x=element_text(size=14),  # X axis title
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size=11, vjust = 1)) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  geom_text(aes(label=MGD),
            position=position_dodge(width=0.9), 
            vjust = -.4)
#+ annotate("text", y=commtable9$Average-1.8, x=.79, label ="5 Year Avg.") 
#+ annotate("text", y=commtable9$Average-3, x=.79, label = paste('=',commtable9$Average, " MGD"))


filename <- paste("Commercial 2014-2018 Bar Graph",".pdf", sep="_")
ggsave(file=filename, path = paste("U:/OWS/Report Development/Annual Water Resources Report/October",eyear+1,"Report/Maps/Bar Graphs/",sep = " "), width=12, height=6)


#####################################################################################################
#mining
mintable11 <- cat_table[c(5,11,17),-2]
rownames(mintable11) <- c()

kable(mintable11, "latex", booktabs = T, align = c('l','c','c','c','c','c','c','c')) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  row_spec(row = 3, bold = TRUE)

################################################################################################
#transform wide to long table
mintable11 <- mintable11[-3,-8]
colnames(mintable11)[colnames(mintable11)=="Source Type"] <- "Source"
colnames(mintable11)[colnames(mintable11)=="5 Year Avg."] <- "Average"
mintable11 <- gather(mintable11,Year, MGD, "2014":'2018', factor_key = TRUE)

#plot bar graph
ggplot(data=mintable11, aes(x=Year, y=MGD, fill = Source)) +
  geom_col(position=position_dodge(), colour = "gray") + 
  geom_hline(yintercept = mintable11$Average, size = .4, colour = "black",linetype = "dashed") +
  labs( y="Million Gallons per Day", fill = "Source Type") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "light gray", 
                                          size=.3),
        legend.position="bottom", 
        legend.box = "horizontal",
        axis.title.x=element_text(size=14),  # X axis title
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size=11, vjust = 1)) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  geom_text(aes(label=MGD),
            position=position_dodge(width=0.9), 
            vjust = -.75)
#+ annotate("text", y=mintable11$Average-1.8, x=.79, label ="5 Year Avg.") 
#+ annotate("text", y=mintable11$Average-3, x=.79, label = paste('=',mintable11$Average, " MGD"))


filename <- paste("Mining 2014-2018 Bar Graph",".pdf", sep="_")
ggsave(file=filename, path = paste("U:/OWS/Report Development/Annual Water Resources Report/October",eyear+1,"Report/Maps/Bar Graphs/",sep = " "), width=12, height=6)


#####################################################################################################
#manuf
mantable13 <- cat_table[c(4,10,16),-2]
rownames(mantable13) <- c()

kable(mantable13, "latex", booktabs = T, align = c('l','c','c','c','c','c','c','c')) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  row_spec(row = 3, bold = TRUE) 
################################################################################################
#transform wide to long table
mantable13 <- mantable13[-3,-8]
colnames(mantable13)[colnames(mantable13)=="Source Type"] <- "Source"
colnames(mantable13)[colnames(mantable13)=="5 Year Avg."] <- "Average"
mantable13 <- gather(mantable13,Year, MGD, "2014":'2018', factor_key = TRUE)

#plot bar graph
ggplot(data=mantable13, aes(x=Year, y=MGD, fill = Source)) +
  geom_col(position=position_dodge(), colour = "gray") + 
  geom_hline(yintercept = mantable13$Average, size = .4, colour = "black",linetype = "dashed") +
  labs( y="Million Gallons per Day", fill = "Source Type") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "light gray", 
                                          size=.3),
        legend.position="bottom", 
        legend.box = "horizontal",
        axis.title.x=element_text(size=14),  # X axis title
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size=11, vjust = 1)) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  geom_text(aes(label=MGD),
            position=position_dodge(width=0.9), 
            vjust = -1)
#+ annotate("text", y=mantable13$Average-1.8, x=.79, label ="5 Year Avg.") 
#+ annotate("text", y=mantable13$Average-3, x=.79, label = paste('=',mantable13$Average, " MGD"))


filename <- paste("Manufacturing Industrial 2014-2018 Bar Graph",".pdf", sep="_")
ggsave(file=filename, path = paste("U:/OWS/Report Development/Annual Water Resources Report/October",eyear+1,"Report/Maps/Bar Graphs/",sep = " "), width=12, height=6)


#####################################################################################################
#muni aka pws
munitable16 <- cat_table[c(6,12,18),-2]
rownames(munitable16) <- c()

kable(munitable16, "latex", booktabs = T, align = c('l','c','c','c','c','c','c','c')) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  row_spec(row = 3, bold = TRUE)

################################################################################################
#transform wide to long table
munitable16 <- munitable16[-3,-8]
colnames(munitable16)[colnames(munitable16)=="Source Type"] <- "Source"
colnames(munitable16)[colnames(munitable16)=="5 Year Avg."] <- "Average"
munitable16 <- gather(munitable16,Year, MGD, "2014":'2018', factor_key = TRUE)

#plot bar graph
ggplot(data=munitable16, aes(x=Year, y=MGD, fill = Source)) +
  geom_col(position=position_dodge(), colour = "gray") + 
  geom_hline(yintercept = munitable16$Average, size = .4, colour = "black",linetype = "dashed") +
  labs( y="Million Gallons per Day", fill = "Source Type") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "light gray", 
                                          size=.3),
        legend.position="bottom", 
        legend.box = "horizontal",
        axis.title.x=element_text(size=14),  # X axis title
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size=11, vjust = 1)) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  geom_text(aes(label=MGD),
            position=position_dodge(width=0.9), 
            vjust = -.2)
#+ annotate("text", y=munitable16$Average-1.8, x=.79, label ="5 Year Avg.") 
#+ annotate("text", y=munitable16$Average-3, x=.79, label = paste('=',munitable16$Average, " MGD"))


filename <- paste("Public Water Supply 2014-2018 Bar Graph",".pdf", sep="_")
ggsave(file=filename, path = paste("U:/OWS/Report Development/Annual Water Resources Report/October",eyear+1,"Report/Maps/Bar Graphs/",sep = " "), width=12, height=6)

#####################################################################################################

power1<- read.csv("C:/Users/maf95834/Desktop/power.csv")
#transform wide to long table
power <- power1[-c(3,6,7),-9]
colnames(power) <- c('Source', 'Power', year.range, 'Average')

power <- gather(power,Year, MGD, "2014":'2018', factor_key = TRUE)

#plot bar graph
ggplot(data=power, aes(x=Year, y=MGD, fill = Source)) +
  geom_col(position=position_dodge(), colour = "gray") + 
  geom_hline(yintercept = power$Average, size = .4, colour = "black",linetype = "dashed") +
  labs( y="Million Gallons per Day", fill = "Source Type") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "light gray", 
                                          size=.3),
        legend.position="bottom", 
        legend.box = "horizontal",
        axis.title.x=element_text(size=14),  # X axis title
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size=11, vjust = 1)) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  geom_text(aes(label=MGD),
            position = position_stack(vjust = .5), 
            vjust = -.2)+
  facet_grid(Source~Power, scales = "free_y") 


#+ scale_y_continuous(breaks = my_breaks)
#my_breaks <- function(x) { if (max(x) < 100) seq(0, 1, .2) else seq(0, 4000, 1000) }
#+ annotate("text", y=power$Average-1.8, x=.79, label ="5 Year Avg.") 
#+ annotate("text", y=power$Average-3, x=.79, label = paste('=',power$Average, " MGD"))


filename <- paste("Power2014-2018BarGraph",".pdf", sep="_")
ggsave(file=filename, path = paste("U:/OWS/Report Development/Annual Water Resources Report/October",eyear+1,"Report/Maps/Bar Graphs/",sep = " "), width=12, height=6)

gw_average <- power[c(1:2),-c(4:5)]

sw_average <- power[c(3:4),-c(4:5)]

#plot bar graph
ggplot(data=power, aes(x=Year, y=MGD, fill = Source)) +
  geom_col(position=position_dodge(), colour = "gray") +
  geom_hline(data = gw_average, yintercept = gw_average$Average, size = .4, colour = "black",linetype = "dashed") +
  #geom_hline(data = sw_average, yintercept = sw_average$Average, size = .4, colour = "black",linetype = "dashed") +
  geom_text(aes(label=MGD),
            position = position_stack(vjust = .5), 
            vjust = -.2)+
  facet_grid(Source~Power, scales = "free_y") 
#use these to format the headers in latex 
#             \begin{tabular}{lccccccp{2cm}}
#              \begin{tabular}{>{\raggedright\arraybackslash}p{4cm}lllcc}
#               \begin{tabular}{lllp{4cm}cc}













#names(multi_yr_avg) <- paste(length(year.range)," Year Avg.",sep="")

#year_frame <- arrange(year_table, Source_Type, Use_Type)




#ardp <- 'U:\\OWS\\Report Development\\Annual Water Resources Report\\October 2019 Report\\Water Use Exports\\All Withdrawals By Use Type\\Water Use Exports By Type and Permit_Use For AWRR\\'

# Merge these 3
# read vwuds tab, add 'ptype' <= 'vwuds' 
# read VWP tab, add 'ptype' <= 'vwp' 
# Read GWP file, add 'ptype' <= 'gwp' 


#source <- "ALL SW VWP AND VWUDS SEPARATED CLEANED.xlsx"
#folder <- ardp

#sheet <- "VWUDS"
#rawdata <- read_excel(paste(folder,source,sep=''),sheet)
#data <- rawdata

