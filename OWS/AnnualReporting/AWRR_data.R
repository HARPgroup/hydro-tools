#library("readxl")
library("dplyr")
library('httr')
library('stringr')
library("kableExtra")
library('stringr')
options(scipen = 999)

#file_extension <- ".html"
file_extension <- ".tex"

#switch between file types to save in common drive folder; html or latex
if (file_extension == ".html") {
  options(knitr.table.format = "html") #"html" for viewing in Rstudio Viewer pane
  file_ext <- ".html" #view in R or browser
} else {
  options(knitr.table.format = "latex") #"latex" when ready to output to Overleaf
  file_ext <- ".tex" #for easy upload to Overleaf
}
#Kable Styling
latexoptions <- c("scale_down")

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

#START LOOP
for (y in year.range) {
  
  print(y)
  startdate <- paste(y, "-01-01",sep='')
  enddate <- paste(y, "-12-31", sep='')
  
  localpath <- tempdir()
  filename <- paste("data.all_",y,".csv",sep="")
  destfile <- paste(localpath,filename,sep="\\") 
  
  #has 3 issuing authorities, does not include power
  download.file(paste("http://deq2.bse.vt.edu/d.dh/ows-awrr-map-export/wd_mgy?ftype_op=not&ftype=power&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=65668&dh_link_admin_reg_issuer_target_id%5B1%5D=91200&dh_link_admin_reg_issuer_target_id%5B2%5D=77498",sep=""), destfile = destfile, method = "libcurl")  
  data.all <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")
  
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
#make Category values capital
cat_table$Category <- str_to_title(cat_table$Category)
print(cat_table)


kable(cat_table, booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(8, width = "5em") %>%
  column_spec(9, width = "5em")

#cat_table <- cat_table2
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

ag_tex <- kable(agtable5, booktabs = T, align = c('l','c','c','c','c','c','c','c'),
    caption = paste(syear,"-",eyear,"Agriculture Water Withdrawals by Source Type (MGD)",sep=" "),
    label = paste(syear,"-",eyear,"Agriculture Water Withdrawal Trends",sep=" "),
    col.names = c("Source Type",
                  colnames(agtable5[2:8]))) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  row_spec(row = 3, bold = TRUE) 

#CUSTOM LATEX CHANGES
#insert hold position header
ag_tex <- gsub(pattern = "{table}[t]", 
                   repl    = "{table}[ht!]", 
                   x       = ag_tex, fixed = T )

#make last column name wrap on 2 rows (adjusts column width) 
ag_tex <- gsub(pattern = "{lccccccc}", 
               repl    = "{lccccccp{2cm}}", 
               x       = ag_tex, fixed = T )

ag_tex %>%
  cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October 2020 Report\\Overleaf\\Agriculture_table",file_ext,sep = ''))
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
        panel.grid.major.y = element_line(colour = "light gray", size=.3),
        legend.position="bottom", 
        legend.box = "horizontal",
        axis.title.x=element_text(size=14),  # X axis title
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size=16, vjust = 1)) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  geom_text(aes(label=MGD),
            position=position_dodge(width=0.9), 
            vjust = -.8)
  # annotate("text", y=agtable5$Average-1.8, x=.79, label =paste((eyear-syear)+1,"Year Avg."))+
  # annotate("text", y=agtable5$Average-3, x=.79, label = paste('=',agtable5$Average, " MGD"))
  # annotate("text", y=agtable5$Average[1:2]-1.8, x=.81, label =paste((eyear-syear)+1,"Year Avg.","=",agtable5$Average[1:2], " MGD"))

filename <- paste("Agriculture",paste(syear,"-",eyear, sep = ""),"Bar_Graph.pdf", sep="_")
ggsave(file=filename, path = paste("U:/OWS/Report Development/Annual Water Resources Report/October",eyear+1,"Report/Overleaf",sep = " "), width=12, height=6)

#####################################################################################################
#irrig
irrigtable7 <- cat_table[c(3,9,15),-2]
rownames(irrigtable7) <- c()

irrig_tex <- kable(irrigtable7,  booktabs = T, align = c('l','c','c','c','c','c','c','c'),
      caption = paste(syear,"-",eyear,"Irrigation Water Withdrawals by Source Type (MGD)",sep=" "),
      label = paste(syear,"-",eyear,"Irrigation Water Withdrawal Trends",sep=" "),
      col.names = c("Source Type",
                    colnames(irrigtable7[2:8]))) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  row_spec(row = 3, bold = TRUE) 

#CUSTOM LATEX CHANGES
#insert hold position header
irrig_tex <- gsub(pattern = "{table}[t]", 
               repl    = "{table}[ht!]", 
               x       = irrig_tex, fixed = T )

#make last column name wrap on 2 rows (adjusts column width) 
irrig_tex <- gsub(pattern = "{lccccccc}", 
               repl    = "{lccccccp{2cm}}", 
               x       = irrig_tex, fixed = T )

irrig_tex %>%
  cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October 2020 Report\\Overleaf\\Irrigation_table",file_ext,sep = ''))

################################################################################################
#transform wide to long table
irrigtable7 <- irrigtable7[-3,-8]
colnames(irrigtable7)[colnames(irrigtable7)=="Source Type"] <- "Source"
colnames(irrigtable7)[colnames(irrigtable7)==paste((eyear-syear)+1,"Year Avg.")] <- "Average"
irrigtable7 <- gather(irrigtable7,Year, MGD, paste(syear):paste(eyear), factor_key = TRUE)

#plot bar graph
ggplot(data=irrigtable7, aes(x=Year, y=MGD, fill = Source)) +
  geom_col(position=position_dodge(), colour = "gray") + 
  geom_hline(yintercept = irrigtable7$Average, size = .4, colour = "black",linetype = "dashed") +
  labs( y="Million Gallons per Day", fill = "Source Type") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "light gray", size=.3),
        legend.position="bottom", 
        legend.box = "horizontal",
        axis.title.x=element_text(size=14),  # X axis title
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size=16, vjust = 1)) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  geom_text(aes(label=MGD),
            position=position_dodge(width=0.9), 
            vjust = -.88)
#+ annotate("text", y=irrigtable7$Average-1.8, x=.79, label ="5 Year Avg.") 
#+ annotate("text", y=irrigtable7$Average-3, x=.79, label = paste('=',irrigtable7$Average, " MGD"))


filename <- paste("Irrigation",paste(syear,"-",eyear, sep = ""),"Bar_Graph.pdf", sep="_")
ggsave(file=filename, path = paste("U:/OWS/Report Development/Annual Water Resources Report/October",eyear+1,"Report/Overleaf",sep = " "), width=12, height=6)

#####################################################################################################

#commercial
commtable9 <- cat_table[c(2,8,14),-2]
rownames(commtable9) <- c()

comm_tex <- kable(commtable9,  booktabs = T, align = c('l','c','c','c','c','c','c','c'),
      caption = paste(syear,"-",eyear,"Commercial Water Withdrawals by Source Type (MGD)",sep=" "),
      label = paste(syear,"-",eyear,"Commercial Water Withdrawal Trends",sep=" "),
      col.names = c("Source Type",
                    colnames(commtable9[2:8]))) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  row_spec(row = 3, bold = TRUE) 

#CUSTOM LATEX CHANGES
#insert hold position header
comm_tex <- gsub(pattern = "{table}[t]", 
                  repl    = "{table}[ht!]", 
                  x       = comm_tex, fixed = T )

#make last column name wrap on 2 rows (adjusts column width) 
comm_tex <- gsub(pattern = "{lccccccc}", 
                  repl    = "{lccccccp{2cm}}", 
                  x       = comm_tex, fixed = T )

comm_tex %>%
  cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October 2020 Report\\Overleaf\\Commercial_table",file_ext,sep = ''))

################################################################################################
#transform wide to long table
commtable9 <- commtable9[-3,-8]
colnames(commtable9)[colnames(commtable9)=="Source Type"] <- "Source"
colnames(commtable9)[colnames(commtable9)==paste((eyear-syear)+1,"Year Avg.")] <- "Average"
commtable9 <- gather(commtable9,Year, MGD, paste(syear):paste(eyear), factor_key = TRUE)

#plot bar graph
ggplot(data=commtable9, aes(x=Year, y=MGD, fill = Source)) +
  geom_col(position=position_dodge(), colour = "gray") + 
  geom_hline(yintercept = commtable9$Average, size = .4, colour = "black",linetype = "dashed") +
  labs( y="Million Gallons per Day", fill = "Source Type") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "light gray", size=.3),
        legend.position="bottom", 
        legend.box = "horizontal",
        axis.title.x=element_text(size=14),  # X axis title
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size=16, vjust = 1)) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  geom_text(aes(label=MGD),
            position=position_dodge(width=0.9), 
            vjust = -.4)
#+ annotate("text", y=commtable9$Average-1.8, x=.79, label ="5 Year Avg.") 
#+ annotate("text", y=commtable9$Average-3, x=.79, label = paste('=',commtable9$Average, " MGD"))


filename <- paste("Commercial",paste(syear,"-",eyear, sep = ""),"Bar_Graph.pdf", sep="_")
ggsave(file=filename, path = paste("U:/OWS/Report Development/Annual Water Resources Report/October",eyear+1,"Report/Overleaf",sep = " "), width=12, height=6)


#####################################################################################################
#mining
mintable11 <- cat_table[c(5,11,17),-2]
rownames(mintable11) <- c()

min_tex <- kable(mintable11,  booktabs = T, align = c('l','c','c','c','c','c','c','c'),
      caption = paste(syear,"-",eyear,"Mining Water Withdrawals by Source Type (MGD)",sep=" "),
      label = paste(syear,"-",eyear,"Mining Water Withdrawal Trends",sep=" "),
      col.names = c("Source Type",
                    colnames(mintable11[2:8]))) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  row_spec(row = 3, bold = TRUE) 

#CUSTOM LATEX CHANGES
#insert hold position header
min_tex <- gsub(pattern = "{table}[t]", 
                 repl    = "{table}[ht!]", 
                 x       = min_tex, fixed = T )

#make last column name wrap on 2 rows (adjusts column width) 
min_tex <- gsub(pattern = "{lccccccc}", 
                 repl    = "{lccccccp{2cm}}", 
                 x       = min_tex, fixed = T )

min_tex %>%
  cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October 2020 Report\\Overleaf\\Mining_table",file_ext,sep = ''))

################################################################################################
#transform wide to long table
mintable11 <- mintable11[-3,-8]
colnames(mintable11)[colnames(mintable11)=="Source Type"] <- "Source"
colnames(mintable11)[colnames(mintable11)==paste((eyear-syear)+1,"Year Avg.")] <- "Average"
mintable11 <- gather(mintable11,Year, MGD, paste(syear):paste(eyear), factor_key = TRUE)

#plot bar graph
ggplot(data=mintable11, aes(x=Year, y=MGD, fill = Source)) +
  geom_col(position=position_dodge(), colour = "gray") + 
  geom_hline(yintercept = mintable11$Average, size = .4, colour = "black",linetype = "dashed") +
  labs( y="Million Gallons per Day", fill = "Source Type") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "light gray", size=.3),
        legend.position="bottom", 
        legend.box = "horizontal",
        axis.title.x=element_text(size=14),  # X axis title
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size=16, vjust = 1)) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  geom_text(aes(label=MGD),
            position=position_dodge(width=0.9), 
            vjust = -.75)
#+ annotate("text", y=mintable11$Average-1.8, x=.79, label ="5 Year Avg.") 
#+ annotate("text", y=mintable11$Average-3, x=.79, label = paste('=',mintable11$Average, " MGD"))


filename <- paste("Mining",paste(syear,"-",eyear, sep = ""),"Bar_Graph.pdf", sep="_")
ggsave(file=filename, path = paste("U:/OWS/Report Development/Annual Water Resources Report/October",eyear+1,"Report/Overleaf",sep = " "), width=12, height=6)


#####################################################################################################
#manuf
mantable13 <- cat_table[c(4,10,16),-2]
rownames(mantable13) <- c()

man_tex <- kable(mantable13,  booktabs = T, align = c('l','c','c','c','c','c','c','c'),
      caption = paste(syear,"-",eyear,"Manufacturing and Industrial Water Withdrawals by Source Type (MGD)",sep=" "),
      label = paste(syear,"-",eyear,"Manufacturing and Industrial Water Withdrawal Trends",sep=" "),
      col.names = c("Source Type",
                    colnames(mantable13[2:8]))) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  row_spec(row = 3, bold = TRUE) 

#CUSTOM LATEX CHANGES
#insert hold position header
man_tex <- gsub(pattern = "{table}[t]", 
                repl    = "{table}[ht!]", 
                x       = man_tex, fixed = T )

#make last column name wrap on 2 rows (adjusts column width) 
man_tex <- gsub(pattern = "{lccccccc}", 
                repl    = "{lccccccp{2cm}}", 
                x       = man_tex, fixed = T )

man_tex %>%
  cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October 2020 Report\\Overleaf\\Manufacturing_table",file_ext,sep = ''))

################################################################################################
#transform wide to long table
mantable13 <- mantable13[-3,-8]
colnames(mantable13)[colnames(mantable13)=="Source Type"] <- "Source"
colnames(mantable13)[colnames(mantable13)==paste((eyear-syear)+1,"Year Avg.")] <- "Average"
mantable13 <- gather(mantable13,Year, MGD, paste(syear):paste(eyear), factor_key = TRUE)

#plot bar graph
ggplot(data=mantable13, aes(x=Year, y=MGD, fill = Source)) +
  geom_col(position=position_dodge(), colour = "gray") + 
  geom_hline(yintercept = mantable13$Average, size = .4, colour = "black",linetype = "dashed") +
  labs( y="Million Gallons per Day", fill = "Source Type") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "light gray", size=.3),
        legend.position="bottom", 
        legend.box = "horizontal",
        axis.title.x=element_text(size=14),  # X axis title
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size=16, vjust = 1)) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  geom_text(aes(label=MGD),
            position=position_dodge(width=0.9), 
            vjust = -1)
#+ annotate("text", y=mantable13$Average-1.8, x=.79, label ="5 Year Avg.") 
#+ annotate("text", y=mantable13$Average-3, x=.79, label = paste('=',mantable13$Average, " MGD"))


filename <- paste("Manufacturing",paste(syear,"-",eyear, sep = ""),"Bar_Graph.pdf", sep="_")
ggsave(file=filename, path = paste("U:/OWS/Report Development/Annual Water Resources Report/October",eyear+1,"Report/Overleaf",sep = " "), width=12, height=6)


#####################################################################################################
#muni aka pws
munitable16 <- cat_table[c(6,12,18),-2]
rownames(munitable16) <- c()

muni_tex <- kable(munitable16,  booktabs = T, align = c('l','c','c','c','c','c','c','c'),
      caption = paste(syear,"-",eyear,"Public Water Supply Water Withdrawals by Source Type (MGD)",sep=" "),
      label = paste(syear,"-",eyear,"Public Water Supply Water Withdrawal Trends",sep=" "),
      col.names = c("Source Type",
                    colnames(munitable16[2:8]))) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  row_spec(row = 3, bold = TRUE) 

#CUSTOM LATEX CHANGES
#insert hold position header
muni_tex <- gsub(pattern = "{table}[t]", 
                repl    = "{table}[ht!]", 
                x       = muni_tex, fixed = T )

#make last column name wrap on 2 rows (adjusts column width) 
muni_tex <- gsub(pattern = "{lccccccc}", 
                repl    = "{lccccccp{2cm}}", 
                x       = muni_tex, fixed = T )

muni_tex %>%
  cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October 2020 Report\\Overleaf\\Public_Water_supply_table",file_ext,sep = ''))


################################################################################################
#transform wide to long table
munitable16 <- munitable16[-3,-8]
colnames(munitable16)[colnames(munitable16)=="Source Type"] <- "Source"
colnames(munitable16)[colnames(munitable16)==paste((eyear-syear)+1,"Year Avg.")] <- "Average"
munitable16 <- gather(munitable16,Year, MGD, paste(syear):paste(eyear), factor_key = TRUE)

#plot bar graph
ggplot(data=munitable16, aes(x=Year, y=MGD, fill = Source)) +
  geom_col(position=position_dodge(), colour = "gray") + 
  geom_hline(yintercept = munitable16$Average, size = .4, colour = "black",linetype = "dashed") +
  labs( y="Million Gallons per Day", fill = "Source Type") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "light gray", size=.3),
        legend.position="bottom", 
        legend.box = "horizontal",
        axis.title.x=element_text(size=14),  # X axis title
        axis.title.y=element_text(size=14),
        axis.text.x = element_text(size=16, vjust = 1)) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  geom_text(aes(label=MGD),
            position=position_dodge(width=0.9), 
            vjust = -.2)
#+ annotate("text", y=munitable16$Average-1.8, x=.79, label ="5 Year Avg.") 
#+ annotate("text", y=munitable16$Average-3, x=.79, label = paste('=',munitable16$Average, " MGD"))


filename <- paste("Public Water Supply",paste(syear,"-",eyear, sep = ""),"Bar_Graph.pdf", sep="_")
ggsave(file=filename, path = paste("U:/OWS/Report Development/Annual Water Resources Report/October",eyear+1,"Report/Overleaf",sep = " "), width=12, height=6)

##############################################################
##############################################################
##############################################################
######################       POWER      ######################
##############################################################
##############################################################
##############################################################
a <- c(
  'fossilpower',
  'nuclearpower'
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
  
  #has 3 issuing authorities, ONLY power
  download.file(paste("https://deq1.bse.vt.edu/d.dh/ows-awrr-map-export/wd_mgy?ftype_op=contains&ftype=power&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=65668&dh_link_admin_reg_issuer_target_id%5B1%5D=91200&dh_link_admin_reg_issuer_target_id%5B2%5D=77498",sep=""), destfile = destfile, method = "libcurl")  
  data.power <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")
  
  data_power <- data.power
  
  #remove duplicates (keeps one row)
  data_power <- distinct(data_power, MP_hydroid, .keep_all = TRUE)
  
  #remove hydropower
  data_power <- data_power %>% filter(Use.Type != "hydropower")
    
  if (length(which(data_power$Use.Type=='facility')) > 0) {
    data_power <- data_power[-which(data_power$Use.Type=='facility'),]
  }
  #rename columns
  colnames(data_power) <- c('HydroID', 'Hydrocode', 'Source_Type',
                      'MP_Name','Facility_hydroid','Facility', 'Use_Type', 'Year',
                      'mgy', 'mgd', 'lat', 'lon', 'fips','locality')
  
  data_power$mgd <- data_power$mgy/365
  #make use type values lowercase
  data_power$Use_Type <- str_to_lower(data_power$Use_Type)
  #change 'Well' and 'Surface Water Intake' values in source_type column to match report headers
  levels(data_power$Source_Type) <- c(levels(data_power$Source_Type), "Groundwater", "Surface Water")
  data_power$Source_Type[data_power$Source_Type == 'Well'] <- 'Groundwater'
  data_power$Source_Type[data_power$Source_Type == 'Surface Water Intake'] <- 'Surface Water'
  
  catsourcesum <- data_power %>% group_by(Use_Type, Source_Type)
  
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

cat_table$multi_yr_avg <- round((rowMeans(cat_table[3:(length(year.range)+2)], na.rm = FALSE, dims = 1)),2)

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
cat_table.total <- cat_table[c(1:4),]
multi_yr_avg.sums <- mean(c(sum(cat_table.total[3]),
                            sum(cat_table.total[4]),
                            sum(cat_table.total[5]),
                            sum(cat_table.total[6]),
                            sum(cat_table.total[7])))

total_pct_chg <- round(((sum(cat_table.total[7])-multi_yr_avg.sums)/multi_yr_avg.sums)*100, 1)

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

print(cat_table)
##############################################################
#Table 19: 2014-2018 Power Generation Water Withdrawals by Source Type (MGD)
powtable19 <- rbind(cat_table[1:2,],cat_table[7,],cat_table[3:4,],cat_table[8,],cat_table[9,])
rownames(powtable19) <- NULL

powtable19 <- sqldf('SELECT "Source Type",
                    CASE 
                    WHEN "Category" LIKE "%fossil%"
                    THEN "Fossil"
                    WHEN "Category" LIKE "%nuclear%"
                    THEN "Nuclear"
                    ELSE "Category"
                    END AS "Category",
                    "2015", "2016", "2017", "2018", "2019", "multi_yr_avg", "% Change 2019 to Avg."
                    FROM powtable19')

pow_tex <- kable(powtable19[2:9], booktabs = T, align = c('l','c','c','c','c','c','c','c'),
      caption = paste(syear,"-",eyear,"Power Generation Water Withdrawals by Source Type (MGD)",sep=" "),
      label = paste(syear,"-",eyear,"Power Generation Water Withdrawal Trends(MGD)",sep=" "),
      col.names = c("Power Type",
                    colnames(powtable19[3:7]),
                    paste((eyear-syear)+1,"Year Avg."),
                    colnames(powtable19[9]))) %>%
  kable_styling(latex_options = c("scale_down")) %>%
  pack_rows("Groundwater", 1, 3,  hline_after = F) %>%
  pack_rows("Surface Water", 4, 6, hline_before = T, hline_after = F) %>%
  row_spec(7, bold=T, extra_css = "border-top: 1px solid")

#CUSTOM LATEX CHANGES
#insert hold position header
pow_tex <- gsub(pattern = "{table}[t]", 
                 repl    = "{table}[ht!]", 
                 x       = pow_tex, fixed = T )

#make last column name wrap on 2 rows (adjusts column width) 
pow_tex <- gsub(pattern  = "{lccccccc}", 
                 repl    = "{lrrrrr>{\\raggedleft\\arraybackslash}p{5em}>{\\raggedleft\\arraybackslash}p{6em}}", 
                 x       = pow_tex, fixed = T,useBytes = F )

pow_tex %>%
  cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October 2020 Report\\Overleaf\\Power_table",file_ext,sep = ''))


########################################################################################

#transform wide to long table
power <- cat_table[1:4,-9]
colnames(power) <- c('Source', 'Power', year.range, 'Average')

power <- gather(power,Year, MGD, paste(syear):paste(eyear), factor_key = TRUE)

mean_mgd <- power[1:4,1:3]
colnames(mean_mgd) <- c('Source', 'Power', 'MGD')

#plot bar graph
ggplot(data=power, aes(x=Year, y=MGD, fill = Source)) +
  geom_col(position=position_dodge(), colour = "gray") + 
  geom_hline(data = mean_mgd,aes(yintercept = MGD), size = .7, colour = "black",linetype = "dashed") +
  #geom_hline(yintercept = power$Average, size = .4, colour = "black",linetype = "dashed") +
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

filename <- paste("Power",paste(syear,"-",eyear, sep = ""),"Bar_Graph.pdf", sep="_")
ggsave(file=filename, path = paste("U:/OWS/Report Development/Annual Water Resources Report/October",eyear+1,"Report/Overleaf/",sep = " "), width=12, height=6)
