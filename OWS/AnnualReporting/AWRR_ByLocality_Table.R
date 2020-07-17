options(scipen=999) #turn off scientific notation
library(sqldf)
library("kableExtra")

save_directory <- "C:/Users/jklei/Desktop/AWRR_2020/Appendix5_7.17.20"

mp_all <- read.csv("C:/Users/jklei/Desktop/AWRR_2020/Appendix5_7.17.20/foundation_dataset_7.17.20/mp_all_2015-2019.csv", header = TRUE, sep = ",")
mp_all <- mp_all[,!(names(mp_all) %in% c("locality"))] #remove duplicate locality name column
LocalityCodeMapping <- read.csv("C:/Users/jklei/Desktop/AWRR_2020/Appendix5_7.17.20/foundation_dataset_7.17.20/LocalityCodeMapping.csv", header = TRUE, sep = ",")

table_caption <- "Water Withdrawals Within Localities in 2019 (MGD)"
table_label   <- "Water Withdrawals Within Localities in 2019 (MGD)"
#########################################################################################
GW <- paste('SELECT sum(mgd) as "GW_Withdrawal", FIPS_NUM, b.Locality
                 FROM "mp_all" a
                 LEFT JOIN "LocalityCodeMapping" b
                 ON a.FIPS = b.FIPS_NUM
                 WHERE a.YEAR = 2019 AND Source_Type = "Groundwater"
                 GROUP BY FIPS_NUM' 
            ,sep='')
GW <- sqldf(GW)


SW <- paste('SELECT sum(mgd) as "SW_Withdrawal" , FIPS_NUM, b.Locality
                 FROM "mp_all" a
                 LEFT JOIN "LocalityCodeMapping" b
                 ON a.FIPS = b.FIPS_NUM
                 WHERE a.YEAR = 2019 AND Source_Type = "Surface Water"
                 GROUP BY FIPS_NUM' 
            ,sep='')
SW <- sqldf(SW)

TOTAL <- paste('SELECT sum(mgd) as "Total_Withdrawal", FIPS_NUM, b.Locality
                 FROM "mp_all" a
                 LEFT JOIN "LocalityCodeMapping" b
                 ON a.FIPS = b.FIPS_NUM
                 WHERE a.YEAR = 2019
                 GROUP BY FIPS_NUM' 
               ,sep='')
TOTAL <- sqldf(TOTAL)
#########################################################################################
VA_TOTAL <- sum(TOTAL$Total_Withdrawal) #VA total for calculating percent of total for each locality

all_fips <- paste('SELECT a.Locality, a.FIPS_NUM, b.GW_Withdrawal, c.SW_Withdrawal, d.Total_Withdrawal, d.Total_Withdrawal/',VA_TOTAL,'*100 AS Percent_of_Total_Withdrawal
                 FROM "LocalityCodeMapping" a
                 LEFT JOIN "GW" b
                 ON a.FIPS_NUM = b.FIPS_NUM
                 LEFT JOIN "SW" c
                 ON a.FIPS_NUM = c.FIPS_NUM
                 LEFT JOIN "TOTAL" d
                 ON a.FIPS_NUM = d.FIPS_NUM'
                  ,sep='')
all_fips <- sqldf(all_fips)

#ADD FINAL ROW OF TOTALS
final_row <- data.frame(Locality = "Total",
                        FIPS_NUM = "",
                        GW_Withdrawal = sum(all_fips$GW_Withdrawal,na.rm=TRUE),
                        SW_Withdrawal = sum(all_fips$SW_Withdrawal,na.rm=TRUE),
                        Total_Withdrawal = VA_TOTAL,
                        Percent_of_Total_Withdrawal = sum(all_fips$Percent_of_Total_Withdrawal,na.rm=TRUE)
)
all_fips <- rbind(all_fips,final_row)

# ROUND TABLE VALUES, WITH FIPS_NUM COL FOR CSV
# all_fips_round <- paste('SELECT Locality, 
#                                 FIPS_NUM,
#                                 round(GW_Withdrawal,3) AS "GW Withdrawal",
#                                 round(SW_Withdrawal,3) AS "SW Withdrawal",
#                                 round(Total_Withdrawal,3) AS "Total Withdrawal",
#                                 round(Percent_of_Total_Withdrawal,1) AS "% of Total Withdrawal"
#                  FROM "all_fips"' 
#                         ,sep='')
# all_fips_round <- sqldf(all_fips_round)
# write.csv(all_fips_round, paste(save_directory,"/ByLocality.csv",sep=""))

# ROUND TABLE VALUES, WITHOUT FIPS_NUM COL 
all_fips_round <- paste('SELECT Locality, 
                                round(GW_Withdrawal,2) AS "GW Withdrawal",
                                round(SW_Withdrawal,2) AS "SW Withdrawal",
                                round(Total_Withdrawal,2) AS "GW + SW Total",
                                round(Percent_of_Total_Withdrawal,2) AS "% of Total Withdrawal"
                 FROM "all_fips"' 
                        ,sep='')
all_fips_round <- sqldf(all_fips_round)

#OUTPUT KABLE LATEX TABLE
ktable <- kable(all_fips_round, 
                "latex", 
                longtable =T, 
                booktabs = T, 
                align = "lcccc",
                caption = table_caption,
                label = table_label) %>%
          row_spec(length(all_fips_round[,1]), bold = T) %>%
          #kable_styling(latex_options = c("repeat_header")) %>%
          kable_styling(latex_options = c("striped", "scale_down")) 
          options(knitr.kable.NA = "0.00")
          
cat(ktable, file = paste(save_directory,"/ByLocality.tex",sep=""), na = "0") #USED TO SAVE .tex FILE OF KABLE OUTPUT
