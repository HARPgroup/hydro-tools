options(scipen=999) #turn off scientific notation
library(sqldf)
library("kableExtra")

syear = 2016
eyear = 2020
mp_all <- read.csv(paste0("U:/OWS/foundation_datasets/awrr/",eyear+1,"/mp_all_wide_",syear,"-",eyear,".csv"))
fips <- read.csv("U:/OWS/foundation_datasets/wsp/wsp2020/fips_codes.csv")

table_caption <- paste0("Water Withdrawals Within Localities in ",eyear," (MGD)")
table_label   <- table_caption
#########################################################################################
GW <- paste('SELECT sum((X2020/365)) as "GW_Withdrawal", FIPS, b.name
                 FROM "mp_all" a
                 LEFT JOIN fips b
                 ON a.FIPS = b.code
                 WHERE Source_Type = "Groundwater"
                 GROUP BY FIPS' 
            ,sep='')
GW <- sqldf(GW)


SW <- paste('SELECT sum((X2020/365)) as "SW_Withdrawal", FIPS, b.name
                 FROM "mp_all" a
                 LEFT JOIN fips b
                 ON a.FIPS = b.code
                 WHERE Source_Type = "Surface Water"
                 GROUP BY FIPS' 
            ,sep='')
SW <- sqldf(SW)

TOTAL <- paste('SELECT sum((X2020/365)) as "Total_Withdrawal", FIPS, b.name
                 FROM "mp_all" a
                 LEFT JOIN fips b
                 ON a.FIPS = b.code
                 GROUP BY FIPS' 
               ,sep='')
TOTAL <- sqldf(TOTAL)
#########################################################################################
VA_TOTAL <- sqldf('SELECT sum(Total_withdrawal) AS Total_Withdrawal
                  FROM TOTAL') #VA total for calculating percent of total for each locality

all_fips <- paste('SELECT a.name, a.code, b.GW_Withdrawal, c.SW_Withdrawal, d.Total_Withdrawal, (d.Total_Withdrawal/',VA_TOTAL,')*100 AS Percent_of_Total_Withdrawal
                 FROM fips a
                 LEFT OUTER JOIN "GW" b
                 ON a.code = b.FIPS
                 LEFT OUTER JOIN "SW" c
                 ON a.code = c.FIPS
                 LEFT OUTER JOIN "TOTAL" d
                 ON a.code = d.FIPS
                  GROUP BY a.code
                  ORDER BY a.name'
                  ,sep='')
all_fips <- sqldf(all_fips)

#ADD FINAL ROW OF TOTALS
final_row <- data.frame(name = "Total",
                        code = "",
                        GW_Withdrawal = sum(all_fips$GW_Withdrawal,na.rm=TRUE),
                        SW_Withdrawal = sum(all_fips$SW_Withdrawal,na.rm=TRUE),
                        Total_Withdrawal = VA_TOTAL,
                        Percent_of_Total_Withdrawal = sum(all_fips$Percent_of_Total_Withdrawal,na.rm=TRUE)
)
all_fips <- rbind(all_fips,final_row)

##ROUND TABLE VALUES, WITH FIPS_NUM COL FOR CSV
all_fips_round <- paste('SELECT name AS Locality,
                                code AS FIPS_CODE,
                                round(GW_Withdrawal,3) AS "GW Withdrawal",
                                round(SW_Withdrawal,3) AS "SW Withdrawal",
                                round(Total_Withdrawal,3) AS "Total Withdrawal",
                                round(Percent_of_Total_Withdrawal,1) AS "% of Total Withdrawal"
                 FROM "all_fips"'
                        ,sep='')
all_fips_round <- sqldf(all_fips_round)
all_fips_round <- sqldf('SELECT Locality, FIPS_CODE, CASE
        WHEN "GW Withdrawal" IS NULL
        THEN 0.00
        ELSE "GW Withdrawal"
        END AS "GW Withdrawal",
        CASE
        WHEN "SW Withdrawal" IS NULL
        THEN 0.00
        ELSE "SW Withdrawal"
        END AS "SW Withdrawal",
        CASE
        WHEN "Total Withdrawal" IS NULL
        THEN 0.00
        ELSE "Total Withdrawal"
        END AS "Total Withdrawal",
        "% of Total Withdrawal"
      FROM all_fips_round')
write.csv(all_fips_round, paste("U:/OWS/foundation_datasets/awrr/",eyear+1,"/ByLocality.csv",sep=""), row.names = F)

# ROUND TABLE VALUES, WITHOUT FIPS_NUM COL 
all_fips_round <- paste('SELECT name AS Locality, 
                                round(GW_Withdrawal,2) AS "GW Withdrawal",
                                round(SW_Withdrawal,2) AS "SW Withdrawal",
                                round(Total_Withdrawal,2) AS "GW + SW Total",
                                round(Percent_of_Total_Withdrawal,2) AS "% of Total Withdrawal"
                 FROM "all_fips"' 
                        ,sep='')
all_fips_round <- sqldf(all_fips_round)
options(knitr.kable.NA = "0.00")
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
          kable_styling(latex_options = "striped")

  cat(ktable, file = paste("U:/OWS/Report Development/Annual Water Resources Report/October ",eyear+1," Report/overleaf/ByLocality.tex",sep = '')) #USED TO SAVE .tex FILE OF KABLE OUTPUT

# BY LOCALITY BY USE TYPE #######################################################################  

  GW <- paste('SELECT sum((X2020/365)) as "GW_Withdrawal", FIPS, b.name, a.Use_Type
                 FROM "mp_all" a
                 LEFT JOIN fips b
                 ON a.FIPS = b.code
                 WHERE Source_Type = "Groundwater"
                 GROUP BY FIPS, Use_Type' 
              ,sep='')
  GW <- sqldf(GW)
  
  
  SW <- paste('SELECT sum((X2020/365)) as "SW_Withdrawal", FIPS, b.name, a.Use_Type
                 FROM "mp_all" a
                 LEFT JOIN fips b
                 ON a.FIPS = b.code
                 WHERE Source_Type = "Surface Water"
                 GROUP BY FIPS, Use_Type' 
              ,sep='')
  SW <- sqldf(SW)
  
  TOTAL <- paste('SELECT sum((X2020/365)) as "Total_Withdrawal", FIPS, b.name, a.Use_Type
                 FROM "mp_all" a
                 LEFT JOIN fips b
                 ON a.FIPS = b.code
                 GROUP BY FIPS, Use_Type' 
                 ,sep='')
  TOTAL <- sqldf(TOTAL)
  #########################################################################################
  VA_TOTAL <- sqldf('SELECT sum(Total_withdrawal) AS Total_Withdrawal
                  FROM TOTAL') #VA total for calculating percent of total for each locality
  
  mp_all_fips <- sqldf('SELECT * FROM mp_all a LEFT OUTER JOIN fips b ON a.FIPS = b.code')
  fips_use <- sqldf('SELECT FIPS AS FIPS_Code, code AS FIPS, name, Use_Type FROM mp_all_fips GROUP BY code, name, Use_type')
  
  
  library(plyr)
  a <- join_all(list(fips_use, GW, SW, TOTAL), by=c('FIPS', 'Use_Type', 'name'), type='left')
aa <- sqldf('SELECT FIPS, name, Use_Type, GW_Withdrawal, SW_Withdrawal, Total_Withdrawal 
            FROM a
            ORDER BY name, Use_type')  
write.csv(aa, "U:/OWS/foundation_datasets/awrr/2021/Bylocality_UseType_table.csv")
