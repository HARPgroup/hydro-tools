library("readxl")
library("kableExtra")

source <- "2019AWRR-TablesGraphs.xlsx"
folder <- "U:/OWS/Report Development/Annual Water Resources Report/October 2019 Report/Water Use Exports/"

# sheet <- "2018TotalGWWithdrawals"
# sheet <- "2018TotalSurfaceWater"
# sheet <- "2018WithdrawalPercentage"
# sheet <- "Agr"
# sheet <- "AgTopByType"
# sheet <- "ByCounty"
# sheet <- "Commercial"
# sheet <- "CommercialTopByType"
# sheet <- "CWSPop"
# sheet <- "GWP2018"
# sheet <- "Irrig"
# sheet <- "IrrigationTopByType"
# sheet <- "Manufact"
# sheet <- "ManufacturingAllTop"
# sheet <- "ManufacturingGroundwaterTop"
# sheet <- "ManufacturingSurfaceWaterTop"
# sheet <- "Mining"
# sheet <- "MiningTopByType"
# sheet <- "MunicipalTopByType"
# sheet <- "PermittedUnpermittedByType"
# sheet <- "Power"
# sheet <- "PowerTopByType"
# sheet <- "Primary2018WithdrawalTable"
# sheet <- "PWS"
# sheet <- "SourceTypeTotal"
# sheet <- "TopUsers"
# sheet <- "TotalWithdrawals2018"
# sheet <- "TypeTopUsers"
# sheet <- "UnpermittedGW2018"
# sheet <- "UnpermittedSW2018"
# sheet <- "VWP2018"


###########################################################################
sheet <- "Table1"
data <- read_excel(paste(folder,source,sep=""),sheet)
kable(data, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>% 
  column_spec(7, width = "5em") %>%
  column_spec(8, width = "5em") %>%
  column_spec(9, width = "5em") %>%
  column_spec(10, width = "4em")

###########################################################################
sheet <- "TotalWithdrawals2018"
data <- read_excel(paste(folder,source,sep=""),sheet)
kable(data, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(1, width = "5em") %>%
  column_spec(2, width = "5em") %>%
  column_spec(3, width = "5em") %>%
  column_spec(4, width = "5em")

###########################################################################
sheet <- "PermittedUnpermittedByType"
data <- read_excel(paste(folder,source,sep=""),sheet)
kable(data, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down"))

###########################################################################
sheet <- "Primary2018WithdrawalTable"
data <- read_excel(paste(folder,source,sep=""),sheet)
kable(data, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(8, width = "5em") %>%
  column_spec(9, width = "5em") %>%
  column_spec(10, width = "5em") %>%
  column_spec(11, width = "5em")
###########################################################################

sheet <- "TopUsers"
data <- read_excel(paste(folder,source,sep=""),sheet)
kable(data, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(1, width = "15em") %>%
  column_spec(4, width = "15em") %>%
  column_spec(5, width = "8em")
###########################################################################

sheet <- "Agr"
data <- read_excel(paste(folder,source,sep=""),sheet)
kable(data, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(7, width = "5em") %>%
  column_spec(8, width = "5em") %>%
  column_spec(9, width = "5em") %>%
  column_spec(10, width = "5em")
###########################################################################

sheet <- "AgTopByType"
data <- read_excel(paste(folder,source,sep=""),sheet)
kable(data, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(1, width = "20em")
###########################################################################

sheet <- "Irrig"
data <- read_excel(paste(folder,source,sep=""),sheet)
kable(data, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(8, width = "5em") %>%
  column_spec(9, width = "5em") %>%
  column_spec(10, width = "5em")
###########################################################################

sheet <- "IrrigationTopByType"
data <- read_excel(paste(folder,source,sep=""),sheet)
kable(data, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(4, width = "10em") 
###########################################################################

sheet <- "Commercial"
data <- read_excel(paste(folder,source,sep=""),sheet)
kable(data, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(7, width = "5em") %>%
  column_spec(8, width = "5em") %>%
  column_spec(9, width = "5em") %>%
  column_spec(10, width = "5em")
###########################################################################

sheet <- "Mining"
data <- read_excel(paste(folder,source,sep=""),sheet)
kable(data, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(7, width = "5em") %>%
  column_spec(8, width = "5em") %>%
  column_spec(9, width = "5em") %>%
  column_spec(10, width = "5em")
###########################################################################

sheet <- "MiningTopByType"
data <- read_excel(paste(folder,source,sep=""),sheet)
kable(data, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(1, width = "20em")
###########################################################################

sheet <- "Manufact"
data <- read_excel(paste(folder,source,sep=""),sheet)
kable(data, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(7, width = "5em") %>%
  column_spec(8, width = "5em") %>%
  column_spec(9, width = "5em") %>%
  column_spec(10, width = "5em")
###########################################################################

sheet <- "ManufacturingAllTop"
data <- read_excel(paste(folder,source,sep=""),sheet)
kable(data, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(1, width = "20em")
###########################################################################

sheet <- "ManufacturingGroundwaterTop"
data <- read_excel(paste(folder,source,sep=""),sheet)
kable(data, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(1, width = "20em")
###########################################################################

sheet <- "ManufacturingSurfaceWaterTop"
data <- read_excel(paste(folder,source,sep=""),sheet)
kable(data, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(1, width = "20em")
###########################################################################

sheet <- "PWS"
data <- read_excel(paste(folder,source,sep=""),sheet)
kable(data, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(7, width = "5em") %>%
  column_spec(8, width = "5em") %>%
  column_spec(9, width = "5em") %>%
  column_spec(10, width = "5em")
###########################################################################

sheet <- "MunicipalTopByType"
data <- read_excel(paste(folder,source,sep=""),sheet)
kable(data, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(1, width = "20em")
###########################################################################

sheet <- "CWSPop"
data <- read_excel(paste(folder,source,sep=""),sheet)
kable(data, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(2, width = "12em") %>%
  column_spec(3, width = "12em") %>%
  column_spec(4, width = "12em")
###########################################################################

sheet <- "Power"
data <- read_excel(paste(folder,source,sep=""),sheet)
kable(data, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(7, width = "5em") %>%
  column_spec(8, width = "5em") %>%
  column_spec(9, width = "5em") %>%
  column_spec(10, width = "5em")
###########################################################################

sheet <- "PowerTopByType"
data <- read_excel(paste(folder,source,sep=""),sheet)
kable(data, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(1, width = "20em")
###########################################################################

sheet <- "ByCounty"
data <- read_excel(paste(folder,source,sep=""),sheet)
kable(data, "latex", longtable =T, booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) 
###########################################################################

sheet <- "ByCountyNEW-TEXT-ONLY"
data <- read_excel(paste(folder,source,sep=""),sheet)
kable(data, "latex", longtable =T, booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) 
###########################################################################
