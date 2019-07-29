library("readxl")

source <- "2019AWRR-TablesGraphs.xlsx"
folder <- "U:/OWS/Report Development/Annual Water Resources Report/October 2019 Report/Water Use Exports/"
sheet <- "PWS"

data <- read_excel(paste(folder,source,sep=""),sheet)
kable(data, "latex", booktabs = T) %>%
  kable_styling(latex_options = "striped")

