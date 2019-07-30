library("readxl")

source <- "2019AWRR-TablesGraphs.xlsx"
folder <- "U:/OWS/Report Development/Annual Water Resources Report/October 2019 Report/Water Use Exports/"
sheet <- "PWS"

data <- read_excel(paste(folder,source,sep=""),sheet)
kable(data, "latex", booktabs = T) %>%
  kable_styling(latex_options = "striped") %>%
  column_spec(1, width = "0.75in")%>%
  column_spec(2:11, width = "0.5in")

kable(data, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down"))
