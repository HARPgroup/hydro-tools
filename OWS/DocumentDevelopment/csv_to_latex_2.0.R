library(xtable)
library("readxl")

source <- "2019AWRR-TablesGraphs.xlsx"
folder <- "C:/Users/nrf46657/Desktop/LaTeX/"
#sheet <- "PWS"
#sheet <- "ByCounty"
sheet <- "Mining"

data <- read_excel(paste(folder,source,sep=""),sheet)


 ############################################################################
 mydf <- data
 xtab <- xtable(mydf)

 rws <- seq(1, (nrow(mydf)-1), by = 2)
 col <- rep("\\rowcolor[HTML]{CBCEFB}", length(rws))

 headercol <- "\\rowcolor[HTML]{6665CD}"

 col <- c(headercol,col)

 align(xtab)  <- "llXXXXXXXXXX|"
 #align(xtab)  <- "llXXXX"

 bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}

 print(xtab,
       file = paste(folder,sheet,".tex",sep=""),
       tabular.environment="tabularx",
       #tabular.environment="longtable",
       #tabular.environment="longtable",
       add.to.row = list(pos = as.list(c(-1,rws)), command = col),
       include.rownames = FALSE,
       width="\\textwidth",
       split=yes,
       size="\\fontsize{8pt}{10pt}\\selectfont"#,
       #sanitize.colnames.function=bold
 )




