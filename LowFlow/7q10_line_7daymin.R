## Graph of how 7 day minimums change over time
rm(list = ls())   # clear variables
# Load necessary libraries
suppressPackageStartupMessages(library('lubridate'))
suppressPackageStartupMessages(library('zoo'))
suppressPackageStartupMessages(library('stringr'))
suppressPackageStartupMessages(library('PearsonDS'))
suppressPackageStartupMessages(library('plyr'))
suppressPackageStartupMessages(library('ggplot2'))

#update to file location of config.local.private
config_file <- "C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\hydro-tools\\"

#----------------------------------------------------------------------------------------
#load functions
source(paste(config_file,'config.local.private',sep='/'))
#save_directory <- "/var/www/html/images/dh/dev"
save_directory <- paste(repo_location,"plots",sep="")
dir.create(save_directory, showWarnings = FALSE) #create "plots" directory if doesn't exist 
source(paste(repo_location,"hydro-tools\\USGS\\usgs_gage_functions.R", sep = ""))

# Initialize variables
x7q10 <- c()
yrs_record <- c()
current_date <- Sys.Date()

## Finding all active VA stream gages
# URL to list of all active streamgages in VA
url <- "http://waterservices.usgs.gov/nwis/dv/?format=rdb&stateCd=va&siteStatus=active&variable=00060"


# Determining how many active gages there are to specify when to stop reading data
all_info <- scan(url, what="character", skip=10, nlines=1)
num_sites <- as.numeric(all_info[6])
gage_info <- scan(url, what="character", skip=11, sep="\t", nlines=num_sites)


# Setting up a pattern to pull only the gage numbers from the string
pattern <- "([[:digit:]]{8})"
x <- str_extract_all(gage_info, pattern)

gage <- as.character(x) #keeps leading zero
### Calculating 7 Day minimums for year and each gage and graphing ###

#i <- 10
for (i in 1:length(gage)) {   
  
  # reset for next gage
  min_7day <- c()
  years <- c()
  historic <- c()
  flows <- c()
  g2 <- c()
  ann_evnts <- c()
  
  # Getting historic record for each site
  if (nchar(gage[i]) == 8) {
    data <- streamgage_historic(gage[i])
  } else {
    next
  }
  
  
  # Create graph for gage without data
  if (class(data)=="try-error") { 
    min_7day <- 0
    years <- 0
    plot_title <- paste("Gage", gage[i], "- Change in 7 Day Minimums")
    plot_subtitle <- "NO DATA AVAILABLE FOR THIS GAGE"
    filename <- paste("/usgs_", gage[i], "_7q10_line_7daymin.png", sep="")
    png(paste(save_directory, filename, sep="")) 
    df <- data.frame(Years=years, Min7Day = min_7day)
    print(ggplot(df) + 
            geom_line(aes(x=Years, y=Min7Day)) + 
            ggtitle(bquote(atop(.(plot_title), atop(italic(.(plot_subtitle)), "")))) +
            xlab("Year") + ylab("Minimum 7 Day Average, cfs") + 
            xlim(1838, 2014) + ylim(0,1) )
    dev.off()
    next
  } 
  
  date <- as.character(data[1,]$Date)
  
  # Catch only full water years (dates past October 1st will not give a full water year) 
  # year is equal to the water year
  if (month(date) == 10 && day(date) > 1) {
    year <- year(date) + 2
  } else if (month(date) > 10) {
    year <- year(date) + 2
  } else { year <- year(date) + 1 }
  
  
  # Get all flow data on record
  historic <- data
  
  # Creates graph for gages that do not even have a full year of data
  if (length(historic$Flow) < 365) {
    min_7day <- 0
    years <- 0
    plot_title <- paste("Gage", gage[i], "- Change in 7 Day Minimums from", year+1, "to", year(current_date))
    plot_subtitle <- "INSUFFICIENT DATA FOR ANNUAL 7 DAY MINIMUM FLOW"
    filename <- paste("/usgs_", gage[i], "_7q10_line_7daymin.png", sep="")
    png(paste(save_directory, filename, sep="")) 
    df <- data.frame(Years=years, Min7Day = min_7day)
    print(ggplot(df) + 
            geom_line(aes(x=Years, y=Min7Day)) + 
            ggtitle(bquote(atop(.(plot_title), atop(italic(.(plot_subtitle)), "")))) +
            xlab("Year") + ylab("Minimum 7 Day Average, cfs") + 
            xlim(year+1,year(current_date)) + ylim(0,1) )
    dev.off()
    next
  }
  
  # Getting flow data ordered by date 	   
  #flows <- zoo(as.numeric(historic[,4]), order.by = historic$datetime)
  flows <- zoo(as.numeric(historic$Flow), order.by = historic$Date)
  
  # When no flow values exist (happens for 'Rat' values)
  if (length(na.omit(flows)) == 0) {
    min_7day <- 0
    years <- 0
    plot_title <- paste("Gage", gage[i], "- Change in 7 Day Minimums from", year+1, "to", year(current_date))
    plot_subtitle <- "INSUFFICIENT DATA FOR ANNUAL 7 DAY MINIMUM FLOW"
    filename <- paste("/usgs_", gage[i], "_7q10_line_7daymin.png", sep="")
    png(paste(save_directory, filename, sep="")) 
    df <- data.frame(Years=years, Min7Day = min_7day)
    print(ggplot(df) + 
            geom_line(aes(x=Years, y=Min7Day)) + 
            ggtitle(bquote(atop(.(plot_title), atop(italic(.(plot_subtitle)), "")))) +
            xlab("Year") + ylab("Minimum 7 Day Average, cfs") + 
            xlim(year+1,year(current_date)) + ylim(0,1) )
    dev.off()
    next
  }
  
  # Calculating 7 day minimums for every water year on record
  g2 <- group2(na.omit(flows)) 
  
  
  ### 7Q10 Calculation ###
  
  ann_evnts <- as.matrix(g2["7 Day Min"])	 
  
  # Creating a vector with no zero events
  nonzero <- c()					#set up vector 'nonzero'
  v <- 0 						#set up counting vector 'v'
  for (k in 1:length(ann_evnts)) {
    if (ann_evnts[k] > 10^-6) {		#any positive, assumed nonzero, numbers
      v <- v+1				#counting vector for building nonzero vector
      nonzero[v] <- ann_evnts[k]	#only nonzero 7 day min annual values 
    } 
  }
  
  
  if (length(nonzero) == 0) {
    min_7day <- 0
    years <- 0
    plot_title <- paste("Gage", gage[i], "- Change in 7 Day Minimums from", year+1, "to", year(current_date))
    plot_subtitle <- "NEGLIGIBLE FLOWS FOR ANNUAL 7 DAY MINIMUM FLOW"
    filename <- paste("/usgs_", gage[i], "_7q10_line_7daymin.png", sep="")
    png(paste(save_directory, filename, sep="")) 
    df <- data.frame(Years=years, Min7Day = min_7day)
    print(ggplot(df) + 
            geom_line(aes(x=Years, y=Min7Day)) + 
            ggtitle(bquote(atop(.(plot_title), atop(italic(.(plot_subtitle)), "")))) +
            xlab("Year") + ylab("Minimum 7 Day Average, cfs") + 
            xlim(year+1,year(current_date)) + ylim(0,1) )
    dev.off()
    next
  }
  
  
  # Accounting for any zero events via adjusted probability
  n <- length(nonzero) 			#number of nonzero events in data set
  N <- length(ann_evnts)			#number of events in data set
  p <- 1/10					#probability of a 10 year return peroiod = 1 in 10 chance
  Padj <- ((p*N)/n)-((N-n)/n)		#adjusted probability to account for any zero events
  #if no zero events, Padj will equal p
  
  # Adjusting for negative Padj (NaNs)
  if (Padj < 0) {
    x7q10 <- 0
  } else {
    # Log-Pearson using the adjusted probability and nonzero annual events
    x <- log(nonzero)
    pars <- PearsonDS:::pearsonIIIfitML(x)
    x7q10 <- exp(qpearsonIII(Padj, params = pars$par))
  }
  
  ### End of 7Q10 Calculation ###
  
  
  # Gets the 7 day minimums and corresponding years for all complete water years
  min_7day <- g2["7 Day Min"][g2["year"] >= (year)]
  years <- g2["year"][g2["year"] >= (year)]
  
  # Vectors set up for plot
  plot_title <- paste("Gage", gage[i], "- Change in 7 Day Minimums from", year+1, "to", tail(years,1))
  plot_subtitle <- ""
  xscale <- c(seq(round_any(year, 5), round_any(year(current_date), 5, f=ceiling), by=5))	
  df <- data.frame(Years=years, Min7Day = min_7day)
  current_7daymin <- round(tail(min_7day,1), digits=2)
  label <- c(paste("7 day minimum =", current_7daymin, "cfs"), paste("7Q10 =", round(x7q10, digits=2), "cfs"))
  
  # Saving file to the correct location
  filename <- paste("/usgs_", gage[i], "_7q10_line_7daymin.png", sep="")
  file_location <- paste(save_directory, filename, sep="")
  write(c(), file = file_location)  #create file where image will be saved
  png(file_location) 
  
  # Line chart to show the change in the 7 day minimum flows over the period of record
  print( ggplot(df) + 
           geom_line(aes(x=Years, y=Min7Day)) + 
           geom_point(aes(x=tail(years, 1), y=current_7daymin, color=label[1]), size=10, pch='-') +
           geom_hline(aes(yintercept=x7q10, color=label[2]), linetype=5, size=1) +	
           scale_color_manual(name="2014 Values", values=c("#339933", "#3399FF"), labels=label) +
           theme(legend.position="bottom") +
           guides(color=guide_legend(nrow=2)) +
           scale_x_continuous(breaks=xscale) +
           ggtitle(bquote(atop(.(plot_title), atop(italic(.(plot_subtitle)), "")))) +
           xlab("Year") + ylab("Minimum 7 Day Average, cfs")  )
  dev.off()
  
}   
