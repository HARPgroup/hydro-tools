# Flow Exceedance Probability Grapher ------------------------------
# Given the inputs of a USGS gage and a GDoc formatted .csv, a 
# graph showing the probability of exceedance for each will be
# generated.  Created by Daniel Hildebrand.

# Inputs -----------------------------------------------------------
# To obtain the proper URL, open the .csv file in GDrive and 
# copy everything left of "/edit" ...
URL_model_daily <- "https://docs.google.com/spreadsheets/d/1TXVFeNmwmDRlnExqO3AN20TH5OjvRzgtdQvXrStQ1zc"
URL_model_daily <- paste0(URL_model_daily, "/pub?output=csv")

# USGS gage number (usually 8 digits)
siteNo <- "02077500"

# River Segment name (for graph)
RiverSeg <- 'OD2_8830_8710'

# Loading Libraries ------------------------------------------------
library(dataRetrieval)

# Loading Model Data -----------------------------------------------
# Reads .csv file containing model data
model_daily = read.csv(URL_model_daily, header = TRUE, sep = ",", stringsAsFactors = FALSE);
# Makes model end on Sept. 30, 2005 (end of water year)
model_daily <- model_daily[(1:7944),]
flow_model <- model_daily[,5]

# Loading USGS Data ------------------------------------------------
pCode <- "00060"
start.date <- "1984-01-01"
end.date <- "2005-09-30"
# MUST end on 9-30 of a year (the end of the water year)

USGSdata <- readNWISdata(siteNumbers = siteNo, parameterCd = pCode,
                         startDate = start.date, endDate = end.date)

# Cleaning up names of USGSdata
names(USGSdata)
USGSdata <- renameNWISColumns(USGSdata)

# Turning USGSdata flow column into a vector
flow_USGS <- USGSdata$Flow

# Reordering vectors by magnitude of flow -------------------------
flow_model <- sort(flow_model, decreasing = TRUE)
flow_USGS <- sort(flow_USGS, decreasing = TRUE)

# Determining the "rank" (0-1) of the flow value ------------------
num_observations <- length(flow_model)
num_observations <- as.numeric(num_observations)
rank_vec <- c(1:num_observations)
rank_vec <- as.numeric(rank_vec)

# Calculating exceedance probability
prob_exceedance <- 100*((rank_vec) / (num_observations + 1))

# Creating flow exceedance plots
# Determining max flow value for plot scale
max <- max(c(max(flow_model), max(flow_USGS)))

# Creating names for plot legends
name_USGS <- paste('Gage', siteNo)
name_model <- paste('Model: River Seg.\n', RiverSeg)

par(cex = 1.1, lwd = 2, mar = c(4.5,4.2,0.5,0.5))
plot(prob_exceedance, flow_model, type = 'l', col = 'blue', 
     log = 'y', ylim = c(1, max), xlim = c(0,100),
     xlab = 'Probability of Exceedance (%)', ylab = 'Flow (cfs)')
lines(prob_exceedance, flow_USGS, col = 'red')
legend('topright', legend = c(name_USGS, name_model),
       bty = 'n', col = c('red','blue'), lwd = 2, cex = 0.9, 
       y.intersp = 0.6, x.intersp = 0.8)

