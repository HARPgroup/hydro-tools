# DOCUMENTATION -----------------------------------------------------------

# Loads previously downloaded data, trims it to proper time frame, removes
# lines of code where gage or model data is NA, area-adjusts data.

# LOADING LIBRARIES -------------------------------------------------------

library('lubridate')

# INPUTS ------------------------------------------------------------------

# Address of "DEQ_Model_vs_USGS_Comparison" folder
# Include "DEQ_Model_vs_USGS_Comparison" in address!
container <- "C:\\Users\\Daniel\\Documents\\HARP\\DEQ_Model_vs_USGS_Comparison"

# USGS Gage number
siteNo <- "03175500"

# Should new or original data be used?
new.or.original <- "new"

# CARRYOVER IF MASTER IS BEING RUN ----------------------------------------
if (exists("container.master") == TRUE) {
  container <- container.master
  siteNo <- siteNo.master
  new.or.original <- new.or.original.master
}

# NEW OR ORIGINAL DATA SWITCH ---------------------------------------------

if (new.or.original == "new") {
  container.cont <- "\\data\\new_(updated)_data"
} else if (new.or.original == "original") {
  container.cont <- "\\data\\original_(reproducible)_data"
} else {
  print("ERROR: neither new or original data specified")
}

# LINKING MODEL SEGMENT ---------------------------------------------------

gage.to.segment <- read.csv(file.path(container, "data", "Gage_To_Segment.csv"),
                            header = TRUE, sep = ',', stringsAsFactors = FALSE)
gage.to.segment <- subset(gage.to.segment, gage.to.segment$gage_number == as.numeric(siteNo))
RivSeg <- gage.to.segment$river_segment

# CREATING DIRECTORIES FOR DATA STORAGE -----------------------------------
dir.create(paste0(container,"\\data\\new_(updated)_data\\derived_data"), showWarnings = FALSE)
dir.create(paste0(container,"\\data\\new_(updated)_data\\derived_data\\trimmed_data"), showWarnings = FALSE)
dir.create(paste0(container,"\\data\\new_(updated)_data\\derived_data\\trimmed+area-adjusted_data"), showWarnings = FALSE)

# LOADING DATA ------------------------------------------------------------

data <- read.csv(paste0(container, container.cont, "/raw_data/merged_data/", siteNo, "_vs_", RivSeg," - Raw Data.csv"))

# REMOVING NA DATA --------------------------------------------------------

data <- data[complete.cases(data),]

# TRIMMING TO WATER YEAR --------------------------------------------------

data.length <- length(data$Date)
start.month <- month(data$Date[1])
end.month <- month(data$Date[data.length])
start.day <- day(data$Date[1])
end.day <- day(data$Date[data.length])

if (start.month <= 9) {
  start.year <- year(data$Date[1])
} else if (start.month == 10 & start.day == 1) {
  start.year <- year(data$Date[1])
} else {
  start.year <- year(data$Date[1]) + 1
}

if (end.month >= 10) {
  end.year <- year(data$Date[data.length])
} else if (end.month == 9 & end.day == 30) {
  end.year <- year(data$Date[data.length])
} else {
  end.year <- year(data$Date[data.length]) - 1
}

start.date <- paste0(start.year, "-10-01")
end.date <- paste0(end.year, "-09-30")

start.line <- which(data$Date == start.date)
end.line <- which(data$Date == end.date)

data <- data[start.line:end.line,]

# ELIMINATING UNNECCESARY COLUMNS -----------------------------------------

data <- data[,c("Date", "Flow", "mod.flow")]
data <- setNames(data, c("date", "gage.flow", "model.flow"))

# AREA-ADJUSTING MODEL FLOW -----------------------------------------------

# Model data is in acre-feet
# USGS gage data is in cfs
# The conversion factor from acre-feet to cfs is 0.504167

data$model.flow <- data$model.flow * 0.504167

# EXPORTING "TRIMMED FLOW" ------------------------------------------------

# Exporting "trimmed flow"
write.csv(data, file = paste0(container, "/data/new_(updated)_data/derived_data/trimmed_data/", siteNo, "_vs_", RivSeg," - Derived Data.csv"))

# AREA-ADJUSTING FLOW -----------------------------------------------------

gage.area <- gage.to.segment$gage_area
model.area <- gage.to.segment$segment_area
data$model.flow <- data$model.flow*(gage.area/model.area)

# EXPORTING "TRIMMED + AREA-ADJUSTED FLOW ---------------------------------

write.csv(data, file = paste0(container, "/data/new_(updated)_data/derived_data/trimmed+area-adjusted_data/", siteNo, "_vs_", RivSeg," - Derived Data.csv"))