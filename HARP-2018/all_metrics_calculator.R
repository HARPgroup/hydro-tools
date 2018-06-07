# DESCRIPTION -----------------------------------------------------------------
# This code calculates each flow metric used in the USGS gage / model output
# comparison.  All changing inputs will be taken at the beginning of the script.
# The majority of the code comes from the fn_IHA.R and max_and_min_comparison.R
# file, with Alyssa Ford also contributing code calculating the Sep10%.  Compiled 
# by Daniel Hildebrand.

# INPUTS ----------------------------------------------------------------------
# Location of the folder containing "7Q10_ALF.R" (which contains metric functions)
fxn_locations = "C:\\Users\\Daniel\\Documents\\HARP\\Important R Scripts";

# URL to modified model output -- must contain 6 columns (obj. ID, year, month, day, ovol, date)
# Copy URL from an opened .csv on GDocs --> delete "edit#" and following characters from URL
# Add "pub?output=csv" at the end of the URL, after a slash
URL_model_daily <- 'https://docs.google.com/spreadsheets/d/1ASLVhU8uCIuSB-Hs3pnQSpRynfMWClV0Czy432Bn6x8/pub?output=csv'

# USGS Gage Number
siteNo <- "03488000"

# LOADING LIBRARIES AND PREP --------------------------------------------------
source(paste(fxn_locations, "7Q10_ALF.R", sep = "/"));

library(pander);
library(httr);
library(dataRetrieval);
library(zoo);
library(lubridate);

# LOADS MODEL DATA ------------------------------------------------------------
model_daily = read.csv(URL_model_daily, header = TRUE, sep = ",", stringsAsFactors = FALSE);
# Makes model end on Sept. 30, 2005 (end of water year)
model_daily <- model_daily[(1:7944),]

Flow_model <- model_daily[,c(6,5)]
Flow_modelv <- as.vector(Flow_model)
colnames(Flow_modelv) <- c("Date", "Flow")
Flow_modelv$Date <- as.POSIXct(Flow_modelv$Date)
f3_model <- zoo(Flow_modelv$Flow, order.by = Flow_modelv$Date)
g2_model <- group2(f3_model)

Flow_modelx <- zoo(Flow_modelv$Flow, order.by = Flow_modelv$Date);

# LOADS USGS GAGE DATA --------------------------------------------------------
pCode <- "00060"
start.date <- "1984-01-01"
end.date <- "2005-09-30"
# NOTE: fn_iha_mlf will fail if it lacks a complete water year, so date must end on 9-30

yahara <- readNWISdv(siteNumbers = siteNo,
                     parameterCd = pCode,
                     startDate = start.date,
                     endDate = end.date)

# Cleans up names
names(yahara)
yahara <- renameNWISColumns(yahara)

# Makes date posix
flows_USGS <- zoo(yahara[,"Flow"], order.by = yahara$Date)

# Setup For ___ Day Min Calculations
Flow_USGS <- yahara[,c(3,4)]
Flow_USGSv <- as.vector(Flow_USGS)
Flow_USGSv$Date <- as.POSIXct(Flow_USGSv$Date)
f3_USGS <- zoo(Flow_USGSv$Flow, order.by = Flow_USGSv$Date)
g2_USGS <- group2(f3_USGS)

# Setup for Monthly Averages Calculations
yahara$Month <- month(ymd(yahara$Date))
USGS_Monthly_Means <- aggregate(yahara$Flow, list(yahara$Month), FUN = mean)
Model_Monthly_Means <- aggregate(model_daily$ovol, list(model_daily$month), FUN = mean)

# JANUARY LOW FLOW ------------------------------------------------------------
met01_Gage_JanLF <- round(fn_iha_mlf(flows_USGS,1)/0.5)*0.5
met01_Mod_JanLF <- round(fn_iha_mlf(Flow_modelx,1)/0.5)*0.5
met01_PctError <- round(((met01_Mod_JanLF - met01_Gage_JanLF) / met01_Gage_JanLF)*1000)*0.1

# FEBRUARY LOW FLOW -----------------------------------------------------------
met02_Gage_FebLF <- round(fn_iha_mlf(flows_USGS,2)/0.5)*0.5
met02_Mod_FebLF <- round(fn_iha_mlf(Flow_modelx,2)/0.5)*0.5
met02_PctError <- round(((met02_Mod_FebLF - met02_Gage_FebLF) / met02_Gage_FebLF)*1000)*0.1

# MARCH LOW FLOW --------------------------------------------------------------
met03_Gage_MarLF <- round(fn_iha_mlf(flows_USGS,3)/0.5)*0.5
met03_Mod_MarLF <- round(fn_iha_mlf(Flow_modelx,3)/0.5)*0.5
met03_PctError <- round(((met03_Mod_MarLF - met03_Gage_MarLF) / met03_Gage_MarLF)*1000)*0.1

# APRIL LOW FLOW --------------------------------------------------------------
met04_Gage_AprLF <- round(fn_iha_mlf(flows_USGS,4)/0.5)*0.5
met04_Mod_AprLF <- round(fn_iha_mlf(Flow_modelx,4)/0.5)*0.5
met04_PctError <- round(((met04_Mod_AprLF - met04_Gage_AprLF) / met04_Gage_AprLF)*1000)*0.1

# MAY LOW FLOW ----------------------------------------------------------------
met05_Gage_MayLF <- round(fn_iha_mlf(flows_USGS,5)/0.5)*0.5
met05_Mod_MayLF <- round(fn_iha_mlf(Flow_modelx,5)/0.5)*0.5
met05_PctError <- round(((met05_Mod_MayLF - met05_Gage_MayLF) / met05_Gage_MayLF)*1000)*0.1

# JUNE LOW FLOW ---------------------------------------------------------------
met06_Gage_JunLF <- round(fn_iha_mlf(flows_USGS,6)/0.5)*0.5
met06_Mod_JunLF <- round(fn_iha_mlf(Flow_modelx,6)/0.5)*0.5
met06_PctError <- round(((met06_Mod_JunLF - met06_Gage_JunLF) / met06_Gage_JunLF)*1000)*0.1

# JULY LOW FLOW ---------------------------------------------------------------
met07_Gage_JulLF <- round(fn_iha_mlf(flows_USGS,7)/0.5)*0.5
met07_Mod_JulLF <- round(fn_iha_mlf(Flow_modelx,7)/0.5)*0.5
met07_PctError <- round(((met07_Mod_JulLF - met07_Gage_JulLF) / met07_Gage_JulLF)*1000)*0.1

# AUGUST LOW FLOW -------------------------------------------------------------
met08_Gage_AugLF <- round(fn_iha_mlf(flows_USGS,8)/0.5)*0.5
met08_Mod_AugLF <- round(fn_iha_mlf(Flow_modelx,8)/0.5)*0.5
met08_PctError <- round(((met08_Mod_AugLF - met08_Gage_AugLF) / met08_Gage_AugLF)*1000)*0.1

# SEPTEMBER LOW FLOW ----------------------------------------------------------
met09_Gage_SepLF <- round(fn_iha_mlf(flows_USGS,9)/0.5)*0.5
met09_Mod_SepLF <- round(fn_iha_mlf(Flow_modelx,9)/0.5)*0.5
met09_PctError <- round(((met09_Mod_SepLF - met09_Gage_SepLF) / met09_Gage_SepLF)*1000)*0.1

# OCTOBER LOW FLOW ------------------------------------------------------------
met10_Gage_OctLF <- round(fn_iha_mlf(flows_USGS,10)/0.5)*0.5
met10_Mod_OctLF <- round(fn_iha_mlf(Flow_modelx,10)/0.5)*0.5
met10_PctError <- round(((met10_Mod_OctLF - met10_Gage_OctLF) / met10_Gage_OctLF)*1000)*0.1

# NOVEMBER LOW FLOW -----------------------------------------------------------
met11_Gage_NovLF <- round(fn_iha_mlf(flows_USGS,11)/0.5)*0.5
met11_Mod_NovLF <- round(fn_iha_mlf(Flow_modelx,11)/0.5)*0.5
met11_PctError <- round(((met11_Mod_NovLF - met11_Gage_NovLF) / met11_Gage_NovLF)*1000)*0.1

# DECEMBER LOW FLOW ------------------------------------------------------------
met12_Gage_DecLF <- round(fn_iha_mlf(flows_USGS,12)/0.5)*0.5
met12_Mod_DecLF <- round(fn_iha_mlf(Flow_modelx,12)/0.5)*0.5
met12_PctError <- round(((met12_Mod_DecLF - met12_Gage_DecLF) / met12_Gage_DecLF)*1000)*0.1

# 1 DAY MIN --------------------------------------------------------------------
yearly_Gage_1DayMin <- g2_USGS[,c(1,2)]
met13_Gage_1DayMin <- round(mean(yearly_Gage_1DayMin$`1 Day Min`)/0.1)*0.1
yearly_Mod_1DayMin <- g2_model[,c(1,2)]
met13_Mod_1DayMin <- round(mean(yearly_Mod_1DayMin$`1 Day Min`)/0.1)*0.1
met13_PctError <- round(((met13_Mod_1DayMin - met13_Gage_1DayMin) / met13_Gage_1DayMin)*1000)*0.1

# 3 DAY MIN --------------------------------------------------------------------
yearly_Gage_3DayMin <- g2_USGS[,c(1,4)]
met14_Gage_3DayMin <- round(mean(yearly_Gage_3DayMin$`3 Day Min`)/0.1)*0.1
yearly_Mod_3DayMin <- g2_model[,c(1,4)]
met14_Mod_3DayMin <- round(mean(yearly_Mod_3DayMin$`3 Day Min`)/0.1)*0.1
met14_PctError <- round(((met14_Mod_3DayMin - met14_Gage_3DayMin) / met14_Gage_3DayMin)*1000)*0.1

# 7 DAY MIN --------------------------------------------------------------------
yearly_Gage_7DayMin <- g2_USGS[,c(1,6)]
met15_Gage_7DayMin <- round(mean(yearly_Gage_7DayMin$`7 Day Min`)/0.1)*0.1
yearly_Mod_7DayMin <- g2_model[,c(1,6)]
met15_Mod_7DayMin <- round(mean(yearly_Mod_7DayMin$`7 Day Min`)/0.1)*0.1
met15_PctError <- round(((met15_Mod_7DayMin - met15_Gage_7DayMin) / met15_Gage_7DayMin)*1000)*0.1

# 30 DAY MIN ------------------------------------------------------------------
yearly_Gage_30DayMin <- g2_USGS[,c(1,8)]
met16_Gage_30DayMin <- round(mean(yearly_Gage_30DayMin$`30 Day Min`)/0.1)*0.1
yearly_Mod_30DayMin <- g2_model[,c(1,8)]
met16_Mod_30DayMin <- round(mean(yearly_Mod_30DayMin$`30 Day Min`)/0.1)*0.1
met16_PctError <- round(((met16_Mod_30DayMin - met16_Gage_30DayMin) / met16_Gage_30DayMin)*1000)*0.1

# 90 DAY MIN ------------------------------------------------------------------
yearly_Gage_90DayMin <- g2_USGS[,c(1,10)]
met17_Gage_90DayMin <- round(mean(yearly_Gage_90DayMin$`90 Day Min`)/0.1)*0.1
yearly_Mod_90DayMin <- g2_model[,c(1,10)]
met17_Mod_90DayMin <- round(mean(yearly_Mod_90DayMin$`90 Day Min`)/0.1)*0.1
met17_PctError <- round(((met17_Mod_90DayMin - met17_Gage_90DayMin) / met17_Gage_90DayMin)*1000)*0.1

# 7Q10 ------------------------------------------------------------------------
met18_Gage_7Q10 <- round(fn_iha_7q10(flows_USGS)/0.1)*0.1
met18_Model_7Q10 <- round(fn_iha_7q10(Flow_modelx)/0.1)*0.1
met18_PctError <- round(((met18_Model_7Q10 - met18_Gage_7Q10) / met18_Gage_7Q10)*1000)*0.1

# DROUGHT OF RECORD -----------------------------------------------------------
met19_Gage_DoR <- fn_iha_DOR(flows_USGS)
met19_Model_DoR <- fn_iha_DOR(Flow_modelx)
met19_PctError <- round(((met19_Model_DoR - met19_Gage_DoR) / met19_Gage_DoR)*1000)*0.1

# DROUGHT OF RECORD YEAR ------------------------------------------------------
met20_Gage_DoR <- fn_iha_DOR_Year(flows_USGS)
met20_Model_DoR <- fn_iha_DOR_Year(Flow_modelx)
if (met20_Gage_DoR == met20_Model_DoR) {
  met20_PctError <- 0
} else {
  met20_PctError <- 100
}

# JANUARY MEAN FLOW -----------------------------------------------------------
met21_Gage_JanMF <- round(USGS_Monthly_Means[1,2]/0.5)*0.5
met21_Model_JanMF <- round(Model_Monthly_Means[1,2]/0.5)*0.5
met21_PctError <- round(((met21_Model_JanMF - met21_Gage_JanMF) / met21_Gage_JanMF)*1000)*0.1

# FEBRUARY MEAN FLOW ----------------------------------------------------------
met22_Gage_FebMF <- round(USGS_Monthly_Means[2,2]/0.5)*0.5
met22_Model_FebMF <- round(Model_Monthly_Means[2,2]/0.5)*0.5
met22_PctError <- round(((met22_Model_FebMF - met22_Gage_FebMF) / met22_Gage_FebMF)*1000)*0.1

# MARCH MEAN FLOW -------------------------------------------------------------
met23_Gage_MarMF <- round(USGS_Monthly_Means[3,2]/0.5)*0.5
met23_Model_MarMF <- round(Model_Monthly_Means[3,2]/0.5)*0.5
met23_PctError <- round(((met23_Model_MarMF - met23_Gage_MarMF) / met23_Gage_MarMF)*1000)*0.1

# APRIL MEAN FLOW -------------------------------------------------------------
met24_Gage_AprMF <- round(USGS_Monthly_Means[4,2]/0.5)*0.5
met24_Model_AprMF <- round(Model_Monthly_Means[4,2]/0.5)*0.5
met24_PctError <- round(((met24_Model_AprMF - met24_Gage_AprMF) / met24_Gage_AprMF)*1000)*0.1

# MAY MEAN FLOW ---------------------------------------------------------------
met25_Gage_MayMF <- round(USGS_Monthly_Means[5,2]/0.5)*0.5
met25_Model_MayMF <- round(Model_Monthly_Means[5,2]/0.5)*0.5
met25_PctError <- round(((met25_Model_MayMF - met25_Gage_MayMF) / met25_Gage_MayMF)*1000)*0.1

# JUNE MEAN FLOW --------------------------------------------------------------
met26_Gage_JunMF <- round(USGS_Monthly_Means[6,2]/0.5)*0.5
met26_Model_JunMF <- round(Model_Monthly_Means[6,2]/0.5)*0.5
met26_PctError <- round(((met26_Model_JunMF - met26_Gage_JunMF) / met26_Gage_JunMF)*1000)*0.1

# JULY MEAN FLOW --------------------------------------------------------------
met27_Gage_JulMF <- round(USGS_Monthly_Means[7,2]/0.5)*0.5
met27_Model_JulMF <- round(Model_Monthly_Means[7,2]/0.5)*0.5
met27_PctError <- round(((met27_Model_JulMF - met27_Gage_JulMF) / met27_Gage_JulMF)*1000)*0.1

# AUGUST MEAN FLOW ------------------------------------------------------------
met28_Gage_AugMF <- round(USGS_Monthly_Means[8,2]/0.5)*0.5
met28_Model_AugMF <- round(Model_Monthly_Means[8,2]/0.5)*0.5
met28_PctError <- round(((met28_Model_AugMF - met28_Gage_AugMF) / met28_Gage_AugMF)*1000)*0.1

# SEPTEMBER MEAN FLOW ---------------------------------------------------------
met29_Gage_SepMF <- round(USGS_Monthly_Means[9,2]/0.5)*0.5
met29_Model_SepMF <- round(Model_Monthly_Means[9,2]/0.5)*0.5
met29_PctError <- round(((met29_Model_SepMF - met29_Gage_SepMF) / met29_Gage_SepMF)*1000)*0.1

# OCTOBER MEAN FLOW -----------------------------------------------------------
met30_Gage_OctMF <- round(USGS_Monthly_Means[10,2]/0.5)*0.5
met30_Model_OctMF <- round(Model_Monthly_Means[10,2]/0.5)*0.5
met30_PctError <- round(((met30_Model_OctMF - met30_Gage_OctMF) / met30_Gage_OctMF)*1000)*0.1

# NOVEMBER MEAN FLOW ----------------------------------------------------------
met31_Gage_NovMF <- round(USGS_Monthly_Means[11,2]/0.5)*0.5
met31_Model_NovMF <- round(Model_Monthly_Means[11,2]/0.5)*0.5
met31_PctError <- round(((met31_Model_NovMF - met31_Gage_NovMF) / met31_Gage_NovMF)*1000)*0.1

# DECEMBER MEAN FLOW ----------------------------------------------------------
met32_Gage_DecMF <- round(USGS_Monthly_Means[12,2]/0.5)*0.5
met32_Model_DecMF <- round(Model_Monthly_Means[12,2]/0.5)*0.5
met32_PctError <- round(((met32_Model_DecMF - met32_Gage_DecMF) / met32_Gage_DecMF)*1000)*0.1

# CREATES OUTPUT MATRIX -------------------------------------------------------
OUTPUT_MATRIX <- matrix(c(met01_Gage_JanLF, met02_Gage_FebLF, 
                          met03_Gage_MarLF, met04_Gage_AprLF,
                          met05_Gage_MayLF, met06_Gage_JunLF,
                          met07_Gage_JulLF, met08_Gage_AugLF,
                          met09_Gage_SepLF, met10_Gage_OctLF,
                          met11_Gage_NovLF, met12_Gage_DecLF,
                          met13_Gage_1DayMin, met14_Gage_3DayMin,
                          met15_Gage_7DayMin, met16_Gage_30DayMin,
                          met17_Gage_90DayMin, met18_Gage_7Q10,
                          met19_Gage_DoR, met20_Gage_DoR,
                          met21_Gage_JanMF, met22_Gage_FebMF,
                          met23_Gage_MarMF, met24_Gage_AprMF,
                          met25_Gage_MayMF, met26_Gage_JunMF, 
                          met27_Gage_JulMF, met28_Gage_AugMF,
                          met29_Gage_SepMF, met30_Gage_OctMF, 
                          met31_Gage_NovMF, met32_Gage_DecMF,
                          met01_Mod_JanLF, met02_Mod_FebLF,
                          met03_Mod_MarLF, met04_Mod_AprLF,
                          met05_Mod_MayLF, met06_Mod_JunLF,
                          met07_Mod_JulLF, met08_Mod_AugLF,
                          met09_Mod_SepLF, met10_Mod_OctLF,
                          met11_Mod_NovLF, met12_Mod_DecLF,
                          met13_Mod_1DayMin, met14_Mod_3DayMin,
                          met15_Mod_7DayMin, met16_Mod_30DayMin,
                          met17_Mod_90DayMin, met18_Model_7Q10,
                          met19_Model_DoR, met20_Model_DoR,
                          met21_Model_JanMF, met22_Model_FebMF,
                          met23_Model_MarMF, met24_Model_AprMF,
                          met25_Model_MayMF, met26_Model_JunMF,
                          met27_Model_JulMF, met28_Model_AugMF,
                          met29_Model_SepMF, met30_Model_OctMF,
                          met31_Model_NovMF, met32_Model_DecMF,
                          met01_PctError, met02_PctError, met03_PctError,
                          met04_PctError, met05_PctError, met06_PctError,
                          met07_PctError, met08_PctError, met09_PctError,
                          met10_PctError, met11_PctError, met12_PctError,
                          met13_PctError, met14_PctError, met15_PctError,
                          met16_PctError, met17_PctError, met18_PctError,
                          met19_PctError, met20_PctError, met21_PctError, 
                          met22_PctError, met23_PctError, met24_PctError,
                          met25_PctError, met26_PctError, met27_PctError,
                          met28_PctError, met29_PctError, met30_PctError,
                          met31_PctError, met32_PctError), 
                        nrow=32, ncol=3)
colnames(OUTPUT_MATRIX) = c("USGS Gage", "Model", "Pct. Error")
rownames(OUTPUT_MATRIX) = c("Jan. Low Flow", "Feb. Low Flow",
                            "Mar. Low Flow", "Apr. Low Flow",
                            "May Low Flow", "Jun. Low Flow",
                            "Jul. Low Flow", "Aug. Low Flow",
                            "Sep. Low Flow", "Oct. Low Flow",
                            "Nov. Low Flow", "Dec. Low Flow",
                            "1 Day Min", "3 Day Min",
                            "7 Day Min", "30 Day Min",
                            "90 Day Min", "7Q10",
                            "Drought of Record", "DoR Year",
                            "Jan. Mean Flow", "Feb. Mean Flow",
                            "Mar. Mean Flow", "Apr. Mean Flow",
                            "May Mean Flow", "Jun. Mean Flow",
                            "Jul. Mean Flow", "Aug. Mean Flow",
                            "Sep. Mean Flow", "Oct. Mean Flow",
                            "Nov. Mean Flow", "Dec. Mean Flow")
