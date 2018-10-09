###################################################################
hydro_tools <- 'C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\hydro-tools'
source(paste(hydro_tools,"USGS","usgs_gage_functions.R", sep = "\\")) #load USGS functions
source(paste(hydro_tools,"LowFlow","fn_iha.R", sep = "\\")) #load IHA functions

# https://va.water.usgs.gov/duration_plots/daily/dp01668000.htm
# USGS Gage number
siteNo <- "01668000" #02055000
gage_flow.df <- streamgage_historic(siteNo)
#gage_flow.df <- clean_historic(gage_flow.df) # Remove any rows with "Ice", "P Ice" or "P Eqp" values for Flow_cd
gage_flow.zoo <- zoo(gage_flow.df$Flow, order.by = gage_flow.df$Date)


gage_ALF <- signif(fn_iha_mlf(gage_flow.zoo,"August"), digits=3)
print(paste("August Low Flow = ",gage_ALF," cfs",sep=""))

gage_7Q10 <- signif(fn_iha_7q10(gage_flow.zoo), digits=3)
print(paste("7Q10 = ",gage_7Q10," cfs",sep=""))
###################################################################
###################################################################
