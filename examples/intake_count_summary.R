
#save_directory <- 'var/www/R/hydro-tools/examples/'
save_directory <- 'C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/plots/'


library(RCurl) 
curlSetOpt(timeout = 2000)
#intakes <- getURL("http://deq2.bse.vt.edu/d.alpha/surface-water-intake-summary-export-nhdplus", timeout = 20000)
intakes <- getURL("http://deq2.bse.vt.edu/d.alpha/surface-water-intake-summary-export-nhdplus")
write.csv(intakes,paste(save_directory,"intakes.csv",sep=""))