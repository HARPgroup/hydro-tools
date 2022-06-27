# load the lseg met getter from a github repo. 
# Note: I got this link URL by browsing to the script file I wanted and clicking on "raw"
source("https://raw.githubusercontent.com/HARPgroup/HARParchive/master/HARP-2021-2022/get_lseg_data.R")

tmet <- get_lseg_data("N51820")

# analyze it
quantile(tmet$dfPRC$PRC, probs=c(0,0.1,0.25,0.5,0.75,0.8,0.9,0.95,1.0))

# plot it. 
# Note: your plots window needs to be wide or it will give you "Error in plot.new() : figure margins too large"
plot(tmet$dfPRC$PRC)
