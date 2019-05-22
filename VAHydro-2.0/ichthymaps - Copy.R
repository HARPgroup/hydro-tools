library(jsonlite)

json_url <- paste("https://www.sciencebase.gov/catalog/item/5446a5a1e4b0f888a81b816d?format=json", sep="")



print(paste("Using ", json_url, sep=''))

raw_data <- fromJSON(json_url)

#------------------------------------------------------------------

url.csv <- "https://www.sciencebase.gov/catalog/file/get/5446a5a1e4b0f888a81b816d?f=__disk__25%2Fed%2F4a%2F25ed4a840a109d160d081bf144a66f615cb765cd"

MyData <- read.csv(file=url.csv, header=TRUE, sep=",")

COMID <- c(8695023,8740963)

COMID.rows <- MyData[which(MyData$COMID_NHDv2 %in% COMID),] #multiple comid
#COMID.rows <- MyData[which(MyData$COMID_NHDv2 == COMID),] #single comid
COMID.NT.TOTAL <- nrow(COMID.rows)


COMID.rows <- MyData[which(MyData$State %in% 'VA'),]
COMID.Taxa.All <- COMID.rows$Name_Taxa
NT.TOTAL.ALL <- length(COMID.Taxa.All)
NT.TOTAL.UNIQUE <- length(unique(COMID.Taxa.All)) 

#------------------------------------------------------------------

ichthy.url.csv <- "https://www.sciencebase.gov/catalog/file/get/5446a5a1e4b0f888a81b816d?f=__disk__25%2Fed%2F4a%2F25ed4a840a109d160d081bf144a66f615cb765cd"
ichthy.dataframe <- read.csv(file=ichthy.url.csv, header=TRUE, sep=",")



watershed.code <- "nhd_huc8_02080104"

datasite <- 'http://deq1.bse.vt.edu/d.bet'
vahydro.url <- paste(datasite,"contained_nhdplusv2",watershed.code ,sep="/")
print(paste("Using ", vahydro.url, sep = ''))
NHDPlusV2.SEGS <- read.csv(vahydro.url, header = TRUE, sep = ",")

#NHDPlusV2.SEGS.orig <- NHDPlusV2.SEGS
#NHDPlusV2.SEGS <- NHDPlusV2.SEGS.orig
#ichthy.dataframe.orig <- ichthy.dataframe

#i<-200
for (i in 1:length(NHDPlusV2.SEGS$COMID)) {
  print(paste("PROCESSING COMID ",i," OF ",length(NHDPlusV2.SEGS$COMID), sep = ''))
  
COMID <- NHDPlusV2.SEGS[i,]$COMID
COMID.rows <- ichthy.dataframe[which(ichthy.dataframe$COMID_NHDv2 == COMID),] #single comid

# SKIP COMID IF THAT NHDPlusV2 SEGMENT ISNT IN ICHTHY DATASET
if (length(COMID.rows$ID) == 0) {
  print(paste("NO ICHTHY DATA FOR COMID ",COMID," (SKIPPING)",sep = ''))
  next
}


  
#test <- NHDPlusV2.SEGS

COMID.Taxa.All <- COMID.rows$Name_Taxa
NT.TOTAL.ALL <- length(COMID.Taxa.All)
NT.TOTAL.UNIQUE <- length(unique(COMID.Taxa.All)) 

print(paste("ICHTHY DATA FOR COMID ",COMID," (NT TOTAL = ",NT.TOTAL.UNIQUE,")",sep = ''))

test[i,"NT_TOTAL"] <- NT.TOTAL.UNIQUE




}






