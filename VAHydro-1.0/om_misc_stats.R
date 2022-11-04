# Automating Miscellaneous stats

#Set auth file location dynamically, and load auth info 
path <- substr(getwd(),1,nchar(getwd())-11)
source(paste(path,"auth.private", sep = "")); 
#setInternet2(TRUE)

# Set up query for batch of model objects
# Internal variable to construct the query
# query types : "cbp_minor", "cbp_major", "huc8", "elementid"
q_type <- "elementid"
# Query params, CBP Minor code (PS), CBP Major code (eg P), HUC8 (eg 03010101)
# Maury river-214737,212345,209629,213095,213357,213151,213003
# Occoquan watershed- 229549,236217,236309,230579,231299,235649,241191
# Pamunkey NT - 207923,207885,207733,207695,224045,223639,207771,224431,207847,207809
# SF Shenandoah at Lynnwood - 232167,234153,236591,233303,240949,229311,241097,232075,242015,237419,242565,234585
# Rivanna - 337716,337730,214993,337728,337726,337724,337722,337720,337718,337712,337852
# Upper and Middle James - 209447,213049,213963,209715,212907,210861,214555,213591,212701,211783,210315,326970,210175,213725,211437,213921,210441,209503,210265,334537,327124,209799,209755,211875,211743,213635,214595,210395,213253,210771,210595,210483,213505,214953,213459,214907,214865,209583,212039,214737,212345,209629,213095,213357,213151,213003,211633,212491,214299,214381,210957,214421,214515,210041,211397,213881,213403,212303,211047,337716,337730,214993,211097,337692,337852,337728,337726,337724,337722,337720,337718,337712

# all segs with 0.0 sept 10 pct flow
query_param <- "224195"
#query_param <- "cova_ws_container,cova_ws_subnodal"


# type, specified by node_type
# Object types : WSP Control node use "cova_fe_project", VAHydro Container use "cova_ws_container,cova_ws_subnodal"
o_type <- "cova_ws_container,cova_ws_subnodal"
# flow var name, indicated by node_type: WSP Control node use "Qreach", VAHydro Container use "Qout"
f_var <- "Qout"
# our 2 domains: 95 is WSP Control, 37 is VAHydro
scenid <- 37
runids <- c(122)

# now, get the elementids that we want
eurl_base<-"http://deq1.bse.vt.edu/om/remote/get_modelData.php?operation=7"
elurl <- paste(eurl_base, "&querytype=", q_type, "&variables=elementid", "&scenarioid=", scenid, "&params=", query_param, "&custom1=", o_type, sep = "")
nameelids <- dat<-read.table(elurl, header = TRUE, sep = ",")

urlbase<-"http://deq1.bse.vt.edu/om/remote/get_modelData.php?elementid="
urlbase2<-"http://deq1.bse.vt.edu/om/remote/get_modelData.php?operation=6&elementid="


# for loop to go through each elementid listed
for (i in 1:length(nameelids[,1] )) {  
   for (j in 1:length(runids)) {	 
      id<-nameelids[i,"elementid"]  # determines which elementid you are currently on

      rid<-runids[j]  
      print(paste("Getting data for run ", rid, " for element ", id))

      # creates the whole url by pasting the element and run ids into it
      filename<-paste(urlbase, id, "&variables=", f_var, ',wd_mgd,ps_cumulative_mgd,wd_cumulative_mgd', "&runid=", rid, "&startdate=1984-10-01&enddate=2005-09-30", sep = "")
	print(paste("Retrieval URL ", filename))

      dat = try(read.table(filename, header = TRUE, sep = ",")) 
      if (class(dat)=='try-error') { 
         # what to do if file empty 
         print(paste("Error: empty file ", filename))
      } else { 
         # Median, Minimum and Mean flow, mean withdrawal and mean discharge
         qmedian <- median(dat[,2])
         qmean <- mean(dat[,2])
         qmin <- min(dat[,2])
         wmean <- mean(dat[,3])
         numts <- length(dat[,2])
         pscmean <- mean(dat[,4])
         wdcmean <- mean(dat[,5])
         
         print(list(
           qmedian = qmedian, 
           qmean = qmean, 
           qmin = qmin, 
           wmean = wmean, 
           numts = numts, 
           pscmean = pscmean, 
           wdcmean = wdcmean
                    )
               )


         # putting data back into WOOOMM
         url <- "http://deq1.bse.vt.edu/om/remote/setModelData.php?hash="
         # building the correct url
         rep_url <- paste(url, hash, "&username=", username, "&elementid=", id, "&runid=", rid, "&reporting_frequency=single&starttime=1984-10-01&endtime=2005-09-30&temporal_res=period", sep = "")

         durl <- paste(rep_url, "&dataname=Qout_mean&dataval=", qmean, sep = "")
         readLines(durl) 
         durl <- paste(rep_url, "&dataname=Qout_median&dataval=", qmedian, sep = "")
         readLines(durl) 
         durl <- paste(rep_url, "&dataname=Qout_min&dataval=", qmin, sep = "")
         readLines(durl) 
         print(paste("Setting wd_mgd_mean =", wmean, sep = ""))
         durl <- paste(rep_url, "&dataname=wd_mgd_mean&dataval=", wmean, sep = "")
         readLines(durl) 
         durl <- paste(rep_url, "&dataname=num_ts&dataval=", numts, sep = "")
         readLines(durl) 
         durl <- paste(rep_url, "&dataname=ps_cumulative_mgd_mean&dataval=", pscmean, sep = "")
         readLines(durl) 

         durl <- paste(rep_url, "&dataname=wd_cumulative_mgd_mean&dataval=", wdcmean, sep = "")
         print(paste("Setting wd_cumulative_mgd_mean =", wdcmean, sep = ""))
         readLines(durl) 
      }
   }	
print("Finished job.")
}  # end of the for loops	
print("Finished All Elements.")