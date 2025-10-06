# DFLOW Support Functions ####
#' getmode
#' @description Find the mode of a vector
#' @details Find the mode of any kind of vector. In the case of ties, it will
#'   return all values that tied in frequency
#' @param v A vector
#' @return A vector with the values that are most frequently used in the table
getmode <- function(v) {
  #Find the unique values of the vector
  uniqv <- unique(v)
  #Count the instances of each unique value
  tab <- tabulate(match(v, uniqv))
  #Return the value that was tabulated the most frequently
  tab <- uniqv[grep(max(tab),tab)]
  return(tab)
}

#' leapYearAssessment
#' @description Check if year is leap year
#' @details Check if the numeric year is a leap year i.e., is the year divisible
#'   by 4 but not 100, unless it is also divisible by 400?
#' @param inYear A single numeric representing a year
#' @return Logical, TRUE or FALSE
leapYearAssessment <- function(inYear){
  condition1 <- (inYear %% 4) == 0
  condition2 <- (inYear %% 100) == 0
  condition3 <- (inYear %% 400) == 0
  LY <- FALSE
  #Leap years are divisible by but also can fall on the turn of the century if
  #divisible by 100 and 400
  if(condition1 & ((condition2 & condition3) | (!condition2))){
        LY <- TRUE
  }
  return(LY)
}


#' isLeapYear
#' @description Check vector for leap years
#' @details lapply wrapper for leapYearAssessment(). Used to determine which
#'   years in a vector of years are leap years
#' @param inYears A numeric vector representing years to check
#' @return Logical vector, TRUE or FALSE
isLeapYear <- function(inYears){
  out <- unlist(lapply(inYears,leapYearAssessment))
  return(out)
}

# Return the quantile of the Pearson III distribution given skewness of g, and
# annual exceedance probability, aep. This is taken from the lmomco package.

#' Ky_Qp3
#' @description Pearson III quantile
#' @details Return the quantile of the Pearson III distribution given skewness
#'   of g, and annual exceedance probability, aep. This is a wrapper for lmomco
#'   package functions.
#' @param g Skewness
#' @param aep Exceedance probability
#' @return Qauntile of Peason III distribution for aep based on skewness g
Ky_Qp3 <- function(g, aep){ 
  param <- list(type = "pe3", para = c(0,1,g), source = "vec2par")
  names(param$para) <- c("mu", "sigma", "gamma")
  lmomco::quape3(aep, param)  
}


# DFLOW Calculations ####

#xQyComp is used within the xQy function to calculate the 7Q10 and other
#relevant low flows.It takes in a vector of annual minimum flows (xQy_ann) and
#the averaging number (i.e. the x in xQy flows). It also uses the return period
#y to determine the xQy flow.

#' xQyComp
#' @description Compute xQy low flow
#' @details xQyComp is used within the xQy() function to calculate the 7Q10 and
#' other relevant low flows. It takes in a vector of annual minimum flows
#' (xQy_ann) and the return period y to determine the xQy flow. These flows are
#' probability adjusted for years with 0 minimum flow using the USGS approach
#' documented in Austin et al. 2011
#' @param xQy_ann A vector of minimum annual averaged flows across duration x.
#'   For instance, these should be annual minimum 7-day flows to calculate a
#'   7Q10
#' @param y Return period i.e. the y in xQy style low flows. For instance, a
#'   7Q10 requires y = 10
#' @return The xQy flow or NA if insufficient data exists
xQyComp <- function(xQy_ann, y){
  #Find the number of years in xQy_ann that represent complete flow years.
  N <- length(xQy_ann)
  
  #Remove zeroes from analysis for logarithmic analysis
  xQy_ann <- xQy_ann[xQy_ann > 0]
  
  #Initialize the vectors to store the output low flows
  xQy_out <- NA
  
  #If there is sufficient data in xQy_ann, proceed with low flow identification
  if(length(xQy_ann) > 2){
    #Take the log of annual low flows
    logFlows <- log(xQy_ann)
    n <- length(logFlows)
    #Step 1: Find statistics of log distribution of dates and flows
    u <- mean(logFlows)
    s <- sqrt(var(logFlows))
    
    #Find skewness using the appropriate method. In DFLOW example, matches type
    #1 but SW toolbox uses type 2
    g <- e1071::skewness(logFlows, type = 2)#Based on SW Toolbox output
    #Step 2: Adjust probability based on old manual and as suggested in USGS pub:
    p <- ((N / y) / n) - ((N - n) / n)
    
    #Step 3: Calculate K skewness factor either based on tabulated values (K_t below) or estimated as K below:
    #With best fit equation
    if(p > 0){
      #z=4.91*((p)^0.14-(1-p)^0.14)
      #K<-(2/g)*((1+(g*z/6)-g^2/36)^3-1) This equation is based on a small skew range (-1 to 1) and isn't appropriate for bigger skew
      if(s > 0){
        #This function from the lmomco package matches the Leon Harper tables
        #fairly well (within 0.005 at most)
        K <- Ky_Qp3(g, p)
        xQy_out <- exp(u + K * s)
      }else{
        xQy_out <- u
      }
      
    }else{
      xQy_out <- 0
    }
  }

  return(list(xQy = xQy_out))
}

#Function to calculate low flows from USGS gage data. Takes in the raw gage data
#gageData, the start and end of
#the analysis season AYS and AYE, the x and y for a custom xQy low flow
#(defaults to 7Q10, which is already included), and whether or not to include
#MDE summer flow

#' xQy
#' @description Compute VPDES low flows and other xQy style flows
#' @details This function calculates xQy style low flows from USGS or other flow
#'   gage data. It takes in a dataframe that has at least a flow column and date
#'   column specified by the user. From there, it divides the data into user
#'   defined analysis seasons, using only complete seasons (adjusted for leap
#'   years) in the analysis. It then used xQyComp() to calculte the probability
#'   adjusted xQy flows and includes by deafult the 1Q10, 7Q10, 30Q10, 30Q5,
#'   90Q10, and the harmonic mean.\cr
#'   Users may enter analysis years or seasons such that the low flows can be
#'   computed across entire user defined years (like the climate or water year)
#'   or across only certain months. The function is careful to use only
#'   independent analysis years, removing any averaged flow that may extend into
#'   the next analysis year. This makes choosing the correct analysis season
#'   very important.\cr
#'   This is functionally equivalent to the VPDES' DFLOW R Shiny application.
#' @param gageDataIn A data frame containing a date and flow column of daily
#'   streamflows
#' @param flowColumn The column name in gageDataIn that contains the flow values
#' @param dateColumn The column name in gageDataIn that contains the date
#'   values. Will be converted to the date class.
#' @param AYS The start of the analysis season in the format MM-DD. For
#'   instance, to calculate low flows across the water year, AYS = '10-01'
#' @param AYE The end of the analysis season in the format MM-DD. For instance,
#'   to calculate low flows across the water year, AYE = '09-31'
#' @param startYear Any calendar year prior to this year will not be included.
#'   User can leave this as NULL (default) to include all data.
#' @param endYear Any calendar year after this year will not be included. User
#'   can leave this as NULL (default) to include all data.
#' @param x User may optionally calculate a custom xQy flow. 1Q10, 7Q10, 30Q10,
#'   90Q10, and 30Q5 are included in the results already. This input serves as
#'   the averaging period.
#' @param y User may optionally calculate a custom xQy flow. 1Q10, 7Q10, 30Q10,
#'   90Q10, and 30Q5 are included in the results already. This input serves as
#'   the return period
#' @param IncludeSummerFlow Included to match inputs of VPDES function. MDE
#'   Summer flows are particular VPDES low flows used in rare analyses. not
#'   included in hydrotools.
#' @return A list of relevant objects that includes a list of the calculated low
#'   flows Flows, the percentile of the calculate low flows pctg, the formatted
#'   gage data formatted_data (which includes only data considered in analysis and the
#'   rolling average flows), a data frame (annualMinimums) that includes a
#'   summary of the rolling flows in each year and identifies the minimum flows,
#'   data frames that identify when minimum flows occurred in each analysis year
#'   for each flow (e.g., n1Q10_annDate), a series of vectors line
#'   n1Q10_ModeMin that identifies the month when annual low flows of that
#'   averaging period most commonly occur, a numeric timecheck that indicates
#'   the number of days in each analysis year on a non-leap year
#' @export xQy
#' @examples
#' #gageDat <- dataRetrieval::readNWISdv("01631000","00060")
#' #gageDat <- dataRetrieval::renameNWISColumns(gageDat)
#' #gageDat <- gageDat[!grepl("P",gageDat$Flow_cd),]
#' #low_flows <- xQy(gageDataIn = gageDat,
#' #   flowColumn = "Flow", dateColumn = "Date",
#' #   AYS = "07-13", AYE = "02-15",
#' #   startYear = NULL, endYear = NULL,
#' #   x = 7, y = 10,
#' #   IncludeSummerFlow = F)
#' #low_flows$Flows
xQy <- function(gageDataIn, flowColumn = "Flow", dateColumn = "Date",
                AYS = "10-01", AYE = "09-30",
                startYear = NULL, endYear = NULL,
                x = 7, y = 10,
                IncludeSummerFlow = FALSE){
  #Create a simplified copy of the dataset to manipulate
  gageData <- gageDataIn[,c(dateColumn,flowColumn)]
  #Treat negative flows as missing flows, per SW toolbox
  gageData$Flow[gageData$Flow < 0] <- NA
  #Ensure date column exists
  gageData$Date <- as.Date(gageData[,dateColumn])
  
  ## Analysis Year ####
  #Initialize Analysis year by making it the calendar year
  gageData$AY <- as.numeric(format(gageData$Date,"%Y"))
  #Initialize a variable to track if this data is in the analysis season and
  #should be kept for the analysis. All data will be kept unless  partial season
  #is given as in a VPDES high flow/winter analysis
  gageData$keepData <- TRUE
  
  #Find the julian day associated with the start and end of the analysis year
  AYS_jday <- as.numeric(format(as.Date(paste0("1994-",AYS)),"%j"))
  AYE_jday <- as.numeric(format(as.Date(paste0("1994-",AYE)),"%j"))
  #Find the julian day associated with the start and end of the analysis year
  #adjusted for leap years
  AYS_jday_LY <- as.numeric(format(as.Date(paste0("1996-",AYS)),"%j"))
  AYE_jday_LY <- as.numeric(format(as.Date(paste0("1996-",AYE)),"%j"))
  
  #Julian day runs from 1:365 on non-leap years and 1:366 on leap years. We
  #need to ensure leap years are adequately captured so we check explicitly
  #for them
  gageData$LY <- isLeapYear(as.numeric(format(gageData$Date,"%Y")))
  LD <- as.numeric(format(as.Date(paste0("1996-02-29")),"%j"))
  #We set up a variable to find out how many days are in the non leap year season
  timecheck <- 0
  #Variables to determine leap year behavior
  leapYearinSeason <- FALSE
  febLDsameCY <- FALSE
  if(AYS == "02-29" | AYE == "02-29"){
    #Analysis year cannot begin or end on a leap day for simplicity
    warning("Analysis year cannot start or end on a leap day.")
  }else if(AYS_jday == AYE_jday){
    #Start and end day cannot be equal
    warning("Analysis Year start and end month day cannot be equal.")
  }else if(AYS_jday > AYE_jday){
    #If the julian day at the start of the analysis year is greater than the
    #julian day of the end of the analysis year, it indicates the user wants to
    #calculate flows for a analysis year such as from Oct 1st to Sep 30th. In
    #these cases, we adjust the analysis year to reflect the calendar year it
    #ends on. This may be impacted by leap day, so we deal with these cases
    #seprately
    gageData$AY[!gageData$LY & as.numeric(format(gageData$Date,"%j")) >= AYS_jday] <- 
      gageData$AY[!gageData$LY & as.numeric(format(gageData$Date,"%j")) >= AYS_jday] + 1
    gageData$AY[gageData$LY & as.numeric(format(gageData$Date,"%j")) >= AYS_jday_LY] <- 
      gageData$AY[gageData$LY &  as.numeric(format(gageData$Date,"%j")) >= AYS_jday_LY] + 1
    #Mark data for removal if it extends past the end of the analysis season
    #prior to the start of the next analysis year
    gageData$keepData[gageData$Date > as.Date(paste0(gageData$AY,"-",AYE))] <- FALSE
    
    #Number of days in a non-leap year season is the difference between this
    #calendar years end date and last years start date
    timecheck <- as.numeric(as.Date(paste0("1995-",AYE)) - as.Date(paste0("1994-",AYS))) + 1
    
    #Does the analysis season contain the leap year?
    if(LD < AYE_jday_LY | LD > AYS_jday_LY){
      leapYearinSeason <- TRUE
      #Is the analysis year of the leap day equal to the calendar year? Only
      #true if the analysis season ends after the leap day
      if(as.numeric(format(as.Date(paste0("1995-",AYE)),"%m")) <= 2){
        febLDsameCY <- TRUE
      }
    }
    
  }else if(AYS_jday < AYE_jday){
    #If the start day is prior to the end day, then the analysis season is only
    #in the present calendar year. Here, mark days for removal if they do not lie
    #in the date range, adjusting for leap years
    gageData$keepData[gageData$Date < as.Date(paste0(gageData$AY,"-",AYS)) |
                        gageData$Date > as.Date(paste0(gageData$AY,"-",AYE))] <- FALSE
    #Number of days in a non-leap year season is just the difference in start
    #and end dates
    timecheck <- as.numeric(as.Date(paste0("1994-",AYE)) - as.Date(paste0("1994-",AYS))) + 1
    #Does the analysis season contain the leap year?
    if(LD < AYE & LD > AYS){
      leapYearinSeason <- TRUE
    }
    #The leap day will always have an analysis year equal to the calendar year
    febLDsameCY<-TRUE
  }
  
  
  ## Filter Years ####
  #Find the data startYear endYear unless input by user
  if(is.null(startYear) || !is.numeric(startYear)){
    startYear <- as.numeric(format(min(gageData$Date),"%Y"))
  }
  if(is.null(endYear) || !is.numeric(endYear)){
    endYear <- as.numeric(format(max(gageData$Date),"%Y"))
  }
  
  #Trim start + end data ####
  #Remove all data prior to the start of the first analysis year and after the
  #end of the last analysis year
  min_AYS_date <- min(gageData$Date[format(gageData$Date,"%m-%d") == AYS])
  max_AYE_date <- max(gageData$Date[format(gageData$Date,"%m-%d") == AYE])
  gageData <- gageData[gageData$Date >= min_AYS_date & gageData$Date <= max_AYE_date,]
  
  #Copy Full Data Set
  #Create a copy of the full gage data and reset dat as the subset above
  gageData_full <- gageData
  
  ## Filter Season ####
  #Remove data before and after the analysis start and end dates on the first
  #and last year. This will only apply if the user has entered a season that
  #does not extend through a whole year
  gageData <- gageData[gageData$Date >= as.Date(paste0(startYear,"-",AYS)) &
                         gageData$Date <= as.Date(paste0(endYear,"-",AYE)),]
  
  
  ## Filter Analysis Season ####
  gageData <- gageData[gageData$keepData,]
  
  
  ## Compute Rolling Means ####
  #Compute rolling means of data and find annual (water Date) minimums and the
  #date in which they occurred (for tracking mode months of low flow)
  
  #Calculate the rolling means using zoo package rollapply, rounding results to
  #the 10 decimal places. Ensure minimums are independent from one another,
  #setting last x-1 days from the analysis as NA since they compute rolling
  #means that extend into the next water year
  rxAvg <- caTools::runmean(gageData$Flow,x, alg = "exact", align = "left", endrule = "NA")
  gageData$rxAvg <- round(rxAvg,10)
  
  r7Avg <- caTools::runmean(gageData$Flow,7, alg = "exact", align = "left", endrule = "NA")
  gageData$r7Avg <- round(r7Avg,10)
  
  r30Avg <- caTools::runmean(gageData$Flow,30, alg = "exact", align = "left", endrule = "NA")
  gageData$r30Avg <- round(r30Avg,10)
  
  r90Avg <- caTools::runmean(gageData$Flow,90, alg = "exact", align = "left", endrule = "NA")
  gageData$r90Avg <- round(r90Avg,10)
  
  #Need to ensure each year is independent, setting any rolling average that
  #passes into the next analysis year as NA
  for(i in unique(gageData$AY)){
    loopData <- gageData[gageData$AY == i,]
    #If sufficient data exists this year, set any flows that extend to the next
    #year as NA
    if((nrow(loopData) - (x - 2)) > 1 && x != 1){
      loopData$rxAvg[(nrow(loopData) - (x - 2)):nrow(loopData)] <- NA
    }
    
    if((nrow(loopData) - (7 - 2)) > 1){
      loopData$r7Avg[(nrow(loopData) - (7 - 2)):nrow(loopData)] <- NA
    }
    
    if((nrow(loopData) - (30 - 2)) > 1){
      loopData$r30Avg[(nrow(loopData) - (30 - 2)):nrow(loopData)] <- NA
    }
    
    if((nrow(loopData) - (90 - 2)) > 1){
      loopData$r90Avg[(nrow(loopData) - (90 - 2)):nrow(loopData)] <- NA
    }
    gageData[gageData$AY == i,] <- loopData
  }
  
  #Find the lowest of each rolling mean flow for each year. Also count the
  #number of records and the number of NA records so we can find complete
  #analysis seasons
  annualMinimums <- sqldf::sqldf("
        SELECT AY, count(*) as count, 
        min(rxAvg) as nxQy_ann,
        min(Flow) as n1Q10_ann,
        min(r7Avg) as n7Q10_ann,
        min(r30Avg) as n30Q10_ann,
        min(r90Avg) as n90Q10_ann,
        SUM(CASE WHEN Flow IS NULL THEN 1 ELSE 0 END) AS null_count
        FROM gageData
        GROUP BY AY
  ")
  
  #If missing flows are present, no minimum can be found for that year
  annualMinimums$nxQy_ann[annualMinimums$null_count > 0] <- NA
  annualMinimums$n1Q10_ann[annualMinimums$null_count > 0] <- NA
  annualMinimums$n7Q10_ann[annualMinimums$null_count > 0] <- NA
  annualMinimums$n30Q10_ann[annualMinimums$null_count > 0] <- NA
  annualMinimums$n90Q10_ann[annualMinimums$null_count > 0] <- NA
  
  #There must be enough data in the season to compute a minimum flow for the
  #custom each x-day flow
  annualMinimums$nxQy_ann[(annualMinimums$count - (x - 2)) <= 1] <- NA
  annualMinimums$n7Q10_ann[(annualMinimums$count - (7 - 2)) <= 1] <- NA
  annualMinimums$n30Q10_ann[(annualMinimums$count - (30 - 2)) <= 1] <- NA
  annualMinimums$n90Q10_ann[(annualMinimums$count - (90 - 2)) <= 1] <- NA
  
  ## Find Min Dates ####
  #For each analysis year, find when the minimum x day flow occurred that year.
  #May be more than once. Here, we use match to find the matching minimum flow
  #in annualMinimum in the correct analysis year
  nxQy_annDate <- gageData$Date[
    gageData$rxAvg == annualMinimums$nxQy_ann[match(gageData$AY,annualMinimums$AY)] &
      gageData$AY ==  annualMinimums$AY[match(gageData$AY,annualMinimums$AY)]]
  n1Q10_annDate <- gageData$Date[
    gageData$Flow == annualMinimums$n1Q10_ann[match(gageData$AY,annualMinimums$AY)] &
      gageData$AY ==  annualMinimums$AY[match(gageData$AY,annualMinimums$AY)]]
  n7Q10_annDate <- gageData$Date[
    gageData$r7Avg == annualMinimums$n7Q10_ann[match(gageData$AY,annualMinimums$AY)] &
      gageData$AY ==  annualMinimums$AY[match(gageData$AY,annualMinimums$AY)]]
  n30Q10_annDate <- gageData$Date[
    gageData$r30Avg == annualMinimums$n30Q10_ann[match(gageData$AY,annualMinimums$AY)] &
      gageData$AY ==  annualMinimums$AY[match(gageData$AY,annualMinimums$AY)]]
  n90Q10_annDate <- gageData$Date[
    gageData$r90Avg == annualMinimums$n90Q10_ann[match(gageData$AY,annualMinimums$AY)] &
      gageData$AY ==  annualMinimums$AY[match(gageData$AY,annualMinimums$AY)]]
  #Match will create NAs where there are NA flows, so we can remove these
  nxQy_annDate <- nxQy_annDate[!is.na(nxQy_annDate)]
  n1Q10_annDate <- n1Q10_annDate[!is.na(n1Q10_annDate)]
  n7Q10_annDate <- n7Q10_annDate[!is.na(n7Q10_annDate)]
  n30Q10_annDate <- n30Q10_annDate[!is.na(n30Q10_annDate)]
  n90Q10_annDate <- n90Q10_annDate[!is.na(n90Q10_annDate)]
  #Create data frames that have the dates in which minimum flows occurred, the
  #analysis year, and the minimum flow that occurred
  nxQy_annDate <- data.frame(Date = nxQy_annDate,
                             AY = gageData$AY[match(nxQy_annDate,gageData$Date)],
                             minFlow = gageData$rxAvg[match(nxQy_annDate,gageData$Date)])
  n1Q10_annDate <- data.frame(Date = n1Q10_annDate,
                              AY = gageData$AY[match(n1Q10_annDate,gageData$Date)],
                              minFlow = gageData$Flow[match(n1Q10_annDate,gageData$Date)])
  n7Q10_annDate <- data.frame(Date = n7Q10_annDate,
                              AY = gageData$AY[match(n7Q10_annDate,gageData$Date)],
                              minFlow = gageData$r7Avg[match(n7Q10_annDate,gageData$Date)])
  n30Q10_annDate <- data.frame(Date = n30Q10_annDate,
                               AY = gageData$AY[match(n30Q10_annDate,gageData$Date)],
                               minFlow = gageData$r30Avg[match(n30Q10_annDate,gageData$Date)])
  n90Q10_annDate <- data.frame(Date = n90Q10_annDate,
                               AY = gageData$AY[match(n90Q10_annDate,gageData$Date)],
                               minFlow = gageData$r90Avg[match(n90Q10_annDate,gageData$Date)])
  
  
  ## Mode Min Flows ####
  #Solve for mode minimum month for each input flow type:
  nxQy_ModeMin  <- getmode(as.numeric(format(nxQy_annDate$Date,"%m")))
  n1Q10_ModeMin <- getmode(as.numeric(format(n1Q10_annDate$Date,"%m")))
  n7Q10_ModeMin <- getmode(as.numeric(format(n7Q10_annDate$Date,"%m")))
  n30Q10_ModeMin<- getmode(as.numeric(format(n30Q10_annDate$Date,"%m")))
  n30Q5_ModeMin <- getmode(as.numeric(format(n30Q10_annDate$Date,"%m")))
  n90Q10_ModeMin <- getmode(as.numeric(format(n90Q10_annDate$Date,"%m")))
  
  ##Clean Data for xQy ####
  #First, determine which analysis years are leap years and correct timecheck as
  #needed
  timecheck_leapYear <- timecheck + 1
  timecheck_vector <- rep(timecheck,nrow(annualMinimums))
  
  #If leapYear is in the anlysis season, ensure timecheck is corrected to adjust
  #for potential leap days
  if(leapYearinSeason){
    if(febLDsameCY){
      #If the leap day is in the same analysis year as the calendar year, we
      #need to shift the timecheck vector to the left
      timecheck_vector[(isLeapYear(unique(gageData$AY) - 1))] <- timecheck_leapYear
    }else{
      timecheck_vector[(isLeapYear(unique(gageData$AY)))] <- timecheck_leapYear
    }
  }
  #Get rid of incomplete analysis years
  min_filter <- annualMinimums[annualMinimums$count >= timecheck_vector,]
  
  #Do not keep NA minimum flows as these cannot be used in the analysis. Store
  #the now filtered vector of flows in their own numeric to pass to xQyComp
  xQy_ann <- min_filter$nxQy_ann[!is.na(min_filter$nxQy_ann)]
  n1Q10_ann <- min_filter$n1Q10_ann[!is.na(min_filter$n1Q10_ann)]
  n7Q10_ann <- min_filter$n7Q10_ann[!is.na(min_filter$n7Q10_ann)]
  n30Q10_ann <- min_filter$n30Q10_ann[!is.na(min_filter$n30Q10_ann)]
  n90Q10_ann <- min_filter$n90Q10_ann[!is.na(min_filter$n90Q10_ann)]
  
  ## Compute Critical Low flows ####
  #Compute the low flows using the data from the loop. gageData is the filtered and
  #formatted gage data and dat2 is the unmodified data used to evaluate the flow
  #percentile
  out_xQy  <- xQyComp(xQy_ann, y)
  out_1Q10 <- xQyComp(n1Q10_ann, 10)
  out_7Q10 <- xQyComp(n7Q10_ann, 10)
  out_30Q10 <- xQyComp(n30Q10_ann, 10)
  out_30Q5 <- xQyComp(n30Q10_ann, 5)
  out_90Q10 <- xQyComp(n90Q10_ann, 10)
  
  ## MDE Summer Flow ####
  #If user wants to include MDE Avg. Summer Flow:
  if(IncludeSummerFlow){
    message("MDE Summer flow not available in hydrotools as it is exclusively used in VPDES analyses")
  }
  
  ## Harmonic Mean ####
  #By using the raw data, are we including data at the beginning and end of record
  #period that is not necessarily in the climate year? Should we clip to start
  #of first analysis year?
  #Human Health Standards: Harmonic Mean. Here we use the adjusted HM formula
  #from USGS SWToolbox and EPA DFLOW as these were used in VPDES permits
  AY_comp <- unique(gageData_full$AY)[annualMinimums$count >= timecheck]
  subst <- gageData_full[!is.na(gageData_full$Flow) & 
                           gageData_full$Flow > 0 & 
                           (gageData_full$AY %in% AY_comp),]
  HM <- sum(1 / subst$Flow)
  NDays <- length(gageData_full$Flow[!is.na(gageData_full$Flow) & (gageData_full$AY %in% AY_comp)])
  NZeros <- length(gageData_full$Flow[!is.na(gageData_full$Flow) & 
                                        gageData_full$Flow == 0 & (gageData_full$AY %in% AY_comp)])
  HM <- ((NDays - NZeros) / HM) * ((NDays - NZeros) / NDays)
  
  ## Flow Percentiles ####
  #What daily percentile does the 7Q10 flow represent? For simplicity, this
  #uses all data and is not concerned with complete analysis years. This is also
  #easier in terms of communication with external groups just checking gages
  #every day
  flowCDF <- ecdf(gageData$Flow)
  out_xQy$pctg <- flowCDF(out_xQy$xQy)
  out_1Q10$pctg <- flowCDF(out_1Q10$xQy)
  out_7Q10$pctg <- flowCDF(out_7Q10$xQy)
  out_30Q10$pctg <- flowCDF(out_30Q10$xQy)
  out_30Q5$pctg <- flowCDF(out_30Q5$xQy)
  out_90Q10$pctg <- flowCDF(out_90Q10$xQy)
  HM_pctg <- flowCDF(HM)

  return(
    list(
      #Critical low flows
      Flows = list(xQy = out_xQy$xQy, n1Q10 = out_1Q10$xQy, n7Q10 = out_7Q10$xQy,
                   n30Q10 = out_30Q10$xQy, n30Q5 = out_30Q5$xQy,
                   n90Q10 = out_90Q10$xQy, HM = HM),
      #Percentiles of critical low flows
      Percentile = list(xQy = out_xQy$pctg, n1Q10 = out_1Q10$pctg, n7Q10 = out_7Q10$pctg,
                  n30Q10 = out_30Q10$pctg, n30Q5 = out_30Q5$pctg,
                  n90Q10 = out_90Q10$pctg, HM = HM_pctg),
      #Formatted and calculated gage data and timecheck
      formatted_data = gageData, timecheck = timecheck, annualMinimums = annualMinimums,
      #Dates when monthly lows occur
      nxQy_annDate = nxQy_annDate,
      n1Q10_annDate = n1Q10_annDate,
      n7Q10_annDate = n7Q10_annDate,
      n30Q10_annDate = n30Q10_annDate,
      n30Q5_annDate = n30Q10_annDate,
      n90Q10_annDate = n90Q10_annDate,
      #Monthly minimum flow modes
      nxQy_ModeMin = nxQy_ModeMin,
      n1Q10_ModeMin = n1Q10_ModeMin,
      n7Q10_ModeMin = n7Q10_ModeMin,
      n30Q10_ModeMin = n30Q10_ModeMin,
      n30Q5_ModeMin = n30Q5_ModeMin,
      n90Q10_ModeMin = n90Q10_ModeMin
    )
  )
}

## Testing ####
gageDat <- dataRetrieval::readNWISdv("01631000","00060")
gageDat <- dataRetrieval::renameNWISColumns(gageDat)
gageDat <- gageDat[!grepl("P",gageDat$Flow_cd),]
low_flows <- xQy(gageDataIn = gageDat,
                 flowColumn = "Flow", dateColumn = "Date",
                 AYS = "03-01", AYE = "07-31",
                 startYear = NULL, endYear = NULL,
                 x = 8, y = 12,
                 IncludeSummerFlow = F)
