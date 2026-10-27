library(hydrotools)
basepath <- '/var/www/R'
source(paste0(basepath,'/config.R'))

#Load an arbitrary gage
gage_obj <- hydrotools::WaterGageDaily$new(gage_id = "01672500", ds_in = ds)


#Test USGS data retrieval
test1 <- WaterGageStats$new(gage_id = gage_obj$gage_id,
                            statistic_type = "doy",
                            computation_type = c("percentile","minimum","maximum"))

#Get percentile for a value. Any errors on function run?
test1$get_percentile_from_value(67,"2000-01-01")
#Change to monthly percentiles
test1$get_percentile_funcs("month")
#Get new percentile. Any errors on function run?
test1$get_percentile_from_value(83.1,"2000-12-01")

#Manually check some percentiles to compare against
test1$gage_data$percentile[test1$gage_data$time_of_year == "12"]
test1$gage_data$value[test1$gage_data$time_of_year == "12"]

#Now initialize with a daily data frame using doy
test2 <- WaterGageStats$new(data_source = gage_obj$gage_data,
                            date_col = gage_obj$date_col, flow_col = gage_obj$flow_col,
                            statistic_type = "doy",
                            computation_type = c("percentile","minimum","maximum"))
#What are the values for an arbitrary index?
test2$gage_data$time[54]
test2$gage_data$value[54]
test2$gage_data$doy_percentile[54]
test2$gage_data$doy_minimum[54]
test2$gage_data$doy_maximum[54]
#Can we match these values?
test2$get_percentile_from_value(28, "1930-11-23")
ecdf(test2$gage_data$value[format(test2$gage_data$time, "%m-%d") == "11-23"])(28)
min(test2$gage_data$value[format(test2$gage_data$time, "%m-%d") == "11-23"])
max(test2$gage_data$value[format(test2$gage_data$time, "%m-%d") == "11-23"])

#Confirm result with second index
test2$gage_data$time[800]
test2$gage_data$value[800]
test2$gage_data$doy_percentile[800]
test2$gage_data$doy_minimum[800]
test2$gage_data$doy_maximum[800]

test2$get_percentile_from_value(143, "1932-12-08")
ecdf(test2$gage_data$value[format(test2$gage_data$time, "%m-%d") == "12-08"])(143)
min(test2$gage_data$value[format(test2$gage_data$time, "%m-%d") == "12-08"])
max(test2$gage_data$value[format(test2$gage_data$time, "%m-%d") == "12-08"])

#Initialize now using monthly statistics
test3 <- WaterGageStats$new(data_source = gage_obj$gage_data,
                            date_col = gage_obj$date_col, flow_col = gage_obj$flow_col,
                            statistic_type = "month",
                            computation_type = c("percentile","minimum","maximum"))
#Are our percentiles calculated correctly?
test3$get_percentile_from_value(100,"2000-05-04")
ecdf(test3$gage_data$value[test3$gage_data$month == 5])(100)
test3$get_percentile_from_value(300,"2000-01-14")
ecdf(test3$gage_data$value[test3$gage_data$month == 1])(300)
test3$gage_data$month_percentile[54]
test3$gage_data$value[54]
test3$gage_data$month[54]
test3$get_percentile_from_value(28,"2001-11-28")
ecdf(test3$gage_data$value[test3$gage_data$month == 11])(28)

#Are mins and maxes calculated correctly?
max(test3$gage_data$value[test3$gage_data$month == 1], na.rm = TRUE)
test3$gage_data$month_maximum[test3$gage_data$month == 1]
min(test3$gage_data$value[test3$gage_data$month == 1], na.rm = TRUE)
test3$gage_data$month_minimum[test3$gage_data$month == 1]


#Now initialize with yearly statistics
test4 <- WaterGageStats$new(data_source = gage_obj$gage_data,
                            date_col = gage_obj$date_col, flow_col = gage_obj$flow_col,
                            statistic_type = "year",
                            computation_type = c("percentile","minimum","maximum"))
#Are our percentiles calculated correctly?
test4$get_percentile_from_value(100,"2008-05-04")
ecdf(test4$gage_data$value[test4$gage_data$year == 2008])(100)
test4$get_percentile_from_value(300,"2020-01-14")
ecdf(test4$gage_data$value[test4$gage_data$year == 2020])(300)
test4$gage_data$year_percentile[54]
test4$gage_data$value[54]
test4$gage_data$year[54]
test4$get_percentile_from_value(28, "1930-11-28")
test4$get_percentile_from_value(c(30,28), c("2007-11-28","2008-11-28"))

#Are mins and maxes calculated correctly?
max(test4$gage_data$value[test4$gage_data$year == 2020])
test4$gage_data$year_maximum[test4$gage_data$year == 2020]
min(test4$gage_data$value[test4$gage_data$year == 2020])
test4$gage_data$year_minimum[test4$gage_data$year == 2020]

#Test water gage daily implementation
gage_obj$calculate_value_percentile(c(30,28), c("2001-11-28","2001-11-28"),"month","continuous")
gage_obj$stats_obj$get_percentile_funcs("month")

gage_obj$calculate_value_percentile(90,"2000-12-01","month","interpolate_USGS")
gage_obj$calculate_value_percentile(190,"2000-12-01","month","interpolate_USGS")
gage_obj$calculate_value_percentile(90,"2000-12-01","doy","interpolate_USGS")
gage_obj$calculate_value_percentile(84,"2000-12-01","month","continuous")
