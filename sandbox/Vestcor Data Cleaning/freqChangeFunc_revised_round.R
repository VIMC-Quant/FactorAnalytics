library(xts)
library(PerformanceAnalytics)
#library(lubridate)
options(scipen = 66666)

to_monthly = function(daily, index_last = TRUE) {
  monthly = apply.monthly(daily, PerformanceAnalytics::Return.cumulative)
  # using last calendar day of the month for index instead of the last available date in data
  if (index_last) {
    end_of_month = function(xdate) {
      # get basic date info
      xdate <- as.Date(xdate)
      xyrmth_part <- format(xdate, "%Y-%m-")
      xyear_old <- as.numeric(format(xdate, "%Y"))
      xmonth_old <- as.numeric(format(xdate, "%m"))
      xday_old <- as.numeric(format(xdate, "%d"))
      
      if(xmonth_old == 12){
        xyear <- xyear_old + 1
        xmonth <- 1
      } else {
        xyear <- xyear_old
        xmonth <- xmonth_old + 1
      }
      xdate_new <- as.Date(paste0(xyear, "-", xmonth, "-", "01"), format = '%Y-%m-%d')-1
      
      return(xdate_new)
    }
    index(monthly) = do.call(c, lapply(index(monthly), end_of_month))
  }
  monthly
}

to_weekly = function(daily, days_in_week = 5,
                     week_ending_day_str = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')){
  
  week_ending_date <- switch(
    week_ending_day_str,
    'Monday' = 1,
    'Tuesday' = 2,
    'Wednesday' = 3,
    'Thursday' = 4,
    'Friday' = 5,
    'Saturday' = 6,
    'Sunday' = 7
  )
  
  ##
  # determine the weekday for input dataset
  week_index <- xts::endpoints(daily, "weeks")
  week_index_v2 <- week_index
  week_index_v2[2:length(week_index_v2)] <- unique(c((week_index[2:length(week_index)] - (days_in_week - week_ending_date)),
                                                   week_index[length(week_index)]))
  weekly = period.apply(daily, week_index_v2, PerformanceAnalytics::Return.cumulative)

  return(weekly)
}

rf_dly = readxl::read_excel("./sandbox/Vestcor Data Cleaning/risk-free 202010 output.xlsx")
rf_dly = xts::xts(rf_dly[3], order.by = rf_dly$CALDT)
head(rf_dly, 10)

# rf_mly1 = to_monthly(rf_dly)
# head(rf_mly1)
# rf_mly2 = to_monthly(rf_dly, index_last=FALSE)
# head(rf_mly2)

rf_wly1 = to_weekly(rf_dly, week_ending_day_str='Friday')
head(rf_wly1)
rf_wly2 = to_weekly(rf_dly, week_ending_day_str='Thursday')
head(rf_wly2)


