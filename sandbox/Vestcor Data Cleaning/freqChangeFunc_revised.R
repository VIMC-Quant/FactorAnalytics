#20220216, Casey Li
# for issue #80&81

library(xts)
library(PerformanceAnalytics)
#library(lubridate)

date_forcing <- function(xdate, opt = c('weekly', 'monthly'), week_ending_date){
  
  # get basic date info
  xdate <- as.Date(xdate)
  xyrmth_part <- format(xdate, "%Y-%m-")
  xyear_old <- as.numeric(format(xdate, "%Y"))
  xmonth_old <- as.numeric(format(xdate, "%m"))
  xday_old <- as.numeric(format(xdate, "%d"))
  
  # further processing
  all_days <- seq.Date(from = as.Date('1900-01-01'), length.out = 365*100*7, by = '1 day')
  if(opt == 'weekly'){
    xmap <- data.frame(xdate = all_days, yday = rep(1:7, 365*100))
    xday_new <- xday_old - xmap$yday[xmap$xdate == xdate] + week_ending_date
    xdate_new <- as.Date(paste0(xyrmth_part, xday_new), format = '%Y-%m-%d')
  } else if(opt == 'monthly'){
    if(xmonth_old == 12){
      xyear <- xyear_old + 1
      xmonth <- 1
    } else {
      xyear <- xyear_old
      xmonth <- xmonth_old + 1
    }
    xdate_new <- as.Date(paste0(xyear, "-", xmonth, "-", "01"), format = '%Y-%m-%d')-1
  } else {
    # do nothing
    xdate_new <- xdate
  }
  
  return(xdate_new)
}


to_monthly = function(daily, index_last=TRUE) {
  monthly = apply.monthly(daily, PerformanceAnalytics::Return.cumulative)
  # using last calendar day of the month for index instead of the last available date in data
  if (index_last) {
    end_of_month = function(date) {
      date <- date_forcing(date, opt = 'monthly')
      return(date)
    }
    index(monthly) = do.call(c, lapply(index(monthly), end_of_month))
  }
  monthly
}

to_weekly = function(daily, week_ending_date = 5, days_in_week = 5, index_last=TRUE) {
  # weekdays starts in Monday = 1
  week_index = xts::endpoints(daily, "weeks")
  week_index[2:length(week_index)] = week_index[2:length(week_index)] - (days_in_week - week_ending_date)
  weekly = period.apply(daily, week_index, PerformanceAnalytics::Return.cumulative)
  # using last calendar day of the designated week for index instead of the last available date in data
  if (index_last) {
    end_of_week = function(date) {
      date <- date_forcing(date, opt = 'weekly', week_ending_date = week_ending_date)
      return(date)
    }
    index(weekly) = do.call(c, lapply(index(weekly), end_of_week))
  }
  weekly
}

rf_dly = readxl::read_excel("./risk-free 202010 output.xlsx")
rf_dly = xts(rf_dly[3], order.by = rf_dly$CALDT)
head(rf_dly, 10)

rf_mly = to_monthly(rf_dly)
head(rf_mly)
rf_mly = to_monthly(rf_dly, index_last=FALSE)
head(rf_mly)

rf_wly = to_weekly(rf_dly)
head(rf_wly)
rf_wly = to_weekly(rf_dly, index_last=FALSE)
head(rf_wly)
rf_wly = to_weekly(rf_dly, week_ending_date=3)
head(rf_wly)
