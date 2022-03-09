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
  n_future_years <- 365*1000
  all_days <- seq.Date(from = as.Date('1900-01-01'), length.out = n_future_years*7, by = '1 day')
  xmap <- data.frame(xdate = all_days, yday = rep(1:7, n_future_years))
  xmap <- xmap[xmap$xdate <= max(index(daily)) & xmap$xdate >= min(index(daily)),]
  daily_df <- data.frame(xdate = as.Date(index(daily)), daily)
  
  master <- merge(xmap, daily_df, all.x = TRUE)
  master[is.na(master)] <- 0
  
  ##
  # determine the begin and end index for return calculation based on week_ending selection
  wedate_idx <- c(1:nrow(master))[master$yday == week_ending_date]
  if(max(wedate_idx) == nrow(master)){
    # if the data ends on selected week end date, the date interval should be shrinked by 1
    beg_idx <- c(1, wedate_idx[1:(length(wedate_idx)-1)]+1)
    end_idx <- wedate_idx
    last_date <- master$xdate[end_idx[length(end_idx)]]
  } else {
    # else, proceed as normal
    beg_idx <- c(1, wedate_idx+1)
    end_idx <- c(wedate_idx, nrow(master))
    last_date <- master$xdate[end_idx[length(end_idx)-1]] + 7 
  }
  
  ##
  # calculate return for each return series
  master_wodates <- master[-c(1:2)]
  rets <- apply(master_wodates, 2, function(x){
    # for each column (daily return), calculate weekly return
    tmp <- sapply(1:length(beg_idx), function(i){
      ret <- prod(1+x[beg_idx[i]:end_idx[i]]) - 1
      return(ret)
    })
    return(tmp)
  })
  
  ##
  # construct final output
  xdates <- master$xdate[end_idx]
  xdates[length(xdates)] <- last_date  # adjust last date to theoretical week end day
  rets <- xts::xts(rets, order.by = xdates)
  names(rets) <- colnames(daily)
  return(rets)
}

rf_dly = readxl::read_excel("./risk-free 202010 output.xlsx")
rf_dly = xts::xts(rf_dly[3], order.by = rf_dly$CALDT)
head(rf_dly, 10)

rf_mly1 = to_monthly(rf_dly)
head(rf_mly1)
rf_mly2 = to_monthly(rf_dly, index_last=FALSE)
head(rf_mly2)

rf_wly1 = to_weekly(rf_dly, week_ending_day_str='Friday')
head(rf_wly1)
rf_wly2 = to_weekly(rf_dly, week_ending_day_str='Thursday')
head(rf_wly2)


