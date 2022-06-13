#######################################################################################################
#
# Full Documentation
# Prepared by Casey Tong Li 202206013
# 
# This R file contains two main functions that transform daily return to monthly/weekly return:
#
#   ###
#   #
#   to_monthly(daily, index_last)
#
#   - purpose: transform daily return into monthly return
#   - arguments:
#     - daily: input daily return dataset name
#     - index_last: it controls whether the return date label will fall on the month end date provided in the dataset 
#                   or the calendar month end date. Regardless, the returns are identical under two scenarios.
#   - example:
#     - daily return: 2020/04/01 X1, 2020/04/02 X2, 2022/04/03 X3, ..., 2022/04/29 X29
#     - if index_last = FALSE, cumulative return of Y as at 2022/04/29 will be generated
#     - if index_last = TRUE, cumulative return of Y as at 2022/04/30 will be generated
#     - under both scenarios, the cumulative returns are identical, only difference is the date label
#
#
#   ###
#   #
#   to_weekly(daily, days_in_week, week_ending_day_str)
#
#   - purpose: transform daily return into weekly return
#   - arguments:
#     - daily: input daily return dataset name
#     - days_in_week: number of days exist in a week, this should be set to fit the dataset, by default 5 in this dataset.
#     - week_ending_day_str: 
#       - it controls what is the week end day. If the week_ending_day is "Wednesday", then Wednesday is the end of a week. 
#         It means the return from previous Thursday to current Wednesday will be included in current Wednesday week's cumulative return.
#       - If the begin of the dataset is before week_ending_day (in this case, Wednesday) , the first week's return would be from the begining
#         of the dataset to the first week_ending day.
#       - If the last week is short of the full week,i.e.if week_ending_day is ''Friday" and 2015-12-31 is a Thursday, then the last four day return will 
#         be labeled as 2016-01-01. 
#
#   - example:
#     - daily return: 1993/01/04 (Monday) X1, 1993/01/05 X2, ..., 1993/01/08 (Friday) X5, 1993/01/11 X6, 1993/01/12 (Tuesday) X7, ......
#     - if week_ending_day_str = Friday, cumulative return of Y as at 1993/01/08 will be generated
#     - if week_ending_day_str = Thursday, cumulative return of Y as at 1993/01/07 will be generated
#     - under the two scenarios, the cumulative returns are different in this case.
#

library(xts)
library(PerformanceAnalytics)
#library(lubridate)
options(scipen = 66666)

to_monthly <- function(daily, index_last = TRUE) {
  monthly <- apply.monthly(daily, PerformanceAnalytics::Return.cumulative)
  # using last calendar day of the month for index instead of the last available date in data
  if (index_last) {
    end_of_month <- function(xdate) {
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
    index(monthly) <- do.call(c, lapply(index(monthly), end_of_month))
  }
  monthly
}

to_weekly <- function(daily, days_in_week = 5,
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
  daily_df <- data.frame(xdate = as.Date(index(daily)), daily)
  xmap <- xmap[xmap$xdate <= max(daily_df$xdate) & xmap$xdate >= min(daily_df$xdate),]

  
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

rf_dly = readxl::read_excel("risk-free 202010 output.xlsx")
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


