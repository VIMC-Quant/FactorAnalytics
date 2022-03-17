libs <- c("DBI", "odbc", 'dplyr', 'plotly', 'lubridate')
lapply(libs,function(x){library(x,character.only=TRUE)})

rm(list=ls())

conn <- dbConnect(odbc(), DSN="CAPITAL-IQ",
                  database = "QIAR_DB",
                  UID = "QIARApp",
                  pwd = "November2019*",
                  port = 1433)

#last day of month dates between '1993-01-01' and '2015-12-31'
dates <- read.csv('dates.csv')
dates <- as.Date(dates[,2],"%m/%d/%Y")

# Get the market cap of every stock in the u.s. common stock universe in USD down to $10 million dollars

query <- "select distinct eomonth(c.pricingDate) as pricingDate, t.tickerSymbol, mc.marketCap*1000000 as marketCap
from (
	SELECT c.companyName, mc.companyId, year(mc.pricingDate) yy, month(mc.pricingDate) mm, max(mc.pricingDate) pricingDate
	FROM Xpressfeed.dbo.ciqCompany c
	join Xpressfeed.dbo.ciqMarketCap mc on c.companyId = mc.companyId
	and mc.pricingDate between '1993-01-01' and '2015-12-31'
	WHERE c.countryId = 213 -- USA
	group by c.companyName, mc.companyId, year(mc.pricingDate) , month(mc.pricingDate)
) c
join Xpressfeed.dbo.ciqMarketCap mc on c.companyId = mc.companyId and c.pricingDate = mc.pricingDate
join xpressfeed.dbo.ciqSecurity se on c.companyId = se.companyId
and mc.pricingDate between isnull(se.securityStartDate, '1900-01-01') and isnull(se.securityEndDate, '2029-01-01')
and se.securitySubTypeId = 1 -- Common Stock
and se.primaryFlag = 1 -- Primary Listing
join xpressfeed.dbo.ciqTradingItem t on se.securityid = t.securityId and t.currencyId = 160 -- USD
and t.primaryFlag = 1
WHERE mc.marketCap>=10 -- 10 million"

r <- dbGetQuery(conn,query)

#Plot universe size by date
#cnts <- count(r,pricingDate)
#plot_ly(cnts, x=~pricingDate, y=~n) %>% layout(title='Universe Size by Date')

pFlags <- do.call(rbind, lapply(dates, function(d) {
  #filter by date (year and month)
  r2 = r[which(r$pricingDate==d),]
  
  # Order by marketCap DESCENDING
  r2 = r2[order(r2$marketCap,decreasing=TRUE),]
  
  # Calculate the cumulative sum of marketCap at each date
  r2 <- cbind(r2,cumulativeCap=cumsum(r2$marketCap))
  
  # Calculate the total marketCap of the universe
  total = sum(r2$marketCap)
  
  if(nrow(r2)>0) {
    # Save at date d
    df <- data.frame(pricingDate=d, MktCap=r2$marketCap, CumulativeCap=r2$cumulativeCap, MktCapPercent=r2$cumulativeCap/total*100)
    
    # Find the marketCap at which you reach 70%, 85%, and 98% coverage of the universe
    first = max(which(df$MktCapPercent<=70))
    second = max(which(df$MktCapPercent<=85))
    third = max(which(df$MktCapPercent<=98))
    
    df <- cbind(df,PercentileFlag=NA)
    df$PercentileFlag[first] <- "70% level is here"
    df$PercentileFlag[second] <- "85% level is here"
    df$PercentileFlag[third] <- "98% level is here"
    
    return(df)
  }
}))

write.csv(pFlags, 'percentileFlags.csv', row.names = FALSE)
