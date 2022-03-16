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

query <- "select c.yy, c.mm, t.tradingItemId, mc.marketCap*1000000 as marketCap
from (
	SELECT c.companyName, mc.companyId, year(mc.pricingDate) yy, month(mc.pricingDate) mm, max(mc.pricingDate) pricingDate
	FROM Xpressfeed.dbo.ciqCompany c
	join Xpressfeed.dbo.ciqMarketCap mc on c.companyId=mc.companyId
	and mc.pricingDate between '1993-01-01' and '2001-12-31'
	WHERE c.countryId=213 -- USA
	and c.companyTypeId=4 -- Public Company
	group by c.companyName, mc.companyId, year(mc.pricingDate) , month(mc.pricingDate)
) c
join Xpressfeed.dbo.ciqMarketCap mc on c.companyId=mc.companyId and c.pricingDate = mc.pricingDate
join xpressfeed.dbo.ciqSecurity se on c.companyId=se.companyId
and mc.pricingDate between isnull(se.securityStartDate, '1900-01-01') and isnull(se.securityEndDate, '2029-01-01')
and se.securitySubTypeId = 1 -- Common Stock
and se.primaryFlag = 1
join xpressfeed.dbo.ciqTradingItem t on se.securityid = t.securityId
and currencyId = 160 -- USD
WHERE mc.marketCap>=10 -- 10 million"

r <- dbGetQuery(conn,query)

query2 <- "select year(m.datadate) as yy, month(m.datadate) as mm, m.tradingItemId, p.capt as marketCap
from qiar_db.dbo.barra_SecurityMasterM m
join Xpressfeed.dbo.ciqTradingItem ti on m.tradingItemId = ti.tradingItemId
join Xpressfeed.dbo.ciqSecurity se on ti.securityId = se.securityId and se.securityName = 'Common Stock' and se.securitySubTypeId = 1 and se.primaryFlag = 1
join qiar_db.dbo.msci_monthlyAssetPrice p on m.barrid = p.barrid and m.datadate = p.datadate
where m.country = 'USA' and m.currency = 'USD' and m.datadate between '2002-01-01' and '2015-12-31' and p.Capt > 10000000"

r <- rbind(r,dbGetQuery(conn,query2))

#Plot universe size by date
#cnts <- count(r,yy,mm)
#cnts <- cbind(cnts, date=paste0(cnts$yy,'-',cnts$mm))
#plot_ly(cnts, x=~date, y=~n) %>% layout(title='Universe Size by Date')

pFlags <- do.call(rbind, lapply(dates, function(d) {
  #filter by date (year and month)
  r2 = r[which(r$yy==year(d) & r$mm==month(d)),]
  
  # Order by marketCap DESCENDING
  r2 = r2[order(r2$marketCap),]
  
  # Calculate the cumulative sum of marketCap at each date
  r2 <- cbind(r2,cumulativeCap=cumsum(r2$marketCap))
  
  # Calculate the total marketCap of the universe
  total = sum(r2$marketCap)
  
  if(nrow(r2)>0) {
    # Save at date d
    df <- data.frame(pricingDate=d, MktCap=r2$marketCap, cumulativeCap=r2$cumulativeCap, MktCapPercent=r2$cumulativeCap/total*100)
    
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
