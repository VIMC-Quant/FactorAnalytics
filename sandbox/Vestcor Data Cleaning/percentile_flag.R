libs <- c("DBI", "odbc", 'dplyr', 'plotly')
lapply(libs, function(x) library(x,character.only=TRUE))

conn <- dbConnect(odbc(), DSN="CAPITAL-IQ",
                  database = "QIAR_DB",
                  UID = "QIARApp",
                  pwd = "November2019*",
                  port = 1433)

# Identify US common stock down to 10 million USD in market cap

query <- "SELECT distinct eomonth(mc.pricingDate) as datadate, c.companyId as id1, ti.tickerSymbol as id2, mc.marketCap*1000000 as Capt
FROM (
	select mc.companyId, year(mc.pricingDate) yy, month(mc.pricingDate) mm, max(pricingDate) pricingDate
	from Xpressfeed.dbo.ciqMarketCap mc
	join Xpressfeed.dbo.ciqCompany c on mc.companyId=c.companyId
	where mc.marketCap between 10 and 100000 and mc.pricingDate between '1993-01-01' and '1996-11-30'
	and c.countryId=213 --USA
	and c.companyTypeId=4 --Public Company
	group by mc.companyId, year(mc.pricingDate), month(mc.pricingDate)
) c
join Xpressfeed.dbo.ciqMarketCap mc on c.companyId=mc.companyId and c.pricingDate = mc.pricingDate
join Xpressfeed.dbo.ciqSecurity s on mc.companyId=s.companyId
and mc.pricingDate between isnull(s.securityStartDate, '1900-01-01') and isnull(s.securityEndDate, '2029-01-01')
join Xpressfeed.dbo.ciqTradingItem ti on s.securityId=ti.securityId
where s.securitySubTypeId = 1 --Common Stock
and s.primaryFlag=1 --Specifies the primary security
and ti.currencyId=160 --USD
and ti.primaryFlag=1 --Specifies the primary trading item for each security"
r <- dbGetQuery(conn,query)
  
data_qry <- "SELECT datadate, cap.Barrid as id1, ID.AssetID as id2, Capt
  FROM [QIAR_DB].[dbo].[msci_MonthlyAssetPrice] cap
  JOIN [QIAR_DB].[dbo].[msci_AssetIdentity] ide on cap.Barrid=ide.Barrid
  and cap.DataDate between ide.StartDate and ide.EndDate
  JOIN [QIAR_DB].[dbo].[msci_AssetID] id on cap.Model=id.Model and cap.Barrid=id.Barrid 
  and cap.DataDate between id.StartDate and id.EndDate
  WHERE cap.model = 'GEMLT' and ide.model='GEMLTESG' and ide.Instrument='STOCK' and ide.ISOCountryCode='USA' and ide.ISOCurrencyCode='USD'
  and Capt >= 10000000 and id.AssetIDType='SEDOL'"
uData <- dbGetQuery(conn, data_qry)

r <- rbind(r,uData)

#Plot universe size by date
# cnts <- count(r,datadate)
# plot_ly(cnts, x=~datadate, y=~n) %>% layout(title='Universe Size by Date')

dates <- sort(unique(r$datadate))
pFlags <- do.call(rbind, lapply(dates, function(d) {
  #order by marketCap descending, calculate the cumulative sum
  mkt_df <- r %>% 
    filter(datadate == d) %>% 
    arrange(desc(Capt)) %>% 
    mutate(CumCapt = cumsum(Capt)) %>% 
    mutate(CaptPct = CumCapt/sum(Capt)) %>% 
    mutate(CaptFlag = "")
  
  #find the marketCap at which 70%, 85%, and 98% are reached
  mkt_df$CaptFlag[max(which(mkt_df$CaptPct<=0.7))] <- "70thPCT"
  mkt_df$CaptFlag[max(which(mkt_df$CaptPct<=0.85))] <- "85thPCT"
  mkt_df$CaptFlag[max(which(mkt_df$CaptPct<=0.98))] <- "98thPCT"
  
  return(mkt_df)
}))

flag <- pFlags %>%
  filter(CaptFlag!="") %>%
  select(datadate, Capt, CumCapt, CaptFlag)

plot_ly(flag, x=~datadate, y=~Capt, color=~CaptFlag, type = 'scatter', mode = 'lines')

write.csv(pFlags, 'percentileFlags.csv', row.names = FALSE)
