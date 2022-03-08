data("factorsSPGMI")
factorsSPGMIvestcor <- factorsSPGMI

# import corrected sector data
stocksTickers310 <- read.csv("stocksTickers310GICSgovindSPGMI_Vestcorv2.csv")

# gics mapping
gics <- data.table(cbind(c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60),
                         c("Energy", "Materials","Industrials",
                           "ConsumDisc","ConsumStap","Healthcare",
                           "Financials","InfoTech","Comservices",
                           "Utilities","RealEstate")))
colnames(gics) <- c("SectorCode","SectorName")

# first match data in stocksTickers...
stocksTickers310$Sector <- gics$SectorName[match(substr(stocksTickers310$GICS.code,start=1,stop=2),gics$SectorCode)]

# next match new sector data in stocksTickers... to data in factorsSPGMIVestcor
factorsSPGMIvestcor$Sector <- stocksTickers310$Sector[match(factorsSPGMIvestcor$TickerLast,stocksTickers310$Ticker)]
