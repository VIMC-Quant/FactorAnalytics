# CleanFAData.R
# Jon Spinney & Casey Li
# March 22, 2022

# sectors incorrectly named in factorsSPGMI/stocksCRSP - need to be fixed.
# four stocks included in the data set within sectors that we want to exclude 
  # (Financials and Real Estate) and replace with four different securities from 
  # the master list of 310 securities found within 
  # "stocksTickers310GICSgovindSPGMI".
# market Capitalization groups ("LargeCap", "MidCap", "SmallCap", and 
  # "MicroCap") assignments are not correct for about 25% of the data set 
  # within factorsSPGMI and should be re-assigned.

################################################################################
### clean sector membership
load("~/R-Work/FactorAnalytics/data/factorsSPGMI.rda")
load("~/R-Work/FactorAnalytics/data/stocksCRSP.rda")

factorsSPGMI_tmp <- factorsSPGMI
stocksCRSP_tmp <- stocksCRSP

unique(factorsSPGMI_tmp$Sector) # misspelled or duplicated sectors
unique(stocksCRSP_tmp$Sector) # misspelled or duplicated sectors

# clean up list of sectors for factorsSPGMI
bad_sectors <- unique(factorsSPGMI_tmp$Sector)
good_sectors <- c("InfoTech","Industrials","Healthcare","ConsumStap","Energy",
                  "Materials","ConsumDisc","ComServices","Utilities",
                  "RealEstate","Healthcare","Financials","ConsumDisc",
                  "InfoTech","ConsumStap","ComServices")
sector_table <- data.frame(cbind(bad_sectors,good_sectors))
colnames(sector_table) <- c("BadSectors","GoodSectors")
factorsSPGMI_tmp$Sector <- sector_table$GoodSectors[match(
  factorsSPGMI_tmp$Sector,sector_table$BadSectors)]

# check - should be 11 unique sectors with correct names as per 
# FAdataCRSPandSPGMI 02 23 22.pdf
unique(factorsSPGMI_tmp$Sector)

# clean up list of sectors for factorsSPGMI
bad_sectors <- unique(stocksCRSP_tmp$Sector)
good_sectors <- c("InfoTech","Industrials","Healthcare","ConsumStap","Energy",
                  "Materials","ConsumDisc","ComServices","Utilities",
                  "RealEstate","Healthcare","Financials","ConsumDisc",
                  "InfoTech","ConsumStap","ComServices")
sector_table <- data.frame(cbind(bad_sectors,good_sectors))
colnames(sector_table) <- c("BadSectors","GoodSectors")
stocksCRSP_tmp$Sector <- sector_table$GoodSectors[match(stocksCRSP_tmp$Sector,
                                                    sector_table$BadSectors)]

# check - should be 11 unique sectors with correct names as per 
# FAdataCRSPandSPGMI 02 23 22.pdf
unique(stocksCRSP_tmp$Sector)

(to_remove <- unique(factorsSPGMI_tmp[factorsSPGMI_tmp$Sector 
                                         %in% 
                                     c("Financials","RealEstate"),]$TickerLast))

# candidates to add
library(data.table)
adds <- data.table(cbind(c("CHDN","CCF","SEE","LCII","SCL","PNR","AME","GEF",
                           "DOW","VFC"),
                         c("ConsumDisc","Materials","Materials","ConsumDisc",
                           "Materials","Undustrials","Industrials","Materials",
                           "Materials","ConsumDisc")))
colnames(adds) <- c("Ticker","Sector")
(adds)

to_add <- c("DOW","LCII","PNR","CCF")

################################################################################
### revise data set membership (replace 4 stocks)
# replace in "factorsSPGMI to start. daily/weekly data sets to be modified later

# check membership
sector_memb <- factorsSPGMI_tmp[,c("TickerLast","Sector")]
counts <- aggregate(data = sector_memb,
                    TickerLast ~ Sector,
                    function(x) length(unique(x)))
counts$Pcts <- counts$TickerLast/sum(counts$TickerLast)
# compare against percents drawn from U.S. equity universe data
counts$MktPcts <- c(.065, .108, .052, .031, .137, .129, .132, .183, .049,
                    .059, .051)
counts$Diff <- counts$Pcts - counts$MktPcts
counts # 3 financials, 1 RealEstate to be removed. 

# tbc

################################################################################
### market cap groups
# cap group membership deemed incorrect, to be replaced.
# suggest a point-in-time replacement based on CRSP 70/85/98 percentile splits
# into LargeCap, MidCap, SmallCap, and MicroCap.
# For simplicity, could add an additional column titled "CapGroupLast" which 
# assigns a constant membership into a group based on the final allocation

# import market percentiles (from CompuStat)
MktCapPercentiles <- read.csv("~/R-Work/FactorAnalytics/sandbox/Vestcor Data Cleaning/MktCapPercentiles.csv")
factorsSPGMI_tmp$MktCap <- exp(factorsSPGMI_tmp$LogMktCap)

factorsSPGMI_tmp$CapGroupLC <- MktCapPercentiles$LC[match(as.Date(factorsSPGMI_tmp$Date),as.Date(MktCapPercentiles$Date))]
factorsSPGMI_tmp$CapGroupMC <- MktCapPercentiles$MC[match(as.Date(factorsSPGMI_tmp$Date),as.Date(MktCapPercentiles$Date))]
factorsSPGMI_tmp$CapGroupSC <- MktCapPercentiles$SC[match(as.Date(factorsSPGMI_tmp$Date),as.Date(MktCapPercentiles$Date))]

factorsSPGMI_tmp$CapGroupPIT <- ifelse(factorsSPGMI_tmp$MktCap < factorsSPGMI_tmp$CapGroupSC,"MicroCap",
                                       ifelse(factorsSPGMI_tmp$MktCap < factorsSPGMI_tmp$CapGroupMC,"SmallCap",
                                              ifelse(factorsSPGMI_tmp$MktCap < factorsSPGMI_tmp$CapGroupLC,"MidCap",
                                                     "LargeCap")))
factorsSPGMI_tmp <- factorsSPGMI_tmp[,c("Date","TickerLast","Ticker","Company",
                                        "CapGroup","CapGroupPIT","GICS",
                                        "Sector","AnnVol12M","Beta60M","BP","EP",
                                        "LogMktCap","PM12M1M","AccrualRatioCF",
                                        "AstAdjChg1YOCF","CFROIC","Chg1YAstTo",
                                        "EBITDAEV","FCFP","PM1M","SEV")]

################################################################################
### save data
save(factorsSPGMI_tmp, file = "factorsSPGMI_tmp.rda")
