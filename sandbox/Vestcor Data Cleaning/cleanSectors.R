data("factorsSPGMI")
factorsSPGMIvestcor <- factorsSPGMI

# clean up list of sectors
bad_sectors <- unique(factorsSPGMIvestcor$Sector)
good_sectors <- c("InfoTech","Industrials","Healthcare","ConsumStap","Energy",
                 "Materials","ConsumDisc","ComServices","Utilities","RealEstate",
                 "Healthcare","Financials","ConsumDisc","InfoTech","ConsumStap",
                 "ComServices")
sector_table <- data.frame(cbind(bad_sectors,good_sectors))
colnames(sector_table) <- c("BadSectors","GoodSectors")
factorsSPGMIvestcor$Sector <- sector_table$GoodSectors[match(factorsSPGMIvestcor$Sector,sector_table$BadSectors)]

# check - should be 11 unique sectors with correct names as per FAdataCRSPandSPGMI 02 23 22.pdf
unique(factorsSPGMIvestcor$Sector)
