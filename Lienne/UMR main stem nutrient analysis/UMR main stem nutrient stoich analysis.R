#calculate N:Si and P:Si using molar concentrations
#Si reported as mg Si/L
#TN reported as mg N/L
#TP reported as mg P/L
UMR_mainstem_nutrients$molarTP = UMR_mainstem_nutrients$TP/30974
UMR_mainstem_nutrients$molarTN = UMR_mainstem_nutrients$TN/14007
UMR_mainstem_nutrients$molarSi = UMR_mainstem_nutrients$SI/28085

UMR_mainstem_nutrients$NSi = UMR_mainstem_nutrients$molarTN/UMR_mainstem_nutrients$molarSi
UMR_mainstem_nutrients$PSi = UMR_mainstem_nutrients$molarTP/UMR_mainstem_nutrients$molarSi

#remove negative values from ratio calculations
UMR_mainstem_nutrients$NSi_ratio = ifelse(UMR_mainstem_nutrients$NSi < 0, "", UMR_mainstem_nutrients$NSi)
UMR_mainstem_nutrients$PSi_ratio = ifelse(UMR_mainstem_nutrients$PSi < 0, "", UMR_mainstem_nutrients$PSi)

plot(UMR_mainstem_nutrients$NSi_ratio, UMR_mainstem_nutrients$PSi_ratio)

#calculate average N:Si and P:Si by day of year (DOY)
library(lubridate)
UMR_mainstem_nutrients$DOY = yday(UMR_mainstem_nutrients$DATE)
UMR_mainstem_stoichavg = aggregate(UMR_mainstem_stoich, by=list(UMR_mainstem_stoich$DOY), FUN=mean)
plot(UMR_mainstem_stoichavg$DOY, UMR_mainstem_nutrients$NSi_ratio, 
     xlab="DOY", ylab="Average N:Si ratio", main="UMR mainstem sites 1-7",
     cex=0.5)

##separate by sites
UMR_mainstem_site1 = subset(UMR_mainstem_nutrients, UMR_mainstem_nutrients$FLDNUM==1)
UMR_mainstem_site2 = subset(UMR_mainstem_nutrients, UMR_mainstem_nutrients$FLDNUM==2)
UMR_mainstem_site3 = subset(UMR_mainstem_nutrients, UMR_mainstem_nutrients$FLDNUM==3)
UMR_mainstem_site4 = subset(UMR_mainstem_nutrients, UMR_mainstem_nutrients$FLDNUM==4)
UMR_mainstem_site5 = subset(UMR_mainstem_nutrients, UMR_mainstem_nutrients$FLDNUM==5)
UMR_mainstem_site6 = subset(UMR_mainstem_nutrients, UMR_mainstem_nutrients$FLDNUM==6)
UMR_mainstem_site7 = subset(UMR_mainstem_nutrients, UMR_mainstem_nutrients$FLDNUM==7)

UMR_site1_stoichavg = data.frame(aggregate(UMR_mainstem_site1, by=list(UMR_mainstem_site1$DOY), FUN=mean))
plot(UMR_site1_stoichavg$DOY, UMR_site1_stoichavg$NSi, xlab="DOY", ylab="Average N:Si", main="UMR Site 1")
plot(UMR_site1_stoichavg$DOY, UMR_site1_stoichavg$PSi, xlab="DOY", ylab="Average P:Si", main="UMR Site 1")

UMR_site2_stoichavg = data.frame(aggregate(UMR_mainstem_site2, by=list(UMR_mainstem_site2$DOY), FUN=mean))
plot(UMR_site2_stoichavg$DOY, UMR_site2_stoichavg$NSi, xlab="DOY", ylab="Average N:Si", main="UMR Site 2")

