#calculate N:P:Si using molar concentrations
#Si reported as mg Si/L
#TN reported as mg N/L
#TP reported as mg P/L
UMRTribs_NutrientStoich_LRS$MolarSi = UMRTribs_NutrientStoich_LRS$SI/28085
UMRTribs_NutrientStoich_LRS$MolarTN = UMRTribs_NutrientStoich_LRS$TN/14007
UMRTribs_NutrientStoich_LRS$MolarTP = UMRTribs_NutrientStoich_LRS$TP/30974

UMRTribs_NutrientStoich_LRS$MolarSi_P = UMRTribs_NutrientStoich_LRS$MolarSi/UMRTribs_NutrientStoich_LRS$MolarTP
UMRTribs_NutrientStoich_LRS$MolarSi_N = UMRTribs_NutrientStoich_LRS$MolarSi/UMRTribs_NutrientStoich_LRS$MolarTN

#remove negative values from stoich calculations
UMRTribs_NutrientStoich_LRS$MolarSi_P = ifelse(UMRTribs_NutrientStoich_LRS$MolarSi_P<0, "", UMRTribs_NutrientStoich_LRS$MolarSi_P)
UMRTribs_NutrientStoich_LRS$MolarSi_N = ifelse(UMRTribs_NutrientStoich_LRS$MolarSi_N<0, "", UMRTribs_NutrientStoich_LRS$MolarSi_N)

#plot Si:N vs. Si:P
plot(UMRTribs_NutrientStoich_LRS$MolarSi_P ~ UMRTribs_NutrientStoich_LRS$MolarSi_N)
abline(v=1, h=20, col="red")

#calculate average N:P:Si by day of year (DOY)
library(lubridate)
UMRTribs_NutrientStoich_LRS$DOY = yday(UMRTribs_NutrientStoich_LRS$DATE)

#separate data by sites
UMR_site1 = subset(UMRTribs_NutrientStoich_LRS, UMRTribs_NutrientStoich_LRS$FLDNUM == 1)
UMR_site2 = subset(UMRTribs_NutrientStoich_LRS, UMRTribs_NutrientStoich_LRS$FLDNUM == 2)
UMR_site3 = subset(UMRTribs_NutrientStoich_LRS, UMRTribs_NutrientStoich_LRS$FLDNUM == 3)
UMR_site4 = subset(UMRTribs_NutrientStoich_LRS, UMRTribs_NutrientStoich_LRS$FLDNUM == 4)
UMR_site5 = subset(UMRTribs_NutrientStoich_LRS, UMRTribs_NutrientStoich_LRS$FLDNUM == 5)
UMR_site6 = subset(UMRTribs_NutrientStoich_LRS, UMRTribs_NutrientStoich_LRS$FLDNUM == 6)
UMR_site7 = subset(UMRTribs_NutrientStoich_LRS, UMRTribs_NutrientStoich_LRS$FLDNUM == 7)

