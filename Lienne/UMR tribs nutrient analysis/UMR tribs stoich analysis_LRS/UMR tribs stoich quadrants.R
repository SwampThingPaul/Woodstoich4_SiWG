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

#takes the mean and sd in each column
UMR_site1_stoichavg = aggregate(UMR_site1, by=list(UMR_site1$DOY), FUN=mean)
UMR_site1_stoichsd = aggregate(UMR_site1, by=list(UMR_site1$DOY), FUN=sd)
UMR_site2_stoichavg = aggregate(UMR_site2, by=list(UMR_site2$DOY), FUN=mean)
UMR_site2_stoichsd = aggregate(UMR_site2, by=list(UMR_site2$DOY), FUN=sd)
UMR_site3_stoichavg = aggregate(UMR_site3, by=list(UMR_site3$DOY), FUN=mean)
UMR_site3_stoichsd = aggregate(UMR_site3, by=list(UMR_site3$DOY), FUN=sd)
UMR_site4_stoichavg = aggregate(UMR_site4, by=list(UMR_site4$DOY), FUN=mean)
UMR_site4_stoichsd = aggregate(UMR_site4, by=list(UMR_site4$DOY), FUN=sd)
UMR_site5_stoichavg = aggregate(UMR_site5, by=list(UMR_site5$DOY), FUN=mean)
UMR_site5_stoichsd = aggregate(UMR_site5, by=list(UMR_site5$DOY), FUN=sd)
UMR_site6_stoichavg = aggregate(UMR_site6, by=list(UMR_site6$DOY), FUN=mean)
UMR_site6_stoichsd = aggregate(UMR_site6, by=list(UMR_site6$DOY), FUN=sd)
UMR_site7_stoichavg = aggregate(UMR_site7, by=list(UMR_site7$DOY), FUN=mean)
UMR_site7_stoichsd = aggregate(UMR_site7, by=list(UMR_site7$DOY), FUN=sd)
#why do the calculated average N:P:Si values aggregate to NA?

UMR_site1_DOYavg = data.frame("DOY"=UMR_site1_stoichavg$Group.1, 
                              "Mean Molar N"=UMR_site1_stoichavg$MolarTN, "SD Molar N"=UMR_site1_stoichsd$MolarTN,
                              "Mean Molar P"=UMR_site1_stoichavg$MolarTP, "SD Molar P"=UMR_site1_stoichsd$MolarTP,
                              "Mean Molar Si"=UMR_site1_stoichavg$MolarSi, "SD Molar Si"=UMR_site1_stoichsd$MolarSi)

#plot average N, P, and Si by DOY
library(ggplot2)
UMR_site1_nutavg = ggplot(data=UMR_site1_DOYavg)+
  geom_point(mapping = aes(x=DOY, y=Mean.Molar.N))











