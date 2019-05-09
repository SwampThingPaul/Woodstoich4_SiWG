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
abline(v=1, h=20, col="red", type=3)
