#parse out trib data from Fixed_summarystats
Trib_nutrientavg = subset(Fixed_summarystats, Fixed_summarystats$dataset=="trib")

#create dataframe of trib averages for Si, N, and P
Trib_Siavg = subset(Trib_nutrientavg, Trib_nutrientavg$parameter=="SI")
Trib_Navg = subset(Trib_nutrientavg, Trib_nutrientavg$parameter=="TN")
Trib_Pavg = subset(Trib_nutrientavg, Trib_nutrientavg$parameter=="TP")

TribLOCAT_Si = data.frame(LOCATCD=Trib_Siavg$LOCATCD, Si_avg=Trib_Siavg$mean.val)
TribLOCAT_TN = data.frame(LOCATCD=Trib_Navg$LOCATCD, TN_avg=Trib_Navg$mean.val)
TribLOCAT_TP = data.frame(LOCATCD=Trib_Pavg$LOCATCD, TN_avg=Trib_Pavg$mean.val)

#combine land use/litho data with nutrient averages
#which LOCATCDs are missing?
Trib_LOCATCD=data.frame(LOCATCD=unique(Trib_nutrientavg$LOCATCD))
setdiff(Trib_LOCATCD, Trib_LULCLith_6.8.19)
#Not sure how to code this, all 26 tribs return

Trib_LULCLith_Siavg = merge(Trib_LULCLith_6.8.19, TribLOCAT_Si, by="LOCATCD")
Trib_LULCLith_TNavg = merge(Trib_LULCLith_6.8.19, TribLOCAT_TN, by="LOCATCD")
Trib_LULCLith_TPavg = merge(Trib_LULCLith_6.8.19, TribLOCAT_TP, by="LOCATCD")

#multiple linear regression for LULC and lithology with average nutrient concentrations