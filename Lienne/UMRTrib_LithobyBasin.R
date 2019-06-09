#Create new column combining basin name and lithology
Trib_LULC_Lith_6_5_19$Basin_Litho = paste(Trib_LULC_Lith_6_5_19$`Basin Name`,Trib_LULC_Lith_6_5_19$LITH62,sep="_")

#sum lithology types within basins
UMR_trib_litho=data.frame(tapply(Trib_LULC_Lith_6_5_19$Area_PCT,Trib_LULC_Lith_6_5_19$Basin_Litho, sum))
