#get unique LOCATCDs for each mainstem site
UMR_mainstem_LOCATCD = data.frame("LOCATCD"=unique(UMR_Mainstem_FixedSite_1991.2018$LOCATCD))

#pair northing and easting values to unique mainstem sites
UMR_mainstem_LOCATCD$northing = merge(UMR_mainstem_LOCATCD, UMR_Mainstem_FixedSite_1991.2018$NORTHING, by="LOCATCD")
