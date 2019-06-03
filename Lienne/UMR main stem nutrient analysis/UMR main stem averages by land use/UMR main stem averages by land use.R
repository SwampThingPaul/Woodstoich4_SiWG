#add FLDNUM to pool_landcover
pool_landcover$FLDNUM = ifelse(pool_landcover$Region=="Pool 4",1,
                               ifelse(pool_landcover$Region=="Pool 8",2,
                                      ifelse(pool_landcover=="Pool 13",3,
                                             ifelse(pool_landcover=="Pool 26",4,
                                                    ifelse(pool_landcover=="OpenRiver",5,6)))))

#Is it better to add highest % of land cover to averages datafile?
UMR_main_stem_averages$landcover = ifelse(UMR_main_stem_averages$FLDNUM==1,"OpenWater",
                                    ifelse(UMR_main_stem_averages$FLDNUM==2,"OpenWater",
                                    ifelse(UMR_main_stem_averages$FLDNUM==3,"Agriculture",
                                    ifelse(UMR_main_stem_averages$FLDNUM==4,"Agriculture",
                                    ifelse(UMR_main_stem_averages$FLDNUM==5,"Agriculture","Agriculture")))))
#probably not the best way to do this...