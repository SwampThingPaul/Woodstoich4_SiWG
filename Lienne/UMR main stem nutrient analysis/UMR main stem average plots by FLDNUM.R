UMR_mainstem_Si = subset(UMR_main_stem_averages, UMR_main_stem_averages$parameter=="SI")
UMR_mainstem_SiTN = subset(UMR_main_stem_averages, UMR_main_stem_averages$parameter=="SiTN")
UMR_mainstem_SiTP = subset(UMR_main_stem_averages, UMR_main_stem_averages$parameter=="SiTP")

library(ggplot2)
#UMR main stem averages box plot for [Si], Si:TN, and Si:TP by FLDNUM
UMR_Siavg_fldnum = ggplot(UMR_mainstem_Si, aes(x=FLDNUM, y=mean.val, group=FLDNUM))+
  geom_boxplot()+
  labs(title="UMR main stem average Si concentration by FLD NUM", y="Mean Si conc (mg SiO2/L)")+
  scale_x_continuous(breaks=seq(0,7,1))
UMR_Siavg_fldnum

UMR_SiTNavg_fldnum = ggplot(UMR_mainstem_SiTN, aes(x=FLDNUM, y=mean.val, group=FLDNUM))+
  geom_boxplot()+
  labs(title="UMR main stem average Si:TN by FLD NUM", y="Molar Si:TN")+
  scale_x_continuous(breaks=seq(0,7,1))
UMR_SiTNavg_fldnum

UMR_SiTPavg_fldnum = ggplot(UMR_mainstem_SiTP, aes(x=FLDNUM, y=mean.val, group=FLDNUM))+
  geom_boxplot()+
  labs(title="UMR main stem average Si:TP by FLD NUM", y="Molar Si:TP")+
  scale_x_continuous(breaks=seq(0,7,1))
UMR_SiTPavg_fldnum
