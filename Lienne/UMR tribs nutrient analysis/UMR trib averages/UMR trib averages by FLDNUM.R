UMR_trib_Si = subset(UMR_trib_averages, UMR_trib_averages$parameter=="SI")
UMR_trib_SiTN = subset(UMR_trib_averages, UMR_trib_averages$parameter=="SiTN")
UMR_trib_SiTP = subset(UMR_trib_averages, UMR_trib_averages$parameter=="SiTP")

library(ggplot2)
#UMR trib averages box plot for [Si], Si:TN, and Si:TP by FLDNUM
UMR_tribSiavg_fldnum = ggplot(UMR_trib_Si, aes(x=FLDNUM, y=mean.val, group=FLDNUM))+
  geom_boxplot()+
  labs(title="UMR tributaries average Si concentration by FLD NUM", y="Si concentration (mg SiO2/L)")+
  scale_x_continuous(breaks=seq(0,7,1))
UMR_tribSiavg_fldnum

UMR_tribSiTNavg_fldnum = ggplot(UMR_trib_SiTN, aes(x=FLDNUM, y=mean.val, group=FLDNUM))+
  geom_boxplot()+
  labs(title="UMR tributaries average Si:TN by FLD NUM", y="Molar Si:TN")+
  scale_x_continuous(breaks=seq(0,7,1))
UMR_tribSiTNavg_fldnum

UMR_tribSiTPavg_fldnum = ggplot(UMR_trib_SiTP, aes(x=FLDNUM, y=mean.val, group=FLDNUM))+
  geom_boxplot()+
  labs(title="UMR tributaries average Si:TP by FLD NUM", y="Molar Si:TP")+
  scale_x_continuous(breaks=seq(0,7,1))
UMR_tribSiTNavg_fldnum
