#add relative downstream position to UMR_trib_averages dataset
Trib_names_locations=Trib_names_locations[c("LOCATCD", "Relative Downstream Position")]
UMR_trib_averages_locat=merge(UMR_trib_averages, Trib_names_locations, by="LOCATCD")
UMR_trib_averages_locat$FLDNUM=as.factor(UMR_trib_averages_locat$FLDNUM)

UMR_trib_Si = subset(UMR_trib_averages_locat, UMR_trib_averages_locat$parameter=="SI")
UMR_trib_SiTN = subset(UMR_trib_averages_locat, UMR_trib_averages_locat$parameter=="SiTN")
UMR_trib_SiTP = subset(UMR_trib_averages_locat, UMR_trib_averages_locat$parameter=="SiTP")

library(ggplot2)
#UMR trib averages box plot for [Si], Si:TN, and Si:TP by relative downstream position
UMR_tribSiavg_DSposi = ggplot(UMR_trib_Si, aes(x=`Relative Downstream Position`, y=mean.val))+
  geom_point(aes(color=FLDNUM), size=3)+
  geom_smooth(method='lm', color="black")+
  labs(title="UMR tributaries average Si concentration", y="Si concentration (mg SiO2/L)")+
  scale_x_continuous(breaks=seq(0,23,1))
UMR_tribSiavg_DSposi

UMR_tribSiTNavg_DSposi = ggplot(UMR_trib_SiTN, aes(x=`Relative Downstream Position`, y=mean.val))+
  geom_point(aes(color=FLDNUM), size=3)+
  geom_smooth(method='lm', color="black")+
  labs(title="UMR tributaries average Si:TN", y="Molar Si:TN")+
  scale_x_continuous(breaks=seq(0,23,1))
UMR_tribSiTNavg_DSposi

UMR_tribSiTPavg_DSposi = ggplot(UMR_trib_SiTP, aes(x=`Relative Downstream Position`, y=mean.val))+
  geom_point(aes(color=FLDNUM), size=3)+
  geom_smooth(method='lm', color="black")+
  labs(title="UMR tributaries average Si:TP", y="Molar Si:TP")+
  scale_x_continuous(breaks=seq(0,23,1))
UMR_tribSiTPavg_DSposi
