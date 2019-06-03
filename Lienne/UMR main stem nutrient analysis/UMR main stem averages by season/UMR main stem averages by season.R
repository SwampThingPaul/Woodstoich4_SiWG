#subset out [Si], Si:TN, and Si:TP for plots
UMR_mainstem_seasonalSiavg = subset(UMR_main_stem_averages_by_season, UMR_main_stem_averages_by_season$parameter=="SI")
UMR_mainstem_seasonalSiTNavg = subset(UMR_main_stem_averages_by_season, UMR_main_stem_averages_by_season$parameter=="SiTN")
UMR_mainstem_seasonalSiTPavg = subset(UMR_main_stem_averages_by_season, UMR_main_stem_averages_by_season$parameter=="SiTP")

library(ggplot2)

##=================
#plot main stem [Si], Si:TN, Si:TP by season, each plot includes all FLD NUMs
##=================
UMR_seasonalSiavg_allFLDNUM = ggplot(UMR_mainstem_seasonalSiavg, aes(x=season, y=mean.val))+
  geom_boxplot()+
  labs(title="UMR main stem average Si concentration by season", y="Mean Si conc (mg SiO2/L")
UMR_seasonalSiavg_allFLDNUM

UMR_seasonalSiTNavg_allFLDNUM = ggplot(UMR_mainstem_seasonalSiTNavg, aes(x=season, y=mean.val))+
  geom_boxplot()+
  labs(title="UMR main stem average Si:TN by season", y="Molar Si:TN")
UMR_seasonalSiTNavg_allFLDNUM

UMR_seasonalSiTPavg_allFLDNUM = ggplot(UMR_mainstem_seasonalSiTPavg, aes(x=season, y=mean.val))+
  geom_boxplot()+
  labs(title="UMR main stem average Si:TP by season", y="Molar Si:TP")
UMR_seasonalSiTPavg_allFLDNUM

##=================
#plot by FLDNUMs with seasons on different plots
#use factor command to order seasons by time rather than alphabetically
##=================
UMR_mainstem_seasonalSiavg$season.f = factor(UMR_mainstem_seasonalSiavg$season, 
                                             levels=c('Summer', 'Fall', 'Winter', 'Spring'))
UMR_seasonalSiavg_byFLDNUM = ggplot(UMR_mainstem_seasonalSiavg, aes(x=FLDNUM, y=mean.val, group=FLDNUM))+
  geom_boxplot()+
  labs(title="UMR main stem average Si concentration by season", y="Mean Si conc (mg SiO2/L")+
  scale_x_continuous(breaks=seq(0,7,1))+
  facet_wrap(~season.f)
UMR_seasonalSiavg_byFLDNUM

UMR_mainstem_seasonalSiTNavg$season.f = factor(UMR_mainstem_seasonalSiTNavg$season, 
                                             levels=c('Summer', 'Fall', 'Winter', 'Spring'))
UMR_seasonalSiTNavg_byFLDNUM = ggplot(UMR_mainstem_seasonalSiTNavg, aes(x=FLDNUM, y=mean.val, group=FLDNUM))+
  geom_boxplot()+
  labs(title="UMR main stem average Si:TN by season", y="Molar Si:TN")+
  scale_x_continuous(breaks=seq(0,7,1))+
  facet_wrap(~season.f)
UMR_seasonalSiTNavg_byFLDNUM

UMR_mainstem_seasonalSiTPavg$season.f = factor(UMR_mainstem_seasonalSiTPavg$season, 
                                               levels=c('Summer', 'Fall', 'Winter', 'Spring'))
UMR_seasonalSiTPavg_byFLDNUM = ggplot(UMR_mainstem_seasonalSiTPavg, aes(x=FLDNUM, y=mean.val, group=FLDNUM))+
  geom_boxplot()+
  labs(title="UMR main stem average Si:TP by season", y="Molar Si:TP")+
  scale_x_continuous(breaks=seq(0,7,1))+
  facet_wrap(~season.f)
UMR_seasonalSiTPavg_byFLDNUM
