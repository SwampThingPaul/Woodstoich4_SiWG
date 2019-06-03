UMR_mainstem_seasonalSiavg = subset(UMR_main_stem_averages_by_season, UMR_main_stem_averages_by_season$parameter=="SI")
UMR_mainstem_seasonalSiTNavg = subset(UMR_main_stem_averages_by_season, UMR_main_stem_averages_by_season$parameter=="SiTN")
UMR_mainstem_seasonalSiTPavg = subset(UMR_main_stem_averages_by_season, UMR_main_stem_averages_by_season$parameter=="SiTP")

#plot main stem [Si], Si:TN, Si:TP by season, each plot includes all FLD NUMs
library(ggplot2)

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


##================
#plot FLDNUMs on different plots
#use facet_grid?
##================
UMR_mainstem_seasonalSiavg$FLDNUM.f = factor(UMR_mainstem_seasonalSiavg$FLDNUM)
UMR_seasonalSiavg_byFLDNUM = ggplot(UMR_mainstem_seasonalSiavg, aes(x=season, y=mean.val))+
  geom_boxplot()+
  facet_grid(. ~ FLDNUM.f)
  labs(title="UMR main stem average Si concentration by season", y="Mean Si conc (mg SiO2/L")
UMR_seasonalSiavg_byFLDNUM
