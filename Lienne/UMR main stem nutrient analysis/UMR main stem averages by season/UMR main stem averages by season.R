#subset out [Si], Si:TN, and Si:TP for plots
UMR_mainstem_seasonalSiavg = subset(UMR_main_stem_averages_by_season, UMR_main_stem_averages_by_season$parameter=="SI")
UMR_mainstem_seasonalSiTNavg = subset(UMR_main_stem_averages_by_season, UMR_main_stem_averages_by_season$parameter=="SiTN")
UMR_mainstem_seasonalSiTPavg = subset(UMR_main_stem_averages_by_season, UMR_main_stem_averages_by_season$parameter=="SiTP")

library(ggplot2)

##=================
#plot main stem [Si], Si:TN, Si:TP by season, each plot includes all FLD NUMs
##=================
UMR_mainstem_seasonalSiavg$season.f = factor(UMR_mainstem_seasonalSiavg$season, 
                                             levels=c('Summer', 'Fall', 'Winter', 'Spring'))
UMR_seasonalSiavg_allFLDNUM = ggplot(UMR_mainstem_seasonalSiavg, aes(x=season.f, y=mean.val))+
  geom_boxplot()+
  labs(title="UMR main stem average Si concentration by season", 
       y="Mean Si conc (mg SiO2/L)",
       x="Season")
UMR_seasonalSiavg_allFLDNUM
ggsave(file="UMR main stem average Si by season.png", width=10, height=7)

UMR_mainstem_seasonalSiTNavg$season.f = factor(UMR_mainstem_seasonalSiTNavg$season, 
                                             levels=c('Summer', 'Fall', 'Winter', 'Spring'))
UMR_seasonalSiTNavg_allFLDNUM = ggplot(UMR_mainstem_seasonalSiTNavg, aes(x=season.f, y=mean.val))+
  geom_boxplot()+
  labs(title="UMR main stem average Si:TN by season", 
       y="Molar Si:TN",
       x="Season")
UMR_seasonalSiTNavg_allFLDNUM
ggsave(file="UMR main stem average SiTN by season.png", width=10, height=7)

UMR_mainstem_seasonalSiTPavg$season.f = factor(UMR_mainstem_seasonalSiTPavg$season, 
                                                           levels=c('Summer', 'Fall', 'Winter', 'Spring'))
UMR_seasonalSiTPavg_allFLDNUM = ggplot(UMR_mainstem_seasonalSiTPavg, aes(x=season.f, y=mean.val))+
  geom_boxplot()+
  labs(title="UMR main stem average Si:TP by season", 
       y="Molar Si:TP",
       x="Season")
UMR_seasonalSiTPavg_allFLDNUM
ggsave(file="UMR main stem average SiTP by season.png", width=10, height=7)

##=================
#ANOVA between seasonal averages to test for significant seasonal differences
#compute summary statistics by group
library(dplyr)
Si_averages=data.frame(group_by(UMR_mainstem_seasonalSiavg, season) %>%
                           summarize(
                             count=n(),
                             mean=mean(mean.val, na.rm=TRUE)
                           ))

UMR_mainstem_SiANOVA = aov(mean.val~season.f, data=UMR_mainstem_seasonalSiavg)
summary(UMR_mainstem_SiANOVA)
#check homogeneity of variances
plot(UMR_mainstem_SiANOVA, 1)
#check normality
plot(UMR_mainstem_SiANOVA, 2)
##extract residuals
UMR_mainstem_SiAVOVA_residuals = residuals(object=UMR_mainstem_SiANOVA)
##run Shapiro-Wilk test
shapiro.test(x=UMR_mainstem_SiAVOVA_residuals)

SiTN_averages=data.frame(group_by(UMR_mainstem_seasonalSiTNavg, season) %>%
                           summarize(
                             count=n(),
                             mean=mean(mean.val, na.rm=TRUE)
                           ))

UMR_mainstem_SiTNANOVA = aov(mean.val~season.f, data=UMR_mainstem_seasonalSiTNavg)
summary(UMR_mainstem_SiTNANOVA)
plot(UMR_mainstem_SiTNANOVA, 1)
plot(UMR_mainstem_SiTNANOVA, 2)
UMR_mainstem_SiTNAVOVA_residuals = residuals(object=UMR_mainstem_SiTNANOVA)
shapiro.test(x=UMR_mainstem_SiTNAVOVA_residuals)
#statistical differences in season for Si:TN
#post-hoc pairwise comparisons
pairwise.t.test(UMR_mainstem_seasonalSiTNavg$mean.val, UMR_mainstem_seasonalSiTNavg$season)

UMR_mainstem_SiTPANOVA = aov(mean.val~season.f, data=UMR_mainstem_seasonalSiTPavg)
summary(UMR_mainstem_SiTPANOVA)
plot(UMR_mainstem_SiTPANOVA, 1)
plot(UMR_mainstem_SiTPANOVA, 2)
UMR_mainstem_SiTPAVOVA_residuals = residuals(object=UMR_mainstem_SiTPANOVA)
shapiro.test(x=UMR_mainstem_SiTPAVOVA_residuals)

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
ggsave(file="UMR main stem FLDNUM Si by season.png", width=10, height=7)

UMR_seasonalSiTNavg_allFLDNUM$season.f = factor(UMR_mainstem_seasonalSiTNavg$season, 
                                             levels=c('Summer', 'Fall', 'Winter', 'Spring'))
UMR_seasonalSiTNavg_byFLDNUM = ggplot(UMR_mainstem_seasonalSiTNavg, aes(x=FLDNUM, y=mean.val, group=FLDNUM))+
  geom_boxplot()+
  labs(title="UMR main stem average Si:TN by season", 
       y="Molar Si:TN")+
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
