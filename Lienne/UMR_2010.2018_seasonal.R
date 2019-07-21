#Get mainstem data from all SRS sites between 2010-2018
UMR_MainStem_SRSdataset_1993.2018$DATE = as.Date(UMR_MainStem_SRSdataset_1993.2018$DATE, "%m/%d/%Y")
UMR_mainstem_SRS_2010.2018 = subset(UMR_MainStem_SRSdataset_1993.2018, 
                                    UMR_MainStem_SRSdataset_1993.2018$year >= 2010)
UMR_mainstem_2010.2018 = UMR_mainstem_SRS_2010.2018[c("DATE", "year", "TN", "TP", "SI")]
names(UMR_mainstem_2010.2018)[2]="YEAR"
UMR_mainstem_2010.2018$SITE_TYPE = "main"

#Get trib data from all sites between 2010-2017
UMRTribs_Filtered_1.26.18$DATE = as.Date(UMRTribs_Filtered_1.26.18$DATE, "%m/%d/%Y")
UMR_tribs_2010.2017 = subset(UMRTribs_Filtered_1.26.18,
                             UMRTribs_Filtered_1.26.18$YEAR >= 2010)
UMR_tribs_2010.2017 = UMR_tribs_2010.2017[c("DATE", "YEAR", "TN", "TP", "SI")]
UMR_tribs_2010.2017$SITE_TYPE = "trib"

#merge all data together
UMR_2010.2018 = merge(UMR_tribs_2010.2017, UMR_mainstem_2010.2018, all=TRUE)
UMR_2010.2018 = na.omit(UMR_2010.2018)

#add month and season to merged data
library(lubridate)
UMR_2010.2018$MONTH = month(UMR_2010.2018$DATE, label=T, abbr=T)
library(hydroTSM)
UMR_2010.2018$SEASON = time2season(UMR_2010.2018$DATE, out.fmt = "seasons", type = "default")

#plot Si concentrations by month in mainstem and tribs
library(ggplot2)
UMR_monthlySi = ggplot(UMR_2010.2018, aes(x=MONTH, y=SI, color=SITE_TYPE))+
  geom_boxplot()+
  labs(title="Si concentrations in UMR mainstem and tributaries", y="Si conc (mg SiO2/L")
UMR_monthlySi

#create box plots for Si concentration, Si:TN, and Si:TP by season
UMR_2010.2018$SEASON.f = factor(UMR_2010.2018$SEASON, levels=c('summer', 'autumm', 'winter', 'spring'))
UMR_seasonalSi = ggplot(UMR_2010.2018, aes(x=SEASON, y=SI))+
  geom_boxplot()+
  labs(title="Si concentrations in UMR mainstem and tributaries", y="Si conc (mg SiO2/L)")
UMR_seasonalSi
