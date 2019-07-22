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

#plot Si concentrations by DOY and month in mainstem and tribs
UMR_2010.2018$DOY = yday(UMR_2010.2018$DATE)
library(ggplot2)
UMR_dailySi = ggplot(UMR_2010.2018, aes(x=DOY, y=SI, color=SITE_TYPE))+
  geom_point()+
  labs(title="Si concentrations in UMR mainstem and tributaries", 
       y="Si conc (mg SiO2/L)")
UMR_dailySi

UMR_monthlySi = ggplot(UMR_2010.2018, aes(x=MONTH, y=SI, color=SITE_TYPE))+
  geom_boxplot()+
  labs(title="Si concentrations in UMR mainstem and tributaries", 
       y="Si conc (mg Si/L)")
UMR_monthlySi
ggsave(file="UMR monthly Si conc.png", width=10, height=7)

#calculate Si:TN and Si:TP
UMR_2010.2018$SiTN = (UMR_2010.2018$SI/28085)/(UMR_2010.2018$TN/14006)
UMR_2010.2018$SiTP = (UMR_2010.2018$SI/28085)/(UMR_2010.2018$TP/30973)

#plot Si:TN and Si:TP by month
UMR_monthlySiTN = ggplot(UMR_2010.2018, aes(x=MONTH, y=SiTN, color=SITE_TYPE))+
  geom_boxplot()+
  labs(title="Si:TN in UMR mainstem and tributaries", 
       y="Molar Si:TN")+
  ylim(0,10)
UMR_monthlySiTN
ggsave(file="UMR monthly SiTN.png", width=10, height=7)

UMR_monthlySiTP = ggplot(UMR_2010.2018, aes(x=MONTH, y=SiTP, color=SITE_TYPE))+
  geom_boxplot()+
  labs(title="Si:TP in UMR mainstem and tributaries", 
       y="Molar Si:TP")
UMR_monthlySiTP
ggsave(file="UMR monthly SiTP.png", width=10, height=7)

#create new dataframe with average Si, Si:TN, Si:TP by month
UMR_monthlyavg = aggregate(UMR_2010.2018, by=list(UMR_2010.2018$MONTH), FUN=mean)
UMR_monthlyavg = UMR_monthlyavg[c("Group.1", "TN", "TP", "SI", "SiTN", "SiTP")]

#plot average Si concentration, Si:TN, and Si:TP by month
UMR_avgmonthlySi = ggplot(UMR_monthlyavg, aes(x=Group.1, y=SI))+
  geom_point()+
  ylab("Average Si concentrations (mg Si/L)")
UMR_avgmonthlySiTN = ggplot(UMR_monthlyavg, aes(x=Group.1, y=SiTN))+
  geom_point()+
  ylab("Average MolarSi:TN")
UMR_avgmonthlySiTP = ggplot(UMR_monthlyavg, aes(x=Group.1, y=SiTP))+
  geom_point()+
  ylab("Average MolarSi:TP")
library(gridExtra)
grid.arrange(UMR_avgmonthlySi, UMR_avgmonthlySiTN, UMR_avgmonthlySiTP)

#calculate average Si, Si:TN, and Si:TP for main stem and tribs seperately
UMR_tribs_2010.2017 = subset(UMR_2010.2018, UMR_2010.2018$SITE_TYPE == "trib")
UMR_mainstem_2010.2018 = subset(UMR_2010.2018, UMR_2010.2018$SITE_TYPE == "main")
UMR_tribavgs = aggregate(UMR_tribs_2010.2017, by=list(UMR_tribs_2010.2017$MONTH), FUN=mean)
UMR_mainavgs = aggregate(UMR_mainstem_2010.2018, by=list(UMR_mainstem_2010.2018$MONTH), FUN=mean)

UMR_avgtribmonthlySi = ggplot(UMR_tribavgs, aes(x=Group.1, y=SI))+
  geom_point()+
  labs(title="UMR tributaries average Si concentrations", y="Si concentration (mg Si/L)")
UMR_avgtribmonthlySiTN = ggplot(UMR_tribavgs, aes(x=Group.1, y=SiTN))+
  geom_point()+
  labs(title="UMR tributaries average Si:TN", y="Average Molar Si:TN")
UMR_avgtribmonthlySiTP = ggplot(UMR_tribavgs, aes(x=Group.1, y=SiTP))+
  geom_point()+
  labs(title="UMR tributaries average Si:TP", y="Average MolarSi:TP")
library(gridExtra)
grid.arrange(UMR_avgtribmonthlySi, UMR_avgtribmonthlySiTN, UMR_avgtribmonthlySiTP)

#create box plots for Si concentration, Si:TN, and Si:TP by season
UMR_2010.2018$SEASON.f = factor(UMR_2010.2018$SEASON, levels=c('summer', 'autumm', 'winter', 'spring'))
UMR_seasonalSi = ggplot(UMR_2010.2018, aes(x=SEASON, y=SI))+
  geom_boxplot()+
  labs(title="Si concentrations in UMR mainstem and tributaries", y="Si conc (mg SiO2/L)")
UMR_seasonalSi
