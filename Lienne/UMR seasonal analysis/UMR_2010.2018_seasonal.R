#Get mainstem data from all SRS sites between 2010-2018
UMR_MainStem_SRSdataset_1993_2018$DATE = as.Date(UMR_MainStem_SRSdataset_1993_2018$DATE, "%m/%d/%Y")
UMR_mainstem_SRS_2010.2018 = subset(UMR_MainStem_SRSdataset_1993_2018, 
                                    UMR_MainStem_SRSdataset_1993_2018$year >= 2010)
UMR_mainstem_2010.2018 = UMR_mainstem_SRS_2010.2018[c("DATE", "year", "TN", "TP", "SI")]
names(UMR_mainstem_2010.2018)[2]="YEAR"
UMR_mainstem_2010.2018$SITE_TYPE = "main"

#Get trib data from all sites between 2010-2017
UMRTribs_Filtered_1_26_18$DATE = as.Date(UMRTribs_Filtered_1_26_18$DATE, "%m/%d/%Y")
UMR_tribs_2010.2017 = subset(UMRTribs_Filtered_1_26_18,
                             UMRTribs_Filtered_1_26_18$YEAR >= 2010)
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
UMR_2010.2018$Msi = UMR_2010.2018$SI/28.085
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

#calculate summary stats for Si, Si:TN, Si:TP by month and site type
library(plyr)
UMR_monthlyavg = ddply(UMR_2010.2018, c("MONTH", "SITE_TYPE"), summarise,
                       Si_N = length(Msi),
                       Si_avg = mean(Msi),
                       Si_SD = sd(Msi),
                       Si_se = Si_SD/sqrt(Si_N),
                       SiTN_N = length(SiTN),
                       SiTN_avg = mean(SiTN),
                       SiTN_SD = sd(SiTN),
                       SiTN_se = SiTN_SD/sqrt(SiTN_N),
                       SiTP_N = length(SiTP),
                       SiTP_avg = mean(SiTP),
                       SiTP_SD = sd(SiTP),
                       SiTP_se = SiTP_SD/sqrt(SiTP_N))

#plot main stem and tributary averages
library(ggplot2)
UMR_monthlySi = ggplot(UMR_monthlyavg, aes(x=MONTH, y=Si_avg, color=SITE_TYPE))+
  geom_errorbar(aes(ymin=Si_avg-Si_se, ymax=Si_avg+Si_se), width=0.1)+
  geom_line(aes(x=MONTH, y=Si_avg, group=SITE_TYPE))+
  geom_point()+
  labs(y="Average Si concentration (mg Si/L)", x="")+
  theme(legend.position = "none")
UMR_monthlySiTN = ggplot(UMR_monthlyavg, aes(x=MONTH, y=SiTN_avg, color=SITE_TYPE))+
  geom_errorbar(aes(ymin=SiTN_avg-SiTN_se, ymax=SiTN_avg+SiTN_se), width=0.1)+
  geom_line(aes(x=MONTH, y=SiTN_avg, group=SITE_TYPE))+
  geom_point()+
  geom_hline(yintercept=1, linetype="dashed")+
  labs(y="Average Molar Si:TN", x="")+
  theme(legend.position = "none")
UMR_monthlySiTP = ggplot(UMR_monthlyavg, aes(x=MONTH, y=SiTP_avg, color=SITE_TYPE))+
  geom_errorbar(aes(ymin=SiTP_avg-SiTP_se, ymax=SiTP_avg+SiTN_se), width=0.1)+
  geom_line(aes(x=MONTH, y=SiTP_avg, group=SITE_TYPE))+
  geom_point()+
  geom_hline(yintercept=16, linetype="dashed")+
  labs(y="Average Molar Si:TP", x="Month")+
  theme(legend.position = "bottom")+
  scale_fill_discrete(name="Site Type")
UMR_monthlySiTP
library(gridExtra)
grid.arrange(UMR_monthlySi, UMR_monthlySiTN, UMR_monthlySiTP)

##==========================
##not using
#UMR_monthlyavg = aggregate(UMR_2010.2018, by=list(UMR_2010.2018$MONTH), FUN=mean)
#UMR_monthlySD = aggregate(UMR_2010.2018, by=list(UMR_2010.2018$MONTH), FUN=sd)
#UMR_monthlyavg = UMR_monthlyavg[c("Group.1", "TN", "TP", "SI", "SiTN", "SiTP")]
#UMR_monthlySD = UMR_monthlySD[c("Group.1", "TN", "TP", "SI", "SiTN", "SiTP")]
#UMR_monthlystats = data.frame("MONTH"=UMR_monthlyavg$Group.1,
#                              "Si_avg"=UMR_monthlyavg$SI,
#                              "SiTN_avg"=UMR_monthlyavg$SiTN,
#                              "SiTP_avg"=UMR_monthlyavg$SiTP,
#                              "Si_SD"=UMR_monthlySD$SI,
#                              "SiTN_SD"=UMR_monthlySD$SiTN,
#                              "SiTP_SD"=UMR_monthlySD$SiTP)

#plot average Si concentration, Si:TN, and Si:TP by month
#UMR_avgmonthlySi = ggplot(UMR_monthlyavg, aes(x=Group.1, y=SI))+
#  geom_point()+
#  ylab("Average Si concentrations (mg Si/L)")
#UMR_avgmonthlySiTN = ggplot(UMR_monthlyavg, aes(x=Group.1, y=SiTN))+
#  geom_point()+
#  ylab("Average MolarSi:TN")
#UMR_avgmonthlySiTP = ggplot(UMR_monthlyavg, aes(x=Group.1, y=SiTP))+
#  geom_point()+
#  ylab("Average MolarSi:TP")
#library(gridExtra)
#grid.arrange(UMR_avgmonthlySi, UMR_avgmonthlySiTN, UMR_avgmonthlySiTP)
#==========================

#redundant with ddply summary stats
#not using
#calculate average Si, Si:TN, and Si:TP for main stem and tribs seperately
#UMR_tribs_2010.2017 = subset(UMR_2010.2018, UMR_2010.2018$SITE_TYPE == "trib")
#UMR_mainstem_2010.2018 = subset(UMR_2010.2018, UMR_2010.2018$SITE_TYPE == "main")
#UMR_tribavgs = aggregate(UMR_tribs_2010.2017, by=list(UMR_tribs_2010.2017$MONTH), FUN=mean)
#UMR_mainavgs = aggregate(UMR_mainstem_2010.2018, by=list(UMR_mainstem_2010.2018$MONTH), FUN=mean)

#UMR_avgtribmonthlySi = ggplot(UMR_tribavgs, aes(x=Group.1, y=SI))+
#  geom_point()+
#  labs(title="UMR tributaries average Si concentrations", y="Si concentration (mg Si/L)")
#UMR_avgtribmonthlySiTN = ggplot(UMR_tribavgs, aes(x=Group.1, y=SiTN))+
#  geom_point()+
#  labs(title="UMR tributaries average Si:TN", y="Average Molar Si:TN")
#UMR_avgtribmonthlySiTP = ggplot(UMR_tribavgs, aes(x=Group.1, y=SiTP))+
#  geom_point()+
#  labs(title="UMR tributaries average Si:TP", y="Average MolarSi:TP")
#library(gridExtra)
#grid.arrange(UMR_avgtribmonthlySi, UMR_avgtribmonthlySiTN, UMR_avgtribmonthlySiTP)

#UMR_avgmainmonthlySi = ggplot(UMR_mainavgs, aes(x=Group.1, y=SI))+
#  geom_point()+
#  labs(title="UMR main stem average Si concentrations", y="Si concentration (mg Si/L)")
#UMR_avgmainmonthlySiTN = ggplot(UMR_mainavgs, aes(x=Group.1, y=SiTN))+
#  geom_point()+
#  labs(title="UMR main stem average Si:TN", y="Average Molar Si:TN")
#UMR_avgmainmonthlySiTP = ggplot(UMR_mainavgs, aes(x=Group.1, y=SiTP))+
# geom_point()+
#  labs(title="UMR main stem average Si:TP", y="Average MolarSi:TP")
#grid.arrange(UMR_avgmainmonthlySi, UMR_avgmainmonthlySiTN, UMR_avgmainmonthlySiTP)

#create box plots for Si concentration, Si:TN, and Si:TP by season
UMR_2010.2018$SEASON.f = factor(UMR_2010.2018$SEASON, levels=c('summer', 'autumm', 'winter', 'spring'))
UMR_seasonalSi = ggplot(UMR_2010.2018, aes(x=SEASON, y=SI))+
  geom_boxplot()+
  labs(title="Si concentrations in UMR mainstem and tributaries", y="Si conc (mg SiO2/L)")
UMR_seasonalSi
