#use SRS summmary stats 2010-2018

#add river mile from LOCATCD
#from Fixed summary stats, not used for SRS
#library(readr)
#UMR_mainstem$rivermile = parse_number(UMR_mainstem$LOCATCD)

#remove LOCATCDs from the Illinois river
#UMR_mainstem = UMR_mainstem2
#is.na(UMR_mainstem$LOCATCD) = startsWith(UMR_mainstem$LOCATCD, "I")
#UMR_mainstem=na.omit(UMR_mainstem)

#get Si, Si:TN, and Si:TP parameters from SRS stats
UMR_mainstem_Si = subset(UMR_mainstem, UMR_mainstem$parameter=="SI")
UMR_mainstem_SiTN = subset(UMR_mainstem, UMR_mainstem$parameter=="SiTN")
UMR_mainstem_SiTP = subset(UMR_mainstem, UMR_mainstem$parameter=="SiTP")

#log transform Si:TN and Si:TP
#don't use for spatial trends
UMR_mainstem_SiTN$log.mean.val = log10(UMR_mainstem_SiTN$mean.val)
UMR_mainstem_SiTP$log.mean.val = log10(UMR_mainstem_SiTP$mean.val)

#plot Si, Si:TN, Si:TP by latitude and river mile
library(ggplot2)
library(ggpmisc)
#=============================================================================
#Si concentration vs. latitude and field number
#UMR_mainstem_Sispatial = ggplot(UMR_mainstem_Si, aes(x=Lat, y=mean.val))+
#  geom_point(size=3)+
#  labs(title="UMR mainstem average Si concentration", y="Si concentration (mg SiO2/L)", x="Degrees latitude")
#UMR_mainstem_Sispatial

SiRM.formula = UMR_mainstem_Si$mean.val ~ UMR_mainstem_Si$FLDNUM
SiRM.lm = lm(data=UMR_mainstem_Si, mean.val~FLDNUM)
summary(SiRM.lm)

UMR_mainstem_SiRM = ggplot(UMR_mainstem_Si, aes(x=FLDNUM, y=mean.val))+
  geom_point(size=3)+
  geom_smooth(method='lm', se=TRUE, color="black")+
  stat_poly_eq(formula=SiRM.formula, 
               aes(label=paste(..rr.label..)),
               label.x=0.85,
               size=8,
               parse = TRUE) +
  labs(y="[Si] (mg Si/L)", x="")+
  scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6"))+
  theme_bw()+
  theme(text=element_text(size=20))
UMR_mainstem_SiRM
#ggsave(file="UMR main stem Si by river mile.png", width=10, height=7)
#=============================================================================
#Si:TN vs. latitude and river mile
#UMR_mainstem_SiTNspatial = ggplot(UMR_mainstem_SiTN, aes(x=Lat, y=mean.val))+
#  geom_point(size=3)+
#  labs(title="UMR mainstem average Si:TN", y="Molar Si:TN", x="Degrees latitude")
#UMR_mainstem_SiTNspatial

SiTNRM.formula = UMR_mainstem_SiTN$mean.val ~ UMR_mainstem_SiTN$FLDNUM
SiTNRM.lm = lm(data=UMR_mainstem_SiTN, mean.val~FLDNUM)
summary(SiTNRM.lm)

UMR_mainstem_SiTNRM = ggplot(UMR_mainstem_SiTN, aes(x=FLDNUM, y=mean.val))+
  geom_point(size=3)+
  geom_smooth(method='lm', se=TRUE, color="black")+
  stat_poly_eq(formula=SiTNRM.formula, 
               aes(label=paste(..rr.label..)),
               label.x=0.85,
               size=8,
               parse = TRUE) +
  scale_x_reverse()+
  geom_hline(yintercept=1, linetype="dashed")+
  labs(y="Molar Si:TN", x="")+
  scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6"))+
  theme_bw()+
  theme(text=element_text(size=20))
UMR_mainstem_SiTNRM
#ggsave(file="UMR main stem SiTN by river mile.png", width=10, height=7)
#==============================================================================
#Si:TP vs. latitude and river mile
#UMR_mainstem_SiTPspatial = ggplot(UMR_mainstem_SiTP, aes(x=Lat, y=mean.val))+
#  geom_point(size=3)+
#  labs(title="UMR mainstem average Si:TP", y="Molar Si:TP", x="Degrees latitude")
#UMR_mainstem_SiTPspatial

SiTPRM.formula = UMR_mainstem_SiTP$mean.val ~ UMR_mainstem_SiTP$FLDNUM
SiTPRM.lm = lm(data=UMR_mainstem_SiTP, mean.val~FLDNUM)
summary(SiTPRM.lm)

UMR_mainstem_SiTPRM = ggplot(UMR_mainstem_SiTP, aes(x=FLDNUM, y=mean.val))+
  geom_point(size=3)+
  geom_smooth(method='lm', se=TRUE, color="black")+
  stat_poly_eq(formula=SiTPRM.formula, 
               aes(label=paste(..rr.label..)),
               label.x=0.85,
               size=8,
               parse = TRUE) +
  scale_x_reverse()+
  geom_hline(yintercept=16, linetype="dashed")+
  labs(y="Molar Si:TP", x="Field Number")+
  scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6"))+
  theme_bw()+
  theme(text=element_text(size=20))
UMR_mainstem_SiTPRM
#ggsave(file="UMR main stem SiTP by river mile.png", width=10, height=7)

library(gridExtra)
grid.arrange(UMR_mainstem_SiRM, UMR_mainstem_SiTNRM, UMR_mainstem_SiTPRM)
