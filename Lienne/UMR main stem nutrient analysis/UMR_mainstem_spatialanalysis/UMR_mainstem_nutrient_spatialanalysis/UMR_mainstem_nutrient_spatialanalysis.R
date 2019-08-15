#use SRS summmary stats 2010-2018
UMR_mainstem = UMR_SRS_summarystats_2010_2018

#add river mile from LOCATCD
#from Fixed summary stats, not used for SRS
#library(readr)
#UMR_mainstem$rivermile = parse_number(UMR_mainstem$LOCATCD)

#add median river mile for each Reach Number
UMR_mainstem$medianRM = ifelse(UMR_mainstem$FLDNUM==1, 1247.5,
                               ifelse(UMR_mainstem$FLDNUM==2, 1112,
                                      ifelse(UMR_mainstem$FLDNUM==3, 868.5,
                                             ifelse(UMR_mainstem$FLDNUM==4, 358, 88))))

#remove LOCATCDs from the Illinois river
UMR_mainstem2 = UMR_mainstem
#is.na(UMR_mainstem$LOCATCD) = startsWith(UMR_mainstem$LOCATCD, "I")
#UMR_mainstem=na.omit(UMR_mainstem)
is.na(UMR_mainstem$FLDNUM) = UMR_mainstem$FLDNUM == 6
UMR_mainstem=na.omit(UMR_mainstem)

#get Si, Si:TN, and Si:TP parameters from SRS stats
UMR_mainstem_Si = subset(UMR_mainstem, UMR_mainstem$parameter=="SI")
UMR_mainstem_Si$MSi = (UMR_mainstem_Si$mean.val/28.086)*1000
UMR_mainstem_SiTN = subset(UMR_mainstem, UMR_mainstem$parameter=="SiTN")
UMR_mainstem_SiTP = subset(UMR_mainstem, UMR_mainstem$parameter=="SiTP")
UMR_mainstem_TN = subset(UMR_mainstem, UMR_mainstem$parameter=="TN")
UMR_mainstem_TN$MTN = (UMR_mainstem_TN$mean.val/14.007)*1000
UMR_mainstem_TP = subset(UMR_mainstem, UMR_mainstem$parameter=="TP")
UMR_mainstem_TP$MTP = (UMR_mainstem_TP$mean.val/30.974)*1000

#log transform Si:TN and Si:TP
#don't use for spatial trends
UMR_mainstem_SiTN$log.mean.val = log10(UMR_mainstem_SiTN$mean.val)
UMR_mainstem_SiTP$log.mean.val = log10(UMR_mainstem_SiTP$mean.val)

#get average Si, TN, and TP concentrations
library(psych)
avgSi = describe(UMR_mainstem_Si$MSi)
avgSiTN = describe(UMR_mainstem_SiTN$mean.val)
avgSiTP = describe(UMR_mainstem_SiTP$mean.val)
avgTN = describe(UMR_mainstem_TN$MTN)
avgTP = describe(UMR_mainstem_TP$MTP)

#plot Si, Si:TN, Si:TP by latitude and river mile
library(ggplot2)
library(ggpmisc)
#=============================================================================
#Si concentration vs. latitude and field number
#UMR_mainstem_Sispatial = ggplot(UMR_mainstem_Si, aes(x=Lat, y=mean.val))+
#  geom_point(size=3)+
#  labs(title="UMR mainstem average Si concentration", y="Si concentration (mg SiO2/L)", x="Degrees latitude")
#UMR_mainstem_Sispatial

SiFLDNUM.formula = UMR_mainstem_Si$mean.val ~ UMR_mainstem_Si$FLDNUM
SiFLDNUM.lm = lm(data=UMR_mainstem_Si, MSi~FLDNUM)
summary(SiFLDNUM.lm)
UMR_mainstem_SiFLDNUM = ggplot(UMR_mainstem_Si, aes(x=FLDNUM, y=MSi))+
  geom_point(size=3)+
  geom_smooth(method='lm', se=TRUE, color="black")+
  stat_poly_eq(formula=SiRM.formula, 
               aes(label=paste(..rr.label..)),
               label.x=0.85,
               size=10,
               parse = TRUE) +
  labs(title="UMR Mainstem", y=expression("[Si]"~(mu~"M")), x="")+
  scale_x_discrete(limits=c("1", "2", "3", "4", "5"))+
  theme_bw()+
  theme(text=element_text(size=30),
        axis.text.x=element_text(size=rel(1)),
        axis.text.y=element_text(size=rel(1)))
UMR_mainstem_SiFLDNUM

SiRM.formula = UMR_mainstem_Si$mean.val ~ UMR_mainstem_Si$medianRM
SiRM.lm = lm(data=UMR_mainstem_Si, MSi~medianRM)
summary(SiRM.lm)
UMR_mainstem_SiRM = ggplot(UMR_mainstem_Si, aes(x=medianRM, y=MSi))+
  geom_point(size=3)+
  geom_smooth(method='lm', se=TRUE, color="black")+
  stat_poly_eq(formula=SiRM.formula, 
               aes(label=paste(..rr.label..)),
               label.x=0.85,
               size=10,
               parse = TRUE) +
  labs(title="Mainstem", y=expression("[Si]"~(mu~"M")), x="")+
  scale_x_reverse()+
  theme_bw()+
  theme(text=element_text(size=30),
        axis.text.x=element_text(size=rel(1)),
        axis.text.y=element_text(size=rel(1)))
UMR_mainstem_SiRM
#ggsave(file="UMR main stem Si by river mile.png", width=10, height=7)
#=============================================================================
#Si:TN vs. latitude and river mile
#UMR_mainstem_SiTNspatial = ggplot(UMR_mainstem_SiTN, aes(x=Lat, y=mean.val))+
#  geom_point(size=3)+
#  labs(title="UMR mainstem average Si:TN", y="Molar Si:TN", x="Degrees latitude")
#UMR_mainstem_SiTNspatial

SiTNFLDNUM.formula = UMR_mainstem_SiTN$mean.val ~ UMR_mainstem_SiTN$FLDNUM
SiTNFLDNUM.lm = lm(data=UMR_mainstem_SiTN, mean.val~FLDNUM)
summary(SiTNFLDNUM.lm)
UMR_mainstem_SiTNFLDNUM = ggplot(UMR_mainstem_SiTN, aes(x=FLDNUM, y=mean.val))+
  geom_point(size=3)+
  geom_smooth(method='lm', se=TRUE, color="black")+
  stat_poly_eq(formula=SiTNFLDNUM.formula, 
               aes(label=paste(..rr.label..)),
               label.x=0.85,
               size=10,
               parse = TRUE) +
  geom_hline(yintercept=1, linetype="dashed")+
  labs(y="Molar Si:TN", x="")+
  scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6"))+
  theme_bw()+
  theme(text=element_text(size=30),
        axis.text.x=element_text(size=rel(1)),
        axis.text.y=element_text(size=rel(1)))
UMR_mainstem_SiTNFLDNUM

SiTNRM.formula = UMR_mainstem_SiTN$mean.val ~ UMR_mainstem_SiTN$medianRM
SiTNRM.lm = lm(data=UMR_mainstem_SiTN, mean.val~medianRM)
summary(SiTNRM.lm)
UMR_mainstem_SiTNRM = ggplot(UMR_mainstem_SiTN, aes(x=medianRM, y=mean.val))+
  geom_point(size=3)+
  geom_smooth(method='lm', se=TRUE, color="black")+
  stat_poly_eq(formula=SiTNRM.formula, 
               aes(label=paste(..rr.label..)),
               label.x=0.85,
               size=10,
               parse = TRUE) +
  geom_hline(yintercept=1, linetype="dashed")+
  labs(y="Molar Si:TN", x="")+
  scale_x_reverse()+
  theme_bw()+
  theme(text=element_text(size=30),
        axis.text.x=element_text(size=rel(1)),
        axis.text.y=element_text(size=rel(1)))
UMR_mainstem_SiTNRM
#ggsave(file="UMR main stem SiTN by river mile.png", width=10, height=7)
#==============================================================================
#Si:TP vs. latitude and river mile
#UMR_mainstem_SiTPspatial = ggplot(UMR_mainstem_SiTP, aes(x=Lat, y=mean.val))+
#  geom_point(size=3)+
#  labs(title="UMR mainstem average Si:TP", y="Molar Si:TP", x="Degrees latitude")
#UMR_mainstem_SiTPspatial

SiTPFLDNUM.formula = UMR_mainstem_SiTP$mean.val ~ UMR_mainstem_SiTP$FLDNUM
SiTPFLDNUM.lm = lm(data=UMR_mainstem_SiTP, mean.val~FLDNUM)
summary(SiTPFLDNUM.lm)
UMR_mainstem_SiTPFLDNUM = ggplot(UMR_mainstem_SiTP, aes(x=FLDNUM, y=mean.val))+
  geom_point(size=3)+
  geom_smooth(method='lm', se=TRUE, color="black")+
  stat_poly_eq(formula=SiTPFLDNUM.formula, 
               aes(label=paste(..rr.label..)),
               label.x=0.85,
               size=10,
               parse = TRUE) +
    geom_hline(yintercept=16, linetype="dashed")+
  labs(y="Molar Si:TP", x="Reach Number")+
  scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6"))+
  theme_bw()+
  theme(text=element_text(size=30),
        axis.text.x=element_text(size=rel(1)),
        axis.text.y=element_text(size=rel(1)))
UMR_mainstem_SiTPFLDNUM

SiTPRM.formula = UMR_mainstem_SiTP$mean.val ~ UMR_mainstem_SiTP$medianRM
SiTPRM.lm = lm(data=UMR_mainstem_SiTP, mean.val~medianRM)
summary(SiTPRM.lm)
UMR_mainstem_SiTPRM = ggplot(UMR_mainstem_SiTP, aes(x=medianRM, y=mean.val))+
  geom_point(size=3)+
  geom_smooth(method='lm', se=TRUE, color="black")+
  stat_poly_eq(formula=SiTPRM.formula, 
               aes(label=paste(..rr.label..)),
               label.x=0.85,
               size=10,
               parse = TRUE) +
  geom_hline(yintercept=16, linetype="dashed")+
  labs(y="Molar Si:TP", x="Median River Kilometer")+
  scale_x_reverse()+
  theme_bw()+
  theme(text=element_text(size=30),
        axis.text.x=element_text(size=rel(1)),
        axis.text.y=element_text(size=rel(1)))
UMR_mainstem_SiTPRM
#ggsave(file="UMR main stem SiTP by river mile.png", width=10, height=7)

TNRM.formula = UMR_mainstem_TN$mean.val ~ UMR_mainstem_TN$FLDNUM
TNRM.lm = lm(data=UMR_mainstem_TN, mean.val~FLDNUM)
summary(TNRM.lm)
UMR_mainstem_TNRM = ggplot(UMR_mainstem_TN, aes(x=FLDNUM, y=mean.val))+
  geom_point(size=3)+
  geom_smooth(method='lm', se=TRUE, color="black")+
  stat_poly_eq(formula=TNRM.formula, 
               aes(label=paste(..rr.label..)),
               label.x=0.85,
               size=10,
               parse = TRUE) +
  geom_hline(yintercept=16, linetype="dashed")+
  labs(y="TN concentration (mg N/L)", x="Field Number")+
  scale_x_discrete(limits=c("1", "2", "3", "4", "5"))+
  theme_bw()+
  theme(text=element_text(size=30),
        axis.text.x=element_text(size=rel(1)),
        axis.text.y=element_text(size=rel(1)))
UMR_mainstem_TNRM

TPRM.formula = UMR_mainstem_TP$mean.val ~ UMR_mainstem_TP$FLDNUM
TPRM.lm = lm(data=UMR_mainstem_TP, mean.val~FLDNUM)
summary(TPRM.lm)
UMR_mainstem_TPRM = ggplot(UMR_mainstem_TP, aes(x=FLDNUM, y=mean.val))+
  geom_point(size=3)+
  geom_smooth(method='lm', se=TRUE, color="black")+
  stat_poly_eq(formula=TPRM.formula, 
               aes(label=paste(..rr.label..)),
               label.x=0.85,
               size=10,
               parse = TRUE) +
  labs(y="TP concentration (mg P/L)", x="Field Number")+
  scale_x_discrete(limits=c("1", "2", "3", "4", "5"))+
  theme_bw()+
  theme(text=element_text(size=30),
        axis.text.x=element_text(size=rel(1)),
        axis.text.y=element_text(size=rel(1)))
UMR_mainstem_TPRM

library(gridExtra)
grid.arrange(UMR_mainstem_SiRM, UMR_mainstem_SiTNRM, UMR_mainstem_SiTPRM)

library(cowplot)

#use plot function from base library
library(base)
par(mfrow=c(3,1), oma=c(5,5,5,5), mar=c(1,3,0,0))
plot(UMR_mainstem_Si$medianRM, UMR_mainstem_Si$MSi,
     xlab="", ylab=expression("Si concentration"~(mu~"M")),
     xaxt='n')
plot(UMR_mainstem_SiTN$FLDNUM, UMR_mainstem_SiTN$mean.val,
     xlab="", ylab="Molar Si:TN",
     xaxt='n')
plot(UMR_mainstem_SiTP$FLDNUM, UMR_mainstem_SiTP$mean.val,
     xlab="Field Number", ylab="Molar Si:TP")
