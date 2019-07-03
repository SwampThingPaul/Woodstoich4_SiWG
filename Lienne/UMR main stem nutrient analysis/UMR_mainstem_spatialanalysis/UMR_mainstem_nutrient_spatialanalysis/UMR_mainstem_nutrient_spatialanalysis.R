#add lat/lon coordinates by UMR LOCATCD
UMR_mainstem = merge(Fixed_summarystats_2010_2018, UMR_mainstem_coordinates, by="LOCATCD")

#add river mile from LOCATCD
library(readr)
UMR_mainstem$rivermile = parse_number(UMR_mainstem$LOCATCD)

#remove LOCATCDs from the Illinois river
UMR_mainstem = UMR_mainstem2
is.na(UMR_mainstem$LOCATCD) = startsWith(UMR_mainstem$LOCATCD, "I")
UMR_mainstem=na.omit(UMR_mainstem)

#get Si, Si:TN, and Si:TP parameters from fixed summary stats
UMR_mainstem_Si = subset(UMR_mainstem, UMR_mainstem$parameter=="SI")
UMR_mainstem_SiTN = subset(UMR_mainstem, UMR_mainstem$parameter=="SiTN")
UMR_mainstem_SiTP = subset(UMR_mainstem, UMR_mainstem$parameter=="SiTP")

#log transform Si:TN and Si:TP
UMR_mainstem_SiTN$log.mean.val = log10(UMR_mainstem_SiTN$mean.val)
UMR_mainstem_SiTP$log.mean.val = log10(UMR_mainstem_SiTP$mean.val)

#plot Si, Si:TN, Si:TP by latitude and river mile
library(ggplot2)
library(ggpmisc)
#=============================================================================
#Si concentration vs. latitude and river mile
UMR_mainstem_Sispatial = ggplot(UMR_mainstem_Si, aes(x=Lat, y=mean.val))+
  geom_point(size=3)+
  labs(title="UMR mainstem average Si concentration", y="Si concentration (mg SiO2/L)", x="Degrees latitude")
UMR_mainstem_Sispatial

SiRM.formula = UMR_mainstem_Si$mean.val ~ UMR_mainstem_Si$rivermile
UMR_mainstem_SiRM = ggplot(UMR_mainstem_Si, aes(x=rivermile, y=mean.val))+
  geom_point(size=3)+
  geom_smooth(method='lm', se=TRUE, color="black")+
  stat_poly_eq(formula=SiRM.formula, 
               aes(label=paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(title="UMR main stem average Si concentration", y="Si concentration (mg SiO2/L)", x="River Mile")
UMR_mainstem_SiRM
ggsave(file="UMR main stem Si by river mile.png", width=10, height=7)
#=============================================================================
#Si:TN vs. latitude and river mile
UMR_mainstem_SiTNspatial = ggplot(UMR_mainstem_SiTN, aes(x=Lat, y=mean.val))+
  geom_point(size=3)+
  labs(title="UMR mainstem average Si:TN", y="Molar Si:TN", x="Degrees latitude")
UMR_mainstem_SiTNspatial

SiTNRM.formula = UMR_mainstem_SiTN$log.mean.val ~ UMR_mainstem_SiTN$rivermile
UMR_mainstem_SiTNRM = ggplot(UMR_mainstem_SiTN, aes(x=rivermile, y=log.mean.val))+
  geom_point(size=3)+
  geom_smooth(method='lm', se=TRUE, color="black")+
  stat_poly_eq(formula=SiTNRM.formula, 
               aes(label=paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(title="UMR main stem average Si:TN", y="log(Molar Si:TN)", x="River Mile")
UMR_mainstem_SiTNRM
ggsave(file="UMR main stem SiTN by river mile.png", width=10, height=7)
#==============================================================================
#Si:TP vs. latitude and river mile
UMR_mainstem_SiTPspatial = ggplot(UMR_mainstem_SiTP, aes(x=Lat, y=mean.val))+
  geom_point(size=3)+
  labs(title="UMR mainstem average Si:TP", y="Molar Si:TP", x="Degrees latitude")
UMR_mainstem_SiTPspatial

SiTPRM.formula = UMR_mainstem_SiTP$log.mean.val ~ UMR_mainstem_SiTP$rivermile
UMR_mainstem_SiTPRM = ggplot(UMR_mainstem_SiTP, aes(x=rivermile, y=log.mean.val))+
  geom_point(size=3)+
  geom_smooth(method='lm', se=TRUE, color="black")+
  stat_poly_eq(formula=SiTPRM.formula, 
               aes(label=paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  labs(title="UMR main stem average Si:TP", y="log(Molar Si:TP)", x="River Mile")
UMR_mainstem_SiTPRM
ggsave(file="UMR main stem SiTP by river mile.png", width=10, height=7)