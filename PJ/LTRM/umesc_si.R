## 
## Woodstoich 4 (Si Working grop)
## Upper Mississippi River Restoration LTRM
## https://umesc.usgs.gov/data_library/water_quality/water_quality_page.html
##
## Code was compiled by Paul Julian
## contact infor: pjulian@ufl.edu

#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

#Libraries
library(AnalystHelper);#devtools::install_github("SwampThingPaul/AnalystHelper")
library(plyr)
library(reshape)
library(zoo);


library(rgdal)
library(tmap)
#Custom Functions

#Paths
setwd("D:/UF/WoodStoich19/Si_WG")

paths=paste0(getwd(),c("/Exports/","/Plots/","/Data/Miss_UpperTrib_dataset/RAW_USGS_TribData/"))
#Folder.Maker(paths);#One and done. Creates folders in working directory.
export.path=paths[1]
plot.path=paths[2]
data.path=paths[3]

#Helper variables 
N.mw=14.0067
P.mw=30.973762
C.mw=12.0107
Si.mw=28.0855

utm15=CRS("+proj=utm +zone=15 +datum=WGS84 +units=m")

##
#shell.exec(data.path)

# Data --------------------------------------------------------------------

dat=read.csv(paste0(data.path,"UMRTribs_Filtered_1.26.18.csv"))
dat$DATETIME=date.fun(paste0(dat$DATE," ",dat$TIME,":00"),tz="America/Chicago",form="%m/%d/%Y %H:%M:%S")
dat$DATE=date.fun(dat$DATE,tz="America/Chicago",form="%m/%d/%Y")
head(dat)

unique(dat$SITE_NAME)
ddply(dat,"SITE_NAME",summarise,N.val=N(SITE_NAME))

#spatial data
sp.dat=ddply(dat,c("SITE_NAME","FLDNUM","EASTING","NORTHING"),summarise,N.val=N(SITE_NAME),min.date=min(DATE),max.date=max(DATE),N.SI=N(SI))
sp.dat$yrs.dat=with(sp.dat,as.numeric(max.date-min.date)*3.17098e-8)
nrow(subset(sp.dat,yrs.dat>20))
plot(yrs.dat~N.val,sp.dat,ylab="Number of Years",xlab="Number of Samples")

sp.dat.shp=SpatialPointsDataFrame(coords=sp.dat[,c("EASTING","NORTHING")], data=sp.dat,proj4string=utm15)

tmap_mode("view")
tm_basemap(leaflet::providers$Esri.WorldImagery,alpha=0.9)+
  tm_shape(sp.dat.shp)+tm_dots(col="FLDNUM",popup.vars=c("SITE_NAME","FLDNUM","N.val","min.date","max.date","yrs.dat"))

##
summary(dat)
names(dat)

idvars=c("FLDNUM", "DATE","DATETIME", "YEAR", "MONTH", "TIME", "SITE_NAME", "WDP", 
         "ZMAX", "SECCHI", "SITETYPE", "SBSTRATE", "NORTHING", "EASTING", 
         "GRIDCODE", "Z15NORTH", "Z15EAST")
params=c("Z", "TEMP", "DO", "PH", "TURB", 
         "COND", "SRP", "TP", "TN", "SS", "CHLS", "PHAEO", "NHX", "NOX", 
         "SI", "CA", "FE", "MN", "K", "CL", "VSS", "CHLFI", "MG")
dat.melt=melt(dat,id.vars=idvars,variable_name="param")

dat.melt$halfMDL=with(dat.melt,ifelse(value<0,abs(value)/2,value))

plot(NORTHING~halfMDL,subset(dat.melt,param=="SI"))
plot(NORTHING~halfMDL,subset(dat.melt,param=="TP"))
plot(NORTHING~halfMDL,subset(dat.melt,param=="TN"))

##
dat.xtab=cast(dat.melt,DATETIME+SITE_NAME~param,value="HalfMDL",mean)

## Hypothesis1: Changes in Si:N or Si:P scaling(slope) is 
# negatively correlated with DIN or SRP concentrations
# indicating eutrophication decouples Si:Nutrient stoichiometry

dat.xtab2=merge(dat.xtab,subset(sp.dat,yrs.dat>4),c("SITE_NAME"))
dat.xtab2$DIN=with(dat.xtab2,NHX+NOX)

# Reversal Evaluation
dat.xtab2$TPReversal=with(dat.xtab2,ifelse(is.na(SRP)==T|is.na(TP)==T,0,ifelse(SRP>(TP*1.3),1,0)));# Reversals identified as 1 reversals consistent with TP rule evaluation
dat.xtab2$TNReversal=with(dat.xtab2,ifelse(is.na(DIN)==T|is.na(TN)==T,0,ifelse(DIN>(TN*1.3),1,0)));

# TN
sum(dat.xtab2$TNReversal,na.rm=T)
par(family="serif",oma=c(1,1,1,1),mar=c(4,4,1,1))
layout(matrix(1:2,1,2,byrow=F))
plot(TN~DIN,dat.xtab2,ylab="TN (mg/L)",xlab="DIN (mg/L)",pch=21,bg=ifelse(TNReversal==1,"dodgerblue1",NA),col=adjustcolor("grey",0.8));abline(0,1,col="dodgerblue1")

# TP
sum(dat.xtab2$TPReversal,na.rm=T)
par(family="serif",oma=c(1,1,1,1),mar=c(4,4,1,1))
plot(TP~SRP,dat.xtab2,ylab="TP (mg/L)",xlab="SRP (mg/L)",pch=21,bg=ifelse(TPReversal==1,"red",NA),col=adjustcolor("grey",0.8));abline(0,1,col="red")

boxplot(SI~as.factor(NORTHING),dat.xtab2,horizontal=T,outline=F)
boxplot(TP~as.factor(NORTHING),subset(dat.xtab2,TPReversal==0),horizontal=T,outline=F)
boxplot(TN~as.factor(NORTHING),subset(dat.xtab2,TNReversal==0),horizontal=T,outline=F)
# TN might be a bigger driver of Si decoupling?