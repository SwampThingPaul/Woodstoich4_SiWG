## 
## Woodstoich 4 (Si Working grop)
## Upper Mississippi River Restoration LTRM
## https://umesc.usgs.gov/data_library/water_quality/water_quality_page.html
## https://www.umesc.usgs.gov/cgi-bin/ltrmp/water/water_meta.pl
##
##
## Version 3
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
library(classInt)

#GIS Libraries
library(rgdal)
library(rgeos)
library(tmap)
library(mapmisc);#for map scale bar and north arrow base plotting.

#Analysis
library(smatr)
library(boot)

# To make tables
library(kableExtra)

#functions

#Paths
setwd("D:/UF/WoodStoich19/Si_WG")

paths=paste0(getwd(),c("/Exports/","/Plots/","/Data/Miss_UpperTrib_dataset/RAW_USGS_TribData/"))
#Folder.Maker(paths);#One and done. Creates folders in working directory.
export.path=paths[1]
plot.path=paths[2]
data.path=paths[3]

gen.gis="D:/_GISData/"

#Helper variables 
N.mw=14.0067
P.mw=30.973762
C.mw=12.0107
Si.mw=28.0855

utm15=CRS("+proj=utm +zone=15 +datum=WGS84 +units=m")
wgs84=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

##
#shell.exec(data.path)

# Data --------------------------------------------------------------------
stratum.lookup=data.frame(STRATUM=1:9,STRATUM_Descript=c("Main Channel","Side Channel","Connected Backwater", "Riverine Lake", "Impoundment","Isolated Backwater","New Terrestrial",NA,"Unexploded Ordinance (Pool 13)"))
FLDNUM.pool.xwalk=data.frame(FLDNUM=1:6,Pool=c("Pool 4","Pool 8","Pool 13","Pool 26","Open River","La Grange"))

srs.dat=read.csv(paste0(data.path,"UMR_MainStem_SRSdataset_1993-2018.csv"))
srs.dat$DATETIME=date.fun(paste0(as.character(srs.dat$DATE)," ",as.character(srs.dat$TIME),":00"),tz="America/Chicago",form="%m/%d/%Y %H:%M:%S")
srs.dat$DATE=date.fun(as.character(srs.dat$DATE),tz="America/Chicago",form="%m/%d/%Y")
srs.dat$dataset="srs"
srs.dat=merge(srs.dat,stratum.lookup,"STRATUM",all.x=T)
srs.dat=merge(srs.dat,FLDNUM.pool.xwalk,"FLDNUM",all.x=T)

# Spatial data
sp.dat=ddply(srs.dat,c("LOCATCD","FLDNUM","dataset"),summarise,EASTING=mean(EASTING,na.rm=T),NORTHING=mean(NORTHING,na.rm=T),N.val=N(LOCATCD),min.date=min(DATE),max.date=max(DATE),N.SI=N(SI),N.TP=N(TP),N.TN=N(TN))
sp.dat$yrs.dat=with(sp.dat,as.numeric(max.date-min.date))*3.17098e-8
subset(sp.dat,is.na(NORTHING))
sp.dat=subset(sp.dat,is.na(NORTHING)==F)
sp.dat.shp=SpatialPointsDataFrame(coords=sp.dat[,c("EASTING","NORTHING")], data=sp.dat,proj4string=utm15)

#tmap_mode("view")
#tm_basemap(leaflet::providers$Esri.WorldImagery,alpha=0.9)+
#  tm_shape(subset(sp.dat.shp,FLDNUM==6))+tm_dots(col="yellow")


dput(names(srs.dat))
srs.dat=rename(srs.dat,c("VSS_QF"="VSSQF","NA."="Na","NAQF"="NaQF"))
names(srs.dat)

idvars=c("FLDNUM","LOCATCD","DATE","dataset","STRATUM_Descript","STRATUM","Pool")
field.params=c("TEMP","DO","TURB")
lab.params=c("SRP","TP","NOX","NHX","TN","SI","VSS","CHLcal")

srs.dat$CHLcalQF=srs.dat$CHLFQF

srs.dat2=data.frame()
#stack field parameters
for(i in 1:length(field.params)){
  tmp=srs.dat[,c(idvars,field.params[i],paste0(field.params[i],"QF"))]
  colnames(tmp)=c(idvars,"data.value","flag")
  tmp$parameter=field.params[i]
  tmp=tmp[,c(idvars,"parameter","data.value","flag")]
  tmp$fatal.flag=with(tmp,ifelse(flag=="","N","Y"))
  srs.dat2=rbind(srs.dat2,tmp)
  print(i)
}
srs.dat2$data.type="Field"

for(i in 1:length(lab.params)){
  tmp=srs.dat[,c(idvars,lab.params[i],paste0(lab.params[i],"QF"))]
  colnames(tmp)=c(idvars,"data.value","flag")
  tmp$data.type="lab"
  tmp$parameter=lab.params[i]
  tmp=tmp[,c(idvars,"parameter","data.value","flag","data.type")]
  tmp$flag=as.factor(tmp$flag)
  tmp$fatal.flag=with(tmp,ifelse(flag==0|is.na(flag)==T,"N","Y"))
  srs.dat2=rbind(srs.dat2,tmp)
  print(i)
}
srs.dat2$data.type="Lab"
srs.dat2=subset(srs.dat2,is.na(data.value)==F)

ddply(srs.dat2,"parameter",summarise,min.val=min(data.value))
srs.dat2$halfMDL=with(srs.dat2,ifelse(data.value<0,abs(data.value)/2,data.value))

#re-Cross Tabulate data again...removing fatally qualified data
dat.xtab=cast(subset(srs.dat2,fatal.flag=="N"),FLDNUM+LOCATCD+STRATUM+STRATUM_Descript+Pool+DATE~parameter,value="halfMDL",mean)
dat.xtab$DIN=with(dat.xtab,NHX+NOX)
dat.xtab$month=as.numeric(format(dat.xtab$DATE,"%m"))
dat.xtab$CY=as.numeric(format(dat.xtab$DATE,"%Y"))
dat.xtab$WY=WY(dat.xtab$DATE,"Fed")

# Reversal Evaluation
dat.xtab$TPReversal=with(dat.xtab,ifelse(is.na(SRP)==T|is.na(TP)==T,0,ifelse(SRP>(TP*1.3),1,0)));# Reversals identified as 1 reversals consistent with TP rule evaluation
dat.xtab$TNReversal=with(dat.xtab,ifelse(is.na(DIN)==T|is.na(TN)==T,0,ifelse(DIN>(TN*1.3),1,0)));

sum(dat.xtab$TNReversal,na.rm=T)
sum(dat.xtab$TPReversal,na.rm=T)

par(family="serif",oma=c(1,1,1,1),mar=c(4,4,1,1))
layout(matrix(1:2,1,2,byrow=F))
plot(TN~DIN,dat.xtab,ylab="TN (mg/L)",xlab="DIN (mg/L)",pch=21,bg=ifelse(TNReversal==1,"dodgerblue1",NA),col=adjustcolor("grey",0.8));abline(0,1,col="dodgerblue1")
plot(TP~SRP,dat.xtab,ylab="TP (mg/L)",xlab="SRP (mg/L)",pch=21,bg=ifelse(TPReversal==1,"red",NA),col=adjustcolor("grey",0.8));abline(0,1,col="red")

dev.off();#clears plots

##
dat.xtab$TP=with(dat.xtab,ifelse(TPReversal==1,NA,TP))
dat.xtab$TN=with(dat.xtab,ifelse(TNReversal==1,NA,TN))
dat.xtab$SRP=with(dat.xtab,ifelse(TPReversal==1,NA,SRP))
dat.xtab$DIN=with(dat.xtab,ifelse(TNReversal==1,NA,DIN))
dat.xtab$TP.ugL=dat.xtab$TP*1000

dat.xtab$Si.mM=with(dat.xtab, SI/Si.mw)
dat.xtab$TP.mM=with(dat.xtab,TP/P.mw)
dat.xtab$TN.mM=with(dat.xtab,TN/N.mw)
dat.xtab$SRP.mM=with(dat.xtab,SRP/P.mw)
dat.xtab$DIN.mM=with(dat.xtab,DIN/N.mw)
dat.xtab$SiTP=with(dat.xtab,Si.mM/TP.mM)
dat.xtab$SiTN=with(dat.xtab,Si.mM/TN.mM)
dat.xtab$SiSRP=with(dat.xtab,Si.mM/SRP.mM)
dat.xtab$SiDIN=with(dat.xtab,Si.mM/DIN.mM)
dat.xtab$TNTP=with(dat.xtab,TN.mM/TP.mM)
dat.xtab$DINSRP=with(dat.xtab,DIN.mM/SRP.mM)

dat.xtab$decade=((dat.xtab$CY)%/%10)*10
dat.xtab$month=as.numeric(format(dat.xtab$DATE,"%m"))
dat.xtab=merge(dat.xtab,data.frame(month=1:12,season=c(rep("Winter",3),rep("Spring",3),rep("Summer",3),rep("Fall",3))))
dat.xtab=subset(dat.xtab,STRATUM%in%c(1:6))

## Stats
idvars=c("LOCATCD","STRATUM","STRATUM_Descript","FLDNUM","Pool","CY","decade","season")
vars=c("TP","TN","SRP","DIN","SI","SiTP","SiTN","SiSRP","SiDIN","TNTP","DINSRP")
dat.xtab.melt=melt(data.frame(dat.xtab[,c(idvars,vars)]),id.vars=idvars,variable_name="parameter")
dat.xtab.melt=subset(dat.xtab.melt,is.na(value)==F)

annual=ddply(dat.xtab.melt,c("CY","STRATUM","STRATUM_Descript","FLDNUM","Pool","parameter"),summarise,mean.val=mean(value,na.rm=T),sd.val=sd(value,na.rm=T),N.val=N(value))
decade=ddply(dat.xtab.melt,c("decade","STRATUM","STRATUM_Descript","FLDNUM","Pool","parameter"),summarise,mean.val=mean(value,na.rm=T),sd.val=sd(value,na.rm=T),N.val=N(value))
season=ddply(dat.xtab.melt,c("season","STRATUM","STRATUM_Descript","FLDNUM","Pool","parameter"),summarise,mean.val=mean(value,na.rm=T),sd.val=sd(value,na.rm=T),N.val=N(value))
pool=ddply(dat.xtab.melt,c("STRATUM","STRATUM_Descript","FLDNUM","Pool","parameter"),summarise,mean.val=mean(value,na.rm=T),sd.val=sd(value,na.rm=T),N.val=N(value))

parameter.units=data.frame(parameter=vars,units=c(rep("mg/L",5),rep("mol:mol",6)))

#Export data to xslx
xlsx::write.xlsx(merge(season,parameter.units,"parameter"),paste0(export.path,"SRS_summarystats.xlsx"),sheetName="Seasonal",row.names = F,showNA=F)
xlsx::write.xlsx(merge(decade,parameter.units,"parameter"),paste0(export.path,"SRS_summarystats.xlsx"),sheetName="Decade",row.names = F,showNA=F,append=T)
xlsx::write.xlsx(merge(annual,parameter.units,"parameter"),paste0(export.path,"SRS_summarystats.xlsx"),sheetName="Annual",row.names = F,showNA=F,append=T)
xlsx::write.xlsx(merge(pool,parameter.units,"parameter"),paste0(export.path,"SRS_summarystats.xlsx"),sheetName="Pool",row.names = F,showNA=F,append=T)
