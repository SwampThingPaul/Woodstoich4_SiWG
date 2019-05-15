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
test=readOGR("https://services.arcgis.com/v01gqwM5QqNysAAi/arcgis/rest/services/LTRM_Study_Area_Boundary/FeatureServer")

tmap_mode("view")
tm_basemap(leaflet::providers$Esri.WorldImagery,alpha=0.9)+
  tm_shape(sp.dat.shp)+tm_dots(col="FLDNUM",popup.vars=c("SITE_NAME","FLDNUM","N.val","min.date","max.date","yrs.dat"))

##
summary(dat)
names(dat)
dat=dat[,c(40,1:39)]

## Half MDL and rempves 0 values
dat2=data.frame(dat[,1:17],apply(dat[,18:40],2,FUN=function(x)abs(x)/2))
summary(dat2)
dat2=data.frame(dat2[,1:19],apply(dat2[,20:40],2,FUN=function(x) ifelse(x==0.0,NA,as.numeric(x))))
summary(dat2)

## 
dat2$DIN=with(dat2,NHX+NOX)

## Hypothesis1: Changes in Si:N or Si:P scaling(slope) is 
# negatively correlated with DIN or SRP concentrations
# indicating eutrophication decouples Si:Nutrient stoichiometry

dat2=merge(dat2,subset(sp.dat,yrs.dat>20)[,c("SITE_NAME","yrs.dat")],by=c("SITE_NAME"))

# Reversal Evaluation
dat2$TPReversal=with(dat2,ifelse(is.na(SRP)==T|is.na(TP)==T,0,ifelse(SRP>(TP*1.3),1,0)));# Reversals identified as 1 reversals consistent with TP rule evaluation
dat2$TNReversal=with(dat2,ifelse(is.na(DIN)==T|is.na(TN)==T,0,ifelse(DIN>(TN*1.3),1,0)));

# TN
sum(dat2$TNReversal,na.rm=T)
par(family="serif",oma=c(1,1,1,1),mar=c(4,4,1,1))
layout(matrix(1:2,1,2,byrow=F))
plot(TN~DIN,dat2,ylab="TN (mg/L)",xlab="DIN (mg/L)",pch=21,bg=ifelse(TNReversal==1,"dodgerblue1",NA),col=adjustcolor("grey",0.8));abline(0,1,col="dodgerblue1")

# TP
sum(dat2$TPReversal,na.rm=T)
plot(TP~SRP,dat2,ylab="TP (mg/L)",xlab="SRP (mg/L)",pch=21,bg=ifelse(TPReversal==1,"red",NA),col=adjustcolor("grey",0.8));abline(0,1,col="red")

dev.off()
boxplot(SI~as.factor(NORTHING),dat2,horizontal=T,outline=F)
boxplot(TP~as.factor(NORTHING),subset(dat2,TPReversal==0),horizontal=T,outline=F)
boxplot(TN~as.factor(NORTHING),subset(dat2,TNReversal==0),horizontal=T,outline=F)
# TN might be a bigger driver of Si decoupling?

## Stoich 
dat2$TP=with(dat2,ifelse(TPReversal==1,NA,TP))
dat2$TN=with(dat2,ifelse(TNReversal==1,NA,TN))
dat2$TP.ugL=dat2$TP*1000

dat2$Si.mM=with(dat2, SI/Si.mw)
dat2$TP.mM=with(dat2,TP/P.mw)
dat2$TN.mM=with(dat2,TN/N.mw)

dat2$log.Si.mM=log(dat2$Si.mM)
dat2$log.TP.mM=log(dat2$TP.mM)
dat2$log.TN.mM=log(dat2$TN.mM)
dat2$FedWY=WY(dat2$DATE,"Fed")
dat2$dec.year=decimal_date(dat2$DATE)

range(dat2$DATE)
range(dat2$TP.ugL,na.rm=T)
range(dat2$TN,na.rm=T)
range(dat2$SI,na.rm=T)

dat2=dat2[order(dat2$SITE_NAME,dat2$DATE),]

## WY average
WY.dat=ddply(dat2,c("FLDNUM","SITE_NAME","FedWY"),summarise,mean.TP=mean(TP.ugL,na.rm=T),N.TP=N(TP.ugL),mean.TN=mean(TN,na.rm=T),N.TN=N(TN),mean.Si=mean(SI,na.rm=T),N.Si=N(SI))
WY.dat$dec.yr=with(WY.dat,FedWY+0.746);#decimal date for Sept 30 

subset(WY.dat,N.TP<6)
subset(WY.dat,N.TN<6)
subset(WY.dat,N.Si<6)
range(WY.dat$FedWY)

## Time Series plots
sites.val=ddply(subset(sp.dat,yrs.dat>20),c("FLDNUM","SITE_NAME"),summarise,N.val=N(SITE_NAME))

xlim.val=date.fun(c("1991-10-01","2017-10-01"));xmaj=seq(xlim.val[1],xlim.val[2],"10 years");xmin=seq(xlim.val[1],xlim.val[2],"1 years")
#tiff(filename=paste0(plot.path,"LTRM_TP.tiff"),width=7,height=6.5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",oma=c(2.5,2,1,0.25),mar=c(1.5,2,0.5,1))
#par(family="serif",oma=c(1,1,1,1),mar=c(4,4,1,1))
layout(matrix(1:28,7,4,byrow=F))

ylim.val=c(1,2600);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor");#by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:nrow(sites.val)){
  plot(TP.ugL~DATE,dat2,ylim=ylim.val,xlim=xlim.val,yaxt="n",xaxt="n",ylab=NA,xlab=NA,type="n",log="y")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(dat2,SITE_NAME==sites.val$SITE_NAME[i]),pt_line(DATE,TP.ugL,2,adjustcolor("dodgerblue1",0.5),1,21,"indianred1",1))
  axis_fun(2,ymaj,ymin,ymaj)
  if(i%in%c(7,14,21,26)){axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj,"%m-%Y"))}else{axis_fun(1,xmaj,xmin,NA)}
  mtext(side=3,sites.val$SITE_NAME[i],cex=0.8)
}
mtext(side=2,line=0.5,outer=T,"Total Phosphorus (\u03BCg L\u207B\u00B9)")
mtext(side=1,line=0.5,outer=T,"Date (Month-Year)")
dev.off()

#tiff(filename=paste0(plot.path,"LTRM_TN.tiff"),width=7,height=6.5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",oma=c(2.5,2,1,0.25),mar=c(1.5,2,0.5,1))
#par(family="serif",oma=c(1,1,1,1),mar=c(4,4,1,1))
layout(matrix(1:28,7,4,byrow=F))

ylim.val=c(0.01,30);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor");#by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:nrow(sites.val)){
  plot(TN~DATE,dat2,ylim=ylim.val,xlim=xlim.val,yaxt="n",xaxt="n",ylab=NA,xlab=NA,type="n",log="y")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(dat2,SITE_NAME==sites.val$SITE_NAME[i]),pt_line(DATE,TN,2,adjustcolor("dodgerblue1",0.5),1,21,"skyblue1",1))
  axis_fun(2,ymaj,ymin,ymaj)
  if(i%in%c(7,14,21,26)){axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj,"%m-%Y"))}else{axis_fun(1,xmaj,xmin,NA)}
  mtext(side=3,sites.val$SITE_NAME[i],cex=0.8)
}
mtext(side=2,line=0.5,outer=T,"Total Nitrogen (mg L\u207B\u00B9)")
mtext(side=1,line=0.5,outer=T,"Date (Month-Year)")
dev.off()

#tiff(filename=paste0(plot.path,"LTRM_SI.tiff"),width=7,height=6.5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",oma=c(2.5,2,1,0.25),mar=c(1.5,2,0.5,1))
#par(family="serif",oma=c(1,1,1,1),mar=c(4,4,1,1))
layout(matrix(1:28,7,4,byrow=F))

ylim.val=c(0.01,10);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor");#by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:nrow(sites.val)){
  plot(SI~DATE,dat2,ylim=ylim.val,xlim=xlim.val,yaxt="n",xaxt="n",ylab=NA,xlab=NA,type="n",log="y")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(dat2,SITE_NAME==sites.val$SITE_NAME[i]),pt_line(DATE,SI,2,adjustcolor("dodgerblue1",0.5),1,21,"olivedrab1",1))
  axis_fun(2,ymaj,ymin,ymaj)
  if(i%in%c(7,14,21,26)){axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj,"%m-%Y"))}else{axis_fun(1,xmaj,xmin,NA)}
  mtext(side=3,sites.val$SITE_NAME[i],cex=0.8)
}
mtext(side=2,line=0.5,outer=T,"Silica (mg L\u207B\u00B9)")
mtext(side=1,line=0.5,outer=T,"Date (Month-Year)")
dev.off()


xlim.val=c(1991,2017);by.x=10;xmaj=seq(round(xlim.val[1]),xlim.val[2],by.x);xmin=seq(round(xlim.val[1]),xlim.val[2],by.x/by.x)
#tiff(filename=paste0(plot.path,"LTRM_TP_WY.tiff"),width=7,height=6.5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",oma=c(2.5,2,1,0.25),mar=c(1.5,2,0.5,1))
#par(family="serif",oma=c(1,1,1,1),mar=c(4,4,1,1))
layout(matrix(1:28,7,4,byrow=F))

ylim.val=c(1,3000);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor");#by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:nrow(sites.val)){
  plot(mean.TP~FedWY,WY.dat,ylim=ylim.val,xlim=xlim.val,yaxt="n",xaxt="n",ylab=NA,xlab=NA,type="n",log="y")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(dat2,SITE_NAME==sites.val$SITE_NAME[i]),points(jitter(FedWY,1),TP.ugL,pch=19,col=adjustcolor("grey",0.75)))
  with(subset(WY.dat,SITE_NAME==sites.val$SITE_NAME[i]),points(FedWY,mean.TP,pch=21,bg=ifelse(N.TP>=6,"indianred1",NA),cex=1.25,lwd=0.2))
  axis_fun(2,ymaj,ymin,ymaj)
  if(i%in%c(7,14,21,26)){axis_fun(1,line=-0.5,xmaj,xmin,xmaj)}else{axis_fun(1,xmaj,xmin,NA)}
  mtext(side=3,sites.val$SITE_NAME[i],cex=0.8)
}
mtext(side=2,line=0.5,outer=T,"Total Phosphorus (\u03BCg L\u207B\u00B9)")
mtext(side=1,line=0.25,outer=T,"Water Year (Oct - Sept)")

plot(0:1,0:1,axes=F,type="n",ylab=NA,xlab=NA)
legend.text=c("Annual Mean TP",  "Grab Samples","< 6 Samples per WY")
pt.cols=c("indianred1",adjustcolor("grey",0.75),"white")
legend(0.5,0.75,legend=legend.text,pch=c(21,19,21),col=c("black",pt.cols[2],"black"),lwd=0.2,lty=NA,pt.bg=pt.cols,pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5)

plot(0:1,0:1,axes=F,type="n",ylab=NA,xlab=NA)
dev.off()

#tiff(filename=paste0(plot.path,"LTRM_TN_WY.tiff"),width=7,height=6.5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",oma=c(2.5,2,1,0.25),mar=c(1.5,2,0.5,1))
layout(matrix(1:28,7,4,byrow=F))

ylim.val=c(0.01,30);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor");#by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:nrow(sites.val)){
  plot(mean.TN~FedWY,WY.dat,ylim=ylim.val,xlim=xlim.val,yaxt="n",xaxt="n",ylab=NA,xlab=NA,type="n",log="y")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(dat2,SITE_NAME==sites.val$SITE_NAME[i]),points(jitter(FedWY,1),TN,pch=19,col=adjustcolor("grey",0.75)))
  with(subset(WY.dat,SITE_NAME==sites.val$SITE_NAME[i]),points(FedWY,mean.TN,pch=21,bg=ifelse(N.TN>=6,"skyblue1",NA),cex=1.25,lwd=0.2))
  axis_fun(2,ymaj,ymin,ymaj)
  if(i%in%c(7,14,21,26)){axis_fun(1,line=-0.5,xmaj,xmin,xmaj)}else{axis_fun(1,xmaj,xmin,NA)}
  mtext(side=3,sites.val$SITE_NAME[i],cex=0.8)
}
mtext(side=2,line=0.5,outer=T,"Total Nitrogen (mg L\u207B\u00B9)")
mtext(side=1,line=0.25,outer=T,"Water Year (Oct - Sept)")

plot(0:1,0:1,axes=F,type="n",ylab=NA,xlab=NA)
legend.text=c("Annual Mean TN",  "Grab Samples","< 6 Samples per WY")
pt.cols=c("skyblue1",adjustcolor("grey",0.75),"white")
legend(0.5,0.75,legend=legend.text,pch=c(21,19,21),col=c("black",pt.cols[2],"black"),lwd=0.2,lty=NA,pt.bg=pt.cols,pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5)
plot(0:1,0:1,axes=F,type="n",ylab=NA,xlab=NA)
dev.off()

#tiff(filename=paste0(plot.path,"LTRM_Si_WY.tiff"),width=7,height=6.5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",oma=c(2.5,2,1,0.25),mar=c(1.5,2,0.5,1))
layout(matrix(1:28,7,4,byrow=F))

ylim.val=c(0.01,10);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor");#by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(i in 1:nrow(sites.val)){
  plot(mean.TN~FedWY,WY.dat,ylim=ylim.val,xlim=xlim.val,yaxt="n",xaxt="n",ylab=NA,xlab=NA,type="n",log="y")
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  with(subset(dat2,SITE_NAME==sites.val$SITE_NAME[i]),points(jitter(FedWY,1),SI,pch=19,col=adjustcolor("grey",0.75)))
  with(subset(WY.dat,SITE_NAME==sites.val$SITE_NAME[i]),points(FedWY,mean.Si,pch=21,bg=ifelse(N.Si>=6,"olivedrab1",NA),cex=1.25,lwd=0.2))
  axis_fun(2,ymaj,ymin,ymaj)
  if(i%in%c(7,14,21,26)){axis_fun(1,line=-0.5,xmaj,xmin,xmaj)}else{axis_fun(1,xmaj,xmin,NA)}
  mtext(side=3,sites.val$SITE_NAME[i],cex=0.8)
}
mtext(side=2,line=0.5,outer=T,"Silica (mg L\u207B\u00B9)")
mtext(side=1,line=0.25,outer=T,"Water Year (Oct - Sept)")

plot(0:1,0:1,axes=F,type="n",ylab=NA,xlab=NA)
legend.text=c("Annual Mean TN",  "Grab Samples","< 6 Samples per WY")
pt.cols=c("olivedrab1",adjustcolor("grey",0.75),"white")
legend(0.5,0.75,legend=legend.text,pch=c(21,19,21),col=c("black",pt.cols[2],"black"),lwd=0.2,lty=NA,pt.bg=pt.cols,pt.cex=1.5,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5)

plot(0:1,0:1,axes=F,type="n",ylab=NA,xlab=NA)
dev.off()