## 
## Woodstoich 4 (Si Working grop)
## Upper Mississippi River Restoration LTRM
## https://umesc.usgs.gov/data_library/water_quality/water_quality_page.html
## https://www.umesc.usgs.gov/cgi-bin/ltrmp/water/water_meta.pl
##
## SMA regression of spatially random sampling (SRS) 
## Si x TP and Si x TN 
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

library(rcompanion)

#functions
sma.CI=function(mod,newdata,CI.interval=0.95){
  #assumes a log-log model
  prob=1-(1-CI.interval)/2
  
  x.val=exp(mod$data[2])[,1]
  y.val=exp(mod$data[1])[,1]
  y.fit=exp((coef(mod)[2]*log(x.val))+coef(mod)[1])
  mod.residual=y.val-y.fit
  n=length(mod.residual)
  se=(sqrt(sum((mod.residual)^2,na.rm=T))/(n-2))/(sqrt(sum((x.val-mean(x.val,na.rm=T))^2)))
  
  x.val.new=newdata
  y.fit.new=exp((coef(mod)[2]*log(x.val.new))+coef(mod)[1])
  se=sqrt(sum((mod.residual)^2) / (n - 2)) * sqrt(1 / n + (x.val.new - mean(x.val))^2 / sum((x.val - mean(x.val))^2))
  upper=y.fit.new+qt(prob,n-2)*se
  lower=y.fit.new-qt(prob,n-2)*se
  return(list(x.val=x.val.new,fit=y.fit.new,upper=upper,lower=lower))
}

mod.ext=function(test){
  intercept=as.numeric(coef(test)[1]);
  slope=as.numeric(coef(test)[2]);
  r2=as.numeric(test$r2);
  Fstat=as.numeric(test$slopetest[[1]][1]);
  pval=as.numeric(test$slopetest[[1]][3]);
  slope.LCI=test$slopetest[[1]]$ci[,1]
  slope.UCI=test$slopetest[[1]]$ci[,2]
  inter.LCI=test$elevtest[[1]]$a.ci[1]
  inter.UCI=test$elevtest[[1]]$a.ci[2]
  rslt=data.frame(slope=slope,intercept=intercept,r2=r2,Fstat=Fstat,pval=pval,slope.LCI=slope.LCI,slope.UCI=slope.UCI,inter.LCI=inter.LCI,inter.UCI=inter.UCI)
  return(rslt)
}

bars=function(x,lower,upper,col,lty=1,length=0.07,lwd=1,...){
  arrows(x,lower,x,upper,col=col,lty=lty,length=length,angle=90,code=3,lwd=lwd);
  options(warn=-1)
}
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


# Pool LULC ---------------------------------------------------------------

lulc=read.csv(paste0(export.path,"pool_20102011_landcover.csv"))
lulc=merge(lulc,data.frame(Region=c("Pool 4","Pool 8","Pool 13","Pool 26","OpenRiver","LaGrange"),Pool=c("Pool 4","Pool 8","Pool 13","Pool 26","Open River","La Grange")),"Region")
lulc.xtab=cast(lulc,Pool~CLASS_15_N,value="area.percent",mean)


vars=c("Pool", "Agriculture", "DeepMarsh", "Developed", "Grassforbs", 
       "OpenWater", "RoadLevee", "FAV", "Sand","ShallowMarsh", "ShrubScrub", "SAV", 
       "UplandForest", "WetForest", "WetMeadow", "WetShrub")
colnames(lulc.xtab)=vars
# SRS Data ----------------------------------------------------------------
srs.dat=read.csv(paste0(data.path,"UMR_MainStem_SRSdataset_1993-2018.csv"))
srs.dat$DATETIME=date.fun(paste0(as.character(srs.dat$DATE)," ",as.character(srs.dat$TIME),":00"),tz="America/Chicago",form="%m/%d/%Y %H:%M:%S")
srs.dat$DATE=date.fun(as.character(srs.dat$DATE),tz="America/Chicago",form="%m/%d/%Y")
srs.dat$dataset="srs"
srs.dat=merge(srs.dat,stratum.lookup,"STRATUM",all.x=T)
srs.dat=merge(srs.dat,FLDNUM.pool.xwalk,"FLDNUM",all.x=T)

# Spatial data
sp.dat=ddply(srs.dat,c("LOCATCD","FLDNUM","dataset","STRATUM"),summarise,EASTING=mean(EASTING,na.rm=T),NORTHING=mean(NORTHING,na.rm=T),N.val=N(LOCATCD),min.date=min(DATE),max.date=max(DATE),N.SI=N(SI),N.TP=N(TP),N.TN=N(TN))
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

dat.xtab$log.Si.mM=log(dat.xtab$Si.mM)
dat.xtab$log.TP.mM=log(dat.xtab$TP.mM)
dat.xtab$log.TN.mM=log(dat.xtab$TN.mM)


dat.xtab=subset(dat.xtab,CY%in%seq(2010,2018,1))
range(dat.xtab$SI,na.rm = T)
format(range(dat.xtab$Si.mM,na.rm=T),scientific = F)


vars=c("LOCATCD","FLDNUM","STRATUM","season","WY","CY","TP.mM","TN.mM","Si.mM","DIN.mM","SRP.mM","TURB","VSS","CHLcal","SiTN","SiTP",'TP.ugL',"TN","SI","SRP","DIN")
dat.xtab.melt=melt(data.frame(dat.xtab[,vars]),id.vars=vars[1:6])
dat.xtab.melt=subset(dat.xtab.melt,is.na(value)==F)

dat.xtab.ann=cast(dat.xtab.melt,FLDNUM+STRATUM+CY~variable,value="value",mean)
dat.xtab.ann$log.Si.mM=log(dat.xtab.ann$Si.mM)
dat.xtab.ann$log.TP.mM=log(dat.xtab.ann$TP.mM)
dat.xtab.ann$log.TN.mM=log(dat.xtab.ann$TN.mM)

dat.xtab.mean=cast(dat.xtab.melt,FLDNUM+STRATUM~variable,value="value",mean)

ddply(dat.xtab.ann,"STRATUM",summarise,min.val=format(min(Si.mM,na.rm=T),scientific=F),max.val=max(Si.mM,na.rm=T))
ddply(dat.xtab.ann,"STRATUM",summarise,min.val=format(min(TP.mM,na.rm=T),scientific=F),max.val=max(TP.mM,na.rm=T))
ddply(dat.xtab.ann,"STRATUM",summarise,min.val=format(min(TN.mM,na.rm=T),scientific=F),max.val=max(TN.mM,na.rm=T))

ddply(dat.xtab.ann,c("FLDNUM","STRATUM"),summarise,N.Si=N(Si.mM),N.TN=N(TN.mM),N.TP=N(TP.mM))

# Summary Stats -----------------------------------------------------------
## Annual data
range(dat.xtab.ann$Si.mM,na.rm=T)
range(dat.xtab.ann$SI,na.rm=T)

range(dat.xtab.ann$TP.mM,na.rm=T)
range(dat.xtab.ann$TP.ugL,na.rm=T)

range(dat.xtab.ann$TN.mM,na.rm=T)
range(dat.xtab.ann$TN,na.rm=T)

cols=colorRampPalette(c("darkseagreen3","goldenrod3","indianred3"))(6)
#tiff(filename=paste0(plot.path,"LTRM_srs_CY_SiTPTN.tiff"),width=6.5,height=4,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
#png(filename=paste0(plot.path,"LTRM_srs_CY_SiTPTN.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",oma=c(0.5,2.5,1,0.75),mar=c(3,1.5,0.25,0.5))
layout(matrix(c(1,2,5,3,4,5),2,3,byrow=T),widths=c(1,1,0.25))

ylim.val=c(0,125);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,1500);by.x=c(250);xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(SiTP~TP.ugL,dat.xtab.ann,ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ylab=NA,xlab=NA,yaxs="i",xaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey80")
for(i in 1:6){
  with(subset(dat.xtab.ann,FLDNUM==FLDNUM.pool.xwalk$FLDNUM[i]),points(TP.ugL,SiTP,pch=21,bg=adjustcolor(cols[i],0.75),col=adjustcolor("black",0.5),lwd=0.1))
}
axis_fun(1,line=-0.5,xmaj,xmin,xmaj);axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.75,"Total Phosphorus (\u03BCg L\u207B\u00B9)")
mtext(side=2,line=2.25,"Si:TP (molar ratio)")

xlim.val=c(0,10);by.x=c(2);xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(SiTP~SI,dat.xtab.ann,ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ylab=NA,xlab=NA,yaxs="i",xaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey80")
for(i in 1:6){
  with(subset(dat.xtab.ann,FLDNUM==FLDNUM.pool.xwalk$FLDNUM[i]),points(SI,SiTP,pch=21,bg=adjustcolor(cols[i],0.75),col=adjustcolor("black",0.5),lwd=0.1))
}
axis_fun(1,line=-0.5,xmaj,xmin,xmaj);axis_fun(2,ymaj,ymin,NA);box(lwd=1)
mtext(side=1,line=1.75,"Dissolved Silicon (mg L\u207B\u00B9)")

ylim.val=c(0,3);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(0,15);by.x=2.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(SiTN~TN,dat.xtab.ann,ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ylab=NA,xlab=NA,yaxs="i",xaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey80")
for(i in 1:6){
  with(subset(dat.xtab.ann,FLDNUM==FLDNUM.pool.xwalk$FLDNUM[i]),points(TN,SiTN,pch=21,bg=adjustcolor(cols[i],0.75),col=adjustcolor("black",0.5),lwd=0.1))
}
axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj));axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=1.75,"Total Nitrogen (mg L\u207B\u00B9)")
mtext(side=2,line=2.25,"Si:TN (molar ratio)")

xlim.val=c(0,10);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(SiTN~SI,dat.xtab.ann,ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ylab=NA,xlab=NA,yaxs="i",xaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey80")
for(i in 1:6){
  with(subset(dat.xtab.ann,FLDNUM==FLDNUM.pool.xwalk$FLDNUM[i]),points(SI,SiTN,pch=21,bg=adjustcolor(cols[i],0.75),col=adjustcolor("black",0.5),lwd=0.1))
}
axis_fun(1,line=-0.5,xmaj,xmin,format(xmaj));axis_fun(2,ymaj,ymin,NA);box(lwd=1)
mtext(side=1,line=1.75,"Dissolved Silicon (mg L\u207B\u00B9)")

plot(0:1,0:1,axes=F,type="n",ylab=NA,xlab=NA)
legend.text=FLDNUM.pool.xwalk$Pool
legend(0.5,0.5,legend=legend.text,pch=21,col=adjustcolor("black",0.5),lwd=0.2,lty=NA,pt.bg=adjustcolor(cols,0.75),pt.cex=1.5,ncol=1,cex=0.75,bty="n",y.intersp=1.5,x.intersp=0.5,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()

SiTP_TP_cortest=with(dat.xtab.ann,cor.test(SiTP,TP.ugL,method="spearman"))
SiTP_Si_cortest=with(dat.xtab.ann,cor.test(SiTP,SI,method="spearman"))
SiTN_TN_cortest=with(dat.xtab.ann,cor.test(SiTN,TN,method="spearman"))
SiTN_Si_cortest=with(dat.xtab.ann,cor.test(SiTN,SI,method="spearman"))

cor.rslts=data.frame(Var1=c("Si:TP","Si:TP","Si:TN","Si:TN"),Var2=c("TP","Si","TN","Si"),
           rho=c(as.numeric(SiTP_TP_cortest$estimate),as.numeric(SiTP_Si_cortest$estimate),as.numeric(SiTN_TN_cortest$estimate),as.numeric(SiTN_Si_cortest$estimate)),
           pval=c(as.numeric(SiTP_TP_cortest$p.value),as.numeric(SiTP_Si_cortest$p.value),as.numeric(SiTN_TN_cortest$p.value),as.numeric(SiTN_Si_cortest$p.value)))
cor.rslts$rho=round(cor.rslts$rho,2)
cor.rslts$pval.txt=with(cor.rslts,ifelse(pval<0.01,"<0.01",signif(round(pval,2),digits=2)))

kable(cor.rslts[,c("Var1","Var2","rho","pval.txt")],
      col.names = c("Variable 1","Variable 2","Spearman's \u03C1","\u03C1-value"),
      align=c("l","l","c","c"),row.names = F)%>%
  kable_styling(bootstrap_options = "striped", full_width = F)



# SMA Analysis ------------------------------------------------------------
### SMA Figure combined 

dat.xtab.ann=merge(dat.xtab.ann,data.frame(STRATUM=1:6,RRes=c(1:3,5,4,6)),"STRATUM")
stratum.lookup=merge(stratum.lookup,data.frame(STRATUM=1:6,RRes=c(1:3,5,4,6)),"STRATUM")

tmp=ddply(dat.xtab.ann,c("STRATUM","FLDNUM"),summarise,N.val=N(FLDNUM))
merge(ddply(tmp,c("STRATUM"),summarise,N.val=N(FLDNUM)),stratum.lookup,"STRATUM")

#tiff(filename=paste0(plot.path,"LTRM_mainstem_UMR2_sma_CY.tiff"),width=6.5,height=4.25,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
#png(filename=paste0(plot.path,"LTRM_mainstem_UMR2_sma_CY.png"),width=6.5,height=4.25,units="in",res=200,type="windows",bg="white")
par(family="serif",oma=c(0.25,2.25,1,0.75),mar=c(2.1,1.25,0.25,0.25))
layout(matrix(c(rep(1,6),2:13,rep(14,6)),4,6,byrow=T),heights=c(0.2,1,1,0.25))
#cols=viridis::viridis_pal(option ="E")(6)
#cols=RColorBrewer::brewer.pal(6,"BrBG")
cols=colorRampPalette(c("darkseagreen3","goldenrod3","indianred3"))(6)

plot(0:1,0:1,ylab=NA,xlab=NA,axes=F,type="n")
text(0.96,0.25,"Slow",xpd=NA,font=1)
text(0.04,0.25,"Fast",xpd=NA,font=1)
text(0.5,2.5,"Relative Residence Time",xpd=NA,font=3,cex=0.8)
arrows(0.93,0.1,0.06,0.1,lwd=1.5,xpd=NA,length=0.15,angle=25,code=1)

sma.rslts.SiTP.SRS=data.frame()
sma.rslts.SiTN.SRS=data.frame()
par(mar=c(2.1,1.25,1,0.25))
for(j in 1:6){
  tmp=subset(dat.xtab.ann,RRes==j)
  
  ylim.val=c(0.02,0.5);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
  xlim.val=c(0.001,0.05);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor")   
  
  plot(Si.mM~TP.mM,tmp,log="xy",ylim=ylim.val,xlim=xlim.val,type="n",yaxt="n",xaxt="n",ylab=NA,xlab=NA)
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  axis_fun(1,line=-0.75,xmaj,xmin,format(xmaj,scientific=F),0.8);
  if(j==1){axis_fun(2,ymaj,ymin,ymaj,cex=0.8)}else{axis_fun(2,ymaj,ymin,NA)};box(lwd=1)
  lines(x=c(0.0001,0.1),y=(c(0.0001,0.1))*16,lty=2,lwd=1.25)
  for(i in 1:6){
    tmp2=subset(tmp,FLDNUM==FLDNUM.pool.xwalk$FLDNUM[i])
    if(nrow(tmp2)==0){next}else{
      with(tmp2,points(TP.mM,Si.mM,pch=21,bg=adjustcolor(cols[i],0.1),col=adjustcolor(cols[i],0.3),lwd=0.2,cex=1.25))
      mod=sma(log.Si.mM~log.TP.mM,tmp2,slope.test=1)
      tmp.mod=mod.ext(mod);
      tmp.mod$FLDNUM=FLDNUM.pool.xwalk$FLDNUM[i]
      tmp.mod$STRATUM=subset(stratum.lookup,RRes==j)$STRATUM
      sma.rslts.SiTP.SRS=rbind(sma.rslts.SiTP.SRS,tmp.mod)
      x.val.min=min(tmp2$TP.mM,na.rm=T)-min(tmp2$TP.mM,na.rm=T)*0.25
      x.val.max=max(tmp2$TP.mM,na.rm=T)+max(tmp2$TP.mM,na.rm=T)*0.25
      x.val=seq(x.val.min,x.val.max,length.out=100)
      mod.fit=sma.CI(mod,x.val)
      with(mod.fit,lines(x.val,fit,lty=1,lwd=1.5,col=adjustcolor(cols[i],1)))
      #with(mod.fit,lines(x.val,lower,lty=2,lwd=1,col=cols[i]))
      #with(mod.fit,lines(x.val,upper,lty=2,lwd=1,col=cols[i]))
    }
  }
  if(j==1){mtext(side=2,line=2,"Dissolved Silicon (mM)",cex=0.8)}
  mtext(side=3,subset(stratum.lookup,RRes==j)$STRATUM_Descript,cex=0.75)
  if(j==4){text(x=xlim.val[1]+0.01,y=ylim.val[1]-0.011,"Total Phosphorus (mM)",xpd=NA,adj=1,cex=1.1)}
}
for(j in 1:6){
  tmp=subset(dat.xtab.ann,RRes==j)
  
  ylim.val=c(0.02,0.5);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
  xlim.val=c(0.05,1.0);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor")   
  
  plot(Si.mM~TN.mM,tmp,log="xy",ylim=ylim.val,xlim=xlim.val,type="n",yaxt="n",xaxt="n",ylab=NA,xlab=NA)
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  axis_fun(1,line=-0.75,xmaj,xmin,format(xmaj,scientific=F),0.8);
  if(j==1){axis_fun(2,ymaj,ymin,ymaj,cex=0.8)}else{axis_fun(2,ymaj,ymin,NA)};box(lwd=1)
  abline(0,1,lty=2)
  for(i in 1:6){
    tmp2=subset(tmp,FLDNUM==FLDNUM.pool.xwalk$FLDNUM[i])
    if(nrow(tmp2)==0){next}else{
      with(tmp2,points(TN.mM,Si.mM,pch=21,bg=adjustcolor(cols[i],0.1),col=adjustcolor(cols[i],0.3),lwd=0.2,cex=1.25))
      mod=sma(log.Si.mM~log.TN.mM,tmp2,slope.test=1)
      tmp.mod=mod.ext(mod);
      tmp.mod$FLDNUM=FLDNUM.pool.xwalk$FLDNUM[i]
      tmp.mod$STRATUM=subset(stratum.lookup,RRes==j)$STRATUM
      sma.rslts.SiTN.SRS=rbind(sma.rslts.SiTN.SRS,tmp.mod)
      x.val.min=min(tmp2$TN.mM,na.rm=T)-min(tmp2$TN.mM,na.rm=T)*0.25
      x.val.max=max(tmp2$TN.mM,na.rm=T)+max(tmp2$TN.mM,na.rm=T)*0.25
      x.val=seq(x.val.min,x.val.max,length.out=100)
      mod.fit=sma.CI(mod,x.val)
      with(mod.fit,lines(x.val,fit,lty=1,lwd=1.5,col=adjustcolor(cols[i],1.1)))
      #with(mod.fit,lines(x.val,lower,lty=2,lwd=1,col=cols[i]))
      #with(mod.fit,lines(x.val,upper,lty=2,lwd=1,col=cols[i]))
    }
  }
  if(j==1){mtext(side=2,line=2,"Dissolved Silicon (mM)",cex=0.8)}
  if(j==4){text(x=xlim.val[1]+0.19,y=ylim.val[1]-0.011,"Total Nitrogen (mM)",xpd=NA,adj=1,cex=1.1)}
}

plot(0:1,0:1,axes=F,type="n",ylab=NA,xlab=NA)
legend.text=c(paste0("Reach ",1:6),"Redfield Ratio");#c(as.character(FLDNUM.pool.xwalk$Pool),"Redfield Ratio")
legend(0.5,-0.25,legend=legend.text,pch=NA,col=c(cols,"black"),lwd=1.5,lty=c(rep(1,6),2),pt.bg=cols,pt.cex=1.5,ncol=7,cex=0.8,bty="n",y.intersp=1.75,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,text.col="white")
legend(0.5,-0.25,legend=legend.text,pch=c(rep(21,6),NA),col=c(cols,"black"),lwd=0.2,lty=NA,pt.bg=adjustcolor(cols,0.5),pt.cex=1.5,ncol=7,cex=0.8,bty="n",y.intersp=1.75,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()    


##
merge(ddply(sma.rslts.SiTP.SRS,c("STRATUM"),summarise,N.val=N(FLDNUM)),stratum.lookup,"STRATUM")

sma.rslts.SiTP.SRS$param="Si X TP"
sma.rslts.SiTP.SRS=merge(sma.rslts.SiTP.SRS,stratum.lookup,"STRATUM")
sma.rslts.SiTP.SRS=merge(sma.rslts.SiTP.SRS,FLDNUM.pool.xwalk,"FLDNUM")
sma.rslts.SiTP.SRS=sma.rslts.SiTP.SRS[order(sma.rslts.SiTP.SRS$STRATUM,sma.rslts.SiTP.SRS$FLDNUM),]
sma.rslts.SiTP.SRS$slope=round(sma.rslts.SiTP.SRS$slope,2)
sma.rslts.SiTP.SRS$intercept=round(sma.rslts.SiTP.SRS$intercept,1)
sma.rslts.SiTP.SRS$Fstat=round(sma.rslts.SiTP.SRS$Fstat,2)
sma.rslts.SiTP.SRS$r2=signif(round(sma.rslts.SiTP.SRS$r2,3))
sma.rslts.SiTP.SRS$pval.txt=with(sma.rslts.SiTP.SRS,ifelse(pval<0.05,"<0.05",signif(round(pval,2),digits=2)))

kable(sma.rslts.SiTP.SRS[,c("param","STRATUM_Descript","FLDNUM","slope","r2","pval.txt")],
      col.names = c("Comparison (Y by X)","Ecosystem Stratum","Reach","SMA Slope","R\u00B2","\u03C1-value"),
      align=c("l","l","l","c","c","c"),row.names = F)%>%
  kable_styling(bootstrap_options = "striped", full_width = F)%>%
  collapse_rows(columns=c(1,2),valign="top")

SMA.slope.compare=data.frame()
for(i in 1:6){
  slope.test=sma(log.Si.mM~log.TP.mM*FLDNUM,subset(dat.xtab.ann,STRATUM==stratum.lookup$STRATUM[i]),slope.test=1,multcomp = T)
  slope.test=slope.test$multcompresult
  if(is.null(slope.test)==T){next}else{
    if(nrow(subset(slope.test,Pval<0.05))==0){
      rslt=data.frame(Group=subset(sma.rslts.SiTP.SRS,STRATUM==i)$FLDNUM,Letter=rep("a",nrow(subset(sma.rslts.SiTP.SRS,STRATUM==i))))
    }else{
      slope.test$comparison=with(slope.test,paste(FLDNUM_1,FLDNUM_2,sep=" - "))
      rslt=cldList(Pval~comparison,data=slope.test)[,c("Group","Letter")]
    }
  }
  rslt$STRATUM=stratum.lookup$STRATUM[i]
  colnames(rslt)=c("FLDNUM","Slope.Pairwise","STRATUM")
  SMA.slope.compare=rbind(SMA.slope.compare,rslt)
}
sma.rslts.SiTP.SRS=merge(sma.rslts.SiTP.SRS,SMA.slope.compare,c("FLDNUM","STRATUM"),all.x=T)

SMA.inter.compare=data.frame()
for(i in 1:6){
  Inter.test=sma(log.Si.mM~log.TP.mM+FLDNUM,subset(dat.xtab.ann,STRATUM==stratum.lookup$STRATUM[i]),slope.test=1,multcomp = T,type="elevation")
  Inter.test=Inter.test$multcompresult
  if(is.null(Inter.test)==T){next}else{
    Inter.test$comparison=with(Inter.test,paste(FLDNUM_1,FLDNUM_2,sep=" - "))
    rslt=cldList(Pval~comparison,data=Inter.test)[,c("Group","Letter")]
  }
  rslt$STRATUM=stratum.lookup$STRATUM[i]
  colnames(rslt)=c("FLDNUM","Intercept.Pairwise","STRATUM")
  SMA.inter.compare=rbind(SMA.inter.compare,rslt)
}
#sma(log.Si.mM~log.TP.mM+FLDNUM,subset(dat.xtab.ann,STRATUM==stratum.lookup$STRATUM[4]),slope.test=1,multcomp = T,type="elevation")

sma.rslts.SiTP.SRS=merge(sma.rslts.SiTP.SRS,SMA.inter.compare,c("FLDNUM","STRATUM"),all.x=T)

sma.rslts.SiTP.SRS$Slope.Pairwise=toupper(sma.rslts.SiTP.SRS$Slope.Pairwise)
sma.rslts.SiTP.SRS$Intercept.Pairwise=toupper(sma.rslts.SiTP.SRS$Intercept.Pairwise)
names(sma.rslts.SiTP.SRS)
options(knitr.kable.NA = '')

sma.rslts.SiTP.SRS=sma.rslts.SiTP.SRS[order(sma.rslts.SiTP.SRS$RRes,sma.rslts.SiTP.SRS$FLDNUM),]
kable(sma.rslts.SiTP.SRS[,c("param","STRATUM_Descript","FLDNUM","r2","intercept","slope","Fstat","pval.txt","Slope.Pairwise","Intercept.Pairwise")],
      col.names = c("Comparison (Y by X)","Aquatic Area","Reach","R\u00B2","Intercept","Slope","F-statistic","\u03C1-value","Slope Similairty","Intercept Similarity"),
      align=c("l","l","l","c","c","c","c","c"),row.names = F)%>%
  kable_styling(bootstrap_options = "striped", full_width = F)%>%
  collapse_rows(columns=c(1,2),valign="top")
#%>%footnote(general=c("Slope and Intercept similarity is based on slope and intercept comparisons within Ecosystems","F-statistic and \u03C1-value corresponds to testing if the slope is significantly different from one (1)."))


##
sma.rslts.SiTN.SRS$param="Si X TN"
sma.rslts.SiTN.SRS=merge(sma.rslts.SiTN.SRS,stratum.lookup,"STRATUM")
sma.rslts.SiTN.SRS=merge(sma.rslts.SiTN.SRS,FLDNUM.pool.xwalk,"FLDNUM")
sma.rslts.SiTN.SRS=sma.rslts.SiTN.SRS[order(sma.rslts.SiTN.SRS$STRATUM,sma.rslts.SiTN.SRS$FLDNUM),]
sma.rslts.SiTN.SRS$slope=round(sma.rslts.SiTN.SRS$slope,2)
sma.rslts.SiTN.SRS$intercept=round(sma.rslts.SiTN.SRS$intercept,1)
sma.rslts.SiTN.SRS$Fstat=round(sma.rslts.SiTN.SRS$Fstat,2)
sma.rslts.SiTN.SRS$r2=signif(round(sma.rslts.SiTN.SRS$r2,3))
sma.rslts.SiTN.SRS$pval.txt=with(sma.rslts.SiTN.SRS,ifelse(pval<0.05,"<0.05",signif(round(pval,2),digits=2)))

kable(sma.rslts.SiTN.SRS[,c("param","STRATUM_Descript","Pool","slope","r2","pval.txt")],
      col.names = c("Comparison (Y by X)","Ecosystem Stratum","Pool","SMA Slope","R\u00B2","\u03C1-value"),
      align=c("l","l","l","c","c","c"),row.names = F)%>%
  kable_styling(bootstrap_options = "striped", full_width = F)%>%
  collapse_rows(columns=c(1,2),valign="top")

SMA.slope.compare=data.frame()
for(i in 1:6){
  slope.test=sma(log.Si.mM~log.TN.mM*FLDNUM,subset(dat.xtab.ann,STRATUM==stratum.lookup$STRATUM[i]),slope.test=1,multcomp = T)
  slope.test=slope.test$multcompresult
  if(is.null(slope.test)==T){next}else{
    if(nrow(subset(slope.test,Pval<0.05))==0){
      rslt=data.frame(Group=subset(sma.rslts.SiTN.SRS,STRATUM==i)$FLDNUM,Letter=rep("a",nrow(subset(sma.rslts.SiTN.SRS,STRATUM==i))))
    }else{
      slope.test$comparison=with(slope.test,paste(FLDNUM_1,FLDNUM_2,sep=" - "))
      rslt=cldList(Pval~comparison,data=slope.test)[,c("Group","Letter")]
    }
  }
  rslt$STRATUM=stratum.lookup$STRATUM[i]
  colnames(rslt)=c("FLDNUM","Slope.Pairwise","STRATUM")
  SMA.slope.compare=rbind(SMA.slope.compare,rslt)
}
sma.rslts.SiTN.SRS=merge(sma.rslts.SiTN.SRS,SMA.slope.compare,c("FLDNUM","STRATUM"),all.x=T)

SMA.inter.compare=data.frame()
for(i in 1:6){
  Inter.test=sma(log.Si.mM~log.TN.mM+FLDNUM,subset(dat.xtab.ann,STRATUM==stratum.lookup$STRATUM[i]),slope.test=1,multcomp = T,type="elevation")
  Inter.test=Inter.test$multcompresult
  if(is.null(Inter.test)==T){next}else{
    Inter.test$comparison=with(Inter.test,paste(FLDNUM_1,FLDNUM_2,sep=" - "))
    rslt=cldList(Pval~comparison,data=Inter.test)[,c("Group","Letter")]
  }
  rslt$STRATUM=stratum.lookup$STRATUM[i]
  colnames(rslt)=c("FLDNUM","Intercept.Pairwise","STRATUM")
  SMA.inter.compare=rbind(SMA.inter.compare,rslt)
}

sma(log.Si.mM~log.TN.mM+FLDNUM,subset(dat.xtab.ann,STRATUM==stratum.lookup$STRATUM[4]),slope.test=1,multcomp = T,type="elevation")

sma.rslts.SiTN.SRS=merge(sma.rslts.SiTN.SRS,SMA.inter.compare,c("FLDNUM","STRATUM"),all.x=T)

sma.rslts.SiTN.SRS$Slope.Pairwise=toupper(sma.rslts.SiTN.SRS$Slope.Pairwise)
sma.rslts.SiTN.SRS$Intercept.Pairwise=toupper(sma.rslts.SiTN.SRS$Intercept.Pairwise)
names(sma.rslts.SiTN.SRS)
options(knitr.kable.NA = '')

sma.rslts.SiTN.SRS=sma.rslts.SiTN.SRS[order(sma.rslts.SiTN.SRS$RRes,sma.rslts.SiTN.SRS$FLDNUM),]
kable(sma.rslts.SiTN.SRS[,c("param","STRATUM_Descript","FLDNUM","r2","intercept","slope","Fstat","pval.txt","Slope.Pairwise","Intercept.Pairwise")],
      col.names = c("Comparison (Y by X)","Aquatic Area","Reach","R\u00B2","Intercept","Slope","F-statistic","\u03C1-value","Slope Similarity","Intercept Similarity"),
      align=c("l","l","l","c","c","c","c","c"),row.names = F)%>%
  kable_styling(bootstrap_options = "striped", full_width = F)%>%
  collapse_rows(columns=c(1,2),valign="top")
#%>%footnote(general=c("Slope and Intercept similarity is based on slope and intercept comparisons within Ecosystems","F-statistic and \u03C1-value corresponds to testing if the slope is significantly different from one (1)."))


##
##
## 
unique(sma.rslts.SiTP.SRS2$STRATUM_Descript)
pch.val.df=data.frame(STRATUM_Descript=c("Main Channel","Side Channel","Connected Backwater", "Impoundment","Riverine Lake","Isolated Backwater"),pch.vals=c(21,22,23,24,25,25))


sma.rslts.SiTP.SRS2=merge(sma.rslts.SiTP.SRS,dat.xtab.mean,c("FLDNUM","STRATUM"))
sma.rslts.SiTP.SRS2=merge(sma.rslts.SiTP.SRS2,pch.val.df,"STRATUM_Descript")
sma.rslts.SiTN.SRS2=merge(sma.rslts.SiTN.SRS,dat.xtab.mean,c("FLDNUM","STRATUM"))
sma.rslts.SiTN.SRS2=merge(sma.rslts.SiTN.SRS2,pch.val.df,"STRATUM_Descript")

ylim.val=c(0,0.6);by.y=0.1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
cols=colorRampPalette(c("darkseagreen3","goldenrod3","indianred3"))(6)
#tiff(filename=paste0(plot.path,"LTRM_R2.tiff"),width=6.5,height=3.75,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
#png(filename=paste0(plot.path,"LTRM_R2.png"),width=6.5,height=3.75,units="in",res=200,type="windows",bg="white")
par(family="serif",oma=c(0.5,2.5,1,0.75),mar=c(3,1.5,0.25,0.5))
layout(matrix(c(1:4,9,5:8,9),2,5,byrow=T),widths=c(rep(1,4),0.6))

ylim.val=c(0,0.6);by.y=0.1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1,6);by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(r2~RRes,sma.rslts.SiTP.SRS2,ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ylab=NA,xlab=NA)# Relative HRT
abline(h=ymaj,v=xmaj,lty=3,col="grey80")
for(i in 1:6){
  with(subset(sma.rslts.SiTP.SRS2,FLDNUM==FLDNUM.pool.xwalk$FLDNUM[i]),points(RRes,r2,pch=21,bg=adjustcolor(cols[i],0.75),col=adjustcolor("black",0.5),lwd=0.1,cex=1.5))
}
axis_fun(1,line=-0.5,xmaj,xmin,xmaj);axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=1,line=1.75,"Relative Residence Time",cex=0.8)
mtext(side=2,line=2.25,"Si x TP SMA R\u00B2",cex=0.8)

xlim.val=c(0,60);by.x=20;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(r2~TURB,sma.rslts.SiTP.SRS2,ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ylab=NA,xlab=NA)# Relative HRT
abline(h=ymaj,v=xmaj,lty=3,col="grey80")
for(i in 1:6){
  with(subset(sma.rslts.SiTP.SRS2,FLDNUM==FLDNUM.pool.xwalk$FLDNUM[i]),points(TURB,r2,pch=21,bg=adjustcolor(cols[i],0.75),col=adjustcolor("black",0.5),lwd=0.1,cex=1.5))
}
axis_fun(1,line=-0.5,xmaj,xmin,xmaj);axis_fun(2,ymaj,ymin,NA);box(lwd=1)
mtext(side=1,line=1.75,"Turbidity (NTU)",cex=0.8)

#xlim.val=c(15,80);by.x=40;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
xlim.val=c(10,100);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor")
plot(r2~CHLcal,sma.rslts.SiTP.SRS2,ylim=ylim.val,xlim=xlim.val,log="x",type="n",axes=F,ylab=NA,xlab=NA)# Relative HRT
abline(h=ymaj,v=xmaj,lty=3,col="grey80")
for(i in 1:6){
  with(subset(sma.rslts.SiTP.SRS2,FLDNUM==FLDNUM.pool.xwalk$FLDNUM[i]),points(CHLcal,r2,pch=21,bg=adjustcolor(cols[i],0.75),col=adjustcolor("black",0.5),lwd=0.1,cex=1.5))
}
axis_fun(1,line=-0.5,xmaj,xmin,xmaj);axis_fun(2,ymaj,ymin,NA);box(lwd=1)
mtext(side=1,line=1.75,"Chlorophyll-A (\u03BCg L\u207B\u00B9)",cex=0.8)

#xlim.val=c(0,0.30);by.x=0.1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
xlim.val=c(0.5,10);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor")#by.x=0.1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(r2~SRP,sma.rslts.SiTP.SRS2,log="x",ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ylab=NA,xlab=NA)# Relative HRT
abline(h=ymaj,v=xmaj,lty=3,col="grey80")
for(i in 1:6){
  with(subset(sma.rslts.SiTP.SRS2,FLDNUM==FLDNUM.pool.xwalk$FLDNUM[i]),points(SRP.mM*1000,r2,pch=21,bg=adjustcolor(cols[i],0.75),col=adjustcolor("black",0.5),lwd=0.1,cex=1.5))
}
axis_fun(1,line=-0.5,xmaj,xmin,xmaj);axis_fun(2,ymaj,ymin,NA);box(lwd=1)
mtext(side=1,line=1.75,"SRP (\u03BCM)",cex=0.8)

ylim.val=c(0,1);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(1,6);by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(r2~RRes,sma.rslts.SiTN.SRS2,ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ylab=NA,xlab=NA)# Relative HRT
abline(h=ymaj,v=xmaj,lty=3,col="grey80")
for(i in 1:6){
  with(subset(sma.rslts.SiTN.SRS2,FLDNUM==FLDNUM.pool.xwalk$FLDNUM[i]),points(RRes,r2,pch=21,bg=adjustcolor(cols[i],0.75),col=adjustcolor("black",0.5),lwd=0.1,cex=1.5))
}
axis_fun(1,line=-0.5,xmaj,xmin,xmaj);axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=1,line=1.75,"Relative Residence Time",cex=0.8)
mtext(side=2,line=2.25,"Si x TN SMA R\u00B2",cex=0.8)

xlim.val=c(0,60);by.x=20;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
plot(r2~TURB,sma.rslts.SiTN.SRS2,ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ylab=NA,xlab=NA)# Relative HRT
abline(h=ymaj,v=xmaj,lty=3,col="grey80")
for(i in 1:6){
  with(subset(sma.rslts.SiTN.SRS2,FLDNUM==FLDNUM.pool.xwalk$FLDNUM[i]),points(TURB,r2,pch=21,bg=adjustcolor(cols[i],0.75),col=adjustcolor("black",0.5),lwd=0.1,cex=1.5))
}
axis_fun(1,line=-0.5,xmaj,xmin,xmaj);axis_fun(2,ymaj,ymin,NA);box(lwd=1)
mtext(side=1,line=1.75,"Turbidity (NTU)",cex=0.8)

#xlim.val=c(15,80);by.x=40;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
xlim.val=c(10,100);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor")
plot(r2~CHLcal,sma.rslts.SiTN.SRS2,ylim=ylim.val,xlim=xlim.val,log="x",type="n",axes=F,ylab=NA,xlab=NA)# Relative HRT
abline(h=ymaj,v=xmaj,lty=3,col="grey80")
for(i in 1:6){
  with(subset(sma.rslts.SiTN.SRS2,FLDNUM==FLDNUM.pool.xwalk$FLDNUM[i]),points(CHLcal,r2,pch=21,bg=adjustcolor(cols[i],0.75),col=adjustcolor("black",0.5),lwd=0.1,cex=1.5))
}
axis_fun(1,line=-0.5,xmaj,xmin,xmaj);axis_fun(2,ymaj,ymin,NA);box(lwd=1)
mtext(side=1,line=1.75,"Chlorophyll-A (\u03BCg L\u207B\u00B9)",cex=0.8)

#xlim.val=c(1,4);by.x=1;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
xlim.val=c(0.01,1);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor")
plot(r2~DIN,sma.rslts.SiTN.SRS2,ylim=ylim.val,xlim=xlim.val,log="x",type="n",axes=F,ylab=NA,xlab=NA)# Relative HRT
abline(h=ymaj,v=xmaj,lty=3,col="grey80")
for(i in 1:6){
  with(subset(sma.rslts.SiTN.SRS2,FLDNUM==FLDNUM.pool.xwalk$FLDNUM[i]),points(DIN.mM,r2,pch=21,bg=adjustcolor(cols[i],0.75),col=adjustcolor("black",0.5),lwd=0.1,cex=1.5))
}
axis_fun(1,line=-0.5,xmaj,xmin,xmaj);axis_fun(2,ymaj,ymin,NA);box(lwd=1)
mtext(side=1,line=1.75,"DIN (mM)",cex=0.8)

plot(0:1,0:1,axes=F,type="n",ylab=NA,xlab=NA)
legend.text=paste0("Reach ",1:6)#FLDNUM.pool.xwalk$Pool
legend(0.5,0.5,legend=legend.text,pch=21,col=adjustcolor("black",0.5),lwd=0.2,lty=NA,pt.bg=adjustcolor(cols,0.75),pt.cex=1.5,ncol=1,cex=0.8,bty="n",y.intersp=1.5,x.intersp=0.5,xpd=NA,xjust=0.5,yjust=0.5,title="River Reach")

#legend.text=c("Main Channel","Side Channel","Connected\nBackwater", "Impoundment","Riverine Lake/\nIsolated Backwater")
#legend(0.5,0.25,legend=legend.text,pch=21:25,col=adjustcolor("black",0.5),lwd=0.2,lty=NA,pt.bg="grey",pt.cex=1,ncol=1,cex=0.75,bty="n",y.intersp=1.25,x.intersp=0.5,xpd=NA,xjust=0.5,yjust=0.5,title="Aquatic Area")

dev.off()





