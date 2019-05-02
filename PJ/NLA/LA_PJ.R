## 
## Woodstoich working group
## USEPA NARS National Lake Assessment data
## 
## Code was compiled by Paul Julian
## contact infor: pjulian@ufl.edu

#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

#Libraries
library(AnalystHelper);#devtools::install_github("SwampThingPaul/AnalystHelper")
library(plyr)
library(reshape)
library(zoo)
library(smatr)

library(tmap)
library(rgdal)

#Paths
setwd("D:/UF/WoodStoich19/Si_WG")
paths=paste0(getwd(),c("/Exports/","/Plots/","/Data/NARS/","/Data/NARS/GIS_NLA"))
#Folder.Maker(paths);#One and done. Creates folders in working directory.
export.path=paths[1]
plot.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]

##Helper functions
N.mw=14.0067
P.mw=30.973762
C.mw=12.0107
Si.mw=28.0855

#https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys
## GIS data
tmap_mode("view")
NLA.basins=readOGR(GIS.path,"National_Basin_withMetrics_20120920")
#eco=readOGR(GIS.path,"NA_CEC_Eco_Level2")

samples2007=read.csv(paste0(data.path,"NLA2007/NLA2007_SampledLakeInformation_20091113.csv"))
nla2007.sites=SpatialPointsDataFrame(coords=samples2007[,c("LON_DD","LAT_DD")],dat=samples2007,proj4string = crs(NLA.basins))
samples2012=read.csv(paste0(data.path,"NLA2007/nla2012_wide_siteinfo_08232016.csv"))#read.csv("https://www.epa.gov/sites/production/files/2016-12/nla2012_wide_siteinfo_08232016.csv")
nla2012.sites=SpatialPointsDataFrame(coords=samples2012[,c("LON_DD83","LAT_DD83")],dat=samples2012,proj4string = crs(NLA.basins))


tm_basemap(leaflet::providers$Esri.WorldImagery,alpha=0.9)+
  #tm_shape(NLA.basins,is.master = T)+tm_polygons(col="indianred1",alpha=0.5)+
  tm_shape(nla2007.sites)+tm_dots(col="indianred1")+
  tm_shape(nla2012.sites)+tm_dots(col="dodgerblue1")

names(samples2007)
unique(samples2007$WSA_ECO3)
unique(samples2007$WSA_ECO9)

samples2007=samples2007[,c("SITE_ID","WSA_ECO9")]
colnames(samples2007)=c("SITE_ID","ECO9")

names(samples2012)
unique(samples2012$LAKE_ORIGIN)
unique(samples2012$LAKE_ORIGIN12)
unique(samples2012$FW_ECO3)
unique(samples2012$FW_ECO9)

samples2012=samples2012[,c("SITE_ID","FW_ECO9")]
colnames(samples2012)=c("SITE_ID","ECO9")

WSA_regions=data.frame(ECO9=c("CPL","NAP", "NPL", "SAP", "SPL", "TPL", "UMW", "WMT", "XER"),
           Eco_Name_full=c("Coastal Plains","Northern Appalachians","Northern Plains",
                       "Southern Appalachians","Southern Plains","Temperate Plains",
                       "Upper Midwest","Western Mountains","Xeric"))

nla2007.sites.CPL=subset(nla2007.sites,WSA_ECO9=="CPL")
nla2012.sites.CPL=subset(nla2012.sites,FW_ECO9=="CPL")

## Water Quality
NLA2007=read.csv(paste0(data.path,"NLA2007/NLA2007_WaterQuality_20091123.csv"))
NLA2007$DATE_COL=date.fun(NLA2007$DATE_COL,form="%m/%d/%Y")
names(NLA2007)

NLA2012=read.csv(paste0(data.path,"NLA2012/nla2012_waterchem_wide.csv"))
NLA2012$DATE_COL=date.fun(NLA2012$DATE_COL,form="%m/%d/%Y")
names(NLA2012)


vars=c("SITE_ID","DATE_COL","LAKE_SAMP","VISIT_NO","SAMPLE_DEPTH","TOC","DOC","NTL_PPM","PTL","SIO2","CHLA")
NLA2007=NLA2007[,vars]

NLA2007=merge(NLA2007,samples2007,"SITE_ID",all.x=T)
#NLA2007=subset(NLA2007,ECO9=="TPL")

NLA2007[NLA2007$PTL==0,]=NA

NLA2007$TOC.mM=NLA2007$TOC/C.mw
NLA2007$DOC.mM=NLA2007$DOC/C.mw
NLA2007$TN.mM=NLA2007$NTL_PPM/N.mw
NLA2007$TP.mM=(NLA2007$PTL*0.001)/P.mw
NLA2007$Si.mM=NLA2007$SIO2/Si.mw

summary(NLA2007)

NLA2007$log.TOC.mM=log(NLA2007$TOC.mM)
NLA2007$log.DOC.mM=log(NLA2007$DOC.mM)
NLA2007$log.TN.mM=log(NLA2007$TN.mM)
NLA2007$log.TP.mM=log(NLA2007$TP.mM)
NLA2007$log.Si.mM=log(NLA2007$Si.mM)


sma.rslt=data.frame()

for(i in 1:length(WSA_regions$ECO9)){
tmp=subset(NLA2007,ECO9==WSA_regions$ECO9[i])
tiff(filename=paste0(plot.path,WSA_regions$ECO9[i],"_NLA2007_sma.tiff"),width=6,height=5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",oma=c(2.5,3,1,0.25),mar=c(1.5,2,0.5,1.25))
layout(matrix(1:9,3,3,byrow=T))
y.lab.line=3.5

ylim.val=range(tmp$DOC.mM,na.rm=T);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
xlim.val=range(tmp$Si.mM,na.rm=T);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor")
plot(DOC.mM~Si.mM,tmp,log="xy",ylim=ylim.val,xlim=xlim.val,yaxt="n",xaxt="n",ylab=NA,xlab=NA,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(tmp,points(Si.mM,DOC.mM,pch=21,bg=adjustcolor("dodgerblue1",0.5),col=adjustcolor("grey",0.5),lwd=0.2,cex=1.25))
mod=sma(log.DOC.mM~log.Si.mM,tmp,method="SMA",slope.test=1)
tmp.rslt=data.frame(ECO=WSA_regions$ECO9[i],Compare_xy="Si x DOC",R2=as.numeric(mod$r2),Slope=round(mod$slopetest[[1]]$b,3),Pval=mod$slopetest[[1]]$p)
sma.rslt=rbind(sma.rslt,tmp.rslt)
y.mod=(coef(mod)[2]*log(xmin))+coef(mod)[1]
lines(xmin,exp(y.mod),lty=2,col="indianred1",lwd=2)
axis_fun(2,ymaj,ymin,ymaj);axis_fun(1,xmaj,xmin,xmaj);box(lwd=1)
mtext(side=2,line=y.lab.line,"DOC (mM)")

plot(0:1,0:1,axes=F,type="n",ylab=NA,xlab=NA)
mtext(side=3,WSA_regions$Eco_Name_full[i])
plot(0:1,0:1,axes=F,type="n",ylab=NA,xlab=NA)

ylim.val=range(tmp$TN.mM,na.rm=T);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
xlim.val=range(tmp$Si.mM,na.rm=T);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor")
plot(TN.mM~Si.mM,tmp,log="xy",ylim=ylim.val,xlim=xlim.val,yaxt="n",xaxt="n",ylab=NA,xlab=NA,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(tmp,points(Si.mM,TN.mM,pch=21,bg=adjustcolor("dodgerblue1",0.5),col=adjustcolor("grey",0.5),lwd=0.2,cex=1.25))
mod=sma(log.TN.mM~log.Si.mM,tmp,method="SMA",slope.test=1)
tmp.rslt=data.frame(ECO=WSA_regions$ECO9[i],Compare_xy="Si x TN",R2=as.numeric(mod$r2),Slope=round(mod$slopetest[[1]]$b,3),Pval=mod$slopetest[[1]]$p)
sma.rslt=rbind(sma.rslt,tmp.rslt)
y.mod=(coef(mod)[2]*log(xmin))+coef(mod)[1]
lines(xmin,exp(y.mod),lty=2,col="indianred1",lwd=2)
axis_fun(2,ymaj,ymin,ymaj);axis_fun(1,xmaj,xmin,xmaj);box(lwd=1)
mtext(side=2,line=y.lab.line,"TN (mM)")

ylim.val=range(tmp$TN.mM,na.rm=T);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
xlim.val=range(tmp$DOC.mM,na.rm=T);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor")
plot(TN.mM~DOC.mM,tmp,log="xy",ylim=ylim.val,xlim=xlim.val,yaxt="n",xaxt="n",ylab=NA,xlab=NA,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(tmp,points(DOC.mM,TN.mM,pch=21,bg=adjustcolor("dodgerblue1",0.5),col=adjustcolor("grey",0.5),lwd=0.2,cex=1.25))
mod=sma(log.TN.mM~log.DOC.mM,tmp,method="SMA",slope.test=1)
tmp.rslt=data.frame(ECO=WSA_regions$ECO9[i],Compare_xy="DOC x TN",R2=as.numeric(mod$r2),Slope=round(mod$slopetest[[1]]$b,3),Pval=mod$slopetest[[1]]$p)
sma.rslt=rbind(sma.rslt,tmp.rslt)
y.mod=(coef(mod)[2]*log(xmin))+coef(mod)[1]
lines(xmin,exp(y.mod),lty=2,col="indianred1",lwd=2)
axis_fun(2,ymaj,ymin,NA);axis_fun(1,xmaj,xmin,xmaj);box(lwd=1)
#mtext(side=2,line=y.lab.line,"TN (mM)")
#mtext(side=1,line=2,"DOC (mM)")

plot(0:1,0:1,axes=F,type="n",ylab=NA,xlab=NA)

ylim.val=range(tmp$TP.mM,na.rm=T);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
xlim.val=range(tmp$Si.mM,na.rm=T);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor")
plot(TP.mM~Si.mM,tmp,log="xy",ylim=ylim.val,xlim=xlim.val,yaxt="n",xaxt="n",ylab=NA,xlab=NA,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(tmp,points(Si.mM,TP.mM,pch=21,bg=adjustcolor("dodgerblue1",0.5),col=adjustcolor("grey",0.5),lwd=0.2,cex=1.25))
mod=sma(log.TP.mM~log.Si.mM,tmp,method="SMA",slope.test=1)
tmp.rslt=data.frame(ECO=WSA_regions$ECO9[i],Compare_xy="Si x TP",R2=as.numeric(mod$r2),Slope=round(mod$slopetest[[1]]$b,3),Pval=mod$slopetest[[1]]$p)
sma.rslt=rbind(sma.rslt,tmp.rslt)
y.mod=(coef(mod)[2]*log(xmin))+coef(mod)[1]
lines(xmin,exp(y.mod),lty=2,col="indianred1",lwd=2)
axis_fun(2,ymaj,ymin,formatC(ymaj,format="fg"));axis_fun(1,xmaj,xmin,xmaj);box(lwd=1)
mtext(side=2,line=y.lab.line,"TP (mM)")
mtext(side=1,line=2,"Si (mM)")

ylim.val=range(tmp$TP.mM,na.rm=T);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
xlim.val=range(tmp$DOC.mM,na.rm=T);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor")
plot(TP.mM~DOC.mM,tmp,log="xy",ylim=ylim.val,xlim=xlim.val,yaxt="n",xaxt="n",ylab=NA,xlab=NA,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(tmp,points(DOC.mM,TP.mM,pch=21,bg=adjustcolor("dodgerblue1",0.5),col=adjustcolor("grey",0.5),lwd=0.2,cex=1.25))
mod=sma(log.TP.mM~log.DOC.mM,tmp,method="SMA",slope.test=1)
tmp.rslt=data.frame(ECO=WSA_regions$ECO9[i],Compare_xy="DOC x TP",R2=as.numeric(mod$r2),Slope=round(mod$slopetest[[1]]$b,3),Pval=mod$slopetest[[1]]$p)
sma.rslt=rbind(sma.rslt,tmp.rslt)
y.mod=(coef(mod)[2]*log(xmin))+coef(mod)[1]
lines(xmin,exp(y.mod),lty=2,col="indianred1",lwd=2)
axis_fun(2,ymaj,ymin,NA);axis_fun(1,xmaj,xmin,xmaj);box(lwd=1)
#mtext(side=2,line=y.lab.line,"TP (mM)")
mtext(side=1,line=2,"DOC (mM)")

ylim.val=range(tmp$TP.mM,na.rm=T);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
xlim.val=range(tmp$TN.mM,na.rm=T);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor")
plot(TP.mM~TN.mM,tmp,log="xy",ylim=ylim.val,xlim=xlim.val,yaxt="n",xaxt="n",ylab=NA,xlab=NA,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(tmp,points(TN.mM,TP.mM,pch=21,bg=adjustcolor("dodgerblue1",0.5),col=adjustcolor("grey",0.5),lwd=0.2,cex=1.25))
mod=sma(log.TP.mM~log.TN.mM,tmp,method="SMA",slope.test=1)
tmp.rslt=data.frame(ECO=WSA_regions$ECO9[i],Compare_xy="TN x TP",R2=as.numeric(mod$r2),Slope=round(mod$slopetest[[1]]$b,3),Pval=mod$slopetest[[1]]$p)
sma.rslt=rbind(sma.rslt,tmp.rslt)
y.mod=(coef(mod)[2]*log(xmin))+coef(mod)[1]
lines(xmin,exp(y.mod),lty=2,col="indianred1",lwd=2)
axis_fun(2,ymaj,ymin,NA);axis_fun(1,xmaj,xmin,xmaj);box(lwd=1)
#mtext(side=2,line=y.lab.line,"TP (mM)")
mtext(side=1,line=2,"TN (mM)")
dev.off()
}