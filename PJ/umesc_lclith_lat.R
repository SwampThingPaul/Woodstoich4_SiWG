## 
## Woodstoich 4 (Si Working grop)
## Upper Mississippi River Restoration LTRM
## https://umesc.usgs.gov/data_library/water_quality/water_quality_page.html
## https://www.umesc.usgs.gov/cgi-bin/ltrmp/water/water_meta.pl
##
## Tributary Landuse and Lithology longitudinal gradient analysis
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
library(openxlsx)
library(gvlma)

#GIS Libraries
library(sp)
library(rgdal)
library(rgeos)
library(tmap)
library(mapmisc);#for map scale bar and north arrow base plotting.

#Paths
wd="D:/UF/WoodStoich19/Si_WG"

paths=paste0(wd,c("/Exports/","/Plots/","/Data/Miss_UpperTrib_dataset/RAW_USGS_TribData/"))
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
dat=read.xlsx(paste0(data.path,"SummaryTables_Trib_7.18.19.xlsx"),sheet=1)

#convert to shapefile
dat.sp=SpatialPointsDataFrame(coords=dat[,c("EASTING","NORTHING")],data=dat,proj4string = utm15)
plot(dat.sp)
dat.sp=spTransform(dat.sp,wgs84)
dat.sp$Lat=coordinates(dat.sp)[,2]

library(tmap)
tmap_mode("view")
tm_shape(dat.sp)+tm_dots()

dat2=dat
dat2$Long=coordinates(dat.sp)[,1]
dat2$Lat=coordinates(dat.sp)[,2]

### Basic Latitudinal correlations analysis
#lith
dat.lith=melt(dat2[,c("LOCATCD","Lat","Dolostone", "Limestone","Shale", "Sandstone", "Other")],id.vars = c("LOCATCD","Lat"))
lith.vals=c("Dolostone", "Limestone","Shale", "Sandstone", "Other")

par(family="serif",oma=c(0.5,2.5,1,0.75),mar=c(3,1.5,0.25,0.5))
layout(matrix(1:4,4,1))
ylim.val=c(0,100)
xlim.val=c(37.2,44.6)

for(i in 1:4){
  plot(value~Lat,subset(dat.lith,variable==lith.vals[i]),ylim=ylim.val,xlim=xlim.val)
  abline(lm(value~Lat,subset(dat.lith,variable==lith.vals[i])))
  mtext(side=3,lith.vals[i])
}

# Linear Model Version
lm.dol=lm(Dolostone~Lat,dat2)
gvlma(lm.dol)
lm.lim=lm(Limestone~Lat,dat2)
gvlma(lm.lim)
lm.shale=lm(Shale~Lat,dat2)
gvlma(lm.shale)
lm.sand=lm(Sandstone~Lat,dat2)
gvlma(lm.sand)

# Spearman's Correlation
lith.cor.rslt=ddply(dat.lith,c("variable"),summarise,rho=cor.test(value,Lat,method="spearman")$estimate,pval=cor.test(value,Lat,method="spearman")$p.value)

lith.cor.rslt$rho=round(lith.cor.rslt$rho,2)
lith.cor.rslt$pval=with(lith.cor.rslt,ifelse(pval<0.05,"<0.05",round(lith.cor.rslt$pval,2)))

library(kableExtra)
kable(lith.cor.rslt,col.names = c("Lithology","r\u209B","\u03C1-value"),
      align=c("l","c","c"),row.names = F)%>%
  kable_styling(bootstrap_options = "striped", full_width = F)


ylim.val=c(0,105);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(37.1,45.1);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

#tiff(filename=paste0(plot.path,"Lith_Lat.tiff"),width=6,height=4,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",oma=c(1,2.25,1,0.75),mar=c(3,2,0.25,0.25))
layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.5))

cols=colorRampPalette(c("grey","grey30"))(4)
plot(value~Lat,dat.lith,ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ylab=NA,xlab=NA)
abline(h=ymaj,v=xmaj,lty=3,col="grey80")

for(i in 1:4){
tmp.dat=subset(dat.lith,variable==lith.vals[i])
mod=mblm::mblm(value~Lat,tmp.dat)
x.val=seq(min(tmp.dat$Lat),max(tmp.dat$Lat),length.out = 50)
mod.pred=data.frame(Lat=x.val,predict.lm(mod,data.frame(Lat=x.val),interval="confidence"))

with(tmp.dat,points(Lat,value,pch=20+i,bg=cols[i],lwd=0.01))
#with(mod.pred,shaded.range(Lat,lwr,upr,cols[i]))
with(mod.pred,lines(Lat,fit,col=cols[i],lwd=3))
}
axis_fun(1,xmaj,xmin,xmaj)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=2,"Latitude (Decimal Degrees)")
mtext(side=2,line=2,"Percent Cover")

plot(0:1,0:1,axes=F,type="n",ylab=NA,xlab=NA)
legend.text=lith.vals[1:4]
legend(0.5,0.5,legend=legend.text,pch=NA,col=cols,lwd=3,lty=1,pt.bg=cols,pt.cex=1.5,ncol=1,cex=0.8,bty="n",y.intersp=1.75,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,text.col="white",title="Lithology")
legend(0.5,0.5,legend=legend.text,pch=21:24,col=c("black"),lwd=0.01,lty=NA,pt.bg=cols,pt.cex=1.5,ncol=1,cex=0.8,bty="n",y.intersp=1.75,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,title="Lithology")
dev.off()


#Land cover
LC.vals=c("openwater", "Devl", "Decid", "MixedEvergreen", "PastureHay", "CultCrops", "Wetlands")
dat.lc=melt(dat2[,c("LOCATCD","Lat",LC.vals)],id.vars = c("LOCATCD","Lat"))

# Spearman's Correlation
lc.cor.rslt=ddply(dat.lc,c("variable"),summarise,rho=cor.test(value,Lat,method="spearman")$estimate,pval=cor.test(value,Lat,method="spearman")$p.value)
lc.cor.rslt$rho=round(lc.cor.rslt$rho,2)
lc.cor.rslt$pval=with(lc.cor.rslt,ifelse(pval<0.05,"<0.05",round(lc.cor.rslt$pval,2)))
lc.cor.rslt

kable(lc.cor.rslt,col.names = c("Land Cover Class","r\u209B","\u03C1-value"),
      align=c("l","c","c"),row.names = F)%>%
  kable_styling(bootstrap_options = "striped", full_width = F)

#tiff(filename=paste0(plot.path,"LC_Lat.tiff"),width=6,height=4,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",oma=c(1,2.25,1,0.75),mar=c(3,2,0.25,0.25))
layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.5))

cols=colorRampPalette(c("grey","grey30"))(7)
plot(value~Lat,dat.lc,ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ylab=NA,xlab=NA)
abline(h=ymaj,v=xmaj,lty=3,col="grey80")

for(i in 1:length(LC.vals)){
  tmp.dat=subset(dat.lc,variable==LC.vals[i])
  mod=mblm::mblm(value~Lat,tmp.dat)
  x.val=seq(min(tmp.dat$Lat),max(tmp.dat$Lat),length.out = 50)
  mod.pred=data.frame(Lat=x.val,predict.lm(mod,data.frame(Lat=x.val),interval="confidence"))
  
  with(tmp.dat,points(Lat,value,pch=c(21:24,21:24)[i],bg=cols[i],lwd=0.01))
  #with(mod.pred,shaded.range(Lat,lwr,upr,cols[i]))
  with(mod.pred,lines(Lat,fit,col=cols[i],lwd=3))
}
axis_fun(1,xmaj,xmin,xmaj)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=2,"Latitude (Decimal Degrees)")
mtext(side=2,line=2,"Percent Cover")

plot(0:1,0:1,axes=F,type="n",ylab=NA,xlab=NA)
legend.text=LC.vals
legend(0.5,0.5,legend=legend.text,pch=NA,col=cols,lwd=3,lty=1,pt.bg=cols,pt.cex=1.5,ncol=1,cex=0.8,bty="n",y.intersp=1.75,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,text.col="white",title="Land Cover")
legend(0.5,0.5,legend=legend.text,pch=c(21:24,21:24),col=c("black"),lwd=0.01,lty=NA,pt.bg=cols,pt.cex=1.5,ncol=1,cex=0.8,bty="n",y.intersp=1.75,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,title="Land Cover")
dev.off()


ylim.val=c(0,105);by.y=25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(37.1,45.1);by.x=2;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)

#tiff(filename=paste0(plot.path,"LithLC__Lat.tiff"),width=6,height=5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",oma=c(2,2.25,1,0.75),mar=c(1.75,2,0.25,0.25))
layout(matrix(1:4,2,2,byrow=T),widths=c(1,0.4))

cols=colorRampPalette(c("grey","grey30"))(4)
plot(value~Lat,dat.lith,ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ylab=NA,xlab=NA)
abline(h=ymaj,v=xmaj,lty=3,col="grey80")

for(i in 1:4){
  tmp.dat=subset(dat.lith,variable==lith.vals[i])
  mod=mblm::mblm(value~Lat,tmp.dat)
  x.val=seq(min(tmp.dat$Lat),max(tmp.dat$Lat),length.out = 50)
  mod.pred=data.frame(Lat=x.val,predict.lm(mod,data.frame(Lat=x.val),interval="confidence"))
  
  with(tmp.dat,points(Lat,value,pch=20+i,bg=cols[i],lwd=0.01))
  #with(mod.pred,shaded.range(Lat,lwr,upr,cols[i]))
  with(mod.pred,lines(Lat,fit,col=cols[i],lwd=3))
}
axis_fun(1,xmaj,xmin,xmaj)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
#mtext(side=1,line=2,"Latitude (Decimal Degrees)")
mtext(side=2,line=2,"Percent Cover")

plot(0:1,0:1,axes=F,type="n",ylab=NA,xlab=NA)
legend.text=lith.vals[1:4]
legend(0.5,0.5,legend=legend.text,pch=NA,col=cols,lwd=3,lty=1,pt.bg=cols,pt.cex=1.5,ncol=1,cex=0.8,bty="n",y.intersp=1.75,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,text.col="white",title="Lithology")
legend(0.5,0.5,legend=legend.text,pch=21:24,col=c("black"),lwd=0.01,lty=NA,pt.bg=cols,pt.cex=1.5,ncol=1,cex=0.8,bty="n",y.intersp=1.75,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,title="Lithology")

cols=colorRampPalette(c("grey","grey30"))(7)
plot(value~Lat,dat.lc,ylim=ylim.val,xlim=xlim.val,type="n",axes=F,ylab=NA,xlab=NA)
abline(h=ymaj,v=xmaj,lty=3,col="grey80")

for(i in 1:length(LC.vals)){
  tmp.dat=subset(dat.lc,variable==LC.vals[i])
  mod=mblm::mblm(value~Lat,tmp.dat)
  x.val=seq(min(tmp.dat$Lat),max(tmp.dat$Lat),length.out = 50)
  mod.pred=data.frame(Lat=x.val,predict.lm(mod,data.frame(Lat=x.val),interval="confidence"))
  
  with(tmp.dat,points(Lat,value,pch=c(21:24,21:24)[i],bg=cols[i],lwd=0.01))
  #with(mod.pred,shaded.range(Lat,lwr,upr,cols[i]))
  with(mod.pred,lines(Lat,fit,col=cols[i],lwd=3))
}
axis_fun(1,xmaj,xmin,xmaj)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=1,line=2,"Latitude (Decimal Degrees)")
mtext(side=2,line=2,"Percent Cover")

plot(0:1,0:1,axes=F,type="n",ylab=NA,xlab=NA)
legend.text=c("Open Water", "Development", "Deciduous Forest", "Mixed Evergreen Forest", "Pasture/Hay", "Cultivated Crops", "Wetlands")
legend(0.5,0.5,legend=legend.text,pch=NA,col=cols,lwd=3,lty=1,pt.bg=cols,pt.cex=1.5,ncol=1,cex=0.8,bty="n",y.intersp=1.75,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,text.col="white",title="Land Cover")
legend(0.5,0.5,legend=legend.text,pch=c(21:24,21:24),col=c("black"),lwd=0.01,lty=NA,pt.bg=cols,pt.cex=1.5,ncol=1,cex=0.8,bty="n",y.intersp=1.75,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,title="Land Cover")
dev.off()