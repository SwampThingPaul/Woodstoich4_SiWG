## 
## Woodstoich 4 (Si Working grop)
## Upper Mississippi River Restoration LTRM
## https://umesc.usgs.gov/data_library/water_quality/water_quality_page.html
## https://www.umesc.usgs.gov/cgi-bin/ltrmp/water/water_meta.pl
##
## SMA regression of fixed sites (Trib and Main) 
## Si x TP and Si x TN 
##
## Code was compiled by Paul Julian
## contact info: pjulian@ufl.edu

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
sum.dat.tab=openxlsx::read.xlsx(paste0(data.path,"SummaryTables_Trib_7.18.19.xlsx"),sheet=1)
stratum.lookup=data.frame(STRATUM=1:9,STRATUM_Descript=c("Main Channel","Side Channel","Connected Backwater", "Riverine Lake", "Impoundment","Isolated Backwater","New Terrestrial",NA,"Unexploded Ordinance (Pool 13)"))
FLDNUM.pool.xwalk=data.frame(FLDNUM=1:6,Pool=c("Pool 4","Pool 8","Pool 13","Pool 26","Open River","La Grange"))

# Fixed Station -----------------------------------------------------------
trib.dat=read.csv(paste0(data.path,"UMR_Tribs_1988-2018.csv"))
trib.dat$DATETIME=date.fun(paste0(as.character(trib.dat$DATE)," ",as.character(trib.dat$TIME),":00"),tz="America/Chicago",form="%m/%d/%Y %H:%M:%S")
trib.dat$DATE=date.fun(as.character(trib.dat$DATE),tz="America/Chicago",form="%m/%d/%Y")
trib.dat$dataset="trib"
#head(trib.dat)
#unique(trib.dat$LOCATCD)
#ddply(trib.dat,"LOCATCD",summarise,N.val=N(LOCATCD))

mainstem.dat=read.csv(paste0(data.path,"UMR_Mainstem_1991-2018.csv"))
mainstem.dat$DATETIME=date.fun(paste0(as.character(mainstem.dat$DATE)," ",as.character(mainstem.dat$TIME),":00"),tz="America/Chicago",form="%m/%d/%Y %H:%M:%S")
mainstem.dat$DATE=date.fun(as.character(mainstem.dat$DATE),tz="America/Chicago",form="%m/%d/%Y")
mainstem.dat$dataset="main"
#head(mainstem.dat)
#unique(mainstem.dat$LOCATCD)
#ddply(mainstem.dat,"LOCATCD",summarise,N.val=N(LOCATCD))

dat=rbind(trib.dat,mainstem.dat)

#Spatial
sp.dat=ddply(dat,c("LOCATCD","FLDNUM","dataset"),summarise,EASTING=mean(EASTING,na.rm=T),NORTHING=mean(NORTHING,na.rm=T),N.val=N(LOCATCD),min.date=min(DATE),max.date=max(DATE),N.SI=N(SI))
sp.dat$yrs.dat=with(sp.dat,as.numeric(max.date-min.date))*3.17098e-8
sp.dat.shp=SpatialPointsDataFrame(coords=sp.dat[,c("EASTING","NORTHING")], data=sp.dat,proj4string=utm15)
sp.dat.shp.wgs=spTransform(sp.dat.shp,wgs84)


# https://www.umesc.usgs.gov/ltrmp/water/qa_flag_primer.pdf
sp.dat.sub=subset(sp.dat,yrs.dat>20)
#illinois.tribs=c("LM07.2M","LM00.5M","SG16.2C","S000.2K","MK04.4M")
#illinois.river=c("I007.0W", "I080.2C", "I080.2M", "I121.2W", "I157.8D")
#sp.dat.sub=subset(sp.dat.sub,!LOCATCD%in%c(illinois.tribs,illinois.river))
site.sub=sp.dat.sub[,c("LOCATCD","FLDNUM","dataset","yrs.dat")]

dput(names(dat))
dat=rename(dat,c("VSS_QF"="VSSQF","NA."="Na","NAQF"="NaQF"))
names(dat)

idvars=c("FLDNUM","LOCATCD","DATE","DATETIME","dataset")
field.params=c("TEMP","DO","PH","TURB","COND","VEL")
lab.params=c("SRP","TP","NOX","NHX","TN","SI","VSS","CL","SO4","CHLcal")

dat$CHLcalQF=dat$CHLFQF
dat=subset(dat,LOCATCD%in%site.sub$LOCATCD)

dat2=data.frame()
#stack field parameters
for(i in 1:length(field.params)){
  tmp=dat[,c(idvars,field.params[i],paste0(field.params[i],"QF"))]
  colnames(tmp)=c(idvars,"data.value","flag")
  tmp$parameter=field.params[i]
  tmp=tmp[,c(idvars,"parameter","data.value","flag")]
  tmp$fatal.flag=with(tmp,ifelse(flag=="","N","Y"))
  dat2=rbind(dat2,tmp)
  print(i)
}
dat2$data.type="Field"

for(i in 1:length(lab.params)){
  tmp=dat[,c(idvars,lab.params[i],paste0(lab.params[i],"QF"))]
  colnames(tmp)=c(idvars,"data.value","flag")
  tmp$data.type="lab"
  tmp$parameter=lab.params[i]
  tmp=tmp[,c(idvars,"parameter","data.value","flag","data.type")]
  tmp$flag=as.factor(tmp$flag)
  tmp$fatal.flag=with(tmp,ifelse(flag==0|is.na(flag)==T,"N","Y"))
  dat2=rbind(dat2,tmp)
  print(i)
}
dat2$data.type="Lab"
dat2=subset(dat2,is.na(data.value)==F)

ddply(dat2,"parameter",summarise,min.val=min(data.value))
dat2$halfMDL=with(dat2,ifelse(data.value<0,abs(data.value)/2,data.value))

dat.xtab.fix=cast(subset(dat2,fatal.flag=="N"),FLDNUM+LOCATCD+dataset+DATE~parameter,value="halfMDL",mean)
dat.xtab.fix$DIN=with(dat.xtab.fix,NHX+NOX)
dat.xtab.fix$month=as.numeric(format(dat.xtab.fix$DATE,"%m"))
dat.xtab.fix$CY=as.numeric(format(dat.xtab.fix$DATE,"%Y"))
dat.xtab.fix$WY=WY(dat.xtab.fix$DATE,"Fed")

# Reversal Evaluation
dat.xtab.fix$TPReversal=with(dat.xtab.fix,ifelse(is.na(SRP)==T|is.na(TP)==T,0,ifelse(SRP>(TP*1.3),1,0)));# Reversals identified as 1 reversals consistent with TP rule evaluation
dat.xtab.fix$TNReversal=with(dat.xtab.fix,ifelse(is.na(DIN)==T|is.na(TN)==T,0,ifelse(DIN>(TN*1.3),1,0)));

sum(dat.xtab.fix$TNReversal,na.rm=T)
sum(dat.xtab.fix$TPReversal,na.rm=T)

par(family="serif",oma=c(1,1,1,1),mar=c(4,4,1,1))
layout(matrix(1:2,1,2,byrow=F))
plot(TN~DIN,dat.xtab.fix,ylab="TN (mg/L)",xlab="DIN (mg/L)",pch=21,bg=ifelse(TNReversal==1,"dodgerblue1",NA),col=adjustcolor("grey",0.8));abline(0,1,col="dodgerblue1")
plot(TP~SRP,dat.xtab.fix,ylab="TP (mg/L)",xlab="SRP (mg/L)",pch=21,bg=ifelse(TPReversal==1,"red",NA),col=adjustcolor("grey",0.8));abline(0,1,col="red")

dev.off();#clears plots

##
dat.xtab.fix$TP=with(dat.xtab.fix,ifelse(TPReversal==1,NA,TP))
dat.xtab.fix$TN=with(dat.xtab.fix,ifelse(TNReversal==1,NA,TN))
dat.xtab.fix$SRP=with(dat.xtab.fix,ifelse(TPReversal==1,NA,SRP))
dat.xtab.fix$DIN=with(dat.xtab.fix,ifelse(TNReversal==1,NA,DIN))
dat.xtab.fix$TP.ugL=dat.xtab.fix$TP*1000

dat.xtab.fix$Si.mM=with(dat.xtab.fix, SI/Si.mw)
dat.xtab.fix$TP.mM=with(dat.xtab.fix,TP/P.mw)
dat.xtab.fix$TN.mM=with(dat.xtab.fix,TN/N.mw)
dat.xtab.fix$SRP.mM=with(dat.xtab.fix,SRP/P.mw)
dat.xtab.fix$DIN.mM=with(dat.xtab.fix,DIN/N.mw)
dat.xtab.fix$SiTP=with(dat.xtab.fix,Si.mM/TP.mM)
dat.xtab.fix$SiTN=with(dat.xtab.fix,Si.mM/TN.mM)
dat.xtab.fix$SiSRP=with(dat.xtab.fix,Si.mM/SRP.mM)
dat.xtab.fix$SiDIN=with(dat.xtab.fix,Si.mM/DIN.mM)
dat.xtab.fix$TNTP=with(dat.xtab.fix,TN.mM/TP.mM)
dat.xtab.fix$DINSRP=with(dat.xtab.fix,DIN.mM/SRP.mM)

dat.xtab.fix$decade=((dat.xtab.fix$CY)%/%10)*10
dat.xtab.fix$month=as.numeric(format(dat.xtab.fix$DATE,"%m"))
dat.xtab.fix=merge(dat.xtab.fix,data.frame(month=1:12,season=c(rep("Winter",3),rep("Spring",3),rep("Summer",3),rep("Fall",3))))

dat.xtab.fix=subset(dat.xtab.fix,CY%in%seq(2010,2018,1))

idvars=c("LOCATCD","dataset","FLDNUM","CY","decade","season")
vars=c("TP","TP.mM","TN","TN.mM","SI","Si.mM")
dat.xtab.fix.melt=melt(data.frame(dat.xtab.fix[,c(idvars,vars)]),id.vars=idvars,variable_name="parameter")
dat.xtab.fix.melt=subset(dat.xtab.fix.melt,is.na(value)==F)

dat.xtab.fix.ann=cast(dat.xtab.fix.melt,LOCATCD+FLDNUM+dataset+CY~parameter,value="value",mean)
dat.xtab.fix.ann$log.Si.mM=log(dat.xtab.fix.ann$Si.mM)
dat.xtab.fix.ann$log.TP.mM=log(dat.xtab.fix.ann$TP.mM)
dat.xtab.fix.ann$log.TN.mM=log(dat.xtab.fix.ann$TN.mM)

unique(subset(dat.xtab.fix.ann,dataset=="trib")$LOCATCD)
unique(subset(dat.xtab.fix.ann,dataset=="main")$LOCATCD)

dat.xtab.fix.ann=merge(dat.xtab.fix.ann,sp.dat[,c("LOCATCD","NORTHING")],"LOCATCD",all.x=T)
dat.xtab.fix.ann=dat.xtab.fix.ann[order(dat.xtab.fix.ann$NORTHING),]


sites.list=ddply(dat.xtab.fix.ann,c("LOCATCD","FLDNUM","dataset","NORTHING"),summarise,N.val=N(Si.mM))
sites.list=sites.list[order(sites.list$FLDNUM,sites.list$NORTHING),]
sites.list=subset(sites.list,dataset=="trib")
sites.list=subset(sites.list,LOCATCD%in%sum.dat.tab$LOCATCD)

cols.rmp=colorRampPalette(c("darkseagreen3","goldenrod3","indianred3"))

FLDNUM.pool.xwalk

#tiff(filename=paste0(plot.path,"LTRM_trib_UMR_sma_CY.tiff"),width=6.5,height=3.75,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
#png(filename=paste0(plot.path,"LTRM_trib_UMR_sma_CY.png"),width=6.5,height=3.75,units="in",res=200,type="windows",bg="white")
par(family="serif",oma=c(0.25,2.25,1,0.75),mar=c(2.1,1.25,1,0.25))
layout(matrix(c(1:12),2,6,byrow=T),heights=c(1,1))

sma.rslts.trib.SiTP.SRS=data.frame()
sma.rslts.trib.SiTN.SRS=data.frame()
for(j in 1:6){
  tmp=subset(dat.xtab.fix.ann,FLDNUM==j)
  tmp.site.list=subset(sites.list,FLDNUM==j)
  
  ylim.val=c(0.04,0.3);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
  xlim.val=c(0.001,0.05);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor")   
  
  plot(Si.mM~TP.mM,tmp,log="xy",ylim=ylim.val,xlim=xlim.val,type="n",yaxt="n",xaxt="n",ylab=NA,xlab=NA)
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  axis_fun(1,line=-0.75,xmaj,xmin,format(xmaj,scientific=F),0.8);
  if(j==1){axis_fun(2,ymaj,ymin,ymaj,cex=0.8)}else{axis_fun(2,ymaj,ymin,NA)};box(lwd=1)
  lines(x=c(0.0001,0.1),y=(c(0.0001,0.1))*20,lty=2,lwd=1.25)
  
  cols=cols.rmp(nrow(tmp.site.list))
  for(i in 1:nrow(tmp.site.list)){
    tmp2=subset(tmp,LOCATCD==tmp.site.list$LOCATCD[i])
    if(nrow(tmp2)==0){next}else{
      with(tmp2,points(TP.mM,Si.mM,pch=21,bg=adjustcolor(cols[i],0.1),col=adjustcolor(cols[i],0.3),lwd=0.2,cex=1.25))
      mod=sma(log.Si.mM~log.TP.mM,tmp2,slope.test=1)
      tmp.mod=mod.ext(mod);
      tmp.mod$LOCATCD=tmp.site.list$LOCATCD[i]
      tmp.mod$FLDNUM=j
      sma.rslts.trib.SiTP.SRS=rbind(sma.rslts.trib.SiTP.SRS,tmp.mod)
      x.val.min=min(tmp2$TP.mM,na.rm=T)-min(tmp2$TP.mM,na.rm=T)*0.25
      x.val.max=max(tmp2$TP.mM,na.rm=T)+max(tmp2$TP.mM,na.rm=T)*0.25
      x.val=seq(x.val.min,x.val.max,length.out=100)
      mod.fit=sma.CI(mod,x.val)
      with(mod.fit,lines(x.val,fit,lty=1,lwd=1.5,col=adjustcolor(cols[i],1)))
    }
  }    
  if(j==1){mtext(side=2,line=2,"Dissolved Silicon (mM)",cex=0.8)}
  #mtext(side=3,subset(FLDNUM.pool.xwalk,FLDNUM==j)$Pool,cex=0.75)
  mtext(side=3,paste0("Reach ",j),cex=0.75)
  if(j==4){text(x=xlim.val[1]+0.01,y=ylim.val[1]-0.013,"Total Phosphorus (mM)",xpd=NA,adj=1,cex=1.1)}
}
for(j in 1:6){
  tmp=subset(dat.xtab.fix.ann,FLDNUM==j)
  tmp.site.list=subset(sites.list,FLDNUM==j)
  
  ylim.val=c(0.04,0.3);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")
  xlim.val=c(0.01,1);xmaj=log.scale.fun(xlim.val,"major");xmin=log.scale.fun(xlim.val,"minor")   
  
  plot(Si.mM~TN.mM,tmp,log="xy",ylim=ylim.val,xlim=xlim.val,type="n",yaxt="n",xaxt="n",ylab=NA,xlab=NA)
  abline(h=ymaj,v=xmaj,lty=3,col="grey")
  axis_fun(1,line=-0.75,xmaj,xmin,format(xmaj,scientific=F),0.8);
  if(j==1){axis_fun(2,ymaj,ymin,ymaj,cex=0.8)}else{axis_fun(2,ymaj,ymin,NA)};box(lwd=1)
  abline(0,1,lty=2,lwd=1.25)
  
  cols=cols.rmp(nrow(tmp.site.list))
  for(i in 1:nrow(tmp.site.list)){
    tmp2=subset(tmp,LOCATCD==tmp.site.list$LOCATCD[i])
    if(nrow(tmp2)==0){next}else{
      with(tmp2,points(TN.mM,Si.mM,pch=21,bg=adjustcolor(cols[i],0.1),col=adjustcolor(cols[i],0.3),lwd=0.2,cex=1.25))
      mod=sma(log.Si.mM~log.TN.mM,tmp2,slope.test=1)
      tmp.mod=mod.ext(mod);
      tmp.mod$LOCATCD=tmp.site.list$LOCATCD[i]
      tmp.mod$FLDNUM=j
      sma.rslts.trib.SiTN.SRS=rbind(sma.rslts.trib.SiTN.SRS,tmp.mod)
      x.val.min=min(tmp2$TN.mM,na.rm=T)-min(tmp2$TN.mM,na.rm=T)*0.25
      x.val.max=max(tmp2$TN.mM,na.rm=T)+max(tmp2$TN.mM,na.rm=T)*0.25
      x.val=seq(x.val.min,x.val.max,length.out=100)
      mod.fit=sma.CI(mod,x.val)
      with(mod.fit,lines(x.val,fit,lty=1,lwd=1.5,col=adjustcolor(cols[i],1)))
    }
  }    
  if(j==1){mtext(side=2,line=2,"Dissolved Silicon (mM)",cex=0.8)}
  if(j==4){text(x=xlim.val[1]+0.10,y=ylim.val[1]-0.013,"Total Nitrogen (mM)",xpd=NA,adj=1,cex=1.1)}
}
dev.off()


# Tables ------------------------------------------------------------------
nrow(sum.dat.tab)
nrow(sma.rslts.trib.SiTP.SRS)

sma.rslts.trib.SiTP.SRS$param="Si X TP"
sma.rslts.trib.SiTP.SRS=merge(sma.rslts.trib.SiTP.SRS,FLDNUM.pool.xwalk,"FLDNUM")
sma.rslts.trib.SiTP.SRS=sma.rslts.trib.SiTP.SRS[order(sma.rslts.trib.SiTP.SRS$FLDNUM),]
sma.rslts.trib.SiTP.SRS$slope=round(sma.rslts.trib.SiTP.SRS$slope,2)
sma.rslts.trib.SiTP.SRS$intercept=round(sma.rslts.trib.SiTP.SRS$intercept,1)
sma.rslts.trib.SiTP.SRS$Fstat=round(sma.rslts.trib.SiTP.SRS$Fstat,2)
sma.rslts.trib.SiTP.SRS$r2=signif(round(sma.rslts.trib.SiTP.SRS$r2,3))
sma.rslts.trib.SiTP.SRS$pval.txt=with(sma.rslts.trib.SiTP.SRS,ifelse(pval<0.05,"<0.05",signif(round(pval,2),digits=2)))

kable(sma.rslts.trib.SiTP.SRS[,c("param","Pool","LOCATCD","r2","intercept","slope","Fstat","pval.txt")],
      col.names = c("Comparison (Y by X)","Pool","Station","R\u00B2","Intercept","Slope","F-statistic","\u03C1-value"),
      align=c("l","l","l","c","c","c"),row.names = F)%>%
  kable_styling(bootstrap_options = "striped", full_width = F)
  
  #%>%collapse_rows(columns=c(1,2),valign="top")

sma.rslts.trib.SiTN.SRS$param="Si X TN"
sma.rslts.trib.SiTN.SRS=merge(sma.rslts.trib.SiTN.SRS,FLDNUM.pool.xwalk,"FLDNUM")
sma.rslts.trib.SiTN.SRS=sma.rslts.trib.SiTN.SRS[order(sma.rslts.trib.SiTN.SRS$FLDNUM),]
sma.rslts.trib.SiTN.SRS$slope=round(sma.rslts.trib.SiTN.SRS$slope,2)
sma.rslts.trib.SiTN.SRS$intercept=round(sma.rslts.trib.SiTN.SRS$intercept,1)
sma.rslts.trib.SiTN.SRS$Fstat=round(sma.rslts.trib.SiTN.SRS$Fstat,2)
sma.rslts.trib.SiTN.SRS$r2=signif(round(sma.rslts.trib.SiTN.SRS$r2,3))
sma.rslts.trib.SiTN.SRS$pval.txt=with(sma.rslts.trib.SiTN.SRS,ifelse(pval<0.05,"<0.05",signif(round(pval,2),digits=2)))

kable(sma.rslts.trib.SiTN.SRS[,c("param","Pool","LOCATCD","r2","intercept","slope","Fstat","pval.txt")],
      col.names = c("Comparison (Y by X)","Pool","Station","R\u00B2","Intercept","Slope","F-statistic","\u03C1-value"),
      align=c("l","l","l","c","c","c"),row.names = F)%>%
  kable_styling(bootstrap_options = "striped", full_width = F)


### 
vars=c("FLDNUM","Pool","LOCATCD","r2", "slope", "Fstat", "pval.txt")
sma.rslts.trib.SiTP.SRS2=sma.rslts.trib.SiTP.SRS[,vars]
colnames(sma.rslts.trib.SiTP.SRS2)=c("FLDNUM","Pool","LOCATCD","SiTP_r2", "SiTP_slope", "SiTP_Fstat", "SiTP_pval.txt")

sma.rslts.trib.SiTN.SRS2=sma.rslts.trib.SiTN.SRS[,vars]
colnames(sma.rslts.trib.SiTN.SRS2)=c("FLDNUM","Pool","LOCATCD","SiTN_r2", "SiTN_slope", "SiTN_Fstat", "SiTN_pval.txt")

test=merge(sma.rslts.trib.SiTP.SRS2,sma.rslts.trib.SiTN.SRS2,c("FLDNUM","Pool","LOCATCD"))

kable(test[,c("FLDNUM","LOCATCD","SiTP_r2", "SiTP_slope", "SiTP_Fstat", "SiTP_pval.txt", "SiTN_r2", "SiTN_slope", "SiTN_Fstat", "SiTN_pval.txt")],
      col.names = c("Reach","Station","R\u00B2","Slope","F-statistic","\u03C1-value","R\u00B2","Slope","F-statistic","\u03C1-value"),
      align=c("l","l","c","c","c","c","c","c","c","c"),row.names = F)%>%
  kable_styling(bootstrap_options = "striped", full_width = F)%>%
  collapse_rows(columns=c(1),valign="top")%>%
  add_header_above(c("","","Si x TP" = 4,"Si x TN" = 4))
