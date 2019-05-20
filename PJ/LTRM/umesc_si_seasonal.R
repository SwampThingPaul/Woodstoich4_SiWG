## 
## Woodstoich 4 (Si Working grop)
## Upper Mississippi River Restoration LTRM
## https://umesc.usgs.gov/data_library/water_quality/water_quality_page.html
## https://www.umesc.usgs.gov/cgi-bin/ltrmp/water/water_meta.pl
##
##
## Seasonal Plots
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
trib.dat=read.csv(paste0(data.path,"UMR_Tribs_1988-2018.csv"))
trib.dat$DATETIME=date.fun(paste0(as.character(trib.dat$DATE)," ",as.character(trib.dat$TIME),":00"),tz="America/Chicago",form="%m/%d/%Y %H:%M:%S")
trib.dat$DATE=date.fun(as.character(trib.dat$DATE),tz="America/Chicago",form="%m/%d/%Y")
head(trib.dat)

mainstem.dat=read.csv(paste0(data.path,"UMR_Mainstem_1991-2018.csv"))
mainstem.dat$DATETIME=date.fun(paste0(as.character(mainstem.dat$DATE)," ",as.character(mainstem.dat$TIME),":00"),tz="America/Chicago",form="%m/%d/%Y %H:%M:%S")
mainstem.dat$DATE=date.fun(as.character(mainstem.dat$DATE),tz="America/Chicago",form="%m/%d/%Y")
head(mainstem.dat)


##
sp.dat.trib=ddply(trib.dat,c("LOCATCD","FLDNUM","EASTING","NORTHING"),summarise,N.val=N(LOCATCD),min.date=min(DATE),max.date=max(DATE),N.SI=N(SI))
sp.dat.trib$yrs.dat=with(sp.dat.trib,as.numeric(max.date-min.date)*3.17098e-8)
nrow(subset(sp.dat.trib,yrs.dat>20))
plot(yrs.dat~N.val,sp.dat.trib,ylab="Number of Years",xlab="Number of Samples")

sp.dat.main=ddply(mainstem.dat,c("LOCATCD","FLDNUM","EASTING","NORTHING"),summarise,N.val=N(LOCATCD),min.date=min(DATE),max.date=max(DATE),N.SI=N(SI))
sp.dat.main$yrs.dat=with(sp.dat.main,as.numeric(max.date-min.date)*3.17098e-8)
nrow(subset(sp.dat.main,yrs.dat>20))
plot(yrs.dat~N.val,sp.dat.main,ylab="Number of Years",xlab="Number of Samples")


# https://www.umesc.usgs.gov/ltrmp/water/qa_flag_primer.pdf
trib.dat=merge(trib.dat,subset(sp.dat.trib,yrs.dat>20)[,c("LOCATCD","yrs.dat")],by=c("LOCATCD"))
unique(trib.dat$SIQF)

dput(names(trib.dat))
trib.dat=rename(trib.dat,c("VSS_QF"="VSSQF","NA."="Na","NAQF"="NaQF"))
names(trib.dat)

idvars=c("FLDNUM","LOCATCD","DATE","DATETIME")
field.params=c("TEMP","DO","PH","TURB","COND","VEL")
lab.params=c("SRP","TP","NOX","NHX","TN","SI","VSS","CL","SO4","CHLcal")

trib.dat$CHLcalQF=trib.dat$CHLFQF
trib.dat2=data.frame()
#stack field parameters
for(i in 1:length(field.params)){
  tmp=trib.dat[,c(idvars,field.params[i],paste0(field.params[i],"QF"))]
  colnames(tmp)=c(idvars,"data.value","flag")
  tmp$parameter=field.params[i]
  tmp=tmp[,c(idvars,"parameter","data.value","flag")]
  tmp$fatal.flag=with(tmp,ifelse(flag=="","N","Y"))
  trib.dat2=rbind(trib.dat2,tmp)
  print(i)
}
trib.dat2$data.type="Field"
#unique(trib.dat2$flag)
#subset(trib.dat2,flag=="")

for(i in 1:length(lab.params)){
  tmp=trib.dat[,c(idvars,lab.params[i],paste0(lab.params[i],"QF"))]
  colnames(tmp)=c(idvars,"data.value","flag")
  tmp$data.type="lab"
  tmp$parameter=lab.params[i]
  tmp=tmp[,c(idvars,"parameter","data.value","flag","data.type")]
  tmp$flag=as.factor(tmp$flag)
  tmp$fatal.flag=with(tmp,ifelse(flag==0|is.na(flag)==T,"N","Y"))
  trib.dat2=rbind(trib.dat2,tmp)
  print(i)
}
trib.dat2=subset(trib.dat2,is.na(data.value)==F);#removes NA data values

ddply(trib.dat2,"parameter",summarise,min.val=min(data.value))
trib.dat2$halfMDL=with(trib.dat2,ifelse(data.value<0,abs(data.value)/2,data.value))

trib.dat.xtab=cast(subset(trib.dat2,fatal.flag=="N"),FLDNUM+LOCATCD+DATE~parameter,value="halfMDL",mean)
trib.dat.xtab$DIN=with(trib.dat.xtab,NHX+NOX)
trib.dat.xtab$month=as.numeric(format(trib.dat.xtab$DATE,"%m"))
trib.dat.xtab$CY=as.numeric(format(trib.dat.xtab$DATE,"%Y"))
trib.dat.xtab$WY=WY(trib.dat.xtab$DATE,"Fed")

# Reversal Evaluation
trib.dat.xtab$TPReversal=with(trib.dat.xtab,ifelse(is.na(SRP)==T|is.na(TP)==T,0,ifelse(SRP>(TP*1.3),1,0)));# Reversals identified as 1 reversals consistent with TP rule evaluation
trib.dat.xtab$TNReversal=with(trib.dat.xtab,ifelse(is.na(DIN)==T|is.na(TN)==T,0,ifelse(DIN>(TN*1.3),1,0)));

sum(trib.dat.xtab$TNReversal,na.rm=T)
sum(trib.dat.xtab$TPReversal,na.rm=T)

par(family="serif",oma=c(1,1,1,1),mar=c(4,4,1,1))
layout(matrix(1:2,1,2,byrow=F))
plot(TN~DIN,trib.dat.xtab,ylab="TN (mg/L)",xlab="DIN (mg/L)",pch=21,bg=ifelse(TNReversal==1,"dodgerblue1",NA),col=adjustcolor("grey",0.8));abline(0,1,col="dodgerblue1")
plot(TP~SRP,trib.dat.xtab,ylab="TP (mg/L)",xlab="SRP (mg/L)",pch=21,bg=ifelse(TPReversal==1,"red",NA),col=adjustcolor("grey",0.8));abline(0,1,col="red")

dev.off();#clears plots

###
## If reversal is identified then data point was removed from analysis
trib.dat.xtab$TP=with(trib.dat.xtab,ifelse(TPReversal==1,NA,TP))
trib.dat.xtab$TN=with(trib.dat.xtab,ifelse(TNReversal==1,NA,TN))
trib.dat.xtab$SRP=with(trib.dat.xtab,ifelse(TPReversal==1,NA,SRP))
trib.dat.xtab$DIN=with(trib.dat.xtab,ifelse(TNReversal==1,NA,DIN))
trib.dat.xtab$TP.ugL=trib.dat.xtab$TP*1000

trib.dat.xtab$Si.mM=with(trib.dat.xtab, SI/Si.mw)
trib.dat.xtab$TP.mM=with(trib.dat.xtab,TP/P.mw)
trib.dat.xtab$TN.mM=with(trib.dat.xtab,TN/N.mw)
trib.dat.xtab$SRP.mM=with(trib.dat.xtab,SRP/P.mw)
trib.dat.xtab$DIN.mM=with(trib.dat.xtab,DIN/N.mw)

trib.dat.xtab$SiTN=with(trib.dat.xtab,Si.mM/TN.mM)
trib.dat.xtab$SiDIN=with(trib.dat.xtab,Si.mM/DIN.mM)
trib.dat.xtab$SiTP=with(trib.dat.xtab,Si.mM/TP.mM)
trib.dat.xtab$SiSRP=with(trib.dat.xtab,Si.mM/SRP.mM)

trib.dat.xtab$log.Si.mM=log(trib.dat.xtab$Si.mM)
trib.dat.xtab$log.TP.mM=log(trib.dat.xtab$TP.mM)
trib.dat.xtab$log.TN.mM=log(trib.dat.xtab$TN.mM)
trib.dat.xtab$log.SRP.mM=log(trib.dat.xtab$SRP.mM)
trib.dat.xtab$log.DIN.mM=log(trib.dat.xtab$DIN.mM)

vars=c("FLDNUM", "LOCATCD","month", "CY", "WY")
trib.dat.month=ddply(trib.dat.xtab,vars,summarise,meanTP=mean(TP.ugL,na.rm=T),meanTN=mean(TN,na.rm=T),meanSi=mean(SI,na.rm=T),meanSiP=mean(SiTP,na.rm=T),meanSiN=mean(SiTN,na.rm=T))

sites.val=ddply(subset(sp.dat.trib,yrs.dat>20),c("FLDNUM","LOCATCD"),summarise,N.val=N(LOCATCD))
xlim.val=c(1990,2018);by.x=10;xmaj=seq(round(xlim.val[1]),xlim.val[2],by.x);xmin=seq(round(xlim.val[1]),xlim.val[2],by.x/by.x)
#tiff(filename=paste0(plot.path,"LTRM_Si_SeasonalMonth.tiff"),width=7,height=6.5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",oma=c(2.5,2,1,0.25),mar=c(1.5,2,0.5,1))
layout(matrix(c(1:26,27,27),7,4,byrow=F))

cols=adjustcolor(colorRampPalette(c("dodgerblue1","forestgreen","indianred1"))(12),0.5)
ylim.val=c(0,15);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(j in 1:length(sites.val$LOCATCD)){
tmp=subset(trib.dat.month,LOCATCD==sites.val$LOCATCD[j])
plot(meanSi~CY,tmp,ylim=ylim.val,xlim=xlim.val,yaxt="n",xaxt="n",ylab=NA,xlab=NA,type="n")
for(i in 1:12){
  with(subset(tmp,month==i),points(CY,meanSi,pch=19,col=cols[i],cex=1))
}
axis_fun(2,ymaj,ymin,ymaj)
if(j%in%c(7,14,21,26)){axis_fun(1,line=-0.5,xmaj,xmin,xmaj)}else{axis_fun(1,xmaj,xmin,NA)}
mtext(side=3,sites.val$LOCATCD[j],cex=0.8)
}
mtext(side=2,line=0.5,outer=T,"Silica (mg L\u207B\u00B9)")
mtext(side=1,line=0.25,outer=T,"Calendar Year")

plot(0:1,0:1,axes=F,type="n",ylab=NA,xlab=NA)
legend.text=month.abb[1:12]
pt.cols=cols
legend(0.5,1,legend=legend.text,pch=19,col=pt.cols,lwd=0.2,lty=NA,pt.bg=pt.cols,pt.cex=2,ncol=2,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5)
dev.off()



xlim.val=c(1,12);by.x=4;xmaj=seq(round(xlim.val[1]),xlim.val[2],by.x);xmin=seq(round(xlim.val[1]),xlim.val[2],by.x/by.x)
#tiff(filename=paste0(plot.path,"LTRM_Si_SeasonalYear.tiff"),width=7,height=6.5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",oma=c(2.5,2,1,0.25),mar=c(1.5,2,0.5,1))
layout(matrix(c(1:26,27,27),7,4,byrow=F))

cols=adjustcolor(colorRampPalette(c("dodgerblue1","forestgreen","indianred1"))(29),0.5)
Years=seq(1990,2018,1)
ylim.val=c(0,15);by.y=5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(j in 1:length(sites.val$LOCATCD)){
  tmp=subset(trib.dat.month,LOCATCD==sites.val$LOCATCD[j])
  plot(meanSi~month,tmp,ylim=ylim.val,xlim=xlim.val,yaxt="n",xaxt="n",ylab=NA,xlab=NA,type="n")
  for(i in 1:length(Years)){
    with(subset(tmp,CY==Years[i]),points(month,meanSi,pch=19,col=cols[i],cex=1))
  }
  axis_fun(2,ymaj,ymin,ymaj)
  if(j%in%c(7,14,21,26)){axis_fun(1,line=-0.5,xmaj,xmin,month.abb[xmaj])}else{axis_fun(1,xmaj,xmin,NA)}
  mtext(side=3,sites.val$LOCATCD[j],cex=0.8)
}
mtext(side=2,line=0.5,outer=T,"Silica (mg L\u207B\u00B9)")
mtext(side=1,line=0.25,outer=T,"Month")
plot(0:1,0:1,axes=F,type="n",ylab=NA,xlab=NA)
legend.text=Years#month.abb[1:12]
pt.cols=cols
legend(0.5,1,legend=legend.text,pch=19,col=pt.cols,lwd=0.2,lty=NA,pt.bg=pt.cols,pt.cex=2,ncol=3,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5)
dev.off()


xlim.val=c(1,12);by.x=4;xmaj=seq(round(xlim.val[1]),xlim.val[2],by.x);xmin=seq(round(xlim.val[1]),xlim.val[2],by.x/by.x)
#tiff(filename=paste0(plot.path,"LTRM_SiTP_SeasonalYear.tiff"),width=7,height=6.5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",oma=c(2.5,2,1,0.25),mar=c(1.5,2,0.5,1))
layout(matrix(c(1:26,27,27),7,4,byrow=F))

cols=adjustcolor(colorRampPalette(c("dodgerblue1","forestgreen","indianred1"))(29),0.5)
Years=seq(1990,2018,1)
ylim.val=c(0,400);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(j in 1:length(sites.val$LOCATCD)){
  tmp=subset(trib.dat.month,LOCATCD==sites.val$LOCATCD[j])
  plot(meanSiP~month,tmp,ylim=ylim.val,xlim=xlim.val,yaxt="n",xaxt="n",ylab=NA,xlab=NA,type="n")
  for(i in 1:length(Years)){
    with(subset(tmp,CY==Years[i]),points(month,meanSiP,pch=19,col=cols[i],cex=1))
  }
  axis_fun(2,ymaj,ymin,ymaj)
  if(j%in%c(7,14,21,26)){axis_fun(1,line=-0.5,xmaj,xmin,month.abb[xmaj])}else{axis_fun(1,xmaj,xmin,NA)}
  mtext(side=3,sites.val$LOCATCD[j],cex=0.8)
}
mtext(side=2,line=0.5,outer=T,"Si:TP (mol:mol)")
mtext(side=1,line=0.25,outer=T,"Month")
plot(0:1,0:1,axes=F,type="n",ylab=NA,xlab=NA)
legend.text=Years#month.abb[1:12]
pt.cols=cols
legend(0.5,1,legend=legend.text,pch=19,col=pt.cols,lwd=0.2,lty=NA,pt.bg=pt.cols,pt.cex=2,ncol=3,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5)
dev.off()

xlim.val=c(1,12);by.x=4;xmaj=seq(round(xlim.val[1]),xlim.val[2],by.x);xmin=seq(round(xlim.val[1]),xlim.val[2],by.x/by.x)
#tiff(filename=paste0(plot.path,"LTRM_SiTN_SeasonalYear.tiff"),width=7,height=6.5,units="in",res=200,type="windows",compression=c("lzw"),bg="white")
par(family="serif",oma=c(2.5,2,1,0.25),mar=c(1.5,2,0.5,1))
layout(matrix(c(1:26,27,27),7,4,byrow=F))

cols=adjustcolor(colorRampPalette(c("dodgerblue1","forestgreen","indianred1"))(29),0.5)
Years=seq(1990,2018,1)
#ylim.val=c(0,15);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
for(j in 1:length(sites.val$LOCATCD)){
  tmp=subset(trib.dat.month,LOCATCD==sites.val$LOCATCD[j])
  ylim.max=ceiling(max(tmp$meanSiN,na.rm=T))
  ylim.val=c(0,ylim.max);by.y=ylim.max/2
  ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
  plot(meanSiN~month,tmp,ylim=ylim.val,xlim=xlim.val,yaxt="n",xaxt="n",ylab=NA,xlab=NA,type="n")
  for(i in 1:length(Years)){
    with(subset(tmp,CY==Years[i]),points(month,meanSiN,pch=19,col=cols[i],cex=1))
  }
  axis_fun(2,ymaj,ymin,format(ymaj))
  if(j%in%c(7,14,21,26)){axis_fun(1,line=-0.5,xmaj,xmin,month.abb[xmaj])}else{axis_fun(1,xmaj,xmin,NA)}
  mtext(side=3,sites.val$LOCATCD[j],cex=0.8)
}
mtext(side=2,line=0.5,outer=T,"Si:TN (mol:mol)")
mtext(side=1,line=0.25,outer=T,"Month")
plot(0:1,0:1,axes=F,type="n",ylab=NA,xlab=NA)
legend.text=Years#month.abb[1:12]
pt.cols=cols
legend(0.5,1,legend=legend.text,pch=19,col=pt.cols,lwd=0.2,lty=NA,pt.bg=pt.cols,pt.cex=2,ncol=3,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5)
dev.off()