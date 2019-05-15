####
###UMR_Trib Data. Si Woodstoich Working Group
#5.14.19

#Playing around with new Trib data

library(plyr)
library(dplyr)

setwd("~/Woodstoich/RawData_5.14.19")

#importing data
Data<-read.table(file="UMR_Tribs_jcc_5.14.19.csv", header=TRUE, sep=',')
names(Data)

#Helper variables from Paul
N.mw=14.0067
P.mw=30.973762
C.mw=12.0107
Si.mw=28.0855

#Adding new colums with data in moles so to calc molar ratios
Data$uMSi<-(Data$SI/Si.mw*1000)
Data$uMNOX<-(Data$NOX/N.mw*1000)
Data$uMNHX<-(Data$NHX/N.mw*1000)
Data$uMSRP<-(Data$SRP/P.mw*1000)
Data$uMDIN<-(Data$uMNOX+Data$uMNHX)
Data$DINSi<-(Data$uMDIN/Data$uMSi)
Data$PSi<-(Data$uMSRP/Data$uMSi)

range(Data$uMSi, na.rm=T)

#subsetting to remove negatives
Data<-subset(Data, uMSi>0 & uMNOX>0 & uMSRP>0 & uMNHX>0)

#Fixing dates and adding year as a column
Data$DATE2=as.Date(Data$DATE, c("%m/%d/%Y"))
Data$YEAR=as.numeric(format(Data$DATE2,c("%Y")))

#making each site name into a unique number
unique(Data$LOCATCD)
Data$SiteNum<-as.numeric(as.factor(Data$LOCATCD))
head(Data$SiteNum)
unique(Data$SiteNum)

#Calc min and max dates for each site
SiteDates<-ddply(Data, c("LOCATCD"), summarise, min.date=min(YEAR), max.date=max(YEAR))
SiteDates$YrsData<-with(SiteDates, as.numeric(max.date-min.date))
LongTermSites<-subset(SiteDates, SiteDates$YrsData>20)

#Now merge these sites with >20 yrs data with larger dataset
Data3<-merge(LongTermSites, Data, by=c("LOCATCD"))
head(Data3)

#calc avg vaues by year at each site
AvgByYr2<-aggregate(cbind(Data3$uMSi, Data3$uMNHX, Data3$uMSRP, Data3$uMNOX, Data3$uMDIN, 
                          Data3$DINSi, Data3$PSi) ~ Data3$LOCATCD + Data3$YEAR, FUN=mean)
names(AvgByYr2)
colnames(AvgByYr2) <- c("LOCATCD", "Year", "AvguMSi", "AvgNH4", "AvgSRP", "AvgNOX", 
                        "AvgDIN", "AvgDIN_SiRatio", "AvgP_SiRatio")
Data4<-AvgByYr2[order(AvgByYr2$LOCATCD),]
range(Data4$Year)
range(Data4$AvgDIN_SiRatio)




##==============================================
##plotting avg annual values over time for each site with >20yrs data 
##exported the results of simple linear regression
##=============================================

library(broom)

#Average DIN:DSi molar ratios over time
df=NULL
par(mfrow=c(4,7), mar=c(2,2.8,2,1.3))
for(i in unique(Data4$LOCATCD)){
  plot(Data4[Data4$LOCATCD==i, "Year"], Data4[Data4$LOCATCD==i, "AvgDIN_SiRatio"],
       xlim=c(1990, 2020), ylim=c(0,16), main=paste("Site", i), ylab="DIN:DSi", 
       mgp=c(1.8,.2,0), las=1, xlab="", xaxt="n", yaxt="n")
  axis(side=1, at = c(1990, 2000, 2010, 2020), tcl=-.4, cex.axis=.8, mgp=c(0,.3,0), font.axis=1)
  axis(side=2, at = c(0,5,10,15), tcl=-.3, cex.axis=.8, mgp=c(0, .5,0), font.axis=1, las=1)
  abline(lm(Data4[Data4$LOCATCD==i, "AvgDIN_SiRatio"]~ Data4[Data4$LOCATCD==i, "Year"]))
  df=rbind(df,data.frame(glance(lm(Data4[Data4$LOCATCD==i, "AvgDIN_SiRatio"]~ Data4[Data4$LOCATCD==i, "Year"]))))
}
dev.off()

write.csv(df, file="R2_DIN_SiRatiosvsTime_5.15.19.csv", row.names=TRUE, na = "NA")




#Average DIP:Si molar ratios over time
df=NULL
par(mfrow=c(4,7), mar=c(2,2.8,2,1.3))
for(i in unique(Data4$LOCATCD)){
  plot(Data4[Data4$LOCATCD==i, "Year"], Data4[Data4$LOCATCD==i, "AvgP_SiRatio"],
       xlim=c(1990, 2020), ylim=c(0,.1), main=paste("Site", i), ylab="SRP:DSi", 
       mgp=c(1.8,.2,0), las=1, xlab="", xaxt="n", yaxt="n")
  axis(side=1, at = c(1990, 2000, 2010, 2020), tcl=-.4, cex.axis=.8, mgp=c(0,.3,0), font.axis=1)
  axis(side=2, at = c(0,0.05, 0.1), tcl=-.3, cex.axis=.8, mgp=c(0, .5,0), font.axis=1, las=1)
  abline(lm(Data4[Data4$LOCATCD==i, "AvgP_SiRatio"]~ Data4[Data4$LOCATCD==i, "Year"]))
  df=rbind(df,data.frame(glance(lm(Data4[Data4$LOCATCD==i, "AvgP_SiRatio"]~ Data4[Data4$LOCATCD==i, "Year"]))))
}
dev.off()

write.csv(df, file="R2_P_SiRatiosvsTime_5.15.19.csv", row.names=TRUE, na = "NA")



#Average DSi values over time
df=NULL
par(mfrow=c(4,7), mar=c(2,2.5,2,1.3))
for(i in unique(Data4$LOCATCD)){
  plot(Data4[Data4$LOCATCD==i, "Year"], Data4[Data4$LOCATCD==i, "AvguMSi"],
       xlim=c(1990, 2020), ylim=range(Data4$AvguMSi), main=paste("Site", i), ylab="DSi uM", 
       mgp=c(1.6,.2,0), las=1, xlab="", xaxt="n", yaxt="n")
  axis(side=1, at = c(1990, 2000, 2010, 2020), tcl=-.4, cex.axis=.8, mgp=c(0,.3,0), font.axis=1)
  axis(side=2, at = c(0,50,100,150,200, 250), tcl=-.3, cex.axis=.8, mgp=c(0, .5,0), font.axis=1, las=1)
  abline(lm(Data4[Data4$LOCATCD==i, "AvguMSi"]~ Data4[Data4$LOCATCD==i, "Year"]))
  df=rbind(df,data.frame(glance(lm(Data4[Data4$LOCATCD==i, "AvguMSi"]~ Data4[Data4$LOCATCD==i, "Year"]))))
  }
dev.off()

write.csv(df, file="R2_DSivsTime_5.15.19.csv", row.names=TRUE, na = "NA")
#glace in the broom package give R2 (and apparently pvalues). tidy gives p values and slopes


#Average DIN values over time
df=NULL
par(mfrow=c(4,7), mar=c(2,2.5,2,1.3))
for(i in unique(Data4$LOCATCD)){
  plot(Data4[Data4$LOCATCD==i, "Year"], Data4[Data4$LOCATCD==i, "AvgDIN"],
       xlim=c(1990, 2020), ylim=range(Data4$AvgDIN), main=paste("Site", i), ylab="DIN uM", 
       mgp=c(1.6,.2,0), las=1, xlab="", xaxt="n", yaxt="n")
  axis(side=1, at = c(1990, 2000, 2010, 2020), tcl=-.4, cex.axis=.8, mgp=c(0,.3,0), font.axis=1)
  axis(side=2, at = c(0,500,1000), tcl=-.3, cex.axis=.8, mgp=c(0, .5,0), font.axis=1, las=1)
  abline(lm(Data4[Data4$LOCATCD==i, "AvgDIN"]~ Data4[Data4$LOCATCD==i, "Year"]))
  df=rbind(df,data.frame(glance(lm(Data4[Data4$LOCATCD==i, "AvgDIN"]~ Data4[Data4$LOCATCD==i, "Year"]))))
}
dev.off()
write.csv(df, file="R2_DINvsTime_5.15.19.csv", row.names=TRUE, na = "NA")


#Average DIP values over time
df=NULL
par(mfrow=c(4,7), mar=c(2,2.5,2,1.3))
for(i in unique(Data4$LOCATCD)){
  plot(Data4[Data4$LOCATCD==i, "Year"], Data4[Data4$LOCATCD==i, "AvgSRP"],
       xlim=c(1990, 2020), ylim=range(Data4$AvgSRP), main=paste("Site", i), ylab="SRP uM", 
       mgp=c(1.6,.2,0), las=1, xlab="", xaxt="n", yaxt="n")
  axis(side=1, at = c(1990, 2000, 2010, 2020), tcl=-.4, cex.axis=.8, mgp=c(0,.3,0), font.axis=1)
  axis(side=2, at = c(0,10,20,30), tcl=-.3, cex.axis=.8, mgp=c(0, .5,0), font.axis=1, las=1)
  abline(lm(Data4[Data4$LOCATCD==i, "AvgSRP"]~ Data4[Data4$LOCATCD==i, "Year"]))
  df=rbind(df,data.frame(glance(lm(Data4[Data4$LOCATCD==i, "AvgSRP"]~ Data4[Data4$LOCATCD==i, "Year"]))))
}
dev.off()

write.csv(df, file="R2_SRPvsTime_5.15.19.csv", row.names=TRUE, na = "NA")







##=======================================================
##below is me f-ing around...ignore. didn't use
##=======================================================

#couldn't figure this out - to define the path inside the loop:
mypath<-file.path(~"Woodstoich","RawData_5.14.19", "img")
jpeg(file=mypath)

#if use below, i have to change site num to locatcd bc got rid of misc numbers:

#testing out the loop here - works! just going into wrong folder and these are each plot individually, not multi-panel:
#so putting in diff folder:
setwd("~/Woodstoich/RawData_5.14.19/img")
for(i in unique(Data4$SiteNumber)){
  jpeg(paste("plot_", i, ".jpeg", sep=""))
  plot(Data4[Data4$SiteNumber==i, "Year"], Data4[Data4$SiteNumber==i, "AvguMSi"],
       xlim=range(Data4$Year), ylim=range(Data4$AvguMSi), main=paste("Site", i), ylab="DSi uM", xlab="")
  dev.off()
}

#couldn't figure out matrix so using mfrow instead
layout(matrix(1:3, ncol=3))
layout(matrix(1:28,7,4,byrow=F))

#Test code for saving plot to folder
jpeg(file = "~/Woodstoich/RawData_5.14.19/img/plot2.jpeg")
plot(Data4$Year, Data4$AvgP_SiRatio)
dev.off()

#lets add trend line to plot
#practice plot not in loop:
plot(Data4$Year, Data4$AvguMSi, ylim=c(5,250), xlim=c(1990,2020), ylab="DSi uM", xlab="", las=1)
title("DSi uM concentrations over time, LOCATCD==WPO2.6M ", line=2, cex.main=.8)
abline(lm(Data4$AvguMSi ~ Data4$Year))
fit1<-lm(Data4$AvguMSi ~ Data4$Year)
fit2<-lm(Data4$AvguMSi ~ Data4$Year)

#this loop works - just need to save the files to a folder
for(i in unique(Data4$SiteNumber)){
  plot(Data4[Data4$SiteNumber==i, "Year"], Data4[Data4$SiteNumber==i, "AvguMSi"],
       xlim=range(Data4$Year), ylim=range(Data4$AvguMSi), main=paste("Site", i), ylab="DSi uM", xlab="")
  }
dev.off()

#this works but ignore
plot(Data4$Year[which(Data4$SiteNumber==1)], Data4$AvguMSi [which(Data4$SiteNumber==1)], ylim=c(5,250), xlim=c(1990,2020), ylab="DSi uM", xlab="", las=1)
title("DSi uM concentrations over time, Site ", line=2, cex.main=.8)




##not using but kept for maybe later
#calc avg vaues by year at each site
AvgByYr<-aggregate(Data$uMSi~ Data$SiteNum + Data$YEAR, FUN=mean)
colnames(AvgByYr) <- c("SiteNumber", "Year", "AvguMSi")
Data3<-AvgByYr[order(AvgByYr$SiteNumber),]


#these are 'or' not 'and' statements
Data5<-subset(Data,subset=uMSi>0 | uMNOX>0 | uMNHX>0 | uMSRP>0)
Data9<-Data[ !is.na(Data$uMSi | Data$uMNOX| Data$uMSRP | Data$NHX) & (Data$uMSi>0 | Data$uMNOX>0 |Data$uMNHX>0 | Data$uMSRP>0) ,]

#Does same thing as above but uses the 'which', which some pple don't like
Data8<-Data[ which(Data$uMSi>0 | Data$uMNOX>0 |Data$uMNHX>0 | Data$uMSRP>0) ,]

