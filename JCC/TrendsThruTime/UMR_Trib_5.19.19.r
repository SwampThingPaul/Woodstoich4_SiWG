####
###UMR_Trib Data. Si Woodstoich Working Group
#5.19.19

##====================================
#What's diff from before?
##Now using TP and TN instead of the inorganic constituents
#and put Si in numerator of ratios, like Paul did

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
Data$Si_DIN<-(Data$uMSi/Data$uMDIN)
Data$Si_SRP<-(Data$uMSi/Data$uMSRP)

Data$uMTP<-(Data$TP/P.mw*1000)
Data$uMTN<-(Data$TN/N.mw*1000)
Data$Si_TP<-(Data$uMSi/Data$uMTP)
Data$Si_TN<-(Data$uMSi/Data$uMTN)

range(Data$uMSi, na.rm=T)

#subsetting to remove negatives
Data<-subset(Data, uMSi>0 & uMNOX>0 & uMSRP>0 & uMNHX>0 & uMTP>0 & uMTN>0)

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
AvgByYr<-aggregate(cbind(Data3$uMSi, Data3$uMNHX, Data3$uMSRP, Data3$uMNOX, Data3$uMDIN, 
                          Data3$Si_TN, Data3$Si_TP, Data3$uMTP, Data3$uMTN) ~ Data3$LOCATCD + Data3$YEAR, FUN=mean)
names(AvgByYr)
colnames(AvgByYr) <- c("LOCATCD", "Year", "AvguMSi", "AvgNH4", "AvgSRP", "AvgNOX", 
                        "AvgDIN", "AvgSi_TNRatio", "AvgSi_TPRatio", "AvgTP", "AvgTN")
Data4<-AvgByYr[order(AvgByYr$LOCATCD),]
range(Data4$AvgSi_TNRatio)


##==============================================
##plotting avg annual values over time for each site with >20yrs data 
##exported the results of simple linear regression
##=============================================

library(broom)
#glace in the broom package give R2 (and apparently pvalues). tidy gives p values and slopes

#making new datafram of site IDs
LocatID<-data.frame(unique(Data4$LOCATCD))


#Average Si:TN molar ratios over time. y axis range: 0.124 to 6.38
#these plots have y axis that includes all values but hard to see trends so zooming in on next plot
df=NULL
plot.path=paste0(getwd(),"/img/")
jpeg(filename=paste0(plot.path, "AvgSiTN_Plot.jpeg"), width=7, height=5, units="in", res=200)
par(mfrow=c(4,7), mar=c(2,2.8,2,1.3))
for(i in unique(Data4$LOCATCD)){
  plot(Data4[Data4$LOCATCD==i, "Year"], Data4[Data4$LOCATCD==i, "AvgSi_TNRatio"],
       xlim=c(1990, 2020), ylim=c(0,6.5), main=paste("Site", i), ylab="Si:TN", 
       mgp=c(1.8,.2,0), las=1, xlab="", xaxt="n", yaxt="n")
  axis(side=1, at = c(1990, 2000, 2010, 2020), tcl=-.4, cex.axis=.8, mgp=c(0,.3,0), font.axis=1)
  axis(side=2, at = c(0,2,4,6), tcl=-.3, cex.axis=.8, mgp=c(0, .5,0), font.axis=1, las=1)
  abline(lm(Data4[Data4$LOCATCD==i, "AvgSi_TNRatio"]~ Data4[Data4$LOCATCD==i, "Year"]))
  df=rbind(df,data.frame(glance(lm(Data4[Data4$LOCATCD==i, "AvgSi_TNRatio"]~ Data4[Data4$LOCATCD==i, "Year"]))))
}
dev.off()

df2<-cbind(LocatID, df)
write.csv(df2, file="R2_Si_TNRatiosvsTime_5.15.19_wNames.csv", row.names=TRUE, na = "NA")

#these margins work for exporting graphs to file folders but use above graphics if want to display the plots in panel in R Studio
#Si:TN plots more zoomed in
df=NULL
plot.path=paste0(getwd(),"/img/")
jpeg(filename=paste0(plot.path, "AvgSiTN_Plot_zoomed_in.jpeg"), width=7, height=4, units="in", res=200)
par(mfrow=c(4,7), mar=c(2,2,2,1.2))
for(i in unique(Data4$LOCATCD)){
  plot(Data4[Data4$LOCATCD==i, "Year"], Data4[Data4$LOCATCD==i, "AvgSi_TNRatio"],
       xlim=c(1990, 2020), ylim=c(0,3), main=paste("Site", i), ylab="Si:TN", 
       mgp=c(1.1,.2,0), las=1, xlab="", xaxt="n", yaxt="n", cex.main=0.8, cex.lab=0.8)
  axis(side=1, at = c(1990, 2000, 2010, 2020), tcl=-.4, cex.axis=.8, mgp=c(0,.3,0), font.axis=1)
  axis(side=2, at = c(0,1,2,3), tcl=-.3, cex.axis=.8, mgp=c(0, .5,0), font.axis=1, las=1)
  abline(lm(Data4[Data4$LOCATCD==i, "AvgSi_TNRatio"]~ Data4[Data4$LOCATCD==i, "Year"]))
  df=rbind(df,data.frame(glance(lm(Data4[Data4$LOCATCD==i, "AvgSi_TNRatio"]~ Data4[Data4$LOCATCD==i, "Year"]))))
}
dev.off()



range(Data4$AvgSi_TPRatio) #3 to 285
#these margins work for exporting graphs to file folders but use above graphics if want to display the plots in panel in R Studio
#Si:TP plots 
plot.path=paste0(getwd(),"/img/")
jpeg(filename=paste0(plot.path, "AvgSiTP_Plot.jpeg"), width=7, height=4, units="in", res=200)
par(mfrow=c(4,7), mar=c(2,2.5,2,0.9))
for(i in unique(Data4$LOCATCD)){
  plot(Data4[Data4$LOCATCD==i, "Year"], Data4[Data4$LOCATCD==i, "AvgSi_TPRatio"],
       xlim=c(1990, 2020), ylim=c(0,290), main=paste("Site", i), ylab="Si:TP", 
       mgp=c(1.7,.1,0), las=1, xlab="", xaxt="n", yaxt="n", cex.main=0.8, cex.lab=0.8)
  axis(side=1, at = c(1990, 2000, 2010, 2020), tcl=-.4, cex.axis=.8, mgp=c(0,.3,0), font.axis=1)
  axis(side=2, at = c(0,100, 200,300), tcl=-.3, cex.axis=.8, mgp=c(0, .5,0), font.axis=1, las=1)
  abline(lm(Data4[Data4$LOCATCD==i, "AvgSi_TPRatio"]~ Data4[Data4$LOCATCD==i, "Year"]))
  df=rbind(df,data.frame(glance(lm(Data4[Data4$LOCATCD==i, "AvgSi_TPRatio"]~ Data4[Data4$LOCATCD==i, "Year"]))))
}
dev.off()


##Same as above except y axis is zoomed in 
#more Si_TP plots
df=NULL
plot.path=paste0(getwd(),"/img/")
jpeg(filename=paste0(plot.path, "AvgSiTP_Plot_ZoomedIn.jpeg"), width=7, height=4, units="in", res=200)
par(mfrow=c(4,7), mar=c(2,2.5,2,0.9))
for(i in unique(Data4$LOCATCD)){
  plot(Data4[Data4$LOCATCD==i, "Year"], Data4[Data4$LOCATCD==i, "AvgSi_TPRatio"],
       xlim=c(1990, 2020), ylim=c(0,150), main=paste("Site", i), ylab="Si:TP", 
       mgp=c(1.7,.1,0), las=1, xlab="", xaxt="n", yaxt="n", cex.main=0.8, cex.lab=0.8)
  axis(side=1, at = c(1990, 2000, 2010, 2020), tcl=-.4, cex.axis=.8, mgp=c(0,.3,0), font.axis=1)
  axis(side=2, at = c(0,50, 100, 150), tcl=-.3, cex.axis=.8, mgp=c(0, .5,0), font.axis=1, las=1)
  abline(lm(Data4[Data4$LOCATCD==i, "AvgSi_TPRatio"]~ Data4[Data4$LOCATCD==i, "Year"]))
  df=rbind(df,data.frame(glance(lm(Data4[Data4$LOCATCD==i, "AvgSi_TPRatio"]~ Data4[Data4$LOCATCD==i, "Year"]))))
}
dev.off()

df2<-cbind(LocatID, df)
write.csv(df2, file="R2_Si_TPRatiosvsTime_wNames.csv", row.names=TRUE, na = "NA")



#Average DSi values over time
df=NULL
plot.path=paste0(getwd(),"/img/")
jpeg(filename=paste0(plot.path, "AvgDSivsTime.jpeg"), width=7, height=4, units="in", res=200)
par(mfrow=c(4,7), mar=c(2,2.3,2,0.9))
for(i in unique(Data4$LOCATCD)){
  plot(Data4[Data4$LOCATCD==i, "Year"], Data4[Data4$LOCATCD==i, "AvguMSi"],
       xlim=c(1990, 2020), ylim=range(Data4$AvguMSi), main=paste("Site", i), ylab="DSi uM", 
       mgp=c(1.6,1.1,0), las=1, xlab="", xaxt="n", yaxt="n", cex.main=0.8, cex.lab=0.8)
  axis(side=1, at = c(1990, 2000, 2010, 2020), tcl=-.3, cex.axis=.8, mgp=c(0,0.3,0), font.axis=1)
  axis(side=2, at = c(0,50,100,150,200, 250), tcl=-.3, cex.axis=.8, mgp=c(0,0.3,0), font.axis=1, las=1)
  abline(lm(Data4[Data4$LOCATCD==i, "AvguMSi"]~ Data4[Data4$LOCATCD==i, "Year"]))
  df=rbind(df,data.frame(glance(lm(Data4[Data4$LOCATCD==i, "AvguMSi"]~ Data4[Data4$LOCATCD==i, "Year"]))))
  }
dev.off()

df2<-cbind(LocatID, df)
write.csv(df2, file="R2_DSivsTime_wNames.csv", row.names=TRUE, na = "NA")



#Average TP values over time
range(Data4$AvgTP) #1.7 to 44
df=NULL
plot.path=paste0(getwd(),"/img/")
jpeg(filename=paste0(plot.path, "AvgTP_Plot.jpeg"), width=7, height=4, units="in", res=200)
par(mfrow=c(4,7), mar=c(2,2.5,2,1.3))
for(i in unique(Data4$LOCATCD)){
  plot(Data4[Data4$LOCATCD==i, "Year"], Data4[Data4$LOCATCD==i, "AvgTP"],
       xlim=c(1990, 2020), ylim=range(Data4$AvgTP), main=paste("Site", i), ylab="TP uM", 
       mgp=c(1.6,.2,0), las=1, xlab="", xaxt="n", yaxt="n", cex.main =0.8, cex.lab=.8)
  axis(side=1, at = c(1990, 2000, 2010, 2020), tcl=-.4, cex.axis=.8, mgp=c(0,.3,0), font.axis=1)
  axis(side=2, at = c(0,20,40), tcl=-.3, cex.axis=.8, mgp=c(0, .5,0), font.axis=1, las=1)
  abline(lm(Data4[Data4$LOCATCD==i, "AvgTP"]~ Data4[Data4$LOCATCD==i, "Year"]))
  df=rbind(df,data.frame(glance(lm(Data4[Data4$LOCATCD==i, "AvgTP"]~ Data4[Data4$LOCATCD==i, "Year"]))))
}
dev.off()

LocatID<-data.frame(unique(Data4$LOCATCD))
df2<-cbind(LocatID, df)
write.csv(df2, file="R2_TPvsTime_5.19.19c.csv", row.names=TRUE, na = "NA")


#zooming in
plot.path=paste0(getwd(),"/img/")
jpeg(filename=paste0(plot.path, "AvgTP_Plot_ZoomedIn.jpeg"), width=7, height=4, units="in", res=200)
par(mfrow=c(4,7), mar=c(2,2.5,2,1.3))
for(i in unique(Data4$LOCATCD)){
  plot(Data4[Data4$LOCATCD==i, "Year"], Data4[Data4$LOCATCD==i, "AvgTP"],
       xlim=c(1990, 2020), ylim=c(0,15), main=paste("Site", i), ylab="TP uM", 
       mgp=c(1.3,.2,0), las=1, xlab="", xaxt="n", yaxt="n", cex.main =0.8, cex.lab=.8)
  axis(side=1, at = c(1990, 2000, 2010, 2020), tcl=-.4, cex.axis=.8, mgp=c(0,.3,0), font.axis=1)
  axis(side=2, at = c(0,5, 10, 15), tcl=-.3, cex.axis=.8, mgp=c(0, .5,0), font.axis=1, las=1)
  abline(lm(Data4[Data4$LOCATCD==i, "AvgTP"]~ Data4[Data4$LOCATCD==i, "Year"]))
  df=rbind(df,data.frame(glance(lm(Data4[Data4$LOCATCD==i, "AvgTP"]~ Data4[Data4$LOCATCD==i, "Year"]))))
}
dev.off()



#Average TN values over time
range(Data4$AvgTN) #134 to 1153
df=NULL
plot.path=paste0(getwd(),"/img/")
jpeg(filename=paste0(plot.path, "AvgTN_Plot.jpeg"), width=7, height=4, units="in", res=200)
par(mfrow=c(4,7), mar=c(2,2.5,2,1))
for(i in unique(Data4$LOCATCD)){
  plot(Data4[Data4$LOCATCD==i, "Year"], Data4[Data4$LOCATCD==i, "AvgTN"],
       xlim=c(1990, 2020), ylim=range(Data4$AvgTN), main=paste("Site", i), ylab="TN uM", 
       mgp=c(1.8,.2,0), las=1, xlab="", xaxt="n", yaxt="n", cex.main =0.8, cex.lab=.8)
  axis(side=1, at = c(1990, 2000, 2010, 2020), tcl=-.4, cex.axis=.8, mgp=c(0,.3,0), font.axis=1)
  axis(side=2, at = c(0,250, 500, 750, 1000), tcl=-.3, cex.axis=.8, mgp=c(0, .5,0), font.axis=1, las=1)
  abline(lm(Data4[Data4$LOCATCD==i, "AvgTN"]~ Data4[Data4$LOCATCD==i, "Year"]))
  df=rbind(df,data.frame(glance(lm(Data4[Data4$LOCATCD==i, "AvgTN"]~ Data4[Data4$LOCATCD==i, "Year"]))))
}
dev.off()

df2<-cbind(LocatID, df)
write.csv(df2, file="R2_TNvsTime_wNames.csv", row.names=TRUE, na = "NA")

#Average TN values over time - zoomed in
range(Data4$AvgTN) #134 to 1153
df=NULL
plot.path=paste0(getwd(),"/img/")
jpeg(filename=paste0(plot.path, "AvgTN_Plot_zoomedIn.jpeg"), width=7, height=4, units="in", res=200)
par(mfrow=c(4,7), mar=c(2,2.5,2,1))
for(i in unique(Data4$LOCATCD)){
  plot(Data4[Data4$LOCATCD==i, "Year"], Data4[Data4$LOCATCD==i, "AvgTN"],
       xlim=c(1990, 2020), ylim=c(0,700), main=paste("Site", i), ylab="TN uM", 
       mgp=c(1.8,.2,0), las=1, xlab="", xaxt="n", yaxt="n", cex.main =0.8, cex.lab=.8)
  axis(side=1, at = c(1990, 2000, 2010, 2020), tcl=-.4, cex.axis=.8, mgp=c(0,.3,0), font.axis=1)
  axis(side=2, at = c(0,300, 600), tcl=-.3, cex.axis=.8, mgp=c(0, .5,0), font.axis=1, las=1)
  abline(lm(Data4[Data4$LOCATCD==i, "AvgTN"]~ Data4[Data4$LOCATCD==i, "Year"]))
  df=rbind(df,data.frame(glance(lm(Data4[Data4$LOCATCD==i, "AvgTN"]~ Data4[Data4$LOCATCD==i, "Year"]))))
}
dev.off()

##==============================================
##playing with Chl data 5.16.19
##==============================================
Data5<-subset(Data3, CHLcal>0 & VSS>0)


#calculating average annual values with smaller dataset that excludes negatives for VSS and Chl a (n=~6000 now)
#this still includes all sites with >20 yrs data
AvgByYr_withChlVSS<-aggregate(cbind(Data5$uMSi, Data5$uMNHX, Data5$uMSRP, Data5$uMNOX, Data5$uMDIN, 
                                    Data5$DINSi, Data5$PSi, Data5$VSS, Data5$CHLcal) ~ Data5$LOCATCD + Data5$YEAR, FUN=mean)
names(AvgByYr_withChlVSS)
colnames(AvgByYr_withChlVSS) <- c("LOCATCD", "Year", "AvguMSi", "AvgNH4", "AvgSRP", "AvgNOX", 
                                  "AvgDIN", "AvgDIN_SiRatio", "AvgP_SiRatio", "AvgVSS", "AvgChl")
Data6<-AvgByYr_withChlVSS[order(AvgByYr_withChlVSS$LOCATCD),]


###===================================
# looping Chl a vs. consitutents at each site
##====================================
range(Data6$AvgChl, na.rm=T)
range(Data6$AvguMSi)
names(Data6)
range(Data5$VSS, na.rm=T)

#Average Chl a vs. DSi at each site
df=NULL
plot.path=paste0(getwd(),"/img/")
jpeg(filename=paste0(plot.path, "AvgDSivsChl_5.16.19b.jpeg"), width=7, height=7, units="in", res=200)

par(mfrow=c(4,7), mar=c(2.9,2.8,2,1.7))
for(i in unique(Data6$LOCATCD)){
  plot(Data6[Data6$LOCATCD==i, "AvgChl"], Data6[Data6$LOCATCD==i, "AvguMSi"],
       xlim=c(0, 35), ylim=c(0,300), main=paste("Site", i), ylab="DSi uM", 
       mgp=c(1.6,.1,0), las=1, xlab="Chl a", xaxt="n", yaxt="n", cex.main=.9)
  axis(side=1, at = c(0, 10, 20, 30), tcl=-.4, cex.axis=.8, mgp=c(0,.3,0), font.axis=1)
  axis(side=2, at = c(0,100, 200, 300), tcl=-.3, cex.axis=.8, mgp=c(0, .5,0), font.axis=1, las=1)
  abline(lm(Data6[Data6$LOCATCD==i, "AvguMSi"]~ Data6[Data6$LOCATCD==i, "AvgChl"]))
  df=rbind(df,data.frame(glance(lm(Data6[Data6$LOCATCD==i, "AvguMSi"]~ Data6[Data6$LOCATCD==i, "AvgChl"]))))
}
dev.off()

write.csv(df, file="R2_DIN_SiRatiosvsTime_5.15.19.csv", row.names=TRUE, na = "NA")








##=======================================================
##below is me f-ing around...
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



##Switched ratios around - not using these any longer
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

