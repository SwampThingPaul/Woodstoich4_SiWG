#separate Si, Si:TN, Si:TP from summary stat files
UMR_mainstem = SRS_summarystats_main_2010_2018
UMR_main = UMR_mainstem[c("parameter", "FLDNUM", "mean.val")]
UMR_main$site_type = "main"
UMR_main$medianRM = ifelse(UMR_main$FLDNUM==1, 1247.5,
       ifelse(UMR_main$FLDNUM==2, 1112,
              ifelse(UMR_main$FLDNUM==3, 868.5,
                     ifelse(UMR_main$FLDNUM==4, 358, 88))))
UMR_main_Si = subset(UMR_main, UMR_main$parameter=="SI")
UMR_main_Si$MSi = (UMR_main_Si$mean.val/28.086)*1000
UMR_main_SiTN = subset(UMR_main, UMR_main$parameter=="SiTN")
UMR_main_SiTP = subset(UMR_main, UMR_main$parameter=="SiTP")
UMR_main_TN = subset(UMR_main, UMR_main$parameter=="TN")
UMR_main_TN$MTN = (UMR_main_TN$mean.val/14.007)*1000
UMR_main_TP = subset(UMR_main, UMR_main$parameter=="TP")
UMR_main_TP$MTP = (UMR_main_TP$mean.val/30.974)*1000

UMR_trib = Fixed_summarystats_trib_2010_2018[c("parameter", "FLDNUM", "mean.val")]
UMR_trib$site_type = "trib"
UMR_trib_Si$MSi = (UMR_trib_Si$mean.val/28.086)*1000
UMR_trib$medianRM = ifelse(UMR_trib$FLDNUM==1, 1247.5,
                           ifelse(UMR_trib$FLDNUM==2, 1112,
                                  ifelse(UMR_trib$FLDNUM==3, 868.5,
                                         ifelse(UMR_trib$FLDNUM==4, 358, 88))))
UMR_trib_Si = subset(UMR_trib, UMR_trib$parameter=="SI")
UMR_trib_TN = subset(UMR_trib, UMR_trib$parameter=="TN")
UMR_trib_TN$MTN = (UMR_trib_TN$mean.val/14.007)*1000
UMR_trib_TP = subset(UMR_trib, UMR_trib$parameter=="TP")
UMR_trib_TP$MTP = (UMR_trib_TP$mean.val/30.974)*1000
UMR_trib_SiTN = subset(UMR_trib, UMR_trib$parameter=="SiTN")
UMR_trib_SiTP = subset(UMR_trib, UMR_trib$parameter=="SiTP")

#merge main and tribs for each parameter
UMR_Si = merge(UMR_main_Si, UMR_trib_Si, all=T)
UMR_SiTN = merge(UMR_main_SiTN, UMR_trib_SiTN, all=T)
UMR_SiTP = merge(UMR_main_SiTP, UMR_trib_SiTP, all=T)

UMR_Si$MSi = (UMR_Si$mean.val/28.086)*1000

#plot mainstem spatial changes
par(mfrow = c(5,1), oma = c(5,5,5,0.25), mar = c(2,4,0,0))
plot(UMR_main_Si$MSi~UMR_main_Si$medianRM,
     xaxt='n', xlim=c(1300,80),
     yaxt='n', ylim=c(80,240), 
     pch=16,
     ylab="", xlab="")
axis(2,at=c(100,140,180,220),labels=c(100,140,180,220),lty=1,col="black", las=2, cex.axis=1.5)
mtext(side=2,line=4.2, cex=1.5,"Dissolved Si (\U03BCM)")
UMR_main_Silm = lm(MSi~medianRM,UMR_main_Si)
Si_mod = summary(UMR_main_Silm)
UMR_main_Siconf = predict(UMR_main_Silm, data.frame(medianRM=seq(80,1300,1)),interval="confidence")
lines(seq(80,1300,1),UMR_main_Siconf[,1])
lines(seq(80,1300,1),UMR_main_Siconf[,2],lty=2)
lines(seq(80,1300,1),UMR_main_Siconf[,3],lty=2)
Si.r2 = Si_mod$r.squared
Silabel = bquote(italic(R)^2 == .(format(Si.r2, digits=3, nsmall=3)))
text(x=250, y=200, cex=2, labels = Silabel)

plot(UMR_main_TN$MTN~UMR_main_TN$medianRM,
     xaxt='n', xlim=c(1300,80),
     cex.axis=1.5, las=2,
     pch=16,
     ylab="", xlab="")
mtext(side=2,line=4.2, cex=1.5 ,"Total N (\U03BCM)")
UMR_main_TNlm = lm(MTN~medianRM,UMR_main_TN)
TN_mod = summary(UMR_main_TNlm)
UMR_main_TNconf = predict(UMR_main_TNlm, data.frame(medianRM=seq(80,1300,1)),interval="confidence")
lines(seq(80,1300,1),UMR_main_TNconf[,1])
lines(seq(80,1300,1),UMR_main_TNconf[,2],lty=2)
lines(seq(80,1300,1),UMR_main_TNconf[,3],lty=2)
TN.r2 = TN_mod$r.squared
TNlabel = bquote(italic(R)^2 == .(format(TN.r2, digits=3, nsmall=3)))
text(x=1200, y=250, cex=2, labels = TNlabel)

plot(UMR_main_TP$MTP~UMR_main_TP$medianRM,
     xaxt='n', xlim=c(1300,80),
     cex.axis=1.5, las=2,
     pch=16,
     ylab="", xlab="")
mtext(side=2,line=4.2, cex=1.5, "Total P (\U03BCM)")
UMR_main_TPlm = lm(MTP~medianRM,UMR_main_TP)
TP_mod = summary(UMR_main_TPlm)
UMR_main_TPconf = predict(UMR_main_TPlm, data.frame(medianRM=seq(80,1300,1)),interval="confidence")
lines(seq(80,1300,1),UMR_main_TPconf[,1])
lines(seq(80,1300,1),UMR_main_TPconf[,2],lty=2)
lines(seq(80,1300,1),UMR_main_TPconf[,3],lty=2)
TP.r2 = TP_mod$r.squared
TPlabel = bquote(italic(R)^2 == .(format(TP.r2, digits=3, nsmall=3)))
text(x=1200, y=10, cex=2, labels = TPlabel)

plot(UMR_main_SiTN$mean.val~UMR_main_SiTN$medianRM,
     xaxt='n', xlim=c(1300,80),
     cex.axis=1.5, las=2,
     ylab="",
     pch=16)
mtext(side=2,line=4.2, cex=1.5, "Molar Si:TN")
UMR_main_SiTNlm = lm(mean.val~medianRM,UMR_main_SiTN)
SiTN_mod=summary(UMR_main_SiTNlm)
UMR_main_SiTNconf = predict(UMR_main_SiTNlm, data.frame(medianRM=seq(80,1300,1)),interval="confidence")
abline(h=1, col="red", lty=2)
lines(seq(80,1300,1),UMR_main_SiTNconf[,1])
lines(seq(80,1300,1),UMR_main_SiTNconf[,2],lty=2)
lines(seq(80,1300,1),UMR_main_SiTNconf[,3],lty=2)
SiTN.r2 = SiTN_mod$r.squared
SiTNlabel = bquote(italic(R)^2 == .(format(SiTN.r2, digits = 3)))
text(x=250, y=1.6, cex=2, labels = SiTNlabel)

plot(UMR_main_SiTP$mean.val~UMR_main_SiTP$medianRM,
     xlim=c(1300,80),
     cex.axis=1.5, las=1,
     ylab="",
     pch=16)
mtext(side=2,line=4.2, cex=1.5, "Molar Si:TP")
mtext(side=1,line=4,cex=1.5, "Median River Kilometer")
UMR_main_SiTPlm = lm(mean.val~medianRM,UMR_main_SiTP)
SiTP_mod=summary(UMR_main_SiTPlm)
UMR_main_SiTPconf = predict(UMR_main_SiTPlm, data.frame(medianRM=seq(80,1300,1)),interval="confidence")
abline(h=16, col="red", lty=2)
lines(seq(80,1300,1),UMR_main_SiTPconf[,1])
lines(seq(80,1300,1),UMR_main_SiTPconf[,2],lty=2)
lines(seq(80,1300,1),UMR_main_SiTPconf[,3],lty=2)
SiTP.r2 = SiTP_mod$r.squared
SiTPlabel = bquote(italic(R)^2 == .(format(SiTP.r2, digits = 3)))
text(x=250, y=70, cex=2, labels = SiTPlabel)

mtext("Mainstem",3,1,cex=1.5,outer=T)

#plot tributary spatial
plot(UMR_trib_Si$MSi~UMR_trib_Si$medianRM,
     xaxt='n', xlim=c(1300,80),
     yaxt='n',
     pch=16,
     ylab="", xlab="")
axis(2,at=c(100,140,180,220),labels=c(100,140,180,220),lty=1,col="black", las=2, cex.axis=1.5)
mtext(side=2,line=4.2, cex=1.5,"Dissolved Si (\U03BCM)")
UMR_trib_Silm = lm(MSi~medianRM,UMR_trib_Si)
Si_tribmod = summary(UMR_trib_Silm)
UMR_trib_Siconf = predict(UMR_trib_Silm, data.frame(medianRM=seq(80,1300,1)),interval="confidence")
lines(seq(80,1300,1),UMR_trib_Siconf[,1])
lines(seq(80,1300,1),UMR_trib_Siconf[,2],lty=2)
lines(seq(80,1300,1),UMR_trib_Siconf[,3],lty=2)
Si.tribr2 = Si_tribmod$r.squared
Sitrib.label = bquote(italic(R)^2 == .(format(Si.tribr2, digits=3, nsmall=3)))
text(x=250, y=200, cex=2, labels = Sitrib.label)

plot(UMR_trib_TN$MTN~UMR_trib_TN$medianRM,
     xaxt='n', xlim=c(1300,80),
     cex.axis=1.5, las=2,
     pch=16,
     ylab="", xlab="")
mtext(side=2,line=4.2, cex=1.5 ,"Total N (\U03BCM)")
UMR_trib_TNlm = lm(MTN~medianRM,UMR_trib_TN)
TN_tribmod = summary(UMR_trib_TNlm)
UMR_trib_TNconf = predict(UMR_trib_TNlm, data.frame(medianRM=seq(80,1300,1)),interval="confidence")
lines(seq(80,1300,1),UMR_trib_TNconf[,1])
lines(seq(80,1300,1),UMR_trib_TNconf[,2],lty=2)
lines(seq(80,1300,1),UMR_trib_TNconf[,3],lty=2)
TN.tribr2 = TN_tribmod$r.squared
#TNlabel = bquote(italic(R)^2 == .(format(TN.tribr2, digits=3, nsmall=3)))
#3 sig figs is 0, just use 0.000
TNtrib.label=bquote(italic(R)^2 == "0.000")
text(x=1200, y=500, cex=2, labels = TNtrib.label)

plot(UMR_trib_TP$MTP~UMR_trib_TP$medianRM,
     xaxt='n', xlim=c(1300,80),
     cex.axis=1.5, las=2,
     pch=16,
     ylab="", xlab="")
mtext(side=2,line=4.2, cex=1.5, "Total P (\U03BCM)")
UMR_trib_TPlm = lm(MTP~medianRM,UMR_trib_TP)
TP_tribmod = summary(UMR_trib_TPlm)
UMR_trib_TPconf = predict(UMR_trib_TPlm, data.frame(medianRM=seq(80,1300,1)),interval="confidence")
lines(seq(80,1300,1),UMR_trib_TPconf[,1])
lines(seq(80,1300,1),UMR_trib_TPconf[,2],lty=2)
lines(seq(80,1300,1),UMR_trib_TPconf[,3],lty=2)
TP.tribr2 = TP_tribmod$r.squared
TPtrib.label = bquote(italic(R)^2 == .(format(TP.tribr2, digits=3, nsmall=3)))
text(x=1200, y=15, cex=2, labels = TPtrib.label)

plot(UMR_trib_SiTN$mean.val~UMR_trib_SiTN$medianRM,
     xaxt='n', xlim=c(1300,80),
     cex.axis=1.5, las=2,
     ylab="",
     pch=16)
mtext(side=2,line=4.2, cex=1.5, "Molar Si:TN")
UMR_trib_SiTNlm = lm(mean.val~medianRM,UMR_trib_SiTN)
SiTN_tribmod=summary(UMR_trib_SiTNlm)
UMR_trib_SiTNconf = predict(UMR_trib_SiTNlm, data.frame(medianRM=seq(80,1300,1)),interval="confidence")
abline(h=1, col="red", lty=2)
lines(seq(80,1300,1),UMR_trib_SiTNconf[,1])
lines(seq(80,1300,1),UMR_trib_SiTNconf[,2],lty=2)
lines(seq(80,1300,1),UMR_trib_SiTNconf[,3],lty=2)
SiTN.tribr2 = SiTN_tribmod$r.squared
#SiTNtrib.label = bquote(italic(R)^2 == .(format(SiTN.tribr2, digits = 3, nsmall=3)))
#force to 3 decimal places
SiTNtrib.label = bquote(italic(R)^2 == "0.014")
text(x=250, y=2, cex=2, labels = SiTNtrib.label)

plot(UMR_trib_SiTP$mean.val~UMR_trib_SiTP$medianRM,
     xlim=c(1300,80),
     cex.axis=1.5, las=1,
     ylab="",
     pch=16)
mtext(side=2,line=4.2, cex=1.5, "Molar Si:TP")
mtext(side=1,line=4, cex=1.5, "Median River Kilometer")
UMR_trib_SiTPlm = lm(mean.val~medianRM,UMR_trib_SiTP)
SiTP_tribmod=summary(UMR_trib_SiTPlm)
UMR_trib_SiTPconf = predict(UMR_trib_SiTPlm, data.frame(medianRM=seq(80,1300,1)),interval="confidence")
abline(h=16, col="red", lty=2)
lines(seq(80,1300,1),UMR_trib_SiTPconf[,1])
lines(seq(80,1300,1),UMR_trib_SiTPconf[,2],lty=2)
lines(seq(80,1300,1),UMR_trib_SiTPconf[,3],lty=2)
SiTP.tribr2 = SiTP_tribmod$r.squared
SiTPtrib.label = bquote(italic(R)^2 == .(format(SiTP.tribr2, digits = 3)))
text(x=250, y=90, cex=2, labels = SiTPtrib.label)

mtext("Tributaries",3,1,cex=1.5,outer=T)
