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

UMR_trib = Fixed_summarystats_trib_2010_2018[c("parameter", "FLDNUM", "mean.val")]
UMR_trib$site_type = "trib"
UMR_trib_Si$MSi = (UMR_trib_Si$mean.val/28.086)*1000
UMR_trib$medianRM = ifelse(UMR_trib$FLDNUM==1, 1247.5,
                           ifelse(UMR_trib$FLDNUM==2, 1112,
                                  ifelse(UMR_trib$FLDNUM==3, 868.5,
                                         ifelse(UMR_trib$FLDNUM==4, 358, 88))))
UMR_trib_Si = subset(UMR_trib, UMR_trib$parameter=="SI")
UMR_trib_SiTN = subset(UMR_trib, UMR_trib$parameter=="SiTN")
UMR_trib_SiTP = subset(UMR_trib, UMR_trib$parameter=="SiTP")

#merge main and tribs for each parameter
UMR_Si = merge(UMR_main_Si, UMR_trib_Si, all=T)
UMR_SiTN = merge(UMR_main_SiTN, UMR_trib_SiTN, all=T)
UMR_SiTP = merge(UMR_main_SiTP, UMR_trib_SiTP, all=T)

UMR_Si$MSi = (UMR_Si$mean.val/28.086)*1000

#plot
par(mfrow = c(3,1), oma = c(5,2,2,0.25), mar = c(0.5,4,0,0))
plot(UMR_main_Si$MSi~UMR_main_Si$medianRM,
     xaxt='n', xlim=c(1300,80),
     cex.axis=1.5, las=2,
     ylab="")
mtext(side=2,line=4,"Average DSi (\U03BCM)")
UMR_main_Silm = lm(MSi~medianRM,UMR_main_Si)
UMR_main_Siconf = predict(UMR_main_Silm, data.frame(medianRM=seq(80,1300,1)),interval="confidence")
lines(seq(80,1300,1),UMR_main_Siconf[,1])
lines(seq(80,1300,1),UMR_main_Siconf[,2],lty=2)
lines(seq(80,1300,1),UMR_main_Siconf[,3],lty=2)

plot(UMR_main_SiTN$mean.val~UMR_main_SiTN$medianRM,
     xaxt='n', xlim=c(1300,80),
     cex.axis=1.5, las=2,
     ylab="")
mtext(side=2,line=4,"Average Molar Si:TN")
UMR_main_SiTNlm = lm(mean.val~medianRM,UMR_main_SiTN)
UMR_main_SiTNconf = predict(UMR_main_SiTNlm, data.frame(medianRM=seq(80,1300,1)),interval="confidence")
lines(seq(80,1300,1),UMR_main_SiTNconf[,1])
lines(seq(80,1300,1),UMR_main_SiTNconf[,2],lty=2)
lines(seq(80,1300,1),UMR_main_SiTNconf[,3],lty=2)

plot(UMR_main_SiTP$mean.val~UMR_main_SiTP$medianRM,
     xlim=c(1300,80),
     cex.axis=1.5, las=1,
     ylab="")
mtext(side=2,line=4,"Average Molar Si:TP")
mtext(side=1,line=3, "Median River Kilometer")
UMR_main_SiTPlm = lm(mean.val~medianRM,UMR_main_SiTP)
UMR_main_SiTPconf = predict(UMR_main_SiTPlm, data.frame(medianRM=seq(80,1300,1)),interval="confidence")
lines(seq(80,1300,1),UMR_main_SiTPconf[,1])
lines(seq(80,1300,1),UMR_main_SiTPconf[,2],lty=2)
lines(seq(80,1300,1),UMR_main_SiTPconf[,3],lty=2)
