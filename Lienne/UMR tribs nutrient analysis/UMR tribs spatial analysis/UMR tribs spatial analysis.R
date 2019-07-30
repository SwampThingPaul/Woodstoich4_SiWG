#Plot tributary Si, Si:TN, and Si:TP by FLDNUM
library(ggplot2)
library(ggpmisc)
UMR_trib_SiLM = UMR_trib_Si$mean.val ~ UMR_trib_Si$FLDNUM
UMR_trib_Si_FLDNUM = ggplot(UMR_trib_Si, aes(x=FLDNUM, y=mean.val))+
  geom_point(size=3)+
  labs(title="UMR Tributaries",y="[Si] (mg Si/L)", x="")+
  scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6"))+
  geom_smooth(method='lm', se=TRUE, color="black")+
  stat_poly_eq(formula=UMR_trib_SiLM, 
               aes(label=paste(..rr.label..)),
               label.x=0.85,
               size=10,
               parse = TRUE)+
  theme_bw()+
  theme(text=element_text(size=30),
        axis.text.x=element_text(size=rel(1)),
        axis.text.y=element_text(size=rel(1)))
UMR_trib_Si_FLDNUM

UMR_trib_SiTNLM = UMR_trib_SiTN$mean.val ~ UMR_trib_SiTN$FLDNUM
UMR_trib_SiTN_FLDNUM = ggplot(UMR_trib_SiTN, aes(x=FLDNUM, y=mean.val))+
  geom_point(size=3)+
  labs(y="Molar Si:TN", x="")+
  scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6"))+
  geom_smooth(method='lm', se=TRUE, color="black")+
  geom_hline(yintercept=1, linetype="dashed")+
  stat_poly_eq(formula=UMR_trib_SiTNLM, 
               aes(label=paste(..rr.label..)),
               label.x=0.85,
               size=10,
               parse = TRUE)+
  theme_bw()+
  theme(text=element_text(size=30),
        axis.text.x=element_text(size=rel(1)),
        axis.text.y=element_text(size=rel(1)))
UMR_trib_SiTN_FLDNUM

UMR_trib_SiTPLM = UMR_trib_SiTP$mean.val ~ UMR_trib_SiTP$FLDNUM
UMR_trib_SiTP_FLDNUM = ggplot(UMR_trib_SiTP, aes(x=FLDNUM, y=mean.val))+
  geom_point(size=3)+
  labs(y="Molar Si:TP", x="Field Number")+
  scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6"))+
  geom_smooth(method='lm', se=TRUE, color="black")+
  geom_hline(yintercept=16, linetype="dashed")+
  stat_poly_eq(formula=UMR_trib_SiTPLM, 
               aes(label=paste(..rr.label..)),
               label.x=0.85,
               size=10,
               parse = TRUE)+
  theme_bw()+
  theme(text=element_text(size=30),
        axis.text.x=element_text(size=rel(1)),
        axis.text.y=element_text(size=rel(1)))
UMR_trib_SiTP_FLDNUM

library(gridExtra)
grid.arrange(UMR_trib_Si_FLDNUM, UMR_trib_SiTN_FLDNUM, UMR_trib_SiTP_FLDNUM)
