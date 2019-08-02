GEMS_GLORI_dNut = data.frame("Cont"=GEMS_GLORI$Cont.,
                             "Si"=GEMS_GLORI$`SiO2 mg/L`,
                             "NO3"=GEMS_GLORI$`N-NO3 mg/L`,
                             "PO4"=GEMS_GLORI$`P-PO4¯³ mg/L`)
GEMS_GLORI_dNut = na.omit(GEMS_GLORI_dNut)

#calculate TN and TP using regression equations from Garnier 2010
GEMS_GLORI_dNut$TN = 1.2*(GEMS_GLORI_dNut$NO3)+0.1
GEMS_GLORI_dNut$TP = 1.5*(GEMS_GLORI_dNut$PO4)+0.1

#calculate molar ratios
GEMS_GLORI_dNut$SiTN = (GEMS_GLORI_dNut$Si/60084)/(GEMS_GLORI_dNut$TN/14007)
GEMS_GLORI_dNut$SiTP = (GEMS_GLORI_dNut$Si/60084)/(GEMS_GLORI_dNut$TP/30974)

#get range of Si:TN and Si:TP
range(GEMS_GLORI_dNut$SiTN)
range(GEMS_GLORI_dNut$SiTP)

#calculate percentile of UMR Si stoich within GEMS-GLORI dataset
Si_stoich_range = data.frame("SiTNmin_UMR"= 0.61,
                             "SiTNmax_UMR"=1.84,
                             "SiTPmin_UMR"=11.65,
                             "SiTPmax_UMR"=88.02,
                             "SiTNmin_GG"=0.06,
                             "SiTNmax_GG"=31.5,
                             "SiTPmin_GG"=0.36,
                             "SiTPmax_GG"=129.46)
GG_percentile = data.frame("SiTN"=sort(GEMS_GLORI_dNut$SiTN),
                           "SiTP"=sort(GEMS_GLORI_dNut$SiTP))
