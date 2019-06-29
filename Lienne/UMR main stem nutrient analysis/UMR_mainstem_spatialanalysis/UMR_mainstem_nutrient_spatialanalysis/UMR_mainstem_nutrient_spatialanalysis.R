#add lat/lon coordinates by UMR LOCATCD


#get Si, Si:TN, and Si:TP parameters from fixed summary stats
UMR_mainstem_Si = subset(Fixed_summarystats_2010_2018, Fixed_summarystats_2010_2018$parameter=="SI")
UMR_mainstem_SiTN = subset(Fixed_summarystats_2010_2018, Fixed_summarystats_2010_2018$parameter=="SiTN")
UMR_mainstem_SiTP = subset(Fixed_summarystats_2010_2018, Fixed_summarystats_2010_2018$parameter=="SiTP")

