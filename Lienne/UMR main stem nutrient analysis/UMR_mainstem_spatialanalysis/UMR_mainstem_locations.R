#remove duplicate LOCATCDs for main stem sites
UMR_mainstem_locat = data.frame("LOCATCD"=UMR_Mainstem_FixedSite_1991_2018$LOCATCD,
                                "northing"=UMR_Mainstem_FixedSite_1991_2018$NORTHING,
                                "easting"=UMR_Mainstem_FixedSite_1991_2018$EASTING)
UMR_mainstem_locat=UMR_mainstem_locat[!duplicated(UMR_mainstem_locat$LOCATCD), ]

#convert northing and easting to lat lon coords using Paul's code:
#==================================
library(sp)
library(rgdal)

utm15=CRS("+proj=utm +zone=15 +datum=WGS84 +units=m")
wgs84=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# convert to shapefile
sp.mainstem.names=SpatialPointsDataFrame(coords=UMR_mainstem_locat[,c("easting","northing")],data=UMR_mainstem_locat,proj4string = utm15)
plot(sp.mainstem.names)
sp.mainstem.names=spTransform(sp.mainstem.names,wgs84)

UMR_mainstem_coords=data.frame(LOCATCD=sp.mainstem.names@data$LOCATCD, Long=coordinates(sp.mainstem.names)[,1],Lat=coordinates(sp.mainstem.names)[,2])
write.csv(UMR_mainstem_coords, file="UMR_mainstem_coordinates.csv",row.names=F)
#===================================