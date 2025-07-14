library(pacman)
pacman::p_load(here, readxl,lubridate,stats,graphics,Hmisc,data.table,utils,mgcv,dplyr,purrr,ecodata,gridExtra) 
here()
#### load .csv files #####
Cod_distribution<-read.csv(here("data/Cod_distribution.csv"))
Cod_distribution$COG_depth_fall<-abs(Cod_distribution$COG_depth_fall)
Cod_distribution$COG_depth_spring<-abs(Cod_distribution$COG_depth_spring)
wmfall_ALL<-read.csv(here("data/depth_lat/wmfall_ALL.csv"))
wmspring_ALL<-read.csv(here("data/depth_lat/wmspring_ALL.csv"))

###clip to years with most data###
Cod_distribution = Cod_distribution[!Cod_distribution$Year > 2019,]
Cod_distribution = Cod_distribution[!Cod_distribution$Year < 1982,]

#####PLOT fall comparisons DEPTH#####
layout(matrix(1:2, ncol=2, byrow=FALSE))
par(mar=c(4.1,4.5,1.5,1), oma=c(1.0,0,1.0,0.1))

plot(COG_depth_fall ~ Year, data=Cod_distribution, main="Mean Fall Depth of Cod Occurance Overtime", xlab="Year",ylab="Mean Depth (m)", type="l",lwd=3,col="#00608A",ylim= c(130,55), cex.lab=1.4,cex.axis=1.1)
abline(lm(Cod_distribution$COG_depth_fall~Cod_distribution$Year),col="#00608A",lwd=2.5,lty=2)

lines(adult_depth ~ Year, data=wmfall_ALL, xlab="Year", type="l",col="#EA4F12",lwd=3)
abline(lm(wmfall_ALL$adult_depth~wmfall_ALL$Year),col="#EA4F12",lwd=2.5,lty=2)

lines(juv_depth ~ Year, data=wmfall_ALL, xlab="Year", type="l",col="#407331",lwd=3)
abline(lm(wmfall_ALL$juv_depth~wmfall_ALL$Year),col="#407331",lwd=2.5,lty=2)


legend(2001, 115, legend=c("All Stock Areas: Adult","DisMAP all Ages & Areas", "All Stock Areas: Juveniles"),
       col=c("#EA4F12", "#00608A","#407331"), lty=1,lwd=3, cex=0.7)

#####PLOT Spring comparisons DEPTH#####
plot(COG_depth_spring ~ Year, data=Cod_distribution, main="Mean Spring Depth of Cod Occurance Overtime", xlab="Year",ylab="Mean Depth (m)", type="l",lwd=3,col="#00608A",ylim= c(130,55), cex.lab=1.4,cex.axis=1.1)
abline(lm(Cod_distribution$COG_depth_spring~Cod_distribution$Year),col="#00608A",lwd=2.5,lty=2)

lines(adult_depth ~ Year, data=wmspring_ALL, xlab="Year", type="l",col="#EA4F12",lwd=3)
abline(lm(wmspring_ALL$adult_depth~wmspring_ALL$Year),col="#EA4F12",lwd=2.5,lty=2)

lines(juv_depth ~ Year, data=wmspring_ALL, xlab="Year", type="l",col="#407331",lwd=3)
abline(lm(wmspring_ALL$juv_depth~wmspring_ALL$Year),col="#407331",lwd=2.5,lty=2)


legend(2005, 115, legend=c("All Stock Areas: Adult","DisMAP all Ages & Areas", "All Stock Areas: Juveniles"),
       col=c("#EA4F12", "#00608A","#407331"), lty=1,lwd=3, cex=0.7)

####################################################################
#PLOT Seasonal Latitude changes together on same plot
layout(matrix(1:2, ncol=2, byrow=FALSE))
par(mar=c(4.1,4.5,1.5,1), oma=c(1.0,0,1.0,0.1))
plot(COG_Lat_fall ~ Year, data=Cod_distribution, main="Mean Fall Latitude of Cod Occurance Overtime", xlab="Year",ylab="Mean Latitude (km)", type="l",lwd=3,col="#00608A", cex.lab=1.4,cex.axis=1.1,ylim= c(41.6,42.8))
abline(lm(Cod_distribution$COG_Lat_fall~Cod_distribution$Year),col="#00608A",lwd=2.5,lty=2)

lines(adult_lat ~ Year, data=wmfall_ALL, xlab="Year", type="l",col="#EA4F12",lwd=3)
abline(lm(wmfall_ALL$adult_lat~wmfall_ALL$Year),col="#EA4F12",lwd=2.5,lty=2)

lines(juv_lat ~ Year, data=wmfall_ALL, xlab="Year", type="l",col="#407331",lwd=3)
abline(lm(wmfall_ALL$juv_lat~wmfall_ALL$Year),col="#407331",lwd=2.5,lty=2)

legend(2005, 42.8, legend=c("All Stock Areas: Adult","DisMAP all Ages & Areas", "All Stock Areas: Juveniles"),
       col=c("#EA4F12", "#00608A","407331"), lty=1,lwd=3, cex=0.7)
###spring latitude
plot(COG_Lat_spring ~ Year, data=Cod_distribution, main="Mean Spring Latitude of Cod Occurance Overtime", xlab="Year",ylab="Mean Latitude (km)", type="l",lwd=3,col="#00608A", cex.lab=1.4,cex.axis=1.1,ylim= c(41.6,42.8))
abline(lm(Cod_distribution$COG_Lat_spring~Cod_distribution$Year),col="#00608A",lwd=2.5,lty=2)

lines(adult_lat ~ Year, data=wmspring_ALL, xlab="Year", type="l",col="#EA4F12",lwd=3)
abline(lm(wmspring_ALL$adult_lat~wmspring_ALL$Year),col="#EA4F12",lwd=2.5,lty=2)

lines(juv_lat ~ Year, data=wmspring_ALL, xlab="Year", type="l",col="#407331",lwd=3)
abline(lm(wmspring_ALL$juv_lat~wmspring_ALL$Year),col="#407331",lwd=2.5,lty=2)

legend(2005, 42.8, legend=c("All Stock Areas: Adult","DisMAP all Ages & Areas", "All Stock Areas: Juveniles"),
       col=c("#EA4F12", "#00608A","407331"), lty=1,lwd=3, cex=0.7)
#####PLOT Spring/fall DISMAP DEPTH#####
layout(matrix(1:2, ncol=2, byrow=FALSE))
par(mar=c(4.1,4.5,1.5,1), oma=c(1.0,0,1.0,0.1))

plot(COG_depth_fall ~ Year, data=Cod_distribution, main="Mean Depth of Cod Occurance Overtime", xlab="Year",ylab="Mean Depth (m)", type="l",lwd=3,col="#00608A",ylim= c(130,85), cex.lab=1.4,cex.axis=1.1)

lines(COG_depth_spring ~ Year, data=Cod_distribution, xlab="Year", type="l",col="#EA4F12",lwd=3)

legend("bottomright",inset = 0.03, legend=c("Fall", "Spring"),
       col=c("#00608A","#EA4F12"), lty=1,lwd=3, cex=0.9, title = "Season")

#####PLOT spring fall DISMAP latitude#####
plot(COG_Lat_fall ~ Year, data=Cod_distribution, main="Mean Latitude of Cod Occurance Overtime", xlab="Year",ylab="Mean Latitude (°)", type="l",lwd=3,col="#00608A",ylim= c(41.8,42.7), cex.lab=1.4,cex.axis=1.1)

lines(COG_Lat_spring ~ Year, data=Cod_distribution, xlab="Year", type="l",col="#EA4F12",lwd=3)

legend("bottomright",inset = 0.03, legend=c("Fall", "Spring"),
       col=c("#00608A","#EA4F12"), lty=1,lwd=3, cex=0.9, title = "Season")
