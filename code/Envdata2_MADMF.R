###### Atlantic Cod stock assessment data GAM work
library(pacman)
pacman::p_load(here, readxl,lubridate,stats,graphics,Hmisc,data.table,utils,mgcv,dplyr,purrr,ecodata,kableExtra,gridExtra,rlang) 
here()

#########################################################Bottom Temperature ####
Hubert_WGOM_bt<-read.csv(here("data/stock_area_data/hubert_monthly/Hubert_WGOM_bt.csv"))
#get 6-month mean values for each year (6-months prior to start of each seasonal survey)
#### Fall #####
####WGOM (March-August means)
Hubert_WGOM_bt_FL <-subset(Hubert_WGOM_bt , month >= 3 & month <= 8)
Hubert_WGOM_bt_FL <- aggregate(bt_temp~year, data=Hubert_WGOM_bt_FL,FUN=mean)
#get anomaly period means (1982-2011)
bt_fall_bp_wgom<-mean(Hubert_WGOM_bt_FL[5:34,2])
#get final bt dataset (years 1982-2020, anomaly adjusted)
Hubert_WGOM_bt_FL$bt_anomaly<- Hubert_WGOM_bt_FL$bt_temp  - bt_fall_bp_wgom
Hubert_WGOM_bt_FL <-Hubert_WGOM_bt_FL[5:43,c(1,3)]
names(Hubert_WGOM_bt_FL)[1]<-"Year"
rm(bt_fall_bp_wgom)
#### Spring #####
#get 6-month mean values for each year (6-months prior to start of each seasonal survey)
####WGOM (November(year-1)-April (year) means)
Hubert_WGOM_bt_SP <-subset(Hubert_WGOM_bt , month==11|month==12|month==1|month==2|month==3|month==4)
#aggregating correct months from each year
Hubert_WGOM_bt_SP$newlag <- ifelse(Hubert_WGOM_bt_SP$month==1|Hubert_WGOM_bt_SP$month==2|Hubert_WGOM_bt_SP$month==3|Hubert_WGOM_bt_SP$month==4,Hubert_WGOM_bt_SP$year,Hubert_WGOM_bt_SP$year+1)
Hubert_WGOM_bt_SP <- aggregate(bt_temp~newlag, data=Hubert_WGOM_bt_SP,FUN=mean)
#get anomaly period means (1982-2011)
bt_spring_bp_wgom<-mean(Hubert_WGOM_bt_SP[5:34,2])
#get final bt dataset (years 1982-2020, anomaly adjusted)
Hubert_WGOM_bt_SP$bt_anomaly<- Hubert_WGOM_bt_SP$bt_temp  - bt_spring_bp_wgom
Hubert_WGOM_bt_SP <-Hubert_WGOM_bt_SP[5:43,c(1,3)]
names(Hubert_WGOM_bt_SP)[1]<-"Year"
rm(Hubert_WGOM_bt,bt_spring_bp_wgom)

###################################################################### SST ####
load(here("data/cod_sst.rda"))
cod_sst <- subset(cod_sst, Year<2021&Year>1977&INDICATOR_NAME=="temperature") 
sst_WGOM<-cod_sst[cod_sst$Region == "WGOM",c(1,2,4)]
rm(cod_sst)
##### Recruitment Models #####
####get 4-month mean values for each year (4-months starting at peak spawning period by stock)####
####WGOM (May-August (year-1) means)####
sst_WGOM_recruitment_FL <-subset(sst_WGOM , Month >= 5 & Month <= 8)
sst_WGOM_recruitment_FL <- aggregate(DATA_VALUE~Year, data=sst_WGOM_recruitment_FL,FUN=mean)
####WGOM (November-February (year) means)####
sst_WGOM_recruitment_SP <-subset(sst_WGOM , Month==11|Month==12|Month==1|Month==2)
sst_WGOM_recruitment_SP$newlag <- ifelse(sst_WGOM_recruitment_SP$Month==1|sst_WGOM_recruitment_SP$Month==2,sst_WGOM_recruitment_SP$Year,sst_WGOM_recruitment_SP$Year+1)
sst_WGOM_recruitment_SP <- aggregate(DATA_VALUE~Year, data=sst_WGOM_recruitment_SP,FUN=mean)
#get anomaly period means (1982-2011)
sst_wgom_recruitment_bpFL<-mean(sst_WGOM_recruitment_FL[1:30,2])
sst_wgom_recruitment_bpSP<-mean(sst_WGOM_recruitment_SP[1:30,2])
#get final bt dataset (years 1982-2020, anomaly adjusted)
sst_WGOM_recruitment_FL$sst_anomaly<- sst_WGOM_recruitment_FL$DATA_VALUE-sst_wgom_recruitment_bpFL
sst_WGOM_recruitment_FL<-sst_WGOM_recruitment_FL[1:39,c(1,3)]

sst_WGOM_recruitment_SP$sst_anomaly<- sst_WGOM_recruitment_SP$DATA_VALUE-sst_wgom_recruitment_bpSP
sst_WGOM_recruitment_SP<-sst_WGOM_recruitment_SP[1:39,c(1,3)]
#lag 1 year
sst_WGOM_recruitment_FL$sst_anomaly<-lag(sst_WGOM_recruitment_FL$sst_anomaly)
sst_WGOM_recruitment_SP$sst_anomaly<-lag(sst_WGOM_recruitment_SP$sst_anomaly)
rm(sst_wgom_recruitment_bpFL,sst_wgom_recruitment_bpSP)
####################################################################### GSI ########
GSI<-ecodata::gsi
GSI<-GSI[325:804,c(1,3)]
names(GSI)[1]<-"Year"
GSI$month<-rep(1:12,times=40)
GSI$Year<-rep(1981:2020,each=12)
##### Recruitment Models #####
####get 6-month mean values for each year (6-months starting at peak spawning period by stock)####
####WGOM (March-Aug (year) means)####
gsi_WGOM_recruitment_FL <-subset(GSI , month >= 3 & month <= 8)
gsi_WGOM_recruitment_FL <- aggregate(Value~Year, data=gsi_WGOM_recruitment_FL,FUN=mean)
names(gsi_WGOM_recruitment_FL)[2]<-"GSI"
#### SPRING ####
####WGOM (November (year-1)-April (year) means)####
gsi_WGOM_GD_SP <-subset(GSI,month==11|month==12|month==1|month==2|month==3|month==4)
#aggregating correct months from each year
gsi_WGOM_GD_SP$newlag <- ifelse(gsi_WGOM_GD_SP$month==1|gsi_WGOM_GD_SP$month==2|gsi_WGOM_GD_SP$month==3|gsi_WGOM_GD_SP$month==4,gsi_WGOM_GD_SP$Year,gsi_WGOM_GD_SP$Year+1)
gsi_WGOM_GD_SP<-aggregate(Value~newlag, data=gsi_WGOM_GD_SP,FUN=mean)
#get final bt dataset (years 1982-2020)
gsi_WGOM_GD_SP$GSI<- gsi_WGOM_GD_SP$Value
gsi_WGOM_GD_SP<-gsi_WGOM_GD_SP[2:40,c(1,3)]
names(gsi_WGOM_GD_SP)[1]<-"Year"
rm(GSI)
######################################################## Cod Heatwave data ####
cod_heatwave<-as.data.frame(ecodata::ESP_heatwave_cod)
WGOM_chw<-cod_heatwave[(cod_heatwave$stock_id=="WGOM") & (cod_heatwave$Var=="cumulative intensity"), ]
names(WGOM_chw)[3] <- "WGOM_hw"
WGOM_chw <- WGOM_chw[, -c(2,4:5)]
#WGOM (not lagged)
hw_WGOM_FL<-WGOM_chw
hw_WGOM_FL$Heatwave<-WGOM_chw$WGOM_hw
names(hw_WGOM_FL)[1]<-"Year"
hw_WGOM_FL<-hw_WGOM_FL[, -c(2)]
#WGOM
hw_recruitment_WGOM_SP<-WGOM_chw
hw_recruitment_WGOM_SP$Heatwave<-lag(WGOM_chw$WGOM_hw)
names(hw_recruitment_WGOM_SP)[1]<-"Year"
hw_recruitment_WGOM_SP<-hw_recruitment_WGOM_SP[, -c(2)]
rm(cod_heatwave,WGOM_chw)
########################################################
##### zooplankton data#####
zoo_Winter_WGOM<-read.csv(here("data/zooplankton/WGOM_winter_zooplankton.csv"))
zoo_Summer_WGOM<-read.csv(here("data/zooplankton/WGOM_summer_zooplankton.csv"))

zoo_Winter_WGOM<-zoo_Winter_WGOM[c(2,9,10)]
names(zoo_Winter_WGOM)[1] <- "Year"
zoo_Winter_WGOM$calfin_100m3<-lag(zoo_Winter_WGOM$calfin_100m3)
zoo_Winter_WGOM$pseudo_100m3<-lag(zoo_Winter_WGOM$pseudo_100m3)
zoo_Winter_WGOM<-zoo_Winter_WGOM[5:35,]

zoo_Summer_WGOM<-zoo_Summer_WGOM[c(2,9,10)]
names(zoo_Summer_WGOM)[1] <- "Year"
zoo_Summer_WGOM$calfin_100m3<-lag(zoo_Summer_WGOM$calfin_100m3)
zoo_Summer_WGOM$pseudo_100m3<-lag(zoo_Summer_WGOM$pseudo_100m3)
zoo_Summer_WGOM<-zoo_Summer_WGOM[6:39,]
####Combine data into separate spring and fall data frames####
### WGOM Recruitment FALL ####
WGOM_recruitment_fall <- list(Hubert_WGOM_bt_FL,sst_WGOM_recruitment_FL,hw_WGOM_FL,gsi_WGOM_recruitment_FL,zoo_Summer_WGOM) #put all data frames into list
WGOM_recruitment_fall<-WGOM_recruitment_fall %>% reduce(full_join, by='Year')#merge all data frames in list
WGOM_recruitment_fall<-WGOM_recruitment_fall[1:39,-c(9)]
### WGOM Recruitment SPRING ####
WGOM_recruitment_spring <- list(Hubert_WGOM_bt_SP,sst_WGOM_recruitment_SP,hw_recruitment_WGOM_SP,gsi_WGOM_GD_SP,zoo_Winter_WGOM) #put all data frames into list
WGOM_recruitment_spring<-WGOM_recruitment_spring%>% reduce(full_join, by='Year')#merge all data frames in list
WGOM_recruitment_spring<-WGOM_recruitment_spring[1:39,-c(9)]
rm(list=setdiff(ls(), c("WGOM_recruitment_spring","WGOM_recruitment_fall")))
