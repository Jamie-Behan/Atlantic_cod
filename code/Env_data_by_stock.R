###### Atlantic Cod stock assessment data GAM work
library(pacman)
pacman::p_load(here, readxl,lubridate,stats,graphics,Hmisc,data.table,utils,mgcv,dplyr,purrr,ecodata,kableExtra,gridExtra) 
here()
#### load .csv files #####
annual_GSI<-read.csv(here("data/annual_GSI.csv"))
names(annual_GSI)[2] <- "Avg_GSI" 
Bottom_temp_fall<-read.csv(here("data/Friedland_fall_mean_bottom_temp_by_stock.csv"))
Bottom_temp_spring<-read.csv(here("data/Friedland_spring_mean_bottom_temp_by_stock.csv"))
Friedland_OISST_fall<-read.csv(here("data/Friedland_OISST_fall.csv"))
Friedland_OISST_spring<-read.csv(here("data/Friedland_OISST_spr.csv"))
cod_NAA<-read.csv(here("data/cod_NAA.csv"))
cod_heatwave<-as.data.frame(ecodata::ESP_heatwave_cod)
SSB_Fall_EGOM<-read.csv(here("data/SSB_estimates/SSB_Fall_EGOM.csv"))
names(SSB_Fall_EGOM)[1] <- "Year"
SSB_Spring_EGOM<-read.csv(here("data/SSB_estimates/SSB_Spring_EGOM.csv"))
names(SSB_Spring_EGOM)[1] <- "Year"
SSB_Fall_WGOM<-read.csv(here("data/SSB_estimates/SSB_Fall_WGOM.csv"))
names(SSB_Fall_WGOM)[1] <- "Year"
SSB_Spring_WGOM<-read.csv(here("data/SSB_estimates/SSB_Spring_WGOM.csv"))
names(SSB_Spring_WGOM)[1] <- "Year"
SSB_Fall_GBK<-read.csv(here("data/SSB_estimates/SSB_Fall_GBK.csv"))
names(SSB_Fall_GBK)[1] <- "Year"
SSB_Spring_GBK<-read.csv(here("data/SSB_estimates/SSB_Spring_GBK.csv"))
names(SSB_Spring_GBK)[1] <- "Year"
SSB_Fall_SNE<-read.csv(here("data/SSB_estimates/SSB_Fall_SNE.csv"))
names(SSB_Fall_SNE)[1] <- "Year"
SSB_Spring_SNE<-read.csv(here("data/SSB_estimates/SSB_Spring_SNE.csv"))
names(SSB_Spring_SNE)[1] <- "Year"
##### zooplankton data#####
zoo_Spring_EGOM<-read.csv(here("data/zooplankton/EGOM_spring_zooplankton.csv"))
zoo_Fall_EGOM<-read.csv(here("data/zooplankton/EGOM_fall_zooplankton.csv"))
zoo_Spring_WGOM<-read.csv(here("data/zooplankton/WGOM_spring_zooplankton.csv"))
zoo_Fall_WGOM<-read.csv(here("data/zooplankton/WGOM_fall_zooplankton.csv"))
zoo_Spring_GBK<-read.csv(here("data/zooplankton/GB_spring_zooplankton.csv"))
zoo_Fall_GBK<-read.csv(here("data/zooplankton/GB_fall_zooplankton.csv"))
zoo_Spring_SNE<-read.csv(here("data/zooplankton/SNE_spring_zooplankton.csv"))
zoo_Fall_SNE<-read.csv(here("data/zooplankton/SNE_fall_zooplankton.csv"))

zoo_Spring_EGOM<-zoo_Spring_EGOM[c(1,11,12)]
names(zoo_Spring_EGOM)[1] <- "Year"
zoo_Fall_EGOM<-zoo_Fall_EGOM[c(1,11,12)]
names(zoo_Fall_EGOM)[1] <- "Year"
zoo_Spring_WGOM<-zoo_Spring_WGOM[c(1,11,12)]
names(zoo_Spring_WGOM)[1] <- "Year"
zoo_Fall_WGOM<-zoo_Fall_WGOM[c(1,11,12)]
names(zoo_Fall_WGOM)[1] <- "Year"
zoo_Spring_GBK<-zoo_Spring_GBK[c(1,11,12)]
names(zoo_Spring_GBK)[1] <- "Year"
zoo_Fall_GBK<-zoo_Fall_GBK[c(1,11,12)]
names(zoo_Fall_GBK)[1] <- "Year"
zoo_Spring_SNE<-zoo_Spring_SNE[c(1,11,12)]
names(zoo_Spring_SNE)[1] <- "Year"
zoo_Fall_SNE<-zoo_Fall_SNE[c(1,11,12)]
names(zoo_Fall_SNE)[1] <- "Year"
##### Get cod heatwave data ####
EGOM_chw<-cod_heatwave[(cod_heatwave$stock_id == "EGOM") & (cod_heatwave$Var == "cumulative intensity"), ]
names(EGOM_chw)[3] <- "EGOM_hw"
EGOM_chw <- EGOM_chw[, -c(2,4:5)]
WGOM_chw<-cod_heatwave[(cod_heatwave$stock_id == "WGOM") & (cod_heatwave$Var == "cumulative intensity"), ]
names(WGOM_chw)[3] <- "WGOM_hw"
WGOM_chw <- WGOM_chw[, -c(2,4:5)]
GB_chw<-cod_heatwave[(cod_heatwave$stock_id == "GBK") & (cod_heatwave$Var == "cumulative intensity"), ]
names(GB_chw)[3] <- "GB_hw"
GB_chw <- GB_chw[, -c(2,4:5)]
SNE_chw<-cod_heatwave[(cod_heatwave$stock_id == "SNE") & (cod_heatwave$Var == "cumulative intensity"), ]
names(SNE_chw)[3] <- "SNE_hw"
SNE_chw <- SNE_chw[, -c(2,4:5)]

c_od_heatwave<-merge(EGOM_chw,WGOM_chw,merge="Time",all=TRUE)
c_od_heatwave<-merge(c_od_heatwave,GB_chw,merge="Time",all=TRUE)
c_od_heatwave<-merge(c_od_heatwave,SNE_chw,merge="Time",all=TRUE)
names(c_od_heatwave)[1] <- "Year"

##### filter cod NAA #######
cod_NAA<- filter(cod_NAA, SURVEY == "NEFSC_BTS")
names(cod_NAA)[2]<-"Year"
cod_NAA<-cod_NAA[c(2,3,7,17)]
EGOM_NAA<-filter(cod_NAA, STOCK == "EGOM")
WGOM_NAA<-filter(cod_NAA, STOCK == "WGOM")
SNE_NAA<-filter(cod_NAA, STOCK == "SNE")
GBK_NAA<-filter(cod_NAA, STOCK == "GBK")
####Combine data into separate spring and fall data frames####
### EGOM FALL ####
EGOM_recruitment_fall <- list(annual_GSI[24:67,c(1,2)], Bottom_temp_fall[,c(1,2)],Friedland_OISST_fall[,c(1,2)],c_od_heatwave[,c(1,2)],EGOM_NAA[ which(EGOM_NAA$SEASON=='FALL'),c(1,3)],SSB_Fall_EGOM,zoo_Fall_EGOM) #put all data frames into list
EGOM_recruitment_fall<-EGOM_recruitment_fall %>% reduce(full_join, by='Year')#merge all data frames in list
EGOM_recruitment_fall<-EGOM_recruitment_fall[6:45, ]
### EGOM SPRING ####
EGOM_recruitment_spring <- list(annual_GSI[24:67,c(1,2)], Bottom_temp_spring[,c(1,2)],Friedland_OISST_spring[,c(1,2)],c_od_heatwave[,c(1,2)],EGOM_NAA[ which(EGOM_NAA$SEASON=='SPRING'),c(1,3)],SSB_Spring_EGOM,zoo_Spring_EGOM)
EGOM_recruitment_spring<-EGOM_recruitment_spring %>% reduce(full_join, by='Year')
EGOM_recruitment_spring<-EGOM_recruitment_spring[6:45, ]
### WGOM FALL ####
WGOM_recruitment_fall <- list(annual_GSI[24:67,c(1,2)], Bottom_temp_fall[,c(1,4)],Friedland_OISST_fall[,c(1,4)],c_od_heatwave[,c(1,3)],WGOM_NAA[ which(WGOM_NAA$SEASON=='FALL'),c(1,3)],SSB_Fall_WGOM,zoo_Fall_WGOM) #put all data frames into list
WGOM_recruitment_fall<-WGOM_recruitment_fall %>% reduce(full_join, by='Year')#merge all data frames in list
WGOM_recruitment_fall<-WGOM_recruitment_fall[6:45, ]
### WGOM SPRING ####
WGOM_recruitment_spring <- list(annual_GSI[24:67,c(1,2)], Bottom_temp_spring[,c(1,4)],Friedland_OISST_spring[,c(1,4)],c_od_heatwave[,c(1,3)],WGOM_NAA[ which(WGOM_NAA$SEASON=='SPRING'),c(1,3)],SSB_Spring_WGOM,zoo_Spring_WGOM)
WGOM_recruitment_spring<-WGOM_recruitment_spring %>% reduce(full_join, by='Year')
WGOM_recruitment_spring<-WGOM_recruitment_spring[6:45, ]
### GBK FALL ####
GBK_recruitment_fall <- list(annual_GSI[24:67,c(1,2)], Bottom_temp_fall[,c(1,3)],Friedland_OISST_fall[,c(1,3)],c_od_heatwave[,c(1,4)],GBK_NAA[ which(GBK_NAA$SEASON=='FALL'),c(1,3)],SSB_Fall_GBK,zoo_Fall_GBK) #put all data frames into list
GBK_recruitment_fall<-GBK_recruitment_fall %>% reduce(full_join, by='Year')#merge all data frames in list
GBK_recruitment_fall<-GBK_recruitment_fall[6:45, ]
### GBK SPRING ####
GBK_recruitment_spring <- list(annual_GSI[24:67,c(1,2)], Bottom_temp_spring[,c(1,3)],Friedland_OISST_spring[,c(1,3)],c_od_heatwave[,c(1,4)],GBK_NAA[ which(GBK_NAA$SEASON=='SPRING'),c(1,3)],SSB_Spring_GBK,zoo_Spring_GBK)
GBK_recruitment_spring<-GBK_recruitment_spring %>% reduce(full_join, by='Year')
GBK_recruitment_spring<-GBK_recruitment_spring[6:45, ]
### SNE FALL ####
SNE_recruitment_fall <- list(annual_GSI[24:67,c(1,2)], Bottom_temp_fall[,c(1,5)],Friedland_OISST_fall[,c(1,5)],c_od_heatwave[,c(1,5)],SNE_NAA[ which(SNE_NAA$SEASON=='FALL'),c(1,3)],SSB_Fall_SNE,zoo_Fall_SNE) #No Fulton K data Available
SNE_recruitment_fall<-SNE_recruitment_fall %>% reduce(full_join, by='Year')#merge all data frames in list
SNE_recruitment_fall<-SNE_recruitment_fall[6:45, ]
### SNE SPRING ####
SNE_recruitment_spring <- list(annual_GSI[24:67,c(1,2)], Bottom_temp_spring[,c(1,5)],Friedland_OISST_spring[,c(1,5)],c_od_heatwave[,c(1,5)],SNE_NAA[ which(SNE_NAA$SEASON=='SPRING'),c(1,3)],SSB_Spring_SNE,zoo_Spring_SNE)
SNE_recruitment_spring<-SNE_recruitment_spring %>% reduce(full_join, by='Year')
SNE_recruitment_spring<-SNE_recruitment_spring[6:45, ]
### renumber rows ####
row.names(EGOM_recruitment_fall) <- 1:nrow(EGOM_recruitment_fall)
row.names(EGOM_recruitment_spring) <- 1:nrow(EGOM_recruitment_spring)
row.names(WGOM_recruitment_fall) <- 1:nrow(WGOM_recruitment_fall)
row.names(WGOM_recruitment_spring) <- 1:nrow(WGOM_recruitment_spring)
row.names(GBK_recruitment_fall) <- 1:nrow(GBK_recruitment_fall)
row.names(GBK_recruitment_spring) <- 1:nrow(GBK_recruitment_spring)
row.names(SNE_recruitment_fall) <- 1:nrow(SNE_recruitment_fall)
row.names(SNE_recruitment_spring) <- 1:nrow(SNE_recruitment_spring)
### remove data I don't need ####
rm(annual_GSI,Bottom_temp_fall,Bottom_temp_spring,Friedland_OISST_fall,Friedland_OISST_spring,EGOM_chw,WGOM_chw,GB_chw,SNE_chw,cod_heatwave,c_od_heatwave,cod_NAA,EGOM_NAA,WGOM_NAA,SNE_NAA,GBK_NAA,SSB_Fall_EGOM,SSB_Fall_GBK,SSB_Fall_SNE,SSB_Fall_WGOM,SSB_Spring_EGOM,SSB_Spring_GBK,SSB_Spring_SNE,SSB_Spring_WGOM,zoo_Fall_EGOM,zoo_Fall_GBK,zoo_Fall_SNE,zoo_Fall_WGOM,zoo_Spring_EGOM,zoo_Spring_GBK,zoo_Spring_SNE,zoo_Spring_WGOM)

###### Anomaly Base Period########
### using 1981-2010 as baseline anomaly period as NOAA does####
#### EGOM ####
bt_fall_bp_egom<-mean(EGOM_recruitment_fall[1:30,3])
bt_spring_bp_egom<-mean(EGOM_recruitment_spring[1:30,3])
sst_fall_bp_egom<-mean(EGOM_recruitment_fall[1:30,4])
sst_spring_bp_egom<-mean(EGOM_recruitment_spring[1:30,4])
#### WGOM ####
bt_fall_bp_wgom<-mean(WGOM_recruitment_fall[1:30,3])
bt_spring_bp_wgom<-mean(WGOM_recruitment_spring[1:30,3])
sst_fall_bp_wgom<-mean(WGOM_recruitment_fall[1:30,4])
sst_spring_bp_wgom<-mean(WGOM_recruitment_spring[1:30,4])
#### GBK ####
bt_fall_bp_gb<-mean(GBK_recruitment_fall[1:30,3])
bt_spring_bp_gb<-mean(GBK_recruitment_spring[1:30,3])
sst_fall_bp_gb<-mean(GBK_recruitment_fall[1:30,4])
sst_spring_bp_gb<-mean(GBK_recruitment_spring[1:30,4])
#### SNE ####
bt_fall_bp_sne<-mean(SNE_recruitment_fall[1:30,3])
bt_spring_bp_sne<-mean(SNE_recruitment_spring[1:30,3])
sst_fall_bp_sne<-mean(SNE_recruitment_fall[1:30,4])
sst_spring_bp_sne<-mean(SNE_recruitment_spring[1:30,4])
##### Calculate temperature anomaly columns#####
#### EGOM ####
EGOM_recruitment_fall$bt_anomaly<- EGOM_recruitment_fall$EGOM_bt  - bt_fall_bp_wgom
EGOM_recruitment_fall$sst_anomaly<- EGOM_recruitment_fall$EGOM_oisst  - sst_fall_bp_wgom
EGOM_recruitment_spring$bt_anomaly<- EGOM_recruitment_spring$EGOM_bt  - bt_spring_bp_wgom
EGOM_recruitment_spring$sst_anomaly<- EGOM_recruitment_spring$EGOM_oisst  - sst_spring_bp_wgom
#### WGOM ####
WGOM_recruitment_fall$bt_anomaly<- WGOM_recruitment_fall$WGOM_bt  - bt_fall_bp_wgom
WGOM_recruitment_fall$sst_anomaly<- WGOM_recruitment_fall$WGOM_oisst  - sst_fall_bp_wgom
WGOM_recruitment_spring$bt_anomaly<- WGOM_recruitment_spring$WGOM_bt  - bt_spring_bp_wgom
WGOM_recruitment_spring$sst_anomaly<- WGOM_recruitment_spring$WGOM_oisst  - sst_spring_bp_wgom
#### GBK ####
GBK_recruitment_fall$bt_anomaly<- GBK_recruitment_fall$GBK_bt  - bt_fall_bp_gb
GBK_recruitment_fall$sst_anomaly<- GBK_recruitment_fall$GBK_oisst  - sst_fall_bp_gb
GBK_recruitment_spring$bt_anomaly<- GBK_recruitment_spring$GBK_bt  - bt_spring_bp_gb
GBK_recruitment_spring$sst_anomaly<- GBK_recruitment_spring$GBK_oisst  - sst_spring_bp_gb
#### SNE ####
SNE_recruitment_fall$bt_anomaly<- SNE_recruitment_fall$SNE_bt  - bt_fall_bp_sne
SNE_recruitment_fall$sst_anomaly<- SNE_recruitment_fall$SNE_oisst  - sst_fall_bp_sne
SNE_recruitment_spring$bt_anomaly<- SNE_recruitment_spring$SNE_bt  - bt_spring_bp_sne
SNE_recruitment_spring$sst_anomaly<- SNE_recruitment_spring$SNE_oisst  - sst_spring_bp_sne

