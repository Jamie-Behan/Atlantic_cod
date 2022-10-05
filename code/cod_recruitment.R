###### American plaice stock assessment data GAM work
library(pacman)
pacman::p_load(here, readxl,lubridate,stats,graphics,Hmisc,data.table,utils,mgcv,dplyr,purrr,ecodata) 
here()
#### load .csv files #####
annual_GSI<-read.csv(here("data/annual_GSI.csv"))
Bottom_temp_fall<-read.csv(here("data/Friedland_fall_mean_bottom_temp_by_stock.csv"))
Bottom_temp_spring<-read.csv(here("data/Friedland_spring_mean_bottom_temp_by_stock.csv"))
Friedland_OISST_fall<-read.csv(here("data/Friedland_OISST_fall.csv"))
Friedland_OISST_spring<-read.csv(here("data/Friedland_OISST_spr.csv"))
FultonKOutput<-read.csv(here("data/FultonKOutput.csv"))
AMO_NAO<-read.csv(here("data/Full_GSI_AMO_NAO.csv"))
cod_heatwave<-as.data.frame(ecodata::ESP_heatwave_cod)
### Reorganize Fulton's K data####
##fall data
FultonK_fall<-FultonKOutput[FultonKOutput$source == "NMFS Fall", ]
FultonK_fall<-aggregate(FultonK~Year+Stock,FultonK_fall,FUN=mean)

EGOM_FK<-FultonK_fall[FultonK_fall$Stock == "Eastern GOM", ]
names(EGOM_FK)[3] <- "EGOM_FK"
WGOM_FK<-FultonK_fall[FultonK_fall$Stock == "Western GOM", ]
names(WGOM_FK)[3] <- "WGOM_FK"
GB_FK<-FultonK_fall[FultonK_fall$Stock == "Georges Bank", ]
names(GB_FK)[3] <- "GB_FK"

FultonK_fall <- list(EGOM_FK[,c(1,3)], WGOM_FK[,c(1,3)], GB_FK[,c(1,3)])
FultonK_fall <-FultonK_fall %>% reduce(full_join, by='Year')

##spring data
FultonK_spring<-FultonKOutput[FultonKOutput$source == "NMFS Spring", ]
FultonK_spring<-aggregate(FultonK~Year+Stock,FultonK_spring,FUN=mean)

EGOM_FK<-FultonK_spring[FultonK_spring$Stock == "Eastern GOM", ]
names(EGOM_FK)[3] <- "EGOM_FK"
WGOM_FK<-FultonK_spring[FultonK_spring$Stock == "Western GOM", ]
names(WGOM_FK)[3] <- "WGOM_FK"
GB_FK<-FultonK_spring[FultonK_spring$Stock == "Georges Bank", ]
names(GB_FK)[3] <- "GB_FK"

FultonK_spring <- list(EGOM_FK[,c(1,3)], WGOM_FK[,c(1,3)], GB_FK[,c(1,3)])
FultonK_spring <-FultonK_spring %>% reduce(full_join, by='Year')

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

####Combine data into separate spring and fall data frames####
### EGOM FALL ####
EGOM_recruitment_fall <- list(AMO_NAO[30:73,c(1,3,4)], annual_GSI[24:67,c(1,2)], Bottom_temp_fall[,c(1,2)],Friedland_OISST_fall[,c(1,2)],FultonK_fall[,c(1,2)],c_od_heatwave[,c(1,2)]) #put all data frames into list

EGOM_recruitment_fall<-EGOM_recruitment_fall %>% reduce(full_join, by='Year')#merge all data frames in list
names(EGOM_recruitment_fall)[4] <- "Avg_GSI" 
EGOM_recruitment_fall<-EGOM_recruitment_fall[-45, ]
### EGOM SPRING ####
EGOM_recruitment_spring <- list(AMO_NAO[30:73,c(1,3,4)], annual_GSI[24:67,c(1,2)], Bottom_temp_spring[,c(1,2)],Friedland_OISST_spring[,c(1,2)],FultonK_spring[,c(1,2)],c_od_heatwave[,c(1,2)])
EGOM_recruitment_spring<-EGOM_recruitment_spring %>% reduce(full_join, by='Year')
names(EGOM_recruitment_spring)[4] <- "Avg_GSI"
EGOM_recruitment_spring<-EGOM_recruitment_spring[-c(45:46), ]
### WGOM FALL ####
WGOM_recruitment_fall <- list(AMO_NAO[30:73,c(1,3,4)], annual_GSI[24:67,c(1,2)], Bottom_temp_fall[,c(1,4)],Friedland_OISST_fall[,c(1,4)],FultonK_fall[,c(1,3)],c_od_heatwave[,c(1,3)]) #put all data frames into list

WGOM_recruitment_fall<-WGOM_recruitment_fall %>% reduce(full_join, by='Year')#merge all data frames in list
names(WGOM_recruitment_fall)[4] <- "Avg_GSI" 
WGOM_recruitment_fall<-WGOM_recruitment_fall[-45, ]
### WGOM SPRING ####
WGOM_recruitment_spring <- list(AMO_NAO[30:73,c(1,3,4)], annual_GSI[24:67,c(1,2)], Bottom_temp_spring[,c(1,4)],Friedland_OISST_spring[,c(1,4)],FultonK_spring[,c(1,3)],c_od_heatwave[,c(1,3)])
WGOM_recruitment_spring<-WGOM_recruitment_spring %>% reduce(full_join, by='Year')
names(WGOM_recruitment_spring)[4] <- "Avg_GSI"
WGOM_recruitment_spring<-WGOM_recruitment_spring[-c(45:46), ]
### GBK FALL ####
GBK_recruitment_fall <- list(AMO_NAO[30:73,c(1,3,4)], annual_GSI[24:67,c(1,2)], Bottom_temp_fall[,c(1,3)],Friedland_OISST_fall[,c(1,3)],FultonK_fall[,c(1,4)],c_od_heatwave[,c(1,4)]) #put all data frames into list

GBK_recruitment_fall<-GBK_recruitment_fall %>% reduce(full_join, by='Year')#merge all data frames in list
names(GBK_recruitment_fall)[4] <- "Avg_GSI" 
GBK_recruitment_fall<-GBK_recruitment_fall[-45, ]
### GBK SPRING ####
GBK_recruitment_spring <- list(AMO_NAO[30:73,c(1,3,4)], annual_GSI[24:67,c(1,2)], Bottom_temp_spring[,c(1,3)],Friedland_OISST_spring[,c(1,3)],FultonK_spring[,c(1,4)],c_od_heatwave[,c(1,4)])
GBK_recruitment_spring<-GBK_recruitment_spring %>% reduce(full_join, by='Year')
names(GBK_recruitment_spring)[4] <- "Avg_GSI"
GBK_recruitment_spring<-GBK_recruitment_spring[-c(45:46), ]
### SNE FALL ####
SNE_recruitment_fall <- list(AMO_NAO[30:73,c(1,3,4)], annual_GSI[24:67,c(1,2)], Bottom_temp_fall[,c(1,5)],Friedland_OISST_fall[,c(1,5)],c_od_heatwave[,c(1,5)]) #No Fulton K data Available

SNE_recruitment_fall<-SNE_recruitment_fall %>% reduce(full_join, by='Year')#merge all data frames in list
names(SNE_recruitment_fall)[4] <- "Avg_GSI" 
SNE_recruitment_fall<-SNE_recruitment_fall[-45, ]
### SNE SPRING ####
SNE_recruitment_spring <- list(AMO_NAO[30:73,c(1,3,4)], annual_GSI[24:67,c(1,2)], Bottom_temp_spring[,c(1,5)],Friedland_OISST_spring[,c(1,5)],c_od_heatwave[,c(1,5)])
SNE_recruitment_spring<-SNE_recruitment_spring %>% reduce(full_join, by='Year')
names(SNE_recruitment_spring)[4] <- "Avg_GSI"
SNE_recruitment_spring<-SNE_recruitment_spring[-c(45:46), ]
### remove data I don't need ####
rm(EGOM_FK,WGOM_FK,GB_FK,FultonKOutput,annual_GSI,Bottom_temp_fall,Bottom_temp_spring,Friedland_OISST_fall,Friedland_OISST_spring,FultonK_fall,FultonK_spring,AMO_NAO,EGOM_chw,WGOM_chw,GB_chw,SNE_chw,cod_heatwave,c_od_heatwave)

###### Anomaly Base Period########
### using 1981-2010 as baseline anomaly period as NOAA does####
#### EGOM ####
bt_fall_bp_egom<-mean(EGOM_recruitment_fall[5:34,5])
bt_spring_bp_egom<-mean(EGOM_recruitment_spring[5:34,5])
sst_fall_bp_egom<-mean(EGOM_recruitment_fall[5:34,6])
sst_spring_bp_egom<-mean(EGOM_recruitment_spring[5:34,6])
#### WGOM ####
bt_fall_bp_wgom<-mean(WGOM_recruitment_fall[5:34,5])
bt_spring_bp_wgom<-mean(WGOM_recruitment_spring[5:34,5])
sst_fall_bp_wgom<-mean(WGOM_recruitment_fall[5:34,6])
sst_spring_bp_wgom<-mean(WGOM_recruitment_spring[5:34,6])
#### GBK ####
bt_fall_bp_gb<-mean(GBK_recruitment_fall[5:34,5])
bt_spring_bp_gb<-mean(GBK_recruitment_spring[5:34,5])
sst_fall_bp_gb<-mean(GBK_recruitment_fall[5:34,6])
sst_spring_bp_gb<-mean(GBK_recruitment_spring[5:34,6])
#### SNE ####
bt_fall_bp_sne<-mean(SNE_recruitment_fall[5:34,5])
bt_spring_bp_sne<-mean(SNE_recruitment_spring[5:34,5])
sst_fall_bp_sne<-mean(SNE_recruitment_fall[5:34,6])
sst_spring_bp_sne<-mean(SNE_recruitment_spring[5:34,6])
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


####save df to use for growth GAMs ####
write.csv(EGOM_recruitment_fall,here("data/stock_area_data/EGOM_recruitment_fall.csv"), row.names = FALSE)
write.csv(EGOM_recruitment_spring,here("data/stock_area_data/EGOM_recruitment_spring.csv"), row.names = FALSE)

write.csv(WGOM_recruitment_fall,here("data/stock_area_data/WGOM_recruitment_fall.csv"), row.names = FALSE)
write.csv(WGOM_recruitment_spring,here("data/stock_area_data/WGOM_recruitment_spring.csv"), row.names = FALSE)

write.csv(GBK_recruitment_fall,here("data/stock_area_data/GBK_recruitment_fall.csv"), row.names = FALSE)
write.csv(GBK_recruitment_spring,here("data/stock_area_data/GBK_recruitment_spring.csv"), row.names = FALSE)

write.csv(SNE_recruitment_fall,here("data/stock_area_data/SNE_recruitment_fall.csv"), row.names = FALSE)
write.csv(SNE_recruitment_spring,here("data/stock_area_data/SNE_recruitment_spring.csv"), row.names = FALSE)
