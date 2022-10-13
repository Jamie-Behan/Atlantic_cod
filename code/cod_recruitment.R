###### American plaice stock assessment data GAM work
library(pacman)
pacman::p_load(here, readxl,lubridate,stats,graphics,Hmisc,data.table,utils,mgcv,dplyr,purrr,ecodata,kableExtra) 
here()
#### load .csv files #####
annual_GSI<-read.csv(here("data/annual_GSI.csv"))
names(annual_GSI)[2] <- "Avg_GSI" 
Bottom_temp_fall<-read.csv(here("data/Friedland_fall_mean_bottom_temp_by_stock.csv"))
Bottom_temp_spring<-read.csv(here("data/Friedland_spring_mean_bottom_temp_by_stock.csv"))
Friedland_OISST_fall<-read.csv(here("data/Friedland_OISST_fall.csv"))
Friedland_OISST_spring<-read.csv(here("data/Friedland_OISST_spr.csv"))
cod_heatwave<-as.data.frame(ecodata::ESP_heatwave_cod)

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
EGOM_recruitment_fall <- list(annual_GSI[24:67,c(1,2)], Bottom_temp_fall[,c(1,2)],Friedland_OISST_fall[,c(1,2)],c_od_heatwave[,c(1,2)]) #put all data frames into list
EGOM_recruitment_fall<-EGOM_recruitment_fall %>% reduce(full_join, by='Year')#merge all data frames in list
EGOM_recruitment_fall<-EGOM_recruitment_fall[-45, ]
### EGOM SPRING ####
EGOM_recruitment_spring <- list(annual_GSI[24:67,c(1,2)], Bottom_temp_spring[,c(1,2)],Friedland_OISST_spring[,c(1,2)],c_od_heatwave[,c(1,2)])
EGOM_recruitment_spring<-EGOM_recruitment_spring %>% reduce(full_join, by='Year')
EGOM_recruitment_spring<-EGOM_recruitment_spring[-c(45:46), ]
### WGOM FALL ####
WGOM_recruitment_fall <- list(annual_GSI[24:67,c(1,2)], Bottom_temp_fall[,c(1,4)],Friedland_OISST_fall[,c(1,4)],c_od_heatwave[,c(1,3)]) #put all data frames into list
WGOM_recruitment_fall<-WGOM_recruitment_fall %>% reduce(full_join, by='Year')#merge all data frames in list
WGOM_recruitment_fall<-WGOM_recruitment_fall[-45, ]
### WGOM SPRING ####
WGOM_recruitment_spring <- list(annual_GSI[24:67,c(1,2)], Bottom_temp_spring[,c(1,4)],Friedland_OISST_spring[,c(1,4)],c_od_heatwave[,c(1,3)])
WGOM_recruitment_spring<-WGOM_recruitment_spring %>% reduce(full_join, by='Year')
WGOM_recruitment_spring<-WGOM_recruitment_spring[-c(45:46), ]
### GBK FALL ####
GBK_recruitment_fall <- list(annual_GSI[24:67,c(1,2)], Bottom_temp_fall[,c(1,3)],Friedland_OISST_fall[,c(1,3)],c_od_heatwave[,c(1,4)]) #put all data frames into list
GBK_recruitment_fall<-GBK_recruitment_fall %>% reduce(full_join, by='Year')#merge all data frames in list
GBK_recruitment_fall<-GBK_recruitment_fall[-45, ]
### GBK SPRING ####
GBK_recruitment_spring <- list(annual_GSI[24:67,c(1,2)], Bottom_temp_spring[,c(1,3)],Friedland_OISST_spring[,c(1,3)],c_od_heatwave[,c(1,4)])
GBK_recruitment_spring<-GBK_recruitment_spring %>% reduce(full_join, by='Year')
GBK_recruitment_spring<-GBK_recruitment_spring[-c(45:46), ]
### SNE FALL ####
SNE_recruitment_fall <- list(annual_GSI[24:67,c(1,2)], Bottom_temp_fall[,c(1,5)],Friedland_OISST_fall[,c(1,5)],c_od_heatwave[,c(1,5)]) #No Fulton K data Available
SNE_recruitment_fall<-SNE_recruitment_fall %>% reduce(full_join, by='Year')#merge all data frames in list
SNE_recruitment_fall<-SNE_recruitment_fall[-45, ]
### SNE SPRING ####
SNE_recruitment_spring <- list(annual_GSI[24:67,c(1,2)], Bottom_temp_spring[,c(1,5)],Friedland_OISST_spring[,c(1,5)],c_od_heatwave[,c(1,5)])
SNE_recruitment_spring<-SNE_recruitment_spring %>% reduce(full_join, by='Year')
SNE_recruitment_spring<-SNE_recruitment_spring[-c(45:46), ]
### remove data I don't need ####
rm(annual_GSI,Bottom_temp_fall,Bottom_temp_spring,Friedland_OISST_fall,Friedland_OISST_spring,EGOM_chw,WGOM_chw,GB_chw,SNE_chw,cod_heatwave,c_od_heatwave)

###### Anomaly Base Period########
### using 1981-2010 as baseline anomaly period as NOAA does####
#### EGOM ####
bt_fall_bp_egom<-mean(EGOM_recruitment_fall[5:34,3])
bt_spring_bp_egom<-mean(EGOM_recruitment_spring[5:34,3])
sst_fall_bp_egom<-mean(EGOM_recruitment_fall[5:34,4])
sst_spring_bp_egom<-mean(EGOM_recruitment_spring[5:34,4])
#### WGOM ####
bt_fall_bp_wgom<-mean(WGOM_recruitment_fall[5:34,3])
bt_spring_bp_wgom<-mean(WGOM_recruitment_spring[5:34,3])
sst_fall_bp_wgom<-mean(WGOM_recruitment_fall[5:34,4])
sst_spring_bp_wgom<-mean(WGOM_recruitment_spring[5:34,4])
#### GBK ####
bt_fall_bp_gb<-mean(GBK_recruitment_fall[5:34,3])
bt_spring_bp_gb<-mean(GBK_recruitment_spring[5:34,3])
sst_fall_bp_gb<-mean(GBK_recruitment_fall[5:34,4])
sst_spring_bp_gb<-mean(GBK_recruitment_spring[5:34,4])
#### SNE ####
bt_fall_bp_sne<-mean(SNE_recruitment_fall[5:34,3])
bt_spring_bp_sne<-mean(SNE_recruitment_spring[5:34,3])
sst_fall_bp_sne<-mean(SNE_recruitment_fall[5:34,4])
sst_spring_bp_sne<-mean(SNE_recruitment_spring[5:34,4])
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
#write.csv(EGOM_recruitment_fall,here("data/stock_area_data/EGOM_recruitment_fall.csv"), row.names = FALSE)
#write.csv(EGOM_recruitment_spring,here("data/stock_area_data/EGOM_recruitment_spring.csv"), row.names = FALSE)

#write.csv(WGOM_recruitment_fall,here("data/stock_area_data/WGOM_recruitment_fall.csv"), row.names = FALSE)
#write.csv(WGOM_recruitment_spring,here("data/stock_area_data/WGOM_recruitment_spring.csv"), row.names = FALSE)

#write.csv(GBK_recruitment_fall,here("data/stock_area_data/GBK_recruitment_fall.csv"), row.names = FALSE)
#write.csv(GBK_recruitment_spring,here("data/stock_area_data/GBK_recruitment_spring.csv"), row.names = FALSE)

#write.csv(SNE_recruitment_fall,here("data/stock_area_data/SNE_recruitment_fall.csv"), row.names = FALSE)
#write.csv(SNE_recruitment_spring,here("data/stock_area_data/SNE_recruitment_spring.csv"), row.names = FALSE)

##### Start here ######
source(here("Code/Gam_data_exploration.R"))

#put dfs in list to apply across functions
df.list <- list(EGOM_recruitment_fall,EGOM_recruitment_spring,WGOM_recruitment_fall,WGOM_recruitment_spring,GBK_recruitment_fall,GBK_recruitment_spring,SNE_recruitment_fall,SNE_recruitment_spring)

#apply functions
lapply(df.list, dotchart_fun_8)
lapply(df.list, hist_fun8)
lapply(df.list, view_boxplot_fun8)
lapply(df.list, shapiro_fun)
lapply(df.list,Mypairs)
#lineplot_seasonal(data,xlab2,ylab2,xlab3,ylab3,xlab4,ylab4,xlab5,ylab5,xlab6,ylab6,xlab7,ylab7)




