###### American plaice stock assessment data GAM work
library(pacman)
pacman::p_load(here, readxl,lubridate,stats,graphics,Hmisc,data.table,utils,mgcv,dplyr,purrr,ecodata) 
here()
#### load .csv files #####
Cod_distribution<-read.csv(here("data/Cod_distribution.csv"))
annual_GSI<-read.csv(here("data/annual_GSI.csv"))
Bottom_temp_fall<-read.csv(here("data/Friedland_fall_mean_bottom_temp_by_stock.csv"))
Bottom_temp_spring<-read.csv(here("data/Friedland_spring_mean_bottom_temp_by_stock.csv"))
Friedland_OISST_fall<-read.csv(here("data/Friedland_OISST_fall.csv"))
Friedland_OISST_spring<-read.csv(here("data/Friedland_OISST_spr.csv"))
EGOM_K<-read.csv(here("data/rel_condition/ADIOS_SV_164712_EGOM_NONE_relative_k.csv"))
WGOM_K<-read.csv(here("data/rel_condition/ADIOS_SV_164712_WGOM_NONE_relative_k.csv"))
GBK_K<-read.csv(here("data/rel_condition/ADIOS_SV_164712_GBK_NONE_relative_k.csv"))
SNEMA_K<-read.csv(here("data/rel_condition/ADIOS_SV_164712_SNEMA_NONE_relative_k.csv"))

### Reorganize Fulton's K data####
fulK_data<-function(data,data_fl,data_sp){
  data<- data[,c(5,7,11)]
  names(data)[1] <- "Season"
  names(data)[2] <- "Year"
  df_fl<- data[data$Season == "FALL", ]
  df_fl<-df_fl[,c(2,3)]
  df_fl<-aggregate(K_rel~Year,df_fl,FUN=mean)
  df_sp<- data[data$Season == "SPRING", ]
  df_sp<-df_sp[,c(2,3)]
  df_sp<-aggregate(K_rel~Year,df_sp,FUN=mean)
  assign(data_fl, df_fl, envir=.GlobalEnv)
  assign(data_sp, df_sp, envir=.GlobalEnv)
}

fulK_data(data=EGOM_K,data_fl = "EGOM_K_FL", data_sp = "EGOM_K_SP")
fulK_data(data=WGOM_K,data_fl = "WGOM_K_FL", data_sp = "WGOM_K_SP")
fulK_data(data=GBK_K,data_fl = "GBK_K_FL", data_sp = "GBK_K_SP")
fulK_data(data=SNEMA_K,data_fl = "SNEMA_K_FL", data_sp = "SNEMA_K_SP")

rm(EGOM_K,WGOM_K,GBK_K,SNEMA_K)