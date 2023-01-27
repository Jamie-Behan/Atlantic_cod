###### Atlantic Cod stock assessment data GAM work
library(pacman)
pacman::p_load(here, readxl,lubridate,stats,graphics,Hmisc,data.table,utils,mgcv,dplyr,purrr,ecodata,kableExtra,gridExtra,rlang) 
here()

#########################################################Bottom Temperature ####
Hubert_EGOM_bt<-read.csv(here("data/stock_area_data/hubert_monthly/Hubert_EGOM_bt.csv"))
Hubert_WGOM_bt<-read.csv(here("data/stock_area_data/hubert_monthly/Hubert_WGOM_bt.csv"))
Hubert_GBK_bt<-read.csv(here("data/stock_area_data/hubert_monthly/Hubert_GBK_bt.csv"))
Hubert_SNE_bt<-read.csv(here("data/stock_area_data/hubert_monthly/Hubert_SNE_bt.csv"))
#get 6-month mean values for each year (6-months prior to start of each seasonal survey)
#### Fall #####
####EGOM (April-September means)####
Hubert_EGOM_bt_FL <-subset(Hubert_EGOM_bt , month >= 4 & month <= 9)
Hubert_EGOM_bt_FL <- aggregate(bt_temp~year, data=Hubert_EGOM_bt_FL,FUN=mean)
#get anomaly period means (1982-2011)
bt_fall_bp_egom<-mean(Hubert_EGOM_bt_FL[5:34,2])
#get final bt dataset (years 1982-2020, anomaly adjusted)
Hubert_EGOM_bt_FL$bt_anomaly<- Hubert_EGOM_bt_FL$bt_temp  - bt_fall_bp_egom
Hubert_EGOM_bt_FL <-Hubert_EGOM_bt_FL[5:43,c(1,3)]
names(Hubert_EGOM_bt_FL)[1]<-"Year"
rm(bt_fall_bp_egom)
####WGOM (March-August means)####
Hubert_WGOM_bt_FL <-subset(Hubert_WGOM_bt , month >= 3 & month <= 8)
Hubert_WGOM_bt_FL <- aggregate(bt_temp~year, data=Hubert_WGOM_bt_FL,FUN=mean)
#get anomaly period means (1982-2011)
bt_fall_bp_wgom<-mean(Hubert_WGOM_bt_FL[5:34,2])
#get final bt dataset (years 1982-2020, anomaly adjusted)
Hubert_WGOM_bt_FL$bt_anomaly<- Hubert_WGOM_bt_FL$bt_temp  - bt_fall_bp_wgom
Hubert_WGOM_bt_FL <-Hubert_WGOM_bt_FL[5:43,c(1,3)]
names(Hubert_WGOM_bt_FL)[1]<-"Year"
rm(bt_fall_bp_wgom)
####GBK (March-August means)####
Hubert_GBK_bt_FL <-subset(Hubert_GBK_bt , month >= 3 & month <= 8)
Hubert_GBK_bt_FL <- aggregate(bt_temp~year, data=Hubert_GBK_bt_FL,FUN=mean)
#get anomaly period means (1982-2011)
bt_fall_bp_gbk<-mean(Hubert_GBK_bt_FL[5:34,2])
#get final bt dataset (years 1982-2020, anomaly adjusted)
Hubert_GBK_bt_FL$bt_anomaly<- Hubert_GBK_bt_FL$bt_temp  - bt_fall_bp_gbk
Hubert_GBK_bt_FL <-Hubert_GBK_bt_FL[5:43,c(1,3)]
names(Hubert_GBK_bt_FL)[1]<-"Year"
rm(bt_fall_bp_gbk)
####SNE (March-August means)####
Hubert_SNE_bt_FL <-subset(Hubert_SNE_bt , month >= 3 & month <= 8)
Hubert_SNE_bt_FL <- aggregate(bt_temp~year, data=Hubert_SNE_bt_FL,FUN=mean)
#get anomaly period means (1982-2011)
bt_fall_bp_sne<-mean(Hubert_SNE_bt_FL[5:34,2])
#get final bt dataset (years 1982-2020, anomaly adjusted)
Hubert_SNE_bt_FL$bt_anomaly<- Hubert_SNE_bt_FL$bt_temp  - bt_fall_bp_sne
Hubert_SNE_bt_FL <-Hubert_SNE_bt_FL[5:43,c(1,3)]
names(Hubert_SNE_bt_FL)[1]<-"Year"
rm(bt_fall_bp_sne)
#### Spring #####
#get 6-month mean values for each year (6-months prior to start of each seasonal survey)
####EGOM (October(year-1)-March (year) means)####
Hubert_EGOM_bt_SP <-subset(Hubert_EGOM_bt , month==10|month==11|month==12|month==1|month==2|month==3)
#aggregating correct months from each year
Hubert_EGOM_bt_SP$newlag <- ifelse(Hubert_EGOM_bt_SP$month==1|Hubert_EGOM_bt_SP$month==2|Hubert_EGOM_bt_SP$month==3,Hubert_EGOM_bt_SP$year,Hubert_EGOM_bt_SP$year+1)
Hubert_EGOM_bt_SP <- aggregate(bt_temp~newlag, data=Hubert_EGOM_bt_SP,FUN=mean)
#get anomaly period means (1982-2011)
bt_spring_bp_egom<-mean(Hubert_EGOM_bt_SP[5:34,2])
#get final bt dataset (years 1982-2020, anomaly adjusted)
Hubert_EGOM_bt_SP$bt_anomaly<- Hubert_EGOM_bt_SP$bt_temp  - bt_spring_bp_egom
Hubert_EGOM_bt_SP <-Hubert_EGOM_bt_SP[5:43,c(1,3)]
names(Hubert_EGOM_bt_SP)[1]<-"Year"
rm(Hubert_EGOM_bt,bt_spring_bp_egom)
####WGOM (October(year-1)-March (year) means)####
Hubert_WGOM_bt_SP <-subset(Hubert_WGOM_bt , month==10|month==11|month==12|month==1|month==2|month==3)
#aggregating correct months from each year
Hubert_WGOM_bt_SP$newlag <- ifelse(Hubert_WGOM_bt_SP$month==1|Hubert_WGOM_bt_SP$month==2|Hubert_WGOM_bt_SP$month==3,Hubert_WGOM_bt_SP$year,Hubert_WGOM_bt_SP$year+1)
Hubert_WGOM_bt_SP <- aggregate(bt_temp~newlag, data=Hubert_WGOM_bt_SP,FUN=mean)
#get anomaly period means (1982-2011)
bt_spring_bp_wgom<-mean(Hubert_WGOM_bt_SP[5:34,2])
#get final bt dataset (years 1982-2020, anomaly adjusted)
Hubert_WGOM_bt_SP$bt_anomaly<- Hubert_WGOM_bt_SP$bt_temp  - bt_spring_bp_wgom
Hubert_WGOM_bt_SP <-Hubert_WGOM_bt_SP[5:43,c(1,3)]
names(Hubert_WGOM_bt_SP)[1]<-"Year"
rm(Hubert_WGOM_bt,bt_spring_bp_wgom)
####GBK (September(year-1)-February (year) means)####
Hubert_GBK_bt_SP <-subset(Hubert_GBK_bt , month==9|month==10|month==11|month==12|month==1|month==2)
#aggregating correct months from each year
Hubert_GBK_bt_SP$newlag <- ifelse(Hubert_GBK_bt_SP$month==1|Hubert_GBK_bt_SP$month==2,Hubert_GBK_bt_SP$year,Hubert_GBK_bt_SP$year+1)
Hubert_GBK_bt_SP <- aggregate(bt_temp~newlag, data=Hubert_GBK_bt_SP,FUN=mean)
#get anomaly period means (1982-2011)
bt_spring_bp_gbk<-mean(Hubert_GBK_bt_SP[5:34,2])
#get final bt dataset (years 1982-2020, anomaly adjusted)
Hubert_GBK_bt_SP$bt_anomaly<- Hubert_GBK_bt_SP$bt_temp  - bt_spring_bp_gbk
Hubert_GBK_bt_SP <-Hubert_GBK_bt_SP[5:43,c(1,3)]
names(Hubert_GBK_bt_SP)[1]<-"Year"
rm(Hubert_GBK_bt,bt_spring_bp_gbk)
####SNE (August(year-1)-January (year) means)####
Hubert_SNE_bt_SP <-subset(Hubert_SNE_bt , month==8|month==9|month==10|month==11|month==12|month==1)
#aggregating correct months from each year
Hubert_SNE_bt_SP$newlag <- ifelse(Hubert_SNE_bt_SP$month==1,Hubert_SNE_bt_SP$year,Hubert_SNE_bt_SP$year+1)
Hubert_SNE_bt_SP <- aggregate(bt_temp~newlag, data=Hubert_SNE_bt_SP,FUN=mean)
#get anomaly period means (1982-2011)
bt_spring_bp_sne<-mean(Hubert_SNE_bt_SP[5:34,2])
#get final bt dataset (years 1982-2020, anomaly adjusted)
Hubert_SNE_bt_SP$bt_anomaly<- Hubert_SNE_bt_SP$bt_temp-bt_spring_bp_sne
Hubert_SNE_bt_SP <-Hubert_SNE_bt_SP[5:43,c(1,3)]
names(Hubert_SNE_bt_SP)[1]<-"Year"
rm(Hubert_SNE_bt,bt_spring_bp_sne)
###################################################################### SST ####
load(here("data/cod_sst.rda"))
cod_sst <- subset(cod_sst, Year<2021&Year>1977&INDICATOR_NAME=="temperature") 
sst_EGOM<-cod_sst[cod_sst$Region == "EGOM",c(1,2,4)]
sst_WGOM<-cod_sst[cod_sst$Region == "WGOM",c(1,2,4)]
sst_GBK<-cod_sst[cod_sst$Region == "GBK",c(1,2,4)]
sst_SNE<-cod_sst[cod_sst$Region == "SNE",c(1,2,4)]
##### Recruitment Models #####
####get 4-month mean values for each year (4-months starting at peak spawning period by stock)####
####EGOM (May-August (year-1) means)####
sst_EGOM_recruitment <-subset(sst_EGOM , Month >= 5 & Month <= 8)
sst_EGOM_recruitment <- aggregate(DATA_VALUE~Year, data=sst_EGOM_recruitment,FUN=mean)
#get anomaly period means (1982-2011)
sst_egom_recruitment_bp<-mean(sst_EGOM_recruitment[1:30,2])
#get final bt dataset (years 1982-2020, anomaly adjusted)
sst_EGOM_recruitment$sst_anomaly<- sst_EGOM_recruitment$DATA_VALUE-sst_egom_recruitment_bp
sst_EGOM_recruitment<-sst_EGOM_recruitment[1:39,c(1,3)]
#lag 1 year
sst_EGOM_recruitment$sst_anomaly<-lag(sst_EGOM_recruitment$sst_anomaly)
rm(sst_egom_recruitment_bp)
####WGOM (May-August (year-1) means)####
sst_WGOM_recruitment <-subset(sst_WGOM , Month >= 5 & Month <= 8)
sst_WGOM_recruitment <- aggregate(DATA_VALUE~Year, data=sst_WGOM_recruitment,FUN=mean)
#get anomaly period means (1982-2011)
sst_wgom_recruitment_bp<-mean(sst_WGOM_recruitment[1:30,2])
#get final bt dataset (years 1982-2020, anomaly adjusted)
sst_WGOM_recruitment$sst_anomaly<- sst_WGOM_recruitment$DATA_VALUE-sst_wgom_recruitment_bp
sst_WGOM_recruitment<-sst_WGOM_recruitment[1:39,c(1,3)]
#lag 1 year
sst_WGOM_recruitment$sst_anomaly<-lag(sst_WGOM_recruitment$sst_anomaly)
rm(sst_wgom_recruitment_bp)
####GBK (Jan-April (year-1) means)####
sst_GBK_recruitment <-subset(sst_GBK , Month >= 1 & Month <= 4)
sst_GBK_recruitment <- aggregate(DATA_VALUE~Year, data=sst_GBK_recruitment,FUN=mean)
#get anomaly period means (1982-2011)
sst_gbk_recruitment_bp<-mean(sst_GBK_recruitment[1:30,2])
#get final bt dataset (years 1982-2020, anomaly adjusted)
sst_GBK_recruitment$sst_anomaly<- sst_GBK_recruitment$DATA_VALUE-sst_gbk_recruitment_bp
sst_GBK_recruitment<-sst_GBK_recruitment[1:39,c(1,3)]
#lag 1 year
sst_GBK_recruitment$sst_anomaly<-lag(sst_GBK_recruitment$sst_anomaly)
rm(sst_gbk_recruitment_bp)
####SNE (Jan-March (year-1) means)####
sst_SNE_recruitment <-subset(sst_SNE , Month >= 1 & Month <= 3)
sst_SNE_recruitment <- aggregate(DATA_VALUE~Year, data=sst_SNE_recruitment,FUN=mean)
#get anomaly period means (1982-2011)
sst_sne_recruitment_bp<-mean(sst_SNE_recruitment[1:30,2])
#get final bt dataset (years 1982-2020, anomaly adjusted)
sst_SNE_recruitment$sst_anomaly<- sst_SNE_recruitment$DATA_VALUE-sst_sne_recruitment_bp
sst_SNE_recruitment<-sst_SNE_recruitment[1:39,c(1,3)]
#lag 1 year
sst_SNE_recruitment$sst_anomaly<-lag(sst_SNE_recruitment$sst_anomaly)
rm(sst_sne_recruitment_bp)
##### Growth and Distribution models #####
####get 6-month mean values for each year (6-months prior to start of seasonal survey)####
#### FALL ####
####EGOM (April-September (year) means)####
sst_EGOM_GD_FL <-subset(sst_EGOM , Month >= 4 & Month <= 9)
sst_EGOM_GD_FL <- aggregate(DATA_VALUE~Year, data=sst_EGOM_GD_FL,FUN=mean)
#get anomaly period means (1982-2011)
sst_egom_GD_FL_bp<-mean(sst_EGOM_GD_FL[1:30,2])
#get final bt dataset (years 1982-2020, anomaly adjusted)
sst_EGOM_GD_FL$sst_anomaly<- sst_EGOM_GD_FL$DATA_VALUE-sst_egom_GD_FL_bp
sst_EGOM_GD_FL<-sst_EGOM_GD_FL[1:39,c(1,3)]
rm(sst_egom_GD_FL_bp)
####WGOM (March-August(year) means)####
sst_WGOM_GD_FL <-subset(sst_WGOM , Month >= 3 & Month <= 8)
sst_WGOM_GD_FL <- aggregate(DATA_VALUE~Year, data=sst_WGOM_GD_FL,FUN=mean)
#get anomaly period means (1982-2011)
sst_wgom_GD_FL_bp<-mean(sst_WGOM_GD_FL[1:30,2])
#get final bt dataset (years 1982-2020, anomaly adjusted)
sst_WGOM_GD_FL$sst_anomaly<- sst_WGOM_GD_FL$DATA_VALUE-sst_wgom_GD_FL_bp
sst_WGOM_GD_FL<-sst_WGOM_GD_FL[1:39,c(1,3)]
rm(sst_wgom_GD_FL_bp)
####GBK (March-August(year) means)####
sst_GBK_GD_FL <-subset(sst_GBK , Month >= 3 & Month <= 8)
sst_GBK_GD_FL <- aggregate(DATA_VALUE~Year, data=sst_GBK_GD_FL,FUN=mean)
#get anomaly period means (1982-2011)
sst_gbk_GD_FL_bp<-mean(sst_GBK_GD_FL[1:30,2])
#get final bt dataset (years 1982-2020, anomaly adjusted)
sst_GBK_GD_FL$sst_anomaly<- sst_GBK_GD_FL$DATA_VALUE-sst_gbk_GD_FL_bp
sst_GBK_GD_FL<-sst_GBK_GD_FL[1:39,c(1,3)]
rm(sst_gbk_GD_FL_bp)
####SNE (March-August(year) means)####
sst_SNE_GD_FL <-subset(sst_SNE , Month >= 3 & Month <= 8)
sst_SNE_GD_FL <- aggregate(DATA_VALUE~Year, data=sst_SNE_GD_FL,FUN=mean)
#get anomaly period means (1982-2011)
sst_sne_GD_FL_bp<-mean(sst_SNE_GD_FL[1:30,2])
#get final bt dataset (years 1982-2020, anomaly adjusted)
sst_SNE_GD_FL$sst_anomaly<- sst_SNE_GD_FL$DATA_VALUE-sst_sne_GD_FL_bp
sst_SNE_GD_FL<-sst_SNE_GD_FL[1:39,c(1,3)]
rm(sst_sne_GD_FL_bp)
#### SPRING ####
####EGOM (October (year-1)-March (year) means)####
sst_EGOM_GD_SP <-subset(sst_EGOM,Month==10|Month==11|Month==12|Month==1|Month==2|Month==3)
#aggregating correct months from each year
sst_EGOM_GD_SP$newlag <- ifelse(sst_EGOM_GD_SP$Month==1|sst_EGOM_GD_SP$Month==2|sst_EGOM_GD_SP$Month==3,sst_EGOM_GD_SP$Year,sst_EGOM_GD_SP$Year+1)
sst_EGOM_GD_SP<-aggregate(DATA_VALUE~newlag, data=sst_EGOM_GD_SP,FUN=mean)
#get anomaly period means (1982-2011)
sst_egom_GD_SP_bp<-mean(sst_EGOM_GD_SP[1:30,2])
#get final bt dataset (years 1982-2020, anomaly adjusted)
sst_EGOM_GD_SP$sst_anomaly<- sst_EGOM_GD_SP$DATA_VALUE-sst_egom_GD_SP_bp
sst_EGOM_GD_SP<-sst_EGOM_GD_SP[1:39,c(1,3)]
names(sst_EGOM_GD_SP)[1]<-"Year"
rm(sst_egom_GD_SP_bp)
####WGOM (October (year-1)-March (year) means)####
sst_WGOM_GD_SP <-subset(sst_WGOM,Month==10|Month==11|Month==12|Month==1|Month==2|Month==3)
#aggregating correct months from each year
sst_WGOM_GD_SP$newlag <- ifelse(sst_WGOM_GD_SP$Month==1|sst_WGOM_GD_SP$Month==2|sst_WGOM_GD_SP$Month==3,sst_WGOM_GD_SP$Year,sst_WGOM_GD_SP$Year+1)
sst_WGOM_GD_SP<-aggregate(DATA_VALUE~newlag, data=sst_WGOM_GD_SP,FUN=mean)
#get anomaly period means (1982-2011)
sst_wgom_GD_SP_bp<-mean(sst_WGOM_GD_SP[1:30,2])
#get final bt dataset (years 1982-2020, anomaly adjusted)
sst_WGOM_GD_SP$sst_anomaly<- sst_WGOM_GD_SP$DATA_VALUE-sst_wgom_GD_SP_bp
sst_WGOM_GD_SP<-sst_WGOM_GD_SP[1:39,c(1,3)]
names(sst_WGOM_GD_SP)[1]<-"Year"
rm(sst_wgom_GD_SP_bp)
####GBK (September (year-1)-February (year) means)####
sst_GBK_GD_SP <-subset(sst_GBK,Month==9|Month==10|Month==11|Month==12|Month==1|Month==2)
#aggregating correct months from each year
sst_GBK_GD_SP$newlag <- ifelse(sst_GBK_GD_SP$Month==1|sst_GBK_GD_SP$Month==2,sst_GBK_GD_SP$Year,sst_GBK_GD_SP$Year+1)
sst_GBK_GD_SP<-aggregate(DATA_VALUE~newlag, data=sst_GBK_GD_SP,FUN=mean)
#get anomaly period means (1982-2011)
sst_gbk_GD_SP_bp<-mean(sst_GBK_GD_SP[1:30,2])
#get final bt dataset (years 1982-2020, anomaly adjusted)
sst_GBK_GD_SP$sst_anomaly<- sst_GBK_GD_SP$DATA_VALUE-sst_gbk_GD_SP_bp
sst_GBK_GD_SP<-sst_GBK_GD_SP[1:39,c(1,3)]
names(sst_GBK_GD_SP)[1]<-"Year"
rm(sst_gbk_GD_SP_bp)
####SNE (August (year-1)-January (year) means)####
sst_SNE_GD_SP <-subset(sst_SNE,Month==8|Month==9|Month==10|Month==11|Month==12|Month==1)
#aggregating correct months from each year
sst_SNE_GD_SP$newlag <- ifelse(sst_SNE_GD_SP$Month==1,sst_SNE_GD_SP$Year,sst_SNE_GD_SP$Year+1)
sst_SNE_GD_SP<-aggregate(DATA_VALUE~newlag, data=sst_SNE_GD_SP,FUN=mean)
#get anomaly period means (1982-2011)
sst_sne_GD_SP_bp<-mean(sst_SNE_GD_SP[1:30,2])
#get final bt dataset (years 1982-2020, anomaly adjusted)
sst_SNE_GD_SP$sst_anomaly<- sst_SNE_GD_SP$DATA_VALUE-sst_sne_GD_SP_bp
sst_SNE_GD_SP<-sst_SNE_GD_SP[1:39,c(1,3)]
names(sst_SNE_GD_SP)[1]<-"Year"
rm(sst_sne_GD_SP_bp)
#### Combining BT and SST data into one df to cleanup environment####
EGOM_recruitment_SP<-merge(Hubert_EGOM_bt_SP,sst_EGOM_recruitment,by="Year")
WGOM_recruitment_SP<-merge(Hubert_WGOM_bt_SP,sst_WGOM_recruitment,by="Year")
GBK_recruitment_SP<-merge(Hubert_GBK_bt_SP,sst_GBK_recruitment,by="Year")
SNE_recruitment_SP<-merge(Hubert_SNE_bt_SP,sst_SNE_recruitment,by="Year")
EGOM_recruitment_FL<-merge(Hubert_EGOM_bt_FL,sst_EGOM_recruitment,by="Year")
WGOM_recruitment_FL<-merge(Hubert_WGOM_bt_FL,sst_WGOM_recruitment,by="Year")
GBK_recruitment_FL<-merge(Hubert_GBK_bt_FL,sst_GBK_recruitment,by="Year")
SNE_recruitment_FL<-merge(Hubert_SNE_bt_FL,sst_SNE_recruitment,by="Year")

EGOM_GD_SP<-merge(Hubert_EGOM_bt_SP,sst_EGOM_GD_SP,by="Year")
WGOM_GD_SP<-merge(Hubert_WGOM_bt_SP,sst_WGOM_GD_SP,by="Year")
GBK_GD_SP<-merge(Hubert_GBK_bt_SP,sst_GBK_GD_SP,by="Year")
SNE_GD_SP<-merge(Hubert_SNE_bt_SP,sst_SNE_GD_SP,by="Year")
EGOM_GD_FL<-merge(Hubert_EGOM_bt_FL,sst_EGOM_GD_FL,by="Year")
WGOM_GD_FL<-merge(Hubert_WGOM_bt_FL,sst_WGOM_GD_FL,by="Year")
GBK_GD_FL<-merge(Hubert_GBK_bt_FL,sst_GBK_GD_FL,by="Year")
SNE_GD_FL<-merge(Hubert_SNE_bt_FL,sst_SNE_GD_FL,by="Year")

rm(Hubert_EGOM_bt_FL,sst_EGOM_recruitment,Hubert_WGOM_bt_FL,sst_WGOM_recruitment,Hubert_GBK_bt_FL,sst_GBK_recruitment,Hubert_SNE_bt_FL,sst_SNE_recruitment,Hubert_EGOM_bt_SP,Hubert_WGOM_bt_SP,Hubert_GBK_bt_SP,Hubert_SNE_bt_SP,cod_sst, sst_EGOM,sst_WGOM,sst_GBK,sst_SNE,sst_EGOM_GD_FL,sst_EGOM_GD_SP,sst_WGOM_GD_FL,sst_WGOM_GD_SP,sst_GBK_GD_FL,sst_GBK_GD_SP,sst_SNE_GD_FL,sst_SNE_GD_SP)
####################################################################### GSI ########
GSI<-ecodata::gsi
GSI<-GSI[325:804,c(1,3)]
names(GSI)[1]<-"Year"
GSI$month<-rep(1:12,times=40)
GSI$Year<-rep(1981:2020,each=12)
##### Recruitment Models #####
####get 6-month mean values for each year (6-months starting at peak spawning period by stock)####
####EGOM (May-October (year-1) means)####
gsi_EGOM_recruitment <-subset(GSI , month >= 5 & month <= 10)
gsi_EGOM_recruitment <- aggregate(Value~Year, data=gsi_EGOM_recruitment,FUN=mean)
names(gsi_EGOM_recruitment)[2]<-"GSI"
#lag 1 year
gsi_EGOM_recruitment$GSI<-lag(gsi_EGOM_recruitment$GSI)
####WGOM (May-October (year-1) means)####
gsi_WGOM_recruitment <-subset(GSI , month >= 5 & month <= 10)
gsi_WGOM_recruitment <- aggregate(Value~Year, data=gsi_WGOM_recruitment,FUN=mean)
names(gsi_WGOM_recruitment)[2]<-"GSI"
#lag 1 year
gsi_WGOM_recruitment$GSI<-lag(gsi_WGOM_recruitment$GSI)
####GBK (January-June (year-1) means)####
gsi_GBK_recruitment <-subset(GSI , month >= 1 & month <= 6)
gsi_GBK_recruitment <- aggregate(Value~Year, data=gsi_GBK_recruitment,FUN=mean)
names(gsi_GBK_recruitment)[2]<-"GSI"
#lag 1 year
gsi_GBK_recruitment$GSI<-lag(gsi_GBK_recruitment$GSI)
####SNE (January-June (year-1) means)####
gsi_SNE_recruitment <-subset(GSI , month >= 1 & month <= 6)
gsi_SNE_recruitment <- aggregate(Value~Year, data=gsi_SNE_recruitment,FUN=mean)
names(gsi_SNE_recruitment)[2]<-"GSI"
#lag 1 year
gsi_SNE_recruitment$GSI<-lag(gsi_SNE_recruitment$GSI)
##### Growth and Distribution models #####
####get 6-month mean values for each year (6-months prior to start of seasonal survey)####
#### FALL ####
####EGOM (April-September (year) means)####
gsi_EGOM_GD_FL <-subset(GSI , month >= 4 & month <= 9)
gsi_EGOM_GD_FL <- aggregate(Value~Year, data=gsi_EGOM_GD_FL,FUN=mean)
#get final bt dataset (years 1982-2020)
gsi_EGOM_GD_FL$GSI<- gsi_EGOM_GD_FL$Value
gsi_EGOM_GD_FL<-gsi_EGOM_GD_FL[2:40,c(1,3)]
####WGOM (March-August(year) means)####
gsi_WGOM_GD_FL <-subset(GSI, month >= 3 & month <= 8)
gsi_WGOM_GD_FL <- aggregate(Value~Year, data=gsi_WGOM_GD_FL,FUN=mean)
#get final bt dataset (years 1982-2020)
gsi_WGOM_GD_FL$GSI<- gsi_WGOM_GD_FL$Value
gsi_WGOM_GD_FL<-gsi_WGOM_GD_FL[2:40,c(1,3)]
####GBK (March-August(year) means)####
gsi_GBK_GD_FL <-subset(GSI , month >= 3 & month <= 8)
gsi_GBK_GD_FL <- aggregate(Value~Year, data=gsi_GBK_GD_FL,FUN=mean)
#get final bt dataset (years 1982-2020)
gsi_GBK_GD_FL$GSI<- gsi_GBK_GD_FL$Value
gsi_GBK_GD_FL<-gsi_GBK_GD_FL[2:40,c(1,3)]
####SNE (March-August(year) means)####
gsi_SNE_GD_FL <-subset(GSI , month >= 3 & month <= 8)
gsi_SNE_GD_FL <- aggregate(Value~Year, data=gsi_SNE_GD_FL,FUN=mean)
#get final bt dataset (years 1982-2020)
gsi_SNE_GD_FL$GSI<- gsi_SNE_GD_FL$Value
gsi_SNE_GD_FL<-gsi_SNE_GD_FL[2:40,c(1,3)]

#### SPRING ####
####EGOM (October (year-1)-March (year) means)####
gsi_EGOM_GD_SP <-subset(GSI,month==10|month==11|month==12|month==1|month==2|month==3)
#aggregating correct months from each year
gsi_EGOM_GD_SP$newlag <- ifelse(gsi_EGOM_GD_SP$month==1|gsi_EGOM_GD_SP$month==2|gsi_EGOM_GD_SP$month==3,gsi_EGOM_GD_SP$Year,gsi_EGOM_GD_SP$Year+1)
gsi_EGOM_GD_SP<-aggregate(Value~newlag, data=gsi_EGOM_GD_SP,FUN=mean)
#get final bt dataset (years 1982-2020)
gsi_EGOM_GD_SP$GSI<- gsi_EGOM_GD_SP$Value
gsi_EGOM_GD_SP<-gsi_EGOM_GD_SP[2:40,c(1,3)]
names(gsi_EGOM_GD_SP)[1]<-"Year"

####WGOM (October (year-1)-March (year) means)####
gsi_WGOM_GD_SP <-subset(GSI,month==10|month==11|month==12|month==1|month==2|month==3)
#aggregating correct months from each year
gsi_WGOM_GD_SP$newlag <- ifelse(gsi_WGOM_GD_SP$month==1|gsi_WGOM_GD_SP$month==2|gsi_WGOM_GD_SP$month==3,gsi_WGOM_GD_SP$Year,gsi_WGOM_GD_SP$Year+1)
gsi_WGOM_GD_SP<-aggregate(Value~newlag, data=gsi_WGOM_GD_SP,FUN=mean)
#get final bt dataset (years 1982-2020)
gsi_WGOM_GD_SP$GSI<- gsi_WGOM_GD_SP$Value
gsi_WGOM_GD_SP<-gsi_WGOM_GD_SP[2:40,c(1,3)]
names(gsi_WGOM_GD_SP)[1]<-"Year"

####GBK (September (year-1)-February (year) means)####
gsi_GBK_GD_SP <-subset(GSI,month==9|month==10|month==11|month==12|month==1|month==2)
#aggregating correct months from each year
gsi_GBK_GD_SP$newlag <- ifelse(gsi_GBK_GD_SP$month==1|gsi_GBK_GD_SP$month==2,gsi_GBK_GD_SP$Year,gsi_GBK_GD_SP$Year+1)
gsi_GBK_GD_SP<-aggregate(Value~newlag, data=gsi_GBK_GD_SP,FUN=mean)
#get final bt dataset (years 1982-2020)
gsi_GBK_GD_SP$GSI<- gsi_GBK_GD_SP$Value
gsi_GBK_GD_SP<-gsi_GBK_GD_SP[2:40,c(1,3)]
names(gsi_GBK_GD_SP)[1]<-"Year"
####SNE (August (year-1)-January (year) means)####
gsi_SNE_GD_SP <-subset(GSI,month==8|month==9|month==10|month==11|month==12|month==1)
#aggregating correct months from each year
gsi_SNE_GD_SP$newlag <- ifelse(gsi_SNE_GD_SP$month==1,gsi_SNE_GD_SP$Year,gsi_SNE_GD_SP$Year+1)
gsi_SNE_GD_SP<-aggregate(Value~newlag, data=gsi_SNE_GD_SP,FUN=mean)
#get final bt dataset (years 1982-2020)
gsi_SNE_GD_SP$GSI<- gsi_SNE_GD_SP$Value
gsi_SNE_GD_SP<-gsi_SNE_GD_SP[2:40,c(1,3)]
names(gsi_SNE_GD_SP)[1]<-"Year"
rm(GSI)
######################################################## Cod Heatwave data ####
cod_heatwave<-as.data.frame(ecodata::ESP_heatwave_cod)
EGOM_chw<-cod_heatwave[(cod_heatwave$stock_id=="EGOM") & (cod_heatwave$Var=="cumulative intensity"), ]
names(EGOM_chw)[3] <- "EGOM_hw"
EGOM_chw <- EGOM_chw[, -c(2,4:5)]
WGOM_chw<-cod_heatwave[(cod_heatwave$stock_id=="WGOM") & (cod_heatwave$Var=="cumulative intensity"), ]
names(WGOM_chw)[3] <- "WGOM_hw"
WGOM_chw <- WGOM_chw[, -c(2,4:5)]
GB_chw<-cod_heatwave[(cod_heatwave$stock_id=="GBK") & (cod_heatwave$Var=="cumulative intensity"), ]
names(GB_chw)[3] <- "GB_hw"
GB_chw <- GB_chw[, -c(2,4:5)]
SNE_chw<-cod_heatwave[(cod_heatwave$stock_id=="SNE") & (cod_heatwave$Var=="cumulative intensity"), ]
names(SNE_chw)[3] <- "SNE_hw"
SNE_chw <- SNE_chw[, -c(2,4:5)]
#EGOM
hw_recruitment_EGOM<-EGOM_chw
hw_recruitment_EGOM$Heatwave<-lag(EGOM_chw$EGOM_hw)
names(hw_recruitment_EGOM)[1]<-"Year"
hw_recruitment_EGOM<-hw_recruitment_EGOM[, -c(2)]
#WGOM
hw_recruitment_WGOM<-WGOM_chw
hw_recruitment_WGOM$Heatwave<-lag(WGOM_chw$WGOM_hw)
names(hw_recruitment_WGOM)[1]<-"Year"
hw_recruitment_WGOM<-hw_recruitment_WGOM[, -c(2)]
#GBK
hw_recruitment_GBK<-GB_chw
hw_recruitment_GBK$Heatwave<-lag(GB_chw$GB_hw)
names(hw_recruitment_GBK)[1]<-"Year"
hw_recruitment_GBK<-hw_recruitment_GBK[, -c(2)]
#SNE
hw_recruitment_SNE<-SNE_chw
hw_recruitment_SNE$Heatwave<-lag(SNE_chw$SNE_hw)
names(hw_recruitment_SNE)[1]<-"Year"
hw_recruitment_SNE<-hw_recruitment_SNE[, -c(2)]
#EGOM Growth Distribution (not lagged)
hw_EGOM<-EGOM_chw
hw_EGOM$Heatwave<-EGOM_chw$EGOM_hw
names(hw_EGOM)[1]<-"Year"
hw_EGOM<-hw_EGOM[, -c(2)]
#WGOM Growth Distribution (not lagged)
hw_WGOM<-WGOM_chw
hw_WGOM$Heatwave<-WGOM_chw$WGOM_hw
names(hw_WGOM)[1]<-"Year"
hw_WGOM<-hw_WGOM[, -c(2)]
#GBK Growth Distribution (not lagged)
hw_GBK<-GB_chw
hw_GBK$Heatwave<-GB_chw$GB_hw
names(hw_GBK)[1]<-"Year"
hw_GBK<-hw_GBK[, -c(2)]
#SNE Growth Distribution (not lagged)
hw_SNE<-SNE_chw
hw_SNE$Heatwave<-SNE_chw$SNE_hw
names(hw_SNE)[1]<-"Year"
hw_SNE<-hw_SNE[, -c(2)]
rm(cod_heatwave,EGOM_chw,WGOM_chw,GB_chw,SNE_chw)
########################################################
##### zooplankton data#####
#using Spring GB and SNE zooplankton data to best align with spawning timing in those areas
zoo_Spring_GBK<-read.csv(here("data/zooplankton/GB_spring_zooplankton.csv"))
zoo_Spring_SNE<-read.csv(here("data/zooplankton/SNE_spring_zooplankton.csv"))
zoo_Spring_GBK<-zoo_Spring_GBK[c(1,8,9)]
names(zoo_Spring_GBK)[1] <- "Year"
zoo_Spring_GBK$calfin_100m3<-lag(zoo_Spring_GBK$calfin_100m3)
zoo_Spring_GBK$pseudo_100m3<-lag(zoo_Spring_GBK$pseudo_100m3)
zoo_Spring_GBK<-zoo_Spring_GBK[6:42,]

zoo_Spring_SNE<-zoo_Spring_SNE[c(1,8,9)]
names(zoo_Spring_SNE)[1] <- "Year"
zoo_Spring_SNE$calfin_100m3<-lag(zoo_Spring_SNE$calfin_100m3)
zoo_Spring_SNE$pseudo_100m3<-lag(zoo_Spring_SNE$pseudo_100m3)
zoo_Spring_SNE<-zoo_Spring_SNE[6:41,]
#using summer zooplankton data for GOM to best align with spring spawning timing in those areas
zoo_Summer_EGOM<-read.csv(here("data/zooplankton/EGOM_summer_zooplankton.csv"))
zoo_Summer_WGOM<-read.csv(here("data/zooplankton/WGOM_summer_zooplankton.csv"))
zoo_Summer_EGOM<-zoo_Summer_EGOM[c(2,9,10)]
names(zoo_Summer_EGOM)[1] <- "Year"
zoo_Summer_EGOM$calfin_100m3<-lag(zoo_Summer_EGOM$calfin_100m3)
zoo_Summer_EGOM$pseudo_100m3<-lag(zoo_Summer_EGOM$pseudo_100m3)
zoo_Summer_EGOM<-zoo_Summer_EGOM[5:35,]

zoo_Summer_WGOM<-zoo_Summer_WGOM[c(2,9,10)]
names(zoo_Summer_WGOM)[1] <- "Year"
zoo_Summer_WGOM$calfin_100m3<-lag(zoo_Summer_WGOM$calfin_100m3)
zoo_Summer_WGOM$pseudo_100m3<-lag(zoo_Summer_WGOM$pseudo_100m3)
zoo_Summer_WGOM<-zoo_Summer_WGOM[6:39,]

#################################### SSB Data ######################################
#load data from "SSB_estimates.R" file. SSB estimates are based on ages 4+
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

##### filter cod NAA for recruitment dataset#######
cod_NAA<-read.csv(here("data/cod_NAA.csv"))
cod_NAA<- filter(cod_NAA, SURVEY == "NEFSC_BTS")
names(cod_NAA)[2]<-"Year"
cod_NAA<-cod_NAA[c(2,3,7,17)]
EGOM_NAA<-filter(cod_NAA, STOCK == "EGOM")
WGOM_NAA<-filter(cod_NAA, STOCK == "WGOM")
SNE_NAA<-filter(cod_NAA, STOCK == "SNE")
GBK_NAA<-filter(cod_NAA, STOCK == "GBK")
rm(cod_NAA)
####Combine data into separate spring and fall data frames####
### EGOM Recruitment FALL ####
EGOM_recruitment_fall <- list(EGOM_recruitment_FL,hw_recruitment_EGOM,gsi_EGOM_recruitment,zoo_Summer_EGOM,EGOM_NAA[which(EGOM_NAA$SEASON=='FALL'),c(1,3)],SSB_Fall_EGOM) #put all data frames into list
EGOM_recruitment_fall<-EGOM_recruitment_fall %>% reduce(full_join, by='Year')#merge all data frames in list
EGOM_recruitment_fall<-EGOM_recruitment_fall[1:39,-c(9)]
### EGOM Recruitment SPRING ####
EGOM_recruitment_spring <- list(EGOM_recruitment_SP,hw_recruitment_EGOM,gsi_EGOM_recruitment,zoo_Summer_EGOM,EGOM_NAA[which(EGOM_NAA$SEASON=='SPRING'),c(1,3)],SSB_Spring_EGOM) #put all data frames into list
EGOM_recruitment_spring<-EGOM_recruitment_spring%>% reduce(full_join, by='Year')#merge all data frames in list
EGOM_recruitment_spring<-EGOM_recruitment_spring[1:39,-c(9)]
### EGOM growth/distribution FALL ####
EGOM_GD_fall <- list(EGOM_GD_FL,hw_EGOM,gsi_EGOM_GD_FL,zoo_Summer_EGOM,EGOM_NAA[which(EGOM_NAA$SEASON=='FALL'),c(1,3)],SSB_Fall_EGOM) #put all data frames into list
EGOM_GD_fall<-EGOM_GD_fall %>% reduce(full_join, by='Year')#merge all data frames in list
EGOM_GD_fall<-EGOM_GD_fall[1:39,-c(9)]
### EGOM growth/distribution SPRING ####
EGOM_GD_spring <- list(EGOM_GD_SP,hw_EGOM,gsi_EGOM_GD_SP,zoo_Summer_EGOM,EGOM_NAA[which(EGOM_NAA$SEASON=='SPRING'),c(1,3)],SSB_Spring_EGOM) #put all data frames into list
EGOM_GD_spring<-EGOM_GD_spring%>% reduce(full_join, by='Year')#merge all data frames in list
EGOM_GD_spring<-EGOM_GD_spring[1:39,-c(9)]
#### Save EGOM .csv's########
write.csv(EGOM_recruitment_fall,here("data/final_env_data/EGOM_recruitment_fall.csv"), row.names=FALSE)
write.csv(EGOM_recruitment_spring,here("data/final_env_data/EGOM_recruitment_spring.csv"), row.names=FALSE)
write.csv(EGOM_GD_fall,here("data/final_env_data/EGOM_GD_fall.csv"), row.names=FALSE)
write.csv(EGOM_GD_spring,here("data/final_env_data/EGOM_GD_spring.csv"), row.names=FALSE)
rm(list=ls(pattern="EGOM"))

### WGOM Recruitment FALL ####
WGOM_recruitment_fall <- list(WGOM_recruitment_FL,hw_recruitment_WGOM,gsi_WGOM_recruitment,zoo_Summer_WGOM,WGOM_NAA[which(WGOM_NAA$SEASON=='FALL'),c(1,3)],SSB_Fall_WGOM) #put all data frames into list
WGOM_recruitment_fall<-WGOM_recruitment_fall %>% reduce(full_join, by='Year')#merge all data frames in list
WGOM_recruitment_fall<-WGOM_recruitment_fall[1:39,-c(9)]
### WGOM Recruitment SPRING ####
WGOM_recruitment_spring <- list(WGOM_recruitment_SP,hw_recruitment_WGOM,gsi_WGOM_recruitment,zoo_Summer_WGOM,WGOM_NAA[which(WGOM_NAA$SEASON=='SPRING'),c(1,3)],SSB_Spring_WGOM) #put all data frames into list
WGOM_recruitment_spring<-WGOM_recruitment_spring%>% reduce(full_join, by='Year')#merge all data frames in list
WGOM_recruitment_spring<-WGOM_recruitment_spring[1:39,-c(9)]
### WGOM growth/distribution FALL ####
WGOM_GD_fall <- list(WGOM_GD_FL,hw_WGOM,gsi_WGOM_GD_FL,zoo_Summer_WGOM,WGOM_NAA[which(WGOM_NAA$SEASON=='FALL'),c(1,3)],SSB_Fall_WGOM) #put all data frames into list
WGOM_GD_fall<-WGOM_GD_fall %>% reduce(full_join, by='Year')#merge all data frames in list
WGOM_GD_fall<-WGOM_GD_fall[1:39,-c(9)]
### WGOM growth/distribution SPRING ####
WGOM_GD_spring <- list(WGOM_GD_SP,hw_WGOM,gsi_WGOM_GD_SP,zoo_Summer_WGOM,WGOM_NAA[which(WGOM_NAA$SEASON=='SPRING'),c(1,3)],SSB_Spring_WGOM) #put all data frames into list
WGOM_GD_spring<-WGOM_GD_spring%>% reduce(full_join, by='Year')#merge all data frames in list
WGOM_GD_spring<-WGOM_GD_spring[1:39,-c(9)]
#### Save WGOM .csv's########
write.csv(WGOM_recruitment_fall,here("data/final_env_data/WGOM_recruitment_fall.csv"), row.names=FALSE)
write.csv(WGOM_recruitment_spring,here("data/final_env_data/WGOM_recruitment_spring.csv"), row.names=FALSE)
write.csv(WGOM_GD_fall,here("data/final_env_data/WGOM_GD_fall.csv"), row.names=FALSE)
write.csv(WGOM_GD_spring,here("data/final_env_data/WGOM_GD_spring.csv"), row.names=FALSE)
rm(list=ls(pattern="WGOM"))
### GBK Recruitment FALL ####
GBK_recruitment_fall <- list(GBK_recruitment_FL,hw_recruitment_GBK,gsi_GBK_recruitment,zoo_Spring_GBK,GBK_NAA[which(GBK_NAA$SEASON=='FALL'),c(1,3)],SSB_Fall_GBK) #put all data frames into list
GBK_recruitment_fall<-GBK_recruitment_fall %>% reduce(full_join, by='Year')#merge all data frames in list
GBK_recruitment_fall<-GBK_recruitment_fall[1:39,-c(9)]
### GBK Recruitment SPRING ####
GBK_recruitment_spring <- list(GBK_recruitment_SP,hw_recruitment_GBK,gsi_GBK_recruitment,zoo_Spring_GBK,GBK_NAA[which(GBK_NAA$SEASON=='SPRING'),c(1,3)],SSB_Spring_GBK) #put all data frames into list
GBK_recruitment_spring<-GBK_recruitment_spring%>% reduce(full_join, by='Year')#merge all data frames in list
GBK_recruitment_spring<-GBK_recruitment_spring[1:39,-c(9)]
### GBK growth/distribution FALL ####
GBK_GD_fall <- list(GBK_GD_FL,hw_GBK,gsi_GBK_GD_FL,zoo_Spring_GBK,GBK_NAA[which(GBK_NAA$SEASON=='FALL'),c(1,3)],SSB_Fall_GBK) #put all data frames into list
GBK_GD_fall<-GBK_GD_fall %>% reduce(full_join, by='Year')#merge all data frames in list
GBK_GD_fall<-GBK_GD_fall[1:39,-c(9)]
### GBK growth/distribution SPRING ####
GBK_GD_spring <- list(GBK_GD_SP,hw_GBK,gsi_GBK_GD_SP,zoo_Spring_GBK,GBK_NAA[which(GBK_NAA$SEASON=='SPRING'),c(1,3)],SSB_Spring_GBK) #put all data frames into list
GBK_GD_spring<-GBK_GD_spring%>% reduce(full_join, by='Year')#merge all data frames in list
GBK_GD_spring<-GBK_GD_spring[1:39,-c(9)]
#### Save GBK .csv's########
write.csv(GBK_recruitment_fall,here("data/final_env_data/GBK_recruitment_fall.csv"), row.names=FALSE)
write.csv(GBK_recruitment_spring,here("data/final_env_data/GBK_recruitment_spring.csv"), row.names=FALSE)
write.csv(GBK_GD_fall,here("data/final_env_data/GBK_GD_fall.csv"), row.names=FALSE)
write.csv(GBK_GD_spring,here("data/final_env_data/GBK_GD_spring.csv"), row.names=FALSE)
rm(list=ls(pattern="GBK"))
### SNE Recruitment FALL ####
SNE_recruitment_fall <- list(SNE_recruitment_FL,hw_recruitment_SNE,gsi_SNE_recruitment,zoo_Spring_SNE,SNE_NAA[which(SNE_NAA$SEASON=='FALL'),c(1,3)],SSB_Fall_SNE) #put all data frames into list
SNE_recruitment_fall<-SNE_recruitment_fall %>% reduce(full_join, by='Year')#merge all data frames in list
SNE_recruitment_fall<-SNE_recruitment_fall[1:39,-c(9)]
### SNE Recruitment SPRING ####
SNE_recruitment_spring <- list(SNE_recruitment_SP,hw_recruitment_SNE,gsi_SNE_recruitment,zoo_Spring_SNE,SNE_NAA[which(SNE_NAA$SEASON=='SPRING'),c(1,3)],SSB_Spring_SNE) #put all data frames into list
SNE_recruitment_spring<-SNE_recruitment_spring%>% reduce(full_join, by='Year')#merge all data frames in list
SNE_recruitment_spring<-SNE_recruitment_spring[1:39,-c(9)]
### SNE growth/distribution FALL ####
SNE_GD_fall <- list(SNE_GD_FL,hw_SNE,gsi_SNE_GD_FL,zoo_Spring_SNE,SNE_NAA[which(SNE_NAA$SEASON=='FALL'),c(1,3)],SSB_Fall_SNE) #put all data frames into list
SNE_GD_fall<-SNE_GD_fall %>% reduce(full_join, by='Year')#merge all data frames in list
SNE_GD_fall<-SNE_GD_fall[1:39,-c(9)]
### SNE growth/distribution SPRING ####
SNE_GD_spring <- list(SNE_GD_SP,hw_SNE,gsi_SNE_GD_SP,zoo_Spring_SNE,SNE_NAA[which(SNE_NAA$SEASON=='SPRING'),c(1,3)],SSB_Spring_SNE) #put all data frames into list
SNE_GD_spring<-SNE_GD_spring%>% reduce(full_join, by='Year')#merge all data frames in list
SNE_GD_spring<-SNE_GD_spring[1:39,-c(9)]
#### Save SNE .csv's########
write.csv(SNE_recruitment_fall,here("data/final_env_data/SNE_recruitment_fall.csv"), row.names=FALSE)
write.csv(SNE_recruitment_spring,here("data/final_env_data/SNE_recruitment_spring.csv"), row.names=FALSE)
write.csv(SNE_GD_fall,here("data/final_env_data/SNE_GD_fall.csv"), row.names=FALSE)
write.csv(SNE_GD_spring,here("data/final_env_data/SNE_GD_spring.csv"), row.names=FALSE)
rm(list=ls(pattern="SNE"))





########################### calculate distribution data all one stock area ##################
nm <- list.files(path =here("data/final_env_data/dist_growth"), pattern = ".csv", full.names = TRUE)
nm2 <- list.files(path =here("data/final_env_data/dist_growth"), pattern = ".csv", full.names =FALSE)
list2env(lapply(setNames(nm, make.names(gsub("*.csv$", "",nm2))),read.csv),envir=.GlobalEnv)
rm(nm,nm2)

distribution_spring<-do.call("rbind", list(EGOM_GD_spring,WGOM_GD_spring,GBK_GD_spring,SNE_GD_spring))
distribution_spring<-aggregate(distribution_spring[,2:9], list(distribution_spring$Year), mean,na.rm=TRUE)
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
distribution_spring[is.nan(distribution_spring)]<-NA
names(distribution_spring)[1]<-"Year"

distribution_fall<-do.call("rbind", list(EGOM_GD_fall,WGOM_GD_fall,GBK_GD_fall,SNE_GD_fall))
distribution_fall<-aggregate(distribution_fall[,2:9], list(distribution_fall$Year), mean,na.rm=TRUE)
distribution_fall[is.nan(distribution_fall)]<-NA
names(distribution_fall)[1]<-"Year"

#drom age1 and SSB columns from df
distribution_spring[1:(length(distribution_spring)-2)]
distribution_fall[1:(length(distribution_fall)-2)]
#save distribution df's
#write.csv(distribution_spring, here("data/final_env_data/distribution/distribution_spring.csv"), row.names=FALSE)
#write.csv(distribution_fall, here("data/final_env_data/distribution/distribution_fall.csv"), row.names=FALSE)
