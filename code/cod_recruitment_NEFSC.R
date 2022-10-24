###### Atlantic Cod stock assessment data GAM work
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
EGOM_recruitment_fall <- list(annual_GSI[24:67,c(1,2)], Bottom_temp_fall[,c(1,2)],Friedland_OISST_fall[,c(1,2)],c_od_heatwave[,c(1,2)],EGOM_NAA[ which(EGOM_NAA$SEASON=='FALL'),c(1,3)],SSB_Fall_EGOM) #put all data frames into list
EGOM_recruitment_fall<-EGOM_recruitment_fall %>% reduce(full_join, by='Year')#merge all data frames in list
EGOM_recruitment_fall<-EGOM_recruitment_fall[6:43, ]
### EGOM SPRING ####
EGOM_recruitment_spring <- list(annual_GSI[24:67,c(1,2)], Bottom_temp_spring[,c(1,2)],Friedland_OISST_spring[,c(1,2)],c_od_heatwave[,c(1,2)],EGOM_NAA[ which(EGOM_NAA$SEASON=='SPRING'),c(1,3)],SSB_Spring_EGOM)
EGOM_recruitment_spring<-EGOM_recruitment_spring %>% reduce(full_join, by='Year')
EGOM_recruitment_spring<-EGOM_recruitment_spring[6:43, ]
### WGOM FALL ####
WGOM_recruitment_fall <- list(annual_GSI[24:67,c(1,2)], Bottom_temp_fall[,c(1,4)],Friedland_OISST_fall[,c(1,4)],c_od_heatwave[,c(1,3)],WGOM_NAA[ which(WGOM_NAA$SEASON=='FALL'),c(1,3)],SSB_Fall_WGOM) #put all data frames into list
WGOM_recruitment_fall<-WGOM_recruitment_fall %>% reduce(full_join, by='Year')#merge all data frames in list
WGOM_recruitment_fall<-WGOM_recruitment_fall[6:43, ]
### WGOM SPRING ####
WGOM_recruitment_spring <- list(annual_GSI[24:67,c(1,2)], Bottom_temp_spring[,c(1,4)],Friedland_OISST_spring[,c(1,4)],c_od_heatwave[,c(1,3)],WGOM_NAA[ which(WGOM_NAA$SEASON=='SPRING'),c(1,3)],SSB_Spring_WGOM)
WGOM_recruitment_spring<-WGOM_recruitment_spring %>% reduce(full_join, by='Year')
WGOM_recruitment_spring<-WGOM_recruitment_spring[6:43, ]
### GBK FALL ####
GBK_recruitment_fall <- list(annual_GSI[24:67,c(1,2)], Bottom_temp_fall[,c(1,3)],Friedland_OISST_fall[,c(1,3)],c_od_heatwave[,c(1,4)],GBK_NAA[ which(GBK_NAA$SEASON=='FALL'),c(1,3)],SSB_Fall_GBK) #put all data frames into list
GBK_recruitment_fall<-GBK_recruitment_fall %>% reduce(full_join, by='Year')#merge all data frames in list
GBK_recruitment_fall<-GBK_recruitment_fall[6:43, ]
### GBK SPRING ####
GBK_recruitment_spring <- list(annual_GSI[24:67,c(1,2)], Bottom_temp_spring[,c(1,3)],Friedland_OISST_spring[,c(1,3)],c_od_heatwave[,c(1,4)],GBK_NAA[ which(GBK_NAA$SEASON=='SPRING'),c(1,3)],SSB_Spring_GBK)
GBK_recruitment_spring<-GBK_recruitment_spring %>% reduce(full_join, by='Year')
GBK_recruitment_spring<-GBK_recruitment_spring[6:43, ]
### SNE FALL ####
SNE_recruitment_fall <- list(annual_GSI[24:67,c(1,2)], Bottom_temp_fall[,c(1,5)],Friedland_OISST_fall[,c(1,5)],c_od_heatwave[,c(1,5)],SNE_NAA[ which(SNE_NAA$SEASON=='FALL'),c(1,3)],SSB_Fall_SNE) #No Fulton K data Available
SNE_recruitment_fall<-SNE_recruitment_fall %>% reduce(full_join, by='Year')#merge all data frames in list
SNE_recruitment_fall<-SNE_recruitment_fall[6:43, ]
### SNE SPRING ####
SNE_recruitment_spring <- list(annual_GSI[24:67,c(1,2)], Bottom_temp_spring[,c(1,5)],Friedland_OISST_spring[,c(1,5)],c_od_heatwave[,c(1,5)],SNE_NAA[ which(SNE_NAA$SEASON=='SPRING'),c(1,3)],SSB_Spring_SNE)
SNE_recruitment_spring<-SNE_recruitment_spring %>% reduce(full_join, by='Year')
SNE_recruitment_spring<-SNE_recruitment_spring[6:43, ]
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
rm(annual_GSI,Bottom_temp_fall,Bottom_temp_spring,Friedland_OISST_fall,Friedland_OISST_spring,EGOM_chw,WGOM_chw,GB_chw,SNE_chw,cod_heatwave,c_od_heatwave,cod_NAA,EGOM_NAA,WGOM_NAA,SNE_NAA,GBK_NAA,SSB_Fall_EGOM,SSB_Fall_GBK,SSB_Fall_SNE,SSB_Fall_WGOM,SSB_Spring_EGOM,SSB_Spring_GBK,SSB_Spring_SNE,SSB_Spring_WGOM)

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


####save df to use for growth GAMs ####
#write.csv(EGOM_recruitment_fall,here("data/stock_area_data/EGOM_recruitment_fall.csv"), row.names = FALSE)
#write.csv(EGOM_recruitment_spring,here("data/stock_area_data/EGOM_recruitment_spring.csv"), row.names = FALSE)

#write.csv(WGOM_recruitment_fall,here("data/stock_area_data/WGOM_recruitment_fall.csv"), row.names = FALSE)
#write.csv(WGOM_recruitment_spring,here("data/stock_area_data/WGOM_recruitment_spring.csv"), row.names = FALSE)

#write.csv(GBK_recruitment_fall,here("data/stock_area_data/GBK_recruitment_fall.csv"), row.names = FALSE)
#write.csv(GBK_recruitment_spring,here("data/stock_area_data/GBK_recruitment_spring.csv"), row.names = FALSE)

#write.csv(SNE_recruitment_fall,here("data/stock_area_data/SNE_recruitment_fall.csv"), row.names = FALSE)
#write.csv(SNE_recruitment_spring,here("data/stock_area_data/SNE_recruitment_spring.csv"), row.names = FALSE)
#### keep only anomaly temperature columns####
EGOM_recruitment_fall<-EGOM_recruitment_fall[c(1:2,5:10)]
EGOM_recruitment_spring<-EGOM_recruitment_spring[c(1:2,5:10)]
WGOM_recruitment_fall<-WGOM_recruitment_fall[c(1:2,5:10)]
WGOM_recruitment_spring<-WGOM_recruitment_spring[c(1:2,5:10)]
GBK_recruitment_fall<-GBK_recruitment_fall[c(1:2,5:10)]
GBK_recruitment_spring<-GBK_recruitment_spring[c(1:2,5:10)]
SNE_recruitment_fall<-SNE_recruitment_fall[c(1:2,5:10)]
SNE_recruitment_spring<-SNE_recruitment_spring[c(1:2,5:10)]
##### Create R/ssb column####
EGOM_recruitment_fall$RSSB<-EGOM_recruitment_fall$Age.1/EGOM_recruitment_fall$SSB
EGOM_recruitment_spring$RSSB<-EGOM_recruitment_spring$Age.1/EGOM_recruitment_spring$SSB
WGOM_recruitment_fall$RSSB<-WGOM_recruitment_fall$Age.1/WGOM_recruitment_fall$SSB
WGOM_recruitment_spring$RSSB<-WGOM_recruitment_spring$Age.1/WGOM_recruitment_spring$SSB
GBK_recruitment_fall$RSSB<-GBK_recruitment_fall$Age.1/GBK_recruitment_fall$SSB
GBK_recruitment_spring$RSSB<-GBK_recruitment_spring$Age.1/GBK_recruitment_spring$SSB
SNE_recruitment_fall$RSSB<-SNE_recruitment_fall$Age.1/SNE_recruitment_fall$SSB
SNE_recruitment_spring$RSSB<-SNE_recruitment_spring$Age.1/SNE_recruitment_spring$SSB
#### create lAGE1 column#######
EGOM_recruitment_fall$lAGE1<-(EGOM_recruitment_fall$Age.1)
WGOM_recruitment_fall$lAGE1<-(WGOM_recruitment_fall$Age.1)
GBK_recruitment_fall$lAGE1<-(GBK_recruitment_fall$Age.1)
SNE_recruitment_fall$lAGE1<-(SNE_recruitment_fall$Age.1)

EGOM_recruitment_spring$lAGE1<-(EGOM_recruitment_spring$Age.1)
WGOM_recruitment_spring$lAGE1<-(WGOM_recruitment_spring$Age.1)
GBK_recruitment_spring$lAGE1<-(GBK_recruitment_spring$Age.1)
SNE_recruitment_spring$lAGE1<-(SNE_recruitment_spring$Age.1)
#add small positive value to all cells to combat cells with 0 values
EGOM_recruitment_fall$lAGE1<-EGOM_recruitment_fall[,"lAGE1"]+0.00001
WGOM_recruitment_fall$lAGE1<-WGOM_recruitment_fall[,"lAGE1"]+0.00001
GBK_recruitment_fall$lAGE1<-GBK_recruitment_fall[,"lAGE1"]+0.00001
SNE_recruitment_fall$lAGE1<-SNE_recruitment_fall[,"lAGE1"]+0.00001

EGOM_recruitment_spring$lAGE1<-EGOM_recruitment_spring[,"lAGE1"]+0.00001
WGOM_recruitment_spring$lAGE1<-WGOM_recruitment_spring[,"lAGE1"]+0.00001
GBK_recruitment_spring$lAGE1<-GBK_recruitment_spring[,"lAGE1"]+0.00001
SNE_recruitment_spring$lAGE1<-SNE_recruitment_spring[,"lAGE1"]+0.00001

SNE_recruitment_fall$RSSB[is.nan(SNE_recruitment_fall$RSSB)]<-NA
SNE_recruitment_spring$RSSB[is.nan(SNE_recruitment_spring$RSSB)]<-NA

EGOM_recruitment_fall$lAGE1<-log(EGOM_recruitment_fall$lAGE1)
WGOM_recruitment_fall$lAGE1<-log(WGOM_recruitment_fall$lAGE1)
GBK_recruitment_fall$lAGE1<-log(GBK_recruitment_fall$lAGE1)
SNE_recruitment_fall$lAGE1<-log(SNE_recruitment_fall$lAGE1)

EGOM_recruitment_spring$lAGE1<-log(EGOM_recruitment_spring$lAGE1)
WGOM_recruitment_spring$lAGE1<-log(WGOM_recruitment_spring$lAGE1)
GBK_recruitment_spring$lAGE1<-log(GBK_recruitment_spring$lAGE1)
SNE_recruitment_spring$lAGE1<-log(SNE_recruitment_spring$lAGE1)
#### remove season columns #####
EGOM_recruitment_fall<-within(EGOM_recruitment_fall,rm("SEASON"))
WGOM_recruitment_fall<-within(WGOM_recruitment_fall,rm("SEASON"))
GBK_recruitment_fall<-within(GBK_recruitment_fall,rm("SEASON"))
SNE_recruitment_fall<-within(SNE_recruitment_fall,rm("SEASON"))

EGOM_recruitment_spring<-within(EGOM_recruitment_spring,rm("SEASON"))
WGOM_recruitment_spring<-within(WGOM_recruitment_spring,rm("SEASON"))
GBK_recruitment_spring<-within(GBK_recruitment_spring,rm("SEASON"))
SNE_recruitment_spring<-within(SNE_recruitment_spring,rm("SEASON"))
##### Start here ######
source(here("Code/Gam_data_exploration.R"))
#put dfs in list to apply across functions
df.list <- list(EGOM_recruitment_fall,EGOM_recruitment_spring,WGOM_recruitment_fall,WGOM_recruitment_spring,GBK_recruitment_fall,GBK_recruitment_spring,SNE_recruitment_fall,SNE_recruitment_spring)

#apply functions
lapply(df.list, dotchart_fun_10)
lapply(df.list, hist_fun10)
lapply(df.list, view_boxplot_fun10)
lapply(df.list, shapiro_fun)
lapply(df.list,Mypairs)
#### view age 1 timeseries ####
lineplot_seasonal(springdata=EGOM_recruitment_spring,
                  falldata=EGOM_recruitment_fall,
                  springY=EGOM_recruitment_spring$Age.1,
                  fallY=EGOM_recruitment_fall$Age.1,
                  fallX= EGOM_recruitment_fall$Year,
                  springX= EGOM_recruitment_spring$Year,
                  main="NEFSC Trawl Survey Numbers at Age 1: EGOM",
                  ylab="Abundance (numbers/tow)",
                  ylim=c(0,3.9))
lineplot_seasonal(springdata=WGOM_recruitment_spring,
                  falldata=WGOM_recruitment_fall,
                  springY=WGOM_recruitment_spring$Age.1,
                  fallY=WGOM_recruitment_fall$Age.1,
                  fallX= WGOM_recruitment_fall$Year,
                  springX= WGOM_recruitment_spring$Year,
                  main="NEFSC Trawl Survey Numbers at Age 1: WGOM",
                  ylab="Abundance (numbers/tow)",
                  ylim=c(0,3.9))
lineplot_seasonal(springdata=GBK_recruitment_spring,
                  falldata=GBK_recruitment_fall,
                  springY=GBK_recruitment_spring$Age.1,
                  fallY=GBK_recruitment_fall$Age.1,
                  fallX= GBK_recruitment_fall$Year,
                  springX= GBK_recruitment_spring$Year,
                  main="NEFSC Trawl Survey Numbers at Age 1: GBK",
                  ylab="Abundance (numbers/tow)",
                  ylim=c(0,3.9))
lineplot_seasonal(springdata=SNE_recruitment_spring,
                  falldata=SNE_recruitment_fall,
                  springY=SNE_recruitment_spring$Age.1,
                  fallY=SNE_recruitment_fall$Age.1,
                  fallX= SNE_recruitment_fall$Year,
                  springX= SNE_recruitment_spring$Year,
                  main="NEFSC Trawl Survey Numbers at Age 1: SNE",
                  ylab="Abundance (numbers/tow)",
                  ylim=c(0,3.9))

#############################################
################ Exploring to see if GAM loop works to test more combinations of gam variables quickly#####
GAM_LOOP_FUN<-function(Edata,k,correlated_vars1,correlated_vars2,correlated_vars3,correlated_vars4,correlated_vars5,correlated_vars6,folder_name,familyXYZ){
  
  #create all combinations of predictors
  predictor_combinations <- lapply(1:length(predictors), FUN = function(x){
    #create combination
    combination <- combn(predictors, m = x) |> as.data.table()
    #add s() to all for gam
    combination <- sapply(combination, FUN = function(y) paste0("s(", y, ",",k,")")) |> as.data.table()
    #collapse
    combination <- summarize_all(combination, .funs = paste0, collapse = "+")
    #unlist
    combination <- unlist(combination)
    #remove names
    names(combination) <- NULL
    #return
    return(combination)
  })
  #create all combinations of predictors
  predictor_combinations1 <- sapply(predictors, FUN = function(y) paste0("s(", y, ",",k,")"))|> as.data.table()
  rownames(predictor_combinations1) <- 1:nrow(predictor_combinations1) 
  #merge combinations of predictors as vector
  predictor_combinations <- do.call(c, predictor_combinations)
  predictor_combinations1 <- do.call(c, predictor_combinations1)
  predictor_combinations <- as.data.frame(predictor_combinations)
  predictor_combinations1 <- as.data.frame(predictor_combinations1)
  names(predictor_combinations1)[1]="predictor_combinations"
  predictor_combinations <- rbind(predictor_combinations,predictor_combinations1)
  
  ### remove list elements that contain duplicate/correlated independent variables
  ## see correlated_vars character list
  predictor_combinations <-predictor_combinations[!grepl(correlated_vars1, predictor_combinations$predictor_combinations)| !grepl(correlated_vars2 ,predictor_combinations$predictor_combinations),]
  
  if(correlated_vars3!="NA"||correlated_vars4!="NA"){
    #
    predictor_combinations <- as.data.frame(predictor_combinations)
    predictor_combinations <-predictor_combinations[!grepl(correlated_vars3, predictor_combinations$predictor_combinations)| !grepl(correlated_vars4,predictor_combinations$predictor_combinations),]
  }
  if(correlated_vars5!="NA"||correlated_vars6!="NA"){
    #
    predictor_combinations <- as.data.frame(predictor_combinations)
    predictor_combinations <-predictor_combinations[!grepl(correlated_vars5, predictor_combinations$predictor_combinations)| !grepl(correlated_vars6,predictor_combinations$predictor_combinations),]  
  }
  #create folder to save results to
  if(!dir.exists("data/trial_results")){
    dir.create("data/trial_results")
  }
  if(!dir.exists(paste0("data/trial_results/",folder_name))){
    dir.create(paste0("data/trial_results/",folder_name))
  }
  if(!dir.exists(paste0("data/trial_results/",folder_name,"/models"))){
    dir.create(paste0("data/trial_results/",folder_name,"/models"))
  }
  
  #create and save hypergrid (all combinations of targets and predictors combinations)
  #create hypergrid and save to trial_results/folder_name
  hypergrid <- expand.grid(target = targets, predictors = predictor_combinations) |> as.data.table()
  #add identifier
  hypergrid[, model := paste0("model", 1:nrow(hypergrid))]
  #save to dev
  fwrite(hypergrid, file = paste0("data/trial_results/",folder_name,"/hypergrid.csv"))
  #if file exists read
  hypergrid <- fread(paste0("data/trial_results/",folder_name,"/hypergrid.csv"))
  
  #loop through hypergrid, create GAM models
  #progressbar
  pb <- txtProgressBar(min = 1, max = nrow(hypergrid), style = 3)
  for(i in 1:nrow(hypergrid)){
    #update progressbar
    setTxtProgressBar(pb, i)
    #select target
    target <- hypergrid[i,]$target
    #select predictors
    predictors <- hypergrid[i,]$predictors
    #create formula
    gam.formula <- as.formula(paste0(target, "~", predictors))
    #run gam
    gam.model <- gam(gam.formula, familyXYZ,method = "REML",Edata)
    #save gam model do trial_results/folder_name/model
    saveRDS(gam.model, file = paste0("data/trial_results/", folder_name,"/models/", hypergrid[i,]$model, ".RDS"))
  }
  
  #example where you extract model performances
  for(i in 1:nrow(hypergrid)){
    #read the right model
    rel.model <- readRDS(paste0("data/trial_results/",folder_name,"/models/", hypergrid[i,]$model, ".RDS"))
    
    #extract model performance, add to hypergrid
    hypergrid[i, AIC := round(rel.model$aic,digits=3)]
    hypergrid[i, s.pv := list(round(summary(rel.model)[["s.pv"]],digits=3))]
    hypergrid[i, dev.expl := round(summary(rel.model)[["dev.expl"]],digits=3)]
    hypergrid[i, family := rel.model$family[1]]
  }
  
  #arrange hypergrid and see resulting df showing model diognisc comparisons
  hypergrid<- dplyr::arrange(hypergrid, hypergrid$target, desc(hypergrid$AIC))
  .GlobalEnv$hypergrid <- hypergrid
}
#############################################
####### 
####Testing log(R) models######
###write column names of dependent "target" variables, and independent "predictors" variables will be all column names other than dependent variables, or any other column name you list (I also listed year)
############Tweedie###################
####EGOM SPRING#####
targets <- c("RSSB")
predictors <- colnames(EGOM_recruitment_spring)[!(colnames(EGOM_recruitment_spring) %in% c("Age.1","RSSB", "Year","SEASON","lAGE1"))]
correlated_vars<-c("bt_anomaly","sst_anomaly")

GAM_LOOP_FUN(Edata=EGOM_recruitment_spring,k="k=5",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3= "NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=tw()")
hypergrid_tw<-hypergrid
hypergrid_tw$s.pv<-as.character(hypergrid_tw$s.pv)
hypergrid_tw<-as.data.frame(hypergrid_tw,stringsAsFactors = F)
hypergrid_tw<-hypergrid_tw[ , !names(hypergrid_tw) %in% c("model")]
#grid.newpage(grid.table(hypergrid_tw))

png("Figures/Model_run_tables/EGOM_spring_recruitment_NEFSC.png",height= 23*nrow(hypergrid_tw), width = 138*ncol(hypergrid_tw))
grid.table(hypergrid_tw)
dev.off()
######WGOM SPRING#####
targets <- c("RSSB")
predictors <- colnames(WGOM_recruitment_spring)[!(colnames(WGOM_recruitment_spring) %in% c("Age.1","RSSB", "Year","SEASON","lAGE1"))]
correlated_vars<-c("bt_anomaly","sst_anomaly")

GAM_LOOP_FUN(Edata=WGOM_recruitment_spring,k="k=5",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3= "NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=tw()")
hypergrid_tw<-hypergrid
hypergrid_tw$s.pv<-as.character(hypergrid_tw$s.pv)
hypergrid_tw<-as.data.frame(hypergrid_tw,stringsAsFactors = F)
hypergrid_tw<-hypergrid_tw[ , !names(hypergrid_tw) %in% c("model")]
#grid.newpage(grid.table(hypergrid_tw))

png("Figures/Model_run_tables/WGOM_spring_recruitment_NEFSC.png",height= 23*nrow(hypergrid_tw), width = 138*ncol(hypergrid_tw))
grid.table(hypergrid_tw)
dev.off()
######GBK SPRING#####
targets <- c("RSSB")
predictors <- colnames(GBK_recruitment_spring)[!(colnames(GBK_recruitment_spring) %in% c("Age.1","RSSB", "Year","SEASON","lAGE1"))]
correlated_vars<-c("bt_anomaly","sst_anomaly")

GAM_LOOP_FUN(Edata=GBK_recruitment_spring,k="k=5",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3= "NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=tw()")
hypergrid_tw<-hypergrid
hypergrid_tw$s.pv<-as.character(hypergrid_tw$s.pv)
hypergrid_tw<-as.data.frame(hypergrid_tw,stringsAsFactors = F)
hypergrid_tw<-hypergrid_tw[ , !names(hypergrid_tw) %in% c("model")]
#grid.newpage(grid.table(hypergrid_tw))

png("Figures/Model_run_tables/GBK_spring_recruitment_NEFSC.png",height= 23*nrow(hypergrid_tw), width = 138*ncol(hypergrid_tw))
grid.table(hypergrid_tw)
dev.off()

####EGOM FALL#####
targets <- c("RSSB")
predictors <- colnames(EGOM_recruitment_fall)[!(colnames(EGOM_recruitment_fall) %in% c("Age.1","RSSB", "Year","SEASON","lAGE1"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","Avg_GSI","EGOM_hw")

GAM_LOOP_FUN(Edata=EGOM_recruitment_fall,k="k=4",correlated_vars1= correlated_vars[3],correlated_vars2= correlated_vars[1],correlated_vars3=correlated_vars[1],correlated_vars4=correlated_vars[2],correlated_vars5=correlated_vars[4],correlated_vars6=correlated_vars[2],folder_name="recruitment",familyXYZ= "family=tw()")
hypergrid_tw<-hypergrid
hypergrid_tw$s.pv<-as.character(hypergrid_tw$s.pv)
hypergrid_tw<-as.data.frame(hypergrid_tw,stringsAsFactors = F)
hypergrid_tw<-hypergrid_tw[ , !names(hypergrid_tw) %in% c("model")]

png("Figures/Model_run_tables/EGOM_fall_recruitment_NEFSC.png",height= 24*nrow(hypergrid_tw), width = 138*ncol(hypergrid_tw))
grid.table(hypergrid_tw)
dev.off()

####WGOM FALL#####
targets <- c("RSSB")
predictors <- colnames(WGOM_recruitment_fall)[!(colnames(WGOM_recruitment_fall) %in% c("Age.1","RSSB", "Year","SEASON","lAGE1"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","Avg_GSI")

GAM_LOOP_FUN(Edata=WGOM_recruitment_fall,k="k=8",correlated_vars1= correlated_vars[3],correlated_vars2= correlated_vars[1],correlated_vars3=correlated_vars[1],correlated_vars4=correlated_vars[2],correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=tw()")
hypergrid_tw<-hypergrid
hypergrid_tw$s.pv<-as.character(hypergrid_tw$s.pv)
hypergrid_tw<-as.data.frame(hypergrid_tw,stringsAsFactors = F)
hypergrid_tw<-hypergrid_tw[ , !names(hypergrid_tw) %in% c("model")]

png("Figures/Model_run_tables/WGOM_fall_recruitment_NEFSC.png",height= 24*nrow(hypergrid_tw), width = 138*ncol(hypergrid_tw))
grid.table(hypergrid_tw)
dev.off()