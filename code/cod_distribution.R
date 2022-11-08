###### Atlantic Cod stock assessment data GAM work####
library(pacman)
pacman::p_load(here, readxl,lubridate,stats,graphics,Hmisc,data.table,utils,mgcv,dplyr,purrr,ecodata,gridExtra) 
here()
source(here("Code/Gam_data_exploration.R"))
#### load .csv files #####
Cod_distribution<-read.csv(here("data/Cod_distribution.csv"))
#wmfall_ALL<-read.csv(here("data/depth_lat/wmfall_ALL.csv"))
#wmspring_ALL<-read.csv(here("data/depth_lat/wmspring_ALL.csv"))
annual_GSI<-read.csv(here("data/annual_GSI.csv"))
Bottom_temp_fall<-read.csv(here("data/Friedland_fall_mean_bottom_temp_by_stock.csv"))
Bottom_temp_spring<-read.csv(here("data/Friedland_spring_mean_bottom_temp_by_stock.csv"))
Friedland_OISST_fall<-read.csv(here("data/Friedland_OISST_fall.csv"))
Friedland_OISST_spring<-read.csv(here("data/Friedland_OISST_spr.csv"))
SSB_Fall_all<-read.csv(here("data/SSB_estimates/SSB_Fall_all.csv"))
names(SSB_Fall_all)[1] <- "Year"
SSB_Spring_all<-read.csv(here("data/SSB_estimates/SSB_Spring_all.csv"))
names(SSB_Spring_all)[1] <- "Year"
##### Get cod heatwave data ####
cod_heatwave<-as.data.frame(ecodata::ESP_heatwave_cod)
#EGOM
EGOM_chw<-cod_heatwave[(cod_heatwave$stock_id == "EGOM") & (cod_heatwave$Var == "cumulative intensity"), ]
names(EGOM_chw)[3] <- "EGOM_hw"
EGOM_chw <- EGOM_chw[, -c(2,4:5)]
#WGOM
WGOM_chw<-cod_heatwave[(cod_heatwave$stock_id == "WGOM") & (cod_heatwave$Var == "cumulative intensity"), ]
names(WGOM_chw)[3] <- "WGOM_hw"
WGOM_chw <- WGOM_chw[, -c(2,4:5)]
#GB
GB_chw<-cod_heatwave[(cod_heatwave$stock_id == "GBK") & (cod_heatwave$Var == "cumulative intensity"), ]
names(GB_chw)[3] <- "GB_hw"
GB_chw <- GB_chw[, -c(2,4:5)]
#SNE
SNE_chw<-cod_heatwave[(cod_heatwave$stock_id == "SNE") & (cod_heatwave$Var == "cumulative intensity"), ]
names(SNE_chw)[3] <- "SNE_hw"
SNE_chw <- SNE_chw[, -c(2,4:5)]

cod_heatwave<-merge(EGOM_chw,WGOM_chw,merge="Time",all=TRUE)
cod_heatwave<-merge(cod_heatwave,GB_chw,merge="Time",all=TRUE)
cod_heatwave<-merge(cod_heatwave,SNE_chw,merge="Time",all=TRUE)

cod_heatwave$mean_c_heatwave <- rowMeans(cod_heatwave[,2:5],na.rm=TRUE)
names(cod_heatwave)[1] <- "Year"
rm(EGOM_chw,GB_chw,WGOM_chw,SNE_chw)
####Combine data into separate data frames by season####
#put all data frames into list
#distribution_fall <- list(wmfall_ALL, annual_GSI, Bottom_temp_fall[,c(1,6)],Friedland_OISST_fall[,c(1,6)],cod_heatwave[,c(1,6)],SSB_Fall_all[,c(1,3)])

distribution_fall <- list(Cod_distribution[,c(1,4,5)], annual_GSI, Bottom_temp_fall[,c(1,6)],Friedland_OISST_fall[,c(1,6)],cod_heatwave[,c(1,6)],SSB_Fall_all[,c(1,3)])
#merge all data frames in list
distribution_fall<-distribution_fall %>% reduce(full_join, by='Year')
names(distribution_fall)[4] <- "Avg_GSI"

#distribution_spring <- list(wmspring_ALL, annual_GSI, Bottom_temp_spring[,c(1,6)],Friedland_OISST_spring[,c(1,6)],cod_heatwave[,c(1,6)],SSB_Spring_all[,c(1,3)])
distribution_spring <- list(Cod_distribution[,c(1,2,3)], annual_GSI, Bottom_temp_spring[,c(1,6)],Friedland_OISST_spring[,c(1,6)],cod_heatwave[,c(1,6)],SSB_Spring_all[,c(1,3)])
distribution_spring<-distribution_spring %>% reduce(full_join, by='Year')
names(distribution_spring)[4] <- "Avg_GSI"
### remove data I don't need ####
rm(annual_GSI,Bottom_temp_fall,Bottom_temp_spring,Friedland_OISST_fall,Friedland_OISST_spring,cod_heatwave,SSB_Fall_all,SSB_Spring_all,wmfall_ALL,wmspring_ALL,Cod_distribution)

###clip to years with most data###
distribution_fall = distribution_fall[!distribution_fall$Year > 2019,]
distribution_fall = distribution_fall[!distribution_fall$Year < 1982,]
distribution_spring = distribution_spring[!distribution_spring$Year > 2019,]
distribution_spring = distribution_spring[!distribution_spring$Year < 1982,]

###reorder by year###
distribution_fall<-distribution_fall %>% arrange(Year)
distribution_spring<-distribution_spring %>% arrange(Year)

###### Anomaly Base Period########
### using 1982-2011 as baseline anomaly period####

#bt_fall_bp<-mean(distribution_fall[1:30,7])
#bt_spring_bp<-mean(distribution_spring[1:30,7])
#sst_fall_bp<-mean(distribution_fall[1:30,8])
#sst_spring_bp<-mean(distribution_spring[1:30,8])

bt_fall_bp<-mean(distribution_fall[1:30,5])
bt_spring_bp<-mean(distribution_spring[1:30,5])
sst_fall_bp<-mean(distribution_fall[1:30,6])
sst_spring_bp<-mean(distribution_spring[1:30,6])
#####
##### Calculate temperature anomaly columns#####

#Fall
distribution_fall$bt_anomaly<- distribution_fall$Avg_bt  - bt_fall_bp
distribution_fall$sst_anomaly<- distribution_fall$Avg_oisst  - sst_fall_bp

#Spring
distribution_spring$bt_anomaly<- distribution_spring$Avg_bt  - bt_spring_bp
distribution_spring$sst_anomaly<- distribution_spring$Avg_oisst  - sst_spring_bp

###Get final dataframes ####
#distribution_fall<-distribution_fall[,c(1:6,9:12)]
#distribution_spring<-distribution_spring[,c(1:6,9:12)]

distribution_fall<-distribution_fall[,c(1:4,7:10)]
distribution_fall$COG_depth_fall<-abs(distribution_fall$COG_depth_fall)
distribution_spring<-distribution_spring[,c(1:4,7:10)]
distribution_spring$COG_depth_spring<-abs(distribution_spring$COG_depth_spring)
#########################
############ START ANALYSIS ##################
#put dfs in list to apply across functions
df.list <- list(distribution_fall,distribution_spring)

#apply functions
lapply(df.list, dotchart_fun_10)
lapply(df.list, hist_fun10)
lapply(df.list, view_boxplot_fun10)
lapply(df.list, shapiro_fun) #all normal except SSB and heatwave
lapply(df.list,Mypairs)
Mypairs(distribution_fall[4:8])
Mypairs(distribution_spring[4:8])
#############################################
################ GAM loop#####
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

####Testing Fall depth & lat models######
############Gaussian###################
targets <- c("COG_Lat_fall","COG_depth_fall")
predictors <- colnames(distribution_fall)[!(colnames(distribution_fall) %in% c("COG_Lat_fall","COG_depth_fall", "Year"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","mean_c_heatwave","Avg_GSI")

GAM_LOOP_FUN(Edata=distribution_fall,k="k=10",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3= correlated_vars[2],correlated_vars4= correlated_vars[3],correlated_vars5= correlated_vars[1],correlated_vars6= correlated_vars[4],folder_name="cod_fall_depth",familyXYZ= "family=gaussian()")
hypergrid$s.pv<-as.character(hypergrid$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/distribution/fall_depth_lat.png",height= 22*nrow(hypergrid_gaus), width = 130*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()

#### Testing Spring depth & lat models########
############Gaussian###################
targets <- c("COG_Lat_spring","COG_depth_spring")
predictors <- colnames(distribution_spring)[!(colnames(distribution_spring) %in% c("COG_Lat_spring","COG_depth_spring", "Year"))]
correlated_vars<-c("bt_anomaly","sst_anomaly")

GAM_LOOP_FUN(Edata=distribution_spring,k="k=8",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3= "NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="cod_spring_lat",familyXYZ= "family=gaussian()")
hypergrid$s.pv<-as.character(hypergrid$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/spring_depth_lat.png",height= 22*nrow(hypergrid_gaus), width = 136*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()

############# PLOT SIGNIFICANT GAM CURVES #######################
##### DEPTH (Fall) vs. potential environmental influences###########
#nothing significant
##### DEPTH (Spring tow) vs. potential environmental influences##########
SP_Depth<-gam(abs(COG_depth_spring) ~ s(SSB, k=10), family=tw(),method = "REML",data=distribution_spring)
summary(SP_Depth)
SP_Depth$aic

png("Figures/residual_plots/distribution/Spring_Depth.png",width = 500, height = 500, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(SP_Depth,pch=20, cex=1.2,cex.lab=1.5)
dev.off()

png("Figures/GAM_curves/distribution/Spring_Depth.png",width = 449, height = 374.5, units = "px",res=90)
layout(matrix(1:1, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(SP_Depth,distribution_spring$SSB,x_lab="SSB (kg/tow)",y_lab="PE on Mean Depth",select1=1,data_Year = distribution_spring$Year,position = "bottomleft",title="Spring Depth")
dev.off()
##### LATITUDE (Fall) vs. potential environmental influences###########

FL_Lat<-gam((COG_Lat_fall) ~ s(SSB, k=10), family=gaussian(),method = "REML",data=distribution_fall)
summary(FL_Lat) # Find significant variables based on p-value
FL_Lat$aic

png("Figures/residual_plots/distribution/Fall_Lat.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(FL_Lat,pch=20, cex=1.2,cex.lab=1.5)
dev.off()

###Plot GAM
png("Figures/GAM_curves/distribution/Fall_Lat.png",width = 449, height = 374.5, units = "px",res=90)
layout(matrix(1:1, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(FL_Lat,distribution_fall$SSB,x_lab="SSB (kg/tow)",y_lab="PE on Mean Latitude",select1=1,data_Year = distribution_spring$Year,position="topleft",title="Fall Latitude")
dev.off()

##### Latitude (Spring tow) vs. potential environmental influences##########
SP_numtow<-gam((COG_Lat_spring) ~ s(mean_c_heatwave, k=10), family=gaussian(),method = "REML",data=distribution_spring) # Build GAM with all possible variables
summary(SP_numtow) # Find significant variables based on p-value
SP_numtow$aic

png("Figures/residual_plots/distribution/Spring_Lat.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(SP_numtow,pch=20, cex=1.2,cex.lab=1.5)
dev.off()
###Plot GAM
png("Figures/GAM_curves/distribution/Spring_Lat.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4.5,4.3,1,1))
layout(matrix(1:1, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(SP_numtow,distribution_spring$mean_c_heatwave,x_lab="Mean Cumulative Heatwave",y_lab="PE on Mean Latitude",select1=1,data_Year = distribution_spring$Year,position="topleft",title="Spring Latitude")
dev.off()

