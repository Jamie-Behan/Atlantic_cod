###### Atlantic Cod stock assessment data GAM work####
library(pacman)
pacman::p_load(here, readxl,lubridate,stats,graphics,Hmisc,data.table,utils,mgcv,dplyr,purrr,ecodata,gridExtra) 
here()
source(here("Code/Gam_data_exploration.R"))
#### load .csv files #####
nm <- list.files(path =here("data/final_env_data/distribution"), pattern = ".csv", full.names = TRUE)
nm2 <- list.files(path =here("data/final_env_data/distribution"), pattern = ".csv", full.names =FALSE)
list2env(lapply(setNames(nm, make.names(gsub("*.csv$", "",nm2))),read.csv),envir=.GlobalEnv)
rm(nm,nm2)

Cod_distribution<-read.csv(here("data/Cod_distribution.csv"))
SSB_Fall_all<-read.csv(here("data/SSB_estimates/SSB_Fall_all.csv"))
names(SSB_Fall_all)[1] <- "Year"
SSB_Spring_all<-read.csv(here("data/SSB_estimates/SSB_Spring_all.csv"))
names(SSB_Spring_all)[1] <- "Year"

####Combine data into separate data frames by season####
distribution_fall <- list(Cod_distribution[,c(1,4,5)],distribution_fall,SSB_Fall_all[,c(1,3)])
distribution_fall<-distribution_fall %>% reduce(full_join, by='Year')

distribution_spring <- list(Cod_distribution[,c(1,2,3)],distribution_spring,SSB_Spring_all[,c(1,3)])
distribution_spring<-distribution_spring %>% reduce(full_join, by='Year')

### remove data I don't need ####
rm(Cod_distribution,SSB_Fall_all,SSB_Spring_all)
###clip to years with most data###
distribution_fall = distribution_fall[!distribution_fall$Year > 2021,]
distribution_fall = distribution_fall[!distribution_fall$Year < 1982,]
distribution_spring = distribution_spring[!distribution_spring$Year > 2021,]
distribution_spring = distribution_spring[!distribution_spring$Year < 1982,]
###reorder by year###
distribution_fall<-distribution_fall %>% arrange(Year)
distribution_spring<-distribution_spring %>% arrange(Year)
###Get final dataframes ####
distribution_fall$COG_depth_fall<-abs(distribution_fall$COG_depth_fall)
distribution_spring$COG_depth_spring<-abs(distribution_spring$COG_depth_spring)
#########################
############ START ANALYSIS ##################
#put dfs in list to apply across functions
df.list <- list(distribution_fall,distribution_spring)

#apply functions
lapply(df.list, dotchart_fun_10)
lapply(df.list, hist_fun10)
lapply(df.list, view_boxplot_fun10)
lapply(df.list, shapiro_fun)

Mypairs(distribution_fall[4:10])
Mypairs(distribution_spring[4:10])

#############################################
################ GAM loop#####
source(here("Code/GAM_forloop.R"))
#keeping GAM_LOOP_FUN because this one includes sstxbt anomaly as automatic correlated because distribution analysis revealed more correlated vars
GAM_LOOP_FUN<-function(Edata,k,correlated_vars1,correlated_vars2,correlated_vars3,correlated_vars4,correlated_vars5,correlated_vars6,folder_name,familyXYZ,number_vars_in_mod){
  
  #create all combinations of predictors
  predictor_combinations <- lapply(1:number_vars_in_mod, FUN = function(x){
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
predictor_combinations <-predictor_combinations[!grepl("bt_anomaly", predictor_combinations$predictor_combinations)| !grepl("sst_anomaly" ,predictor_combinations$predictor_combinations),]

if(correlated_vars1!="NA"||correlated_vars2!="NA"){
  #
  predictor_combinations <- as.data.frame(predictor_combinations)
  predictor_combinations <-predictor_combinations[!grepl(correlated_vars1, predictor_combinations$predictor_combinations)| !grepl(correlated_vars2,predictor_combinations$predictor_combinations),]
}
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
correlated_vars<-c("bt_anomaly","sst_anomaly","Heatwave","GSI")

GAM_LOOP_FUN(Edata=distribution_fall,k="k=10",correlated_vars1=correlated_vars[1],correlated_vars2=correlated_vars[3],correlated_vars3=correlated_vars[2],correlated_vars4=correlated_vars[3],correlated_vars5=correlated_vars[1],correlated_vars6=correlated_vars[4],folder_name="cod_fall_depth",familyXYZ= "family=gaussian()",number_vars_in_mod = (length(predictors)-4))
allorsome(all=TRUE)

#### Testing Spring depth & lat models########
############Gaussian###################
targets <- c("COG_Lat_spring","COG_depth_spring")
predictors <- colnames(distribution_spring)[!(colnames(distribution_spring) %in% c("COG_Lat_spring","COG_depth_spring", "Year"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","SSB","calfin_100m3","pseudo_100m3","GSI")

GAM_LOOP_FUN(Edata=distribution_spring,k="k=10",correlated_vars1=correlated_vars[2],correlated_vars2=correlated_vars[3],correlated_vars3=correlated_vars[4],correlated_vars4=correlated_vars[5],correlated_vars5=correlated_vars[1],correlated_vars6=correlated_vars[6],folder_name="cod_spring_lat",familyXYZ= "family=gaussian()",number_vars_in_mod = (length(predictors)-4))
allorsome(all=TRUE)

############# PLOT SIGNIFICANT GAM CURVES #######################
##### DEPTH (Fall) vs. potential environmental influences###########
FL_Depth<-gam(abs(COG_depth_fall) ~ s(calfin_100m3, k=10), family=gaussian(),method = "REML",data=distribution_fall)
summary(FL_Depth)
FL_Depth$aic

plot(influence.gam(FL_Depth))
abline(h = 4/nrow(distribution_fall), lty = 2, col = "red") # add cutoff line

png("Figures/residual_plots/distribution/Fall_Depth.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(FL_Depth,pch=20, cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/distribution/Fall_Depth.png",width = 449, height = 374.5, units = "px")
layout(matrix(1:1, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(FL_Depth,distribution_fall$calfin_100m3,x_lab="Calanus Density (/100m3)",y_lab="PE on Mean Depth",select1=1,data_Year = distribution_fall$Year,position = "topright",title="Fall Depth")
dev.off()
##### DEPTH (Spring tow) vs. potential environmental influences##########
SP_Depth<-gam(abs(COG_depth_spring) ~ s(SSB, k=10), family=gaussian(),method = "REML",data=distribution_spring)
summary(SP_Depth)
SP_Depth$aic

png("Figures/residual_plots/distribution/Spring_Depth.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(SP_Depth,pch=20, cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/distribution/Spring_Depth.png",width = 449, height = 374.5, units = "px")
layout(matrix(1:1, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(SP_Depth,distribution_spring$SSB,x_lab="SSB (kg/tow)",y_lab="PE on Mean Depth",select1=1,data_Year = distribution_spring$Year,position = "bottomleft",title="Spring Depth")
dev.off()
##### LATITUDE (Fall) vs. potential environmental influences###########
FL_Lat<-gam((COG_Lat_fall) ~ s(SSB,k=10), family=gaussian(),method = "REML",data=distribution_fall)
summary(FL_Lat)
FL_Lat$aic

png("Figures/residual_plots/distribution/Fall_Lat.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(FL_Lat,pch=20, cex=1,cex.lab=1.3)
dev.off()

###Plot GAM
png("Figures/GAM_curves/distribution/Fall_Lat.png",width =449, height = 374.5, units = "px")
layout(matrix(1:1, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(FL_Lat,distribution_fall$SSB,x_lab="SSB (kg/tow)",y_lab="PE on Mean Latitude",select1=1,data_Year = distribution_spring$Year,position="topleft",title="Fall Latitude")
dev.off()

##### Latitude (Spring tow) vs. potential environmental influences##########
SP_numtow<-gam((COG_Lat_spring) ~ s(Heatwave, k=10), family=gaussian(),method = "REML",data=distribution_spring)
summary(SP_numtow)
SP_numtow$aic

png("Figures/residual_plots/distribution/Spring_Lat.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(SP_numtow,pch=20, cex=1,cex.lab=1.3)
dev.off()
###Plot GAM
png("Figures/GAM_curves/distribution/Spring_Lat.png",width =449, height = 374.5, units = "px")
par(mar=c(4.5,4.3,1,1))
layout(matrix(1:1, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(SP_numtow,distribution_spring$Heatwave,x_lab="Heatwave (C)",y_lab="PE on Mean Latitude",select1=1,data_Year = distribution_spring$Year,position="topleft",title="Spring Latitude")
dev.off()
