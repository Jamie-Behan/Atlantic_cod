library(pacman)
pacman::p_load(here, readxl,lubridate,stats,graphics,Hmisc,data.table,utils,mgcv,dplyr,purrr,ecodata,kableExtra,gridExtra) 
#### load .csv files #####
nm <- list.files(path =here("data/final_env_data/growth"), pattern = ".csv", full.names = TRUE)
nm2 <- list.files(path =here("data/final_env_data/growth"), pattern = ".csv", full.names =FALSE)
list2env(lapply(setNames(nm, make.names(gsub("*.csv$", "",nm2))),read.csv),envir=.GlobalEnv)
rm(nm,nm2)

EGOM_K<-read.csv(here("data/rel_condition/ADIOS_SV_164712_EGOM_NONE_relative_k.csv"))
WGOM_K<-read.csv(here("data/rel_condition/ADIOS_SV_164712_WGOM_NONE_relative_k.csv"))
GBK_K<-read.csv(here("data/rel_condition/ADIOS_SV_164712_GBK_NONE_relative_k.csv"))
SNEMA_K<-read.csv(here("data/rel_condition/ADIOS_SV_164712_SNEMA_NONE_relative_k.csv"))

EGOM_WAAa_fall<-read.csv(here("data/WAA/anomaly_data/WAA_EGOM_fall.csv"))
EGOM_WAAa_spring<-read.csv(here("data/WAA/anomaly_data/WAA_EGOM_spring.csv"))
WGOM_WAAa_fall<-read.csv(here("data/WAA/anomaly_data/WAA_WGOM_fall.csv"))
WGOM_WAAa_spring<-read.csv(here("data/WAA/anomaly_data/WAA_WGOM_spring.csv"))
GBK_WAAa_fall<-read.csv(here("data/WAA/anomaly_data/WAA_GBK_fall.csv"))
GBK_WAAa_spring<-read.csv(here("data/WAA/anomaly_data/WAA_GBK_spring.csv"))
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

#####merge WAAa dfs with environmental dfs#####
EGOM_Growth_fall<-merge(EGOM_Growth_fall,EGOM_K_FL,by="Year",all=TRUE)
EGOM_Growth_spring<-merge(EGOM_Growth_spring,EGOM_K_SP,by="Year",all=TRUE)
WGOM_Growth_fall<-merge(WGOM_Growth_fall,WGOM_K_FL,by="Year",all=TRUE)
WGOM_Growth_spring<-merge(WGOM_Growth_spring,WGOM_K_SP,by="Year",all=TRUE)
GBK_Growth_fall<-merge(GBK_Growth_fall,GBK_K_FL,by="Year",all=TRUE)
GBK_Growth_spring<-merge(GBK_Growth_spring,GBK_K_SP,by="Year",all=TRUE)
SNE_Growth_fall<-merge(SNE_Growth_fall,SNEMA_K_FL,by="Year",all=TRUE)
SNE_Growth_spring<-merge(SNE_Growth_spring,SNEMA_K_SP,by="Year",all=TRUE)
#####merge WAAa dfs with environmental dfs#####
EGOM_Growth_fall<-merge(EGOM_Growth_fall,EGOM_WAAa_fall,by="Year",all=TRUE)
EGOM_Growth_spring<-merge(EGOM_Growth_spring,EGOM_WAAa_spring,by="Year",all=TRUE)
WGOM_Growth_fall<-merge(WGOM_Growth_fall,WGOM_WAAa_fall,by="Year",all=TRUE)
WGOM_Growth_spring<-merge(WGOM_Growth_spring,WGOM_WAAa_spring,by="Year",all=TRUE)
GBK_Growth_fall<-merge(GBK_Growth_fall,GBK_WAAa_fall,by="Year",all=TRUE)
GBK_Growth_spring<-merge(GBK_Growth_spring,GBK_WAAa_spring,by="Year",all=TRUE)
#####remove dfs I don't Need#####
rm(EGOM_K_FL,EGOM_K_SP,WGOM_K_FL,WGOM_K_SP,GBK_K_FL,GBK_K_SP,SNEMA_K_FL,SNEMA_K_SP,EGOM_recruitment_fall,EGOM_recruitment_spring,WGOM_recruitment_fall,WGOM_recruitment_spring,GBK_recruitment_fall,GBK_recruitment_spring,SNE_recruitment_fall,SNE_recruitment_spring,EGOM_WAA,WGOM_WAA,GBK_WAA,SNE_WAA,WAA_EGOM_FL,WAA_GBK_FL,WAA_GBK_SP,WAA_SNE_FL,WAA_SNE_SP,df,WAA_WGOM_SP,WAA_WGOM_FL,WAA_EGOM_SP,EGOM_WAAa_fall,EGOM_WAAa_spring,WGOM_WAAa_fall,WGOM_WAAa_spring,GBK_WAAa_fall,GBK_WAAa_spring)
##### Start here ######
source(here("Code/Gam_data_exploration.R"))
#put dfs in list to apply across functions
df.list <- list(EGOM_Growth_fall,EGOM_Growth_spring,WGOM_Growth_fall[2:11],WGOM_Growth_spring[2:11],GBK_Growth_fall[2:11],GBK_Growth_spring[2:11],SNE_Growth_fall,SNE_Growth_spring)

#apply functions
lapply(df.list, dotchart_fun_10)
lapply(df.list, hist_fun10)
lapply(df.list, view_boxplot_fun10)
lapply(df.list, shapiro_fun) #all normal except SSB and heatwave
#lapply(df.list,Mypairs)
Mypairs(EGOM_Growth_fall[2:8])
Mypairs(EGOM_Growth_spring[2:8])
Mypairs(WGOM_Growth_fall[2:8])
Mypairs(WGOM_Growth_spring[2:8])
Mypairs(GBK_Growth_fall[2:8])
Mypairs(GBK_Growth_spring[2:8])
Mypairs(SNE_Growth_fall[2:8])
Mypairs(SNE_Growth_spring[2:8])
#### view relative K timeseries ####
layout(matrix(1:4, ncol=2, byrow=TRUE))
par(mar=c(4.1,4.5,1.5,1), oma=c(1.0,0,1.0,0.1))
lineplot_seasonal(springdata=EGOM_Growth_spring,
                  falldata=EGOM_Growth_fall,
                  springY=EGOM_Growth_spring$K_rel,
                  fallY=EGOM_Growth_fall$K_rel,
                  fallX= EGOM_Growth_fall$Year,
                  springX= EGOM_Growth_spring$Year,
                  main="NEFSC Trawl Survey Relative Condition: EGOM",
                  ylab="Relative Condition (K)",
                  ylim=c(0.9,1.25))
lineplot_seasonal(springdata=WGOM_Growth_spring,
                  falldata=WGOM_Growth_fall,
                  springY=WGOM_Growth_spring$K_rel,
                  fallY=WGOM_Growth_fall$K_rel,
                  fallX= WGOM_Growth_fall$Year,
                  springX= WGOM_Growth_spring$Year,
                  main="NEFSC Trawl Survey Relative Condition: WGOM",
                  ylab="Relative Condition (K)",
                  ylim=c(0.9,1.25))
lineplot_seasonal(springdata=GBK_Growth_spring,
                  falldata=GBK_Growth_fall,
                  springY=GBK_Growth_spring$K_rel,
                  fallY=GBK_Growth_fall$K_rel,
                  fallX= GBK_Growth_fall$Year,
                  springX= GBK_Growth_spring$Year,
                  main="NEFSC Trawl Survey Relative Condition: GBK",
                  ylab="Relative Condition (K)",
                  ylim=c(0.9,1.25))
lineplot_seasonal(springdata=SNE_Growth_spring,
                  falldata=SNE_Growth_fall,
                  springY=SNE_Growth_spring$K_rel,
                  fallY=SNE_Growth_fall$K_rel,
                  fallX= SNE_Growth_fall$Year,
                  springX= SNE_Growth_spring$Year,
                  main="NEFSC Trawl Survey Relative Condition: SNE",
                  ylab="Relative Condition (K)",
                  ylim=c(0.9,1.25))
#### view weight at age anomaly timeseries ####

png(here("Figures/Raw_data_trends/WAAanomaly.png"),height= 800, width = 1700,res=125)
layout(matrix(1:6, ncol=3, byrow=FALSE))
par(mar=c(4.1,4.5,1.5,1), oma=c(1.0,0,1.0,0.1))
##EGOM FALL
plot(age2_anomaly~Year, data=EGOM_Growth_fall, main="Weight at Age Anomalies: EGOM Fall",
     xlab="Year",ylab="Weight at Age Anomaly (Kg)", type="l",lwd=3,col="#407331",ylim= c(-4.6,5.5),xlim=c(1982,2019), cex.lab=1.4,cex.axis=1.1)
abline(h=0,col="#6B6B6B",lwd=2.0,lty=2)
legend("topleft",inset=c(0.02,0.02), legend=c("Age 2"), col=c("#407331"), lty=1,lwd=3.5, cex=0.9)
##EGOM Spring
plot(age1_anomaly~Year, data=EGOM_Growth_spring, main="Weight at Age Anomalies: EGOM Spring",
     xlab="Year",ylab="Weight at Age Anomaly (Kg)", type="l",lwd=3,col="#3B4620",ylim= c(-4.6,5.5),xlim=c(1982,2019), cex.lab=1.4,cex.axis=1.1)
abline(h=0,col="#6B6B6B",lwd=2.0,lty=2)
legend("topleft",inset=c(0.02,0.02), legend=c("Age 1"), col=c("#3B4620"), lty=1,lwd=3.5, cex=0.9)
##WGOM Fall
plot(age1_anomaly~Year, data=WGOM_Growth_fall, main="Weight at Age Anomalies: WGOM Fall",
     xlab="Year",ylab="Weight at Age Anomaly (Kg)", type="l",lwd=3,col="#3B4620",ylim= c(-4.6,5.5),xlim=c(1982,2019), cex.lab=1.4,cex.axis=1.1)
lines(age2_anomaly~Year, data=WGOM_Growth_fall, xlab="Year", type="l",col="#407331",lwd=3)
lines(age3_anomaly~Year, data=WGOM_Growth_fall, xlab="Year", type="l",col="#00608A",lwd=3)
lines(age4_anomaly~Year, data=WGOM_Growth_fall, xlab="Year", type="l",col="#13A49D",lwd=3)
lines(age5_anomaly~Year, data=WGOM_Growth_fall, xlab="Year", type="l",col="#ABB400",lwd=3)
lines(age6_anomaly~Year, data=WGOM_Growth_fall, xlab="Year", type="l",col="#EACA00",lwd=3)
abline(h=0,col="#6B6B6B",lwd=2.0,lty=2)
legend("topright",inset=c(0.02,0.02), legend=c("Age 1", "Age 2","Age 3", "Age 4","Age 5","Age 6"), col=c("#3B4620", "#407331","#00608A", "#13A49D","#ABB400", "#EACA00"), lty=1,lwd=3.5, cex=0.9)
##WGOM Spring
plot(age1_anomaly~Year, data=WGOM_Growth_spring, main="Weight at Age Anomalies: WGOM Spring",
     xlab="Year",ylab="Weight at Age Anomaly (Kg)", type="l",lwd=3,col="#3B4620",ylim= c(-4.6,5.5),xlim=c(1982,2019), cex.lab=1.4,cex.axis=1.1)
lines(age7_anomaly~Year, data=WGOM_Growth_spring, xlab="Year", type="l",col="#F0713F",lwd=3)
lines(age2_anomaly~Year, data=WGOM_Growth_spring, xlab="Year", type="l",col="#407331",lwd=3)
lines(age3_anomaly~Year, data=WGOM_Growth_spring, xlab="Year", type="l",col="#00608A",lwd=3)
lines(age4_anomaly~Year, data=WGOM_Growth_spring, xlab="Year", type="l",col="#13A49D",lwd=3)
lines(age5_anomaly~Year, data=WGOM_Growth_spring, xlab="Year", type="l",col="#ABB400",lwd=3)
lines(age6_anomaly~Year, data=WGOM_Growth_spring, xlab="Year", type="l",col="#EACA00",lwd=3)

abline(h=0,col="#6B6B6B",lwd=2.0,lty=2)
legend("topright",inset=c(0.02,0.02), legend=c("Age 1", "Age 2","Age 3", "Age 4","Age 5","Age 6","Age7"), col=c("#3B4620", "#407331","#00608A", "#13A49D","#ABB400", "#EACA00","#F0713F"), lty=1,lwd=3.5, cex=0.9)

##GBK Fall
plot(age1_anomaly~Year, data=GBK_Growth_fall, main="Weight at Age Anomalies: GBK Fall",
     xlab="Year",ylab="Weight at Age Anomaly (Kg)", type="l",lwd=3,col="#3B4620",ylim= c(-4.6,5.5),xlim=c(1982,2019), cex.lab=1.4,cex.axis=1.1)
lines(age2_anomaly~Year, data=GBK_Growth_fall, xlab="Year", type="l",col="#407331",lwd=3)
lines(age3_anomaly~Year, data=GBK_Growth_fall, xlab="Year", type="l",col="#00608A",lwd=3)
lines(age4_anomaly~Year, data=GBK_Growth_fall, xlab="Year", type="l",col="#13A49D",lwd=3)
lines(age5_anomaly~Year, data=GBK_Growth_fall, xlab="Year", type="l",col="#ABB400",lwd=3)
abline(h=0,col="#6B6B6B",lwd=2.0,lty=2)
legend("topright",inset=c(0.02,0.02), legend=c("Age 1", "Age 2","Age 3", "Age 4","Age 5"), col=c("#3B4620", "#407331","#00608A", "#13A49D","#ABB400"), lty=1,lwd=3.5, cex=0.9)

##GBK Spring
plot(age1_anomaly~Year, data=GBK_Growth_spring, main="Weight at Age Anomalies: GBK Spring",
     xlab="Year",ylab="Weight at Age Anomaly (Kg)", type="l",lwd=3,col="#3B4620",ylim= c(-4.6,5.5),xlim=c(1982,2019), cex.lab=1.4,cex.axis=1.1)
lines(age7_anomaly~Year, data=GBK_Growth_spring, xlab="Year", type="l",col="#F0713F",lwd=3)
lines(age2_anomaly~Year, data=GBK_Growth_spring, xlab="Year", type="l",col="#407331",lwd=3)
lines(age3_anomaly~Year, data=GBK_Growth_spring, xlab="Year", type="l",col="#00608A",lwd=3)
lines(age4_anomaly~Year, data=GBK_Growth_spring, xlab="Year", type="l",col="#13A49D",lwd=3)
lines(age5_anomaly~Year, data=GBK_Growth_spring, xlab="Year", type="l",col="#ABB400",lwd=3)
lines(age6_anomaly~Year, data=GBK_Growth_spring, xlab="Year", type="l",col="#EACA00",lwd=3)

abline(h=0,col="#6B6B6B",lwd=2.0,lty=2)
legend("topright",inset=c(0.02,0.02), legend=c("Age 1", "Age 2","Age 3", "Age 4","Age 5","Age 6","Age 7"), col=c("#3B4620", "#407331","#00608A", "#13A49D","#ABB400","#EACA00","#F0713F"), lty=1,lwd=3.5, cex=0.9)
dev.off()

################ GAM LOOP FUNCTION#####
source(here("Code/GAM_forloop.R"))
#### keep GAMloop function because this one automatically includes sstxbt anomaly as a pairwise correlation
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
#############################################
#############
####Testing Weight at Age & relative K models######
####EGOM SPRING#####
targets <- c("age1_anomaly","K_rel")
predictors <- colnames(EGOM_Growth_spring)[!(colnames(EGOM_Growth_spring) %in% c("Year","K_rel","age1_anomaly"))]

GAM_LOOP_FUN(Edata=EGOM_Growth_spring,k="k=4",correlated_vars1="NA",correlated_vars2="NA",correlated_vars3="NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=gaussian()",number_vars_in_mod= (length(predictors)-4))
allorsome(all=FALSE)
####EGOM FALL#####
targets <- c("age2_anomaly","K_rel")
predictors <- colnames(EGOM_Growth_fall)[!(colnames(EGOM_Growth_fall) %in% c("Year","K_rel","age2_anomaly","SSB"))]
correlated_vars<-c("sst_anomaly","Heatwave")

GAM_LOOP_FUN(Edata=EGOM_Growth_fall,k="k=8",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3="NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=gaussian()",number_vars_in_mod= (length(predictors)-4))
allorsome(all=FALSE)
######WGOM SPRING#####
targets <- c("age1_anomaly","age2_anomaly","age3_anomaly","age4_anomaly","age5_anomaly","age6_anomaly","age7_anomaly","K_rel")
predictors <- colnames(WGOM_Growth_spring)[!(colnames(WGOM_Growth_spring) %in% c("Year","K_rel","age1_anomaly","age2_anomaly","age3_anomaly","age4_anomaly","age5_anomaly","age6_anomaly","age7_anomaly"))]

GAM_LOOP_FUN(Edata=WGOM_Growth_spring,k="k=7",correlated_vars1="NA",correlated_vars2="NA",correlated_vars3= "NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=gaussian()",number_vars_in_mod= (length(predictors)-4))
allorsome(all=FALSE)
####WGOM FALL#####
targets <- c("age1_anomaly","age2_anomaly","age3_anomaly","age4_anomaly","age5_anomaly","age6_anomaly","K_rel")
predictors <- colnames(WGOM_Growth_fall)[!(colnames(WGOM_Growth_fall) %in% c("Year","K_rel","age1_anomaly","age2_anomaly","age3_anomaly","age4_anomaly","age5_anomaly","age6_anomaly"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","Heatwave")

GAM_LOOP_FUN(Edata=WGOM_Growth_fall,k="k=7",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[3],correlated_vars3="NA",correlated_vars4="NA",correlated_vars5=correlated_vars[2],correlated_vars6=correlated_vars[3],folder_name="recruitment",familyXYZ= "family=gaussian()",number_vars_in_mod= (length(predictors)-4))
allorsome(all=FALSE)
######GBK SPRING#####
targets <- c("age1_anomaly","age2_anomaly","age3_anomaly","age4_anomaly","age5_anomaly","age6_anomaly","age7_anomaly","K_rel")
predictors <- colnames(GBK_Growth_spring)[!(colnames(GBK_Growth_spring) %in% c("Year","K_rel","age1_anomaly","age2_anomaly","age3_anomaly","age4_anomaly","age5_anomaly","age6_anomaly","age7_anomaly"))]
correlated_vars<-c("bt_anomaly","GSI")

GAM_LOOP_FUN(Edata=GBK_Growth_spring,k="k=8",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3= "NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=gaussian()",number_vars_in_mod= (length(predictors)-4))
allorsome(all=FALSE)
####GBK FALL#####
targets <- c("age1_anomaly","age2_anomaly","age3_anomaly","age4_anomaly","age5_anomaly","K_rel")
predictors <- colnames(GBK_Growth_fall)[!(colnames(GBK_Growth_fall) %in% c("Year","K_rel","age1_anomaly","age2_anomaly","age3_anomaly","age4_anomaly","age5_anomaly"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","GSI","Heatwave")

GAM_LOOP_FUN(Edata=GBK_Growth_fall,k="k=8",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[4],correlated_vars3= correlated_vars[2],correlated_vars4= correlated_vars[4],correlated_vars5= correlated_vars[1],correlated_vars6= correlated_vars[3],folder_name="recruitment",familyXYZ= "family=gaussian()",number_vars_in_mod= (length(predictors)-4))
allorsome(all=FALSE)
######SNE SPRING#####
targets <- c("K_rel")
predictors <- colnames(SNE_Growth_spring)[!(colnames(SNE_Growth_spring) %in% c("Year","K_rel"))]

GAM_LOOP_FUN(Edata=SNE_Growth_spring,k="k=4",correlated_vars1="NA",correlated_vars2="NA",correlated_vars3= "NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=gaussian()",number_vars_in_mod= (length(predictors)-4))
allorsome(all=FALSE)
######SNE FALL#####
#not enough data
############### PLOT SIGNIFICANT GAM CURVES #######################
##### EGOM SPRING####
#WAA1 #bad resids
egomWAA1<-gam(age1_anomaly ~ s(pseudo_100m3,k=8), family=gaussian(),method = "REML",data=EGOM_Growth_spring)
summary(egomWAA1)
egomWAA1$aic
##### EGOM FALL####
#krel
egomkrel<-gam(K_rel ~ s(bt_anomaly, k=10), family=gaussian(),method = "REML",data=EGOM_Growth_fall)
summary(egomkrel)
egomkrel$aic
png("Figures/residual_plots/growth/EGOM_fall_krel.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(egomkrel,pch=20, cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/growth/EGOM_fall_krel.png",width = 449, height = 374.5, units = "px")
par(mar=c(4.5,4.5,0.6,1))
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN(egomkrel,EGOM_Growth_fall$sst_anomaly,x_lab="BT Anomaly (C)",y_lab="PE on Rel. Condition",select1=1,data_Year = EGOM_Growth_fall$Year,position = "bottomleft",title="EGOM Rel. Condition Fall")
dev.off()
##### WGOM SPRING####
#WAA1
#nothing significant
#WAA2
wgomWAA2<-gam(age2_anomaly ~ s(SSB, k=8)+s(calfin_100m3, k=8)+s(pseudo_100m3,k=8), family=gaussian(),method = "REML",data=WGOM_Growth_spring)
summary(wgomWAA2)
wgomWAA2$aic
png("Figures/residual_plots/growth/WGOM_spring_WAA2.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomWAA2,pch=20, cex=1,cex.lab=1.3)
dev.off()
png("Figures/GAM_curves/growth/WGOM_spring_WAA2.png",width = 898, height =749, units = "px")
layout(matrix(1:4, ncol=2, byrow=FALSE))
GAM_CURVE_FUN(wgomWAA2,WGOM_Growth_spring$SSB,x_lab="SSB (kg/tow)",y_lab="PE on WAA Anomaly",select1=1,data_Year = WGOM_Growth_spring$Year,position = "topleft",title="WGOM WAA2 Spring")
GAM_CURVE_FUN(wgomWAA2,WGOM_Growth_spring$calfin_100m3,x_lab="Calanus (/100m3)",y_lab="PE on WAA Anomaly",select1=2,data_Year = WGOM_Growth_spring$Year,position = "topleft",title="WGOM WAA2 Spring")
GAM_CURVE_FUN(wgomWAA2,WGOM_Growth_spring$pseudo_100m3,x_lab="Pseudocalanus (/100m3)",y_lab="PE on WAA Anomaly",select1=3,data_Year = WGOM_Growth_spring$Year,position = "topleft",title="WGOM WAA2 Spring")
dev.off()
#WAA3
wgomWAA3<-gam(age3_anomaly ~ s(calfin_100m3, k=8)+s(pseudo_100m3,k=8), family=gaussian(),method = "REML",data=WGOM_Growth_spring)
summary(wgomWAA3)
wgomWAA3$aic

plot(influence.gam(wgomWAA3))
abline(h = 4/nrow(WGOM_Growth_spring), lty = 2, col = "red") # add cutoff line

png("Figures/residual_plots/growth/WGOM_spring_WAA3.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomWAA3,pch=20, cex=1,cex.lab=1.3)
dev.off()
png("Figures/GAM_curves/growth/WGOM_spring_WAA3.png",width = 898, height =374.5, units = "px")
layout(matrix(1:2, ncol=2, byrow=FALSE))
GAM_CURVE_FUN(wgomWAA3,WGOM_Growth_spring$calfin_100m3,x_lab="Calanus (/100m3)",y_lab="PE on WAA Anomaly",select1=1,data_Year = WGOM_Growth_spring$Year,position = "topright",title="WGOM WAA3 Spring")
GAM_CURVE_FUN(wgomWAA3,WGOM_Growth_spring$pseudo_100m3,x_lab="Pseudocalanus (/100m3)",y_lab="PE on WAA Anomaly",select1=2,data_Year = WGOM_Growth_spring$Year,position = "topleft",title="WGOM WAA3 Spring")
dev.off()
#WAA4
wgomWAA4<-gam(age4_anomaly ~ s(GSI,k=10), family=gaussian(),method = "REML",data=WGOM_Growth_spring)
summary(wgomWAA4)
wgomWAA4$aic
png("Figures/residual_plots/growth/WGOM_spring_WAA4.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomWAA4,pch=20, cex=1,cex.lab=1.3)
dev.off()
png("Figures/GAM_curves/growth/WGOM_spring_WAA4.png",width = 449, height = 374.5, units = "px")
GAM_CURVE_FUN(wgomWAA4,WGOM_Growth_spring$GSI,x_lab="GSI (Δ Deg Lat)",y_lab="PE on WAA Anomaly",select1=1,data_Year = WGOM_Growth_spring$Year,position = "bottomleft",title="WGOM WAA4 Spring")
dev.off()
#WAA5
wgomWAA5<-gam(age5_anomaly ~ s(Heatwave, k=10), family=gaussian(),method = "REML",data=WGOM_Growth_spring)
summary(wgomWAA5)
wgomWAA5$aic
png("Figures/residual_plots/growth/WGOM_spring_WAA5.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomWAA5,pch=20, cex=1,cex.lab=1.3)
dev.off()
png("Figures/GAM_curves/growth/WGOM_spring_WAA5.png",width = 449, height = 374.5, units = "px")
layout(matrix(1:1, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(wgomWAA5,WGOM_Growth_spring$Heatwave,x_lab="Heatwave (C)",y_lab="PE on WAA Anomaly",select1=1,data_Year = WGOM_Growth_spring$Year,position = "bottomleft",title="WGOM WAA5 Spring")
dev.off()
#WAA6
wgomWAA6<-gam(age6_anomaly ~ s(Heatwave,k=10), family=gaussian(),method = "REML",data=WGOM_Growth_spring)
summary(wgomWAA6)
wgomWAA6$aic
png("Figures/residual_plots/growth/WGOM_spring_WAA6.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomWAA6,pch=20, cex=1,cex.lab=1.3)
dev.off()
png("Figures/GAM_curves/growth/WGOM_spring_WAA6.png",width = 449, height = 374.5, units = "px")
GAM_CURVE_FUN(wgomWAA6,WGOM_Growth_spring$Heatwave,x_lab="Mean Cumulative Heatwave (C)",y_lab="PE on WAA Anomaly",select1=1,data_Year = WGOM_Growth_spring$Year,position = "bottomleft",title="WGOM WAA6 Spring")
dev.off()
#krel
wgomkrel<-gam(K_rel ~ s(GSI,k=10), family=gaussian(),method = "REML",data=WGOM_Growth_spring)
summary(wgomkrel)
wgomkrel$aic
png("Figures/residual_plots/growth/WGOM_spring_krel.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomkrel,pch=20, cex=1,cex.lab=1.3)
dev.off()
png("Figures/GAM_curves/growth/WGOM_spring_krel.png",width = 449, height = 374.5, units = "px")
GAM_CURVE_FUN(wgomkrel,WGOM_Growth_spring$GSI,x_lab="GSI (Deg Lat)",y_lab="PE on Relative Condition",select1=1,data_Year = WGOM_Growth_spring$Year,position = "bottomright",title="WGOM Rel. Condition Spring")
dev.off()
##### WGOM FALL####
#WAA1
wgomWAA1<-gam(age1_anomaly ~ s(GSI,k=10), family=gaussian(),method = "REML",data=WGOM_Growth_fall)
summary(wgomWAA1)
wgomWAA1$aic
png("Figures/residual_plots/growth/WGOM_fall_WAA1.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomWAA1,pch=20, cex=1,cex.lab=1.3)
dev.off()
png("Figures/GAM_curves/growth/WGOM_fall_WAA1.png",width = 449, height = 374.5, units = "px")
layout(matrix(1:1, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(wgomWAA1,WGOM_Growth_fall$GSI,x_lab="Mean GSI (Δ Deg Lat)",y_lab="PE on WAA Anomaly",select1=1,data_Year = WGOM_Growth_fall$Year,position = "topleft",title="WGOM WAA1 Fall")
dev.off()
#WAA3 #bad resids
#WAA4
wgomWAA4<-gam(age4_anomaly ~ s(pseudo_100m3,k=10), family=gaussian(),method = "REML",data=WGOM_Growth_fall)
summary(wgomWAA4)
wgomWAA4$aic
png("Figures/residual_plots/growth/WGOM_fall_WAA4.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomWAA4,pch=20, cex=1,cex.lab=1.3)
dev.off()
png("Figures/GAM_curves/growth/WGOM_fall_WAA4.png",width =449, height = 374.5, units = "px")
layout(matrix(1:1, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(wgomWAA4,WGOM_Growth_fall$pseudo_100m3,x_lab="Pseudocalanus Abundance (/100m3",y_lab="PE on WAA Anomaly",select1=1,data_Year = WGOM_Growth_fall$Year,position = "topleft",title="WGOM WAA4 Fall")
dev.off()
#WAA5
wgomWAA5<-gam(age5_anomaly ~ s(Heatwave, k=10), family=gaussian(),method = "REML",data=WGOM_Growth_fall)
summary(wgomWAA5)
wgomWAA5$aic
png("Figures/residual_plots/growth/WGOM_fall_WAA5.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomWAA5,pch=20, cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/growth/WGOM_fall_WAA5.png",width = 449, height = 374.5, units = "px")
par(mar=c(4.5,4.5,0.6,1))
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN(wgomWAA5,WGOM_Growth_fall$Heatwave,x_lab="Mean Cumulative Heatwave (C)",y_lab="PE on WAA Anomaly",select1=1,data_Year = WGOM_Growth_fall$Year,position = "bottomleft",title="WGOM WAA5 Fall")
dev.off()
#WAA6
wgomWAA6<-gam(age6_anomaly ~ s(sst_anomaly, k=10), family=gaussian(),method = "REML",data=WGOM_Growth_fall)
summary(wgomWAA6)
wgomWAA6$aic
png("Figures/residual_plots/growth/WGOM_fall_WAA6.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomWAA6,pch=20, cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/growth/WGOM_fall_WAA6.png",width = 449, height = 374.5, units = "px")
par(mar=c(4.5,4.5,0.6,1))
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN(wgomWAA6,WGOM_Growth_fall$sst_anomaly,x_lab="SST Anomaly (C)",y_lab="PE on WAA Anomaly",select1=1,data_Year = WGOM_Growth_fall$Year,position = "bottom",title="WGOM WAA6 Fall")
dev.off()
#krel
wgomkrel<-gam(K_rel ~ s(sst_anomaly, k=10), family=gaussian(),method = "REML",data=WGOM_Growth_fall)
summary(wgomkrel)
wgomkrel$aic

plot(influence.gam(wgomkrel))
abline(h = 4/nrow(WGOM_Growth_fall), lty = 2, col = "red") # add cutoff line

png("Figures/residual_plots/growth/WGOM_fall_krel.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomkrel,pch=20, cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/growth/WGOM_fall_krel.png",width = 449, height = 374.5, units = "px")
par(mar=c(4.5,4.5,0.6,1))
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN(wgomkrel,WGOM_Growth_fall$sst_anomaly,x_lab="SST Anomaly (C)",y_lab="PE on Rel. Condition",select1=1,data_Year = WGOM_Growth_fall$Year,position = "topleft",title="WGOM Rel. Condition Fall")
dev.off()
##### GBK SPRING####
#WAA2
gbkWAA2<-gam(age2_anomaly ~ s(sst_anomaly, k=10), family=gaussian(),method = "REML",data=GBK_Growth_spring)
summary(gbkWAA2)
gbkWAA2$aic
png("Figures/residual_plots/growth/GBK_spring_WAA2.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(gbkWAA2,pch=20, cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/growth/GBK_spring_WAA2.png",width = 449, height = 374.5, units = "px")
par(mar=c(4.5,4.5,0.6,1))
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN(gbkWAA2,GBK_Growth_spring$sst_anomaly,x_lab="SST Anomaly (C)",y_lab="PE on WAA Anomaly",select1=1,data_Year = GBK_Growth_spring$Year,position = "topright",title="GBK WAA2 Spring")
dev.off()
#WAA3
gbkWAA3<-gam(age3_anomaly ~ s(bt_anomaly, k=10)+s(calfin_100m3,k=10), family=gaussian(),method = "REML",data=GBK_Growth_spring)
summary(gbkWAA3)
gbkWAA3$aic
png("Figures/residual_plots/growth/GBK_spring_WAA3.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(gbkWAA3,pch=20, cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/growth/GBK_spring_WAA3.png",width =898, height = 374.5, units = "px")
layout(matrix(1:2, ncol=2, byrow=TRUE))
GAM_CURVE_FUN(gbkWAA3,GBK_Growth_spring$bt_anomaly,x_lab="Bt Anomaly (C)",y_lab="PE on WAA Anomaly",select1=1,data_Year = GBK_Growth_spring$Year,position = "topright",title="GBK WAA3 Spring")
GAM_CURVE_FUN(gbkWAA3,GBK_Growth_spring$calfin_100m3,x_lab="Calanus Abundance (/100m3)",y_lab="PE on WAA Anomaly",select1=2,data_Year = GBK_Growth_spring$Year,position = "topright",title="GBK WAA3 Spring")
dev.off()
#WAA4
gbkWAA4<-gam(age4_anomaly ~ s(Heatwave,k=10), family=gaussian(),method = "REML",data=GBK_Growth_spring)
summary(gbkWAA4)
gbkWAA4$aic
png("Figures/residual_plots/growth/GBK_spring_WAA4.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(gbkWAA4,pch=20, cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/growth/GBK_spring_WAA4.png",width =449, height = 374.5, units = "px")
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN(gbkWAA4,GBK_Growth_spring$Heatwave,x_lab="Heatwave (C)",y_lab="PE on WAA Anomaly",select1=1,data_Year = GBK_Growth_spring$Year,position = "bottomleft",title="GBK WAA4 Spring")
dev.off()
#WAA5
gbkWAA5<-gam(age5_anomaly ~ s(Heatwave, k=10), family=gaussian(),method = "REML",data=GBK_Growth_spring)
summary(gbkWAA5)
gbkWAA5$aic
png("Figures/residual_plots/growth/GBK_spring_WAA5.png",width = 449, height = 374.5, units = "px")
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(gbkWAA5,pch=20, cex=1,cex.lab=1.3)
dev.off()
png("Figures/GAM_curves/growth/GBK_spring_WAA5.png",width =449, height = 374.5, units = "px")
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN(gbkWAA5,GBK_Growth_spring$Heatwave,x_lab="Heatwave (C)",y_lab="PE on WAA Anomaly",select1=1,data_Year = GBK_Growth_spring$Year,position = "bottomleft",title="GBK WAA5 Spring")
dev.off()

#WAA6 #Bad resids
#WAA7 #bad resids
#krel
gbkkrel<-gam(K_rel ~ s(pseudo_100m3,k=10), family=gaussian(),method = "REML",data=GBK_Growth_spring)
summary(gbkkrel)
gbkkrel$aic
png("Figures/residual_plots/growth/GBK_spring_krel.png",width = 449, height = 374.5, units = "px")
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(gbkkrel,pch=20, cex=1,cex.lab=1.3)
dev.off()
png("Figures/GAM_curves/growth/GBK_spring_krel.png",width =449, height = 374.5, units = "px")
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN(gbkkrel,GBK_Growth_spring$pseudo_100m3,x_lab="Pseudocalanus Abundance (/100m3)",y_lab="PE on Relative Condition",select1=1,data_Year = GBK_Growth_spring$Year,position = "topleft",title="GBK Rel. Condition Spring")
dev.off()
##### GBK FALL####
#WAA1
#nothing significant
#WAA2
gbkWAA2<-gam(age2_anomaly ~ s(calfin_100m3, k=10)+s(sst_anomaly,k=10), family=gaussian(),method = "REML",data=GBK_Growth_fall)
summary(gbkWAA2)
gbkWAA2$aic
png("Figures/residual_plots/growth/GBK_fall_WAA2.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(gbkWAA2,pch=20, cex=1,cex.lab=1.3)
dev.off()
png("Figures/GAM_curves/growth/GBK_fall_WAA2.png",width =898, height = 374.5, units = "px")
par(mar=c(4.5,4.5,0.6,1))
layout(matrix(1:2, ncol=2, byrow=TRUE))
GAM_CURVE_FUN(gbkWAA2,GBK_Growth_fall$calfin_100m3,x_lab="Calanus Abundance (/100m3)",y_lab="PE on WAA Anomaly",select1=1,data_Year = GBK_Growth_fall$Year,position = "bottomleft",title="GBK WAA2 Fall")
GAM_CURVE_FUN(gbkWAA2,GBK_Growth_fall$sst_anomaly,x_lab="SST Anomaly (C)",y_lab="PE on WAA Anomaly",select1=2,data_Year = GBK_Growth_fall$Year,position = "bottomleft",title="GBK WAA2 Fall")
dev.off()
#WAA3
gbkWAA3<-gam(age3_anomaly ~ s(Heatwave,k=10), family=gaussian(),method = "REML",data=GBK_Growth_fall)
summary(gbkWAA3)
gbkWAA3$aic
png("Figures/residual_plots/growth/GBK_fall_WAA3.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(gbkWAA3,pch=20, cex=1,cex.lab=1.3)
dev.off()
png("Figures/GAM_curves/growth/GBK_fall_WAA3.png",width = 449, height = 374.5, units = "px")
par(mar=c(4.5,4.5,0.6,1))
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN(gbkWAA3,GBK_Growth_fall$Heatwave,x_lab="Heatwave (C)",y_lab="PE on WAA Anomaly",select1=1,data_Year = GBK_Growth_fall$Year,position = "bottomleft",title="GBK WAA3 Fall")
dev.off()
#WAA4
gbkWAA4<-gam(age4_anomaly ~ s(calfin_100m3,k=10), family=gaussian(),method = "REML",data=GBK_Growth_fall)
summary(gbkWAA4)
gbkWAA4$aic

plot(influence.gam(gbkWAA4))
abline(h = 4/nrow(GBK_Growth_fall), lty = 2, col = "red") # add cutoff line

png("Figures/residual_plots/growth/GBK_fall_WAA4.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(gbkWAA4,pch=20, cex=1,cex.lab=1.3)
dev.off()
png("Figures/GAM_curves/growth/GBK_fall_WAA4.png",width =449, height = 374.5, units = "px")
par(mar=c(4.5,4.5,0.6,1))
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN(gbkWAA4,GBK_Growth_fall$pseudo_100m3,x_lab="Calanus Abundance (/100m3)",y_lab="PE on WAA Anomaly",select1=1,data_Year = GBK_Growth_fall$Year,position = "bottomleft",title="GBK WAA4 Fall")
dev.off()
#WAA5 #bad resids
##### SNE SPRING####
#krel #bad resids
snekrel<-gam(K_rel ~ s(sst_anomaly, k=4)+s(Heatwave,k=4)+s(SSB,k=4), family=gaussian(),method = "REML",data=SNE_Growth_spring)
summary(snekrel)
snekrel$aic
################




#### spring/fall condition GSI####
png("Figures/GAM_curves/growth/wgom_GSI_krel.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4.5,4.5,2.5,1))
plot(Wgomk, select =1, scale =0,ylab = expression("PE on Relative Condition"), xlab = expression("Mean GSI (Δ Deg Lat)"), cex.lab=1.6,cex.axis=1.3,col="#00608A",shade = TRUE,shade.col=t_col("#00608A",70,"plot_ylwt"),lwd = 4,lty=1,xlim = c(-1,1.5),ylim = c(-0.05,0.05),rug=FALSE,main="GSI Effects on Cod Relative Condition")
rug(WGOM_Growth_fall$Avg_GSI, ticksize = 0.05, side = 1, lwd = 2.5, col = "black")
abline(h=0, lty=2, col="black", lwd=2.0)
par(new = TRUE)
plot(wgomKsp, select =1, scale =0,ylab = "", xlab = "",col="#535353",axes = FALSE,shade = TRUE,
     shade.col=t_col("#535353",70,"plot_red"),lwd = 4,lty=1,xlim = c(-1,1.5),ylim = c(-0.05,0.05),rug=FALSE)
legend("topleft", inset=0.04,
       legend = c("Fall","Spring"),col = c("#00608A","#535353"),
       cex = 1,lwd = c(4,4),lty = c(2,2),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
dev.off()
#### spring/fall WAA pseudocalanus####
png("Figures/GAM_curves/growth/wgom_pseudo_WAA.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4.5,4.5,2.5,1))
plot(wgom1, select =1, scale =0,ylab = expression("PE on WAA Anomaly"), xlab = expression("Pseudocalanus Density (/100m3)"), cex.lab=1.6,cex.axis=1.3,col="#00608A",shade = TRUE,shade.col=t_col("#00608A",70,"plot_ylwt"),lwd = 4,lty=1,xlim = c(-1.5,1),ylim = c(-1,1),rug=FALSE,main="Zooplankton Effects on Cod WAA Anomaly")
rug(WGOM_Growth_spring$pseudo_100m3, ticksize = 0.05, side = 1, lwd = 2.5, col = "#00608A")
abline(h=0, lty=2, col="black", lwd=2.0)
par(new = TRUE)
plot(wgomWAA3, select =1, scale =0,ylab = "", xlab = "",col="#535353",axes = FALSE,shade = TRUE,
     shade.col=t_col("#535353",70,"plot_red"),lwd = 4,lty=1,xlim = c(-1.5,1),ylim = c(-1,1),rug=FALSE)
rug(WGOM_Growth_fall$pseudo_100m3, ticksize = 0.05, side = 1, lwd = 2.5, col = "#535353")
legend("topleft", inset=0.04,
       legend = c("Spring Age 1","Fall Age 3"),col = c("#00608A","#535353"),
       cex = 1,lwd = c(4,4),lty = c(2,2),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
dev.off()
#### spring WAA 5&6 calanus####
png("Figures/GAM_curves/growth/wgom_calanus_WAA56.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4.5,4.5,2.5,1))
plot(wgomWAA5, select =1, scale =0,ylab = expression("PE on WAA Anomaly"), xlab = expression("Calanus Density (/100m3)"), cex.lab=1.6,cex.axis=1.3,col="#00608A",shade = TRUE,shade.col=t_col("#00608A",70,"plot_ylwt"),lwd = 4,lty=1,xlim = c(-0.6,0.4),ylim = c(-1.5,4),rug=FALSE,main="Zooplankton Effects on Cod WAA Anomaly")
rug(WGOM_Growth_spring$calfin_100m3, ticksize = 0.05, side = 1, lwd = 2.5, col = "black")
abline(h=0, lty=2, col="black", lwd=2.0)
par(new = TRUE)
plot(wgomWAA6, select =1, scale =0,ylab = "", xlab = "",col="#535353",axes = FALSE,shade = TRUE,
     shade.col=t_col("#535353",70,"plot_red"),lwd = 4,lty=1,xlim = c(-0.6,0.4),ylim = c(-1.5,4),rug=FALSE)
legend("topleft", inset=0.04,
       legend = c("Spring Age 5","Spring Age 6"),col = c("#00608A","#535353"),
       cex = 1,lwd = c(4,4),lty = c(2,2),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
dev.off()