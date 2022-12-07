library(here)
source(here("Code/Env_data_by_stock.R"))
#### load .csv files #####
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

### turn recruitment data into growth appropriate data frames####
EGOM_growth_fall<-merge(EGOM_recruitment_fall[c(1,2,5,8:12)],EGOM_K_FL,by="Year",all=TRUE)
EGOM_growth_spring<-merge(EGOM_recruitment_spring[c(1,2,5,8:12)],EGOM_K_SP,by="Year",all=TRUE)
WGOM_growth_fall<-merge(WGOM_recruitment_fall[c(1,2,5,8:12)],WGOM_K_FL,by="Year",all=TRUE)
WGOM_growth_spring<-merge(WGOM_recruitment_spring[c(1,2,5,8:12)],WGOM_K_SP,by="Year",all=TRUE)
GBK_growth_fall<-merge(GBK_recruitment_fall[c(1,2,5,8:12)],GBK_K_FL,by="Year",all=TRUE)
GBK_growth_spring<-merge(GBK_recruitment_spring[c(1,2,5,8:12)],GBK_K_SP,by="Year",all=TRUE)
SNE_growth_fall<-merge(SNE_recruitment_fall[c(1,2,5,8:12)],SNEMA_K_FL,by="Year",all=TRUE)
SNE_growth_spring<-merge(SNE_recruitment_spring[c(1,2,5,8:12)],SNEMA_K_SP,by="Year",all=TRUE)

#####merge WAAa dfs with environmental dfs#####
EGOM_growth_fall<-merge(EGOM_growth_fall,EGOM_WAAa_fall,by="Year",all=TRUE)
EGOM_growth_spring<-merge(EGOM_growth_spring,EGOM_WAAa_spring,by="Year",all=TRUE)
WGOM_growth_fall<-merge(WGOM_growth_fall,WGOM_WAAa_fall,by="Year",all=TRUE)
WGOM_growth_spring<-merge(WGOM_growth_spring,WGOM_WAAa_spring,by="Year",all=TRUE)
GBK_growth_fall<-merge(GBK_growth_fall,GBK_WAAa_fall,by="Year",all=TRUE)
GBK_growth_spring<-merge(GBK_growth_spring,GBK_WAAa_spring,by="Year",all=TRUE)
#####remove dfs I don't Need#####
rm(EGOM_K_FL,EGOM_K_SP,WGOM_K_FL,WGOM_K_SP,GBK_K_FL,GBK_K_SP,SNEMA_K_FL,SNEMA_K_SP,EGOM_recruitment_fall,EGOM_recruitment_spring,WGOM_recruitment_fall,WGOM_recruitment_spring,GBK_recruitment_fall,GBK_recruitment_spring,SNE_recruitment_fall,SNE_recruitment_spring,EGOM_WAA,WGOM_WAA,GBK_WAA,SNE_WAA,WAA_EGOM_FL,WAA_GBK_FL,WAA_GBK_SP,WAA_SNE_FL,WAA_SNE_SP,df,WAA_WGOM_SP,WAA_WGOM_FL,WAA_EGOM_SP,EGOM_WAAa_fall,EGOM_WAAa_spring,WGOM_WAAa_fall,WGOM_WAAa_spring,GBK_WAAa_fall,GBK_WAAa_spring)
##### Start here ######
source(here("Code/Gam_data_exploration.R"))
#put dfs in list to apply across functions
df.list <- list(EGOM_growth_fall,EGOM_growth_spring,WGOM_growth_fall[2:11],WGOM_growth_spring[2:11],GBK_growth_fall[2:11],GBK_growth_spring[2:11],SNE_growth_fall,SNE_growth_spring)

#apply functions
lapply(df.list, dotchart_fun_10)
lapply(df.list, hist_fun10)
lapply(df.list, view_boxplot_fun10)
lapply(df.list, shapiro_fun) #all normal except SSB and heatwave
lapply(df.list,Mypairs)
Mypairs(SNE_growth_fall[2:9])
#### view relative K timeseries ####
layout(matrix(1:4, ncol=2, byrow=TRUE))
par(mar=c(4.1,4.5,1.5,1), oma=c(1.0,0,1.0,0.1))
lineplot_seasonal(springdata=EGOM_growth_spring,
                  falldata=EGOM_growth_fall,
                  springY=EGOM_growth_spring$K_rel,
                  fallY=EGOM_growth_fall$K_rel,
                  fallX= EGOM_growth_fall$Year,
                  springX= EGOM_growth_spring$Year,
                  main="NEFSC Trawl Survey Relative Condition: EGOM",
                  ylab="Relative Condition (K)",
                  ylim=c(0.9,1.1))
lineplot_seasonal(springdata=WGOM_growth_spring,
                  falldata=WGOM_growth_fall,
                  springY=WGOM_growth_spring$K_rel,
                  fallY=WGOM_growth_fall$K_rel,
                  fallX= WGOM_growth_fall$Year,
                  springX= WGOM_growth_spring$Year,
                  main="NEFSC Trawl Survey Relative Condition: WGOM",
                  ylab="Relative Condition (K)",
                  ylim=c(0.9,1.1))
lineplot_seasonal(springdata=GBK_growth_spring,
                  falldata=GBK_growth_fall,
                  springY=GBK_growth_spring$K_rel,
                  fallY=GBK_growth_fall$K_rel,
                  fallX= GBK_growth_fall$Year,
                  springX= GBK_growth_spring$Year,
                  main="NEFSC Trawl Survey Relative Condition: GBK",
                  ylab="Relative Condition (K)",
                  ylim=c(0.9,1.1))
lineplot_seasonal(springdata=SNE_growth_spring,
                  falldata=SNE_growth_fall,
                  springY=SNE_growth_spring$K_rel,
                  fallY=SNE_growth_fall$K_rel,
                  fallX= SNE_growth_fall$Year,
                  springX= SNE_growth_spring$Year,
                  main="NEFSC Trawl Survey Relative Condition: SNE",
                  ylab="Relative Condition (K)",
                  ylim=c(0.9,1.1))
#### view weight at age anomaly timeseries ####

png(here("Figures/Raw_data_trends/WAAanomaly.png"),height= 800, width = 1700,res=125)
layout(matrix(1:6, ncol=3, byrow=FALSE))
par(mar=c(4.1,4.5,1.5,1), oma=c(1.0,0,1.0,0.1))
##EGOM FALL
plot(age2_anomaly~Year, data=EGOM_growth_fall, main="Weight at Age Anomalies: EGOM Fall",
     xlab="Year",ylab="Weight at Age Anomaly (Kg)", type="l",lwd=3,col="#407331",ylim= c(-4.6,5.5),xlim=c(1982,2019), cex.lab=1.4,cex.axis=1.1)
abline(h=0,col="#6B6B6B",lwd=2.0,lty=2)
legend("topleft",inset=c(0.02,0.02), legend=c("Age 2"), col=c("#407331"), lty=1,lwd=3.5, cex=0.9)
##EGOM Spring
plot(age1_anomaly~Year, data=EGOM_growth_spring, main="Weight at Age Anomalies: EGOM Spring",
     xlab="Year",ylab="Weight at Age Anomaly (Kg)", type="l",lwd=3,col="#3B4620",ylim= c(-4.6,5.5),xlim=c(1982,2019), cex.lab=1.4,cex.axis=1.1)
abline(h=0,col="#6B6B6B",lwd=2.0,lty=2)
legend("topleft",inset=c(0.02,0.02), legend=c("Age 1"), col=c("#3B4620"), lty=1,lwd=3.5, cex=0.9)
##WGOM Fall
plot(age1_anomaly~Year, data=WGOM_growth_fall, main="Weight at Age Anomalies: WGOM Fall",
     xlab="Year",ylab="Weight at Age Anomaly (Kg)", type="l",lwd=3,col="#3B4620",ylim= c(-4.6,5.5),xlim=c(1982,2019), cex.lab=1.4,cex.axis=1.1)
lines(age2_anomaly~Year, data=WGOM_growth_fall, xlab="Year", type="l",col="#407331",lwd=3)
lines(age3_anomaly~Year, data=WGOM_growth_fall, xlab="Year", type="l",col="#00608A",lwd=3)
lines(age4_anomaly~Year, data=WGOM_growth_fall, xlab="Year", type="l",col="#13A49D",lwd=3)
lines(age5_anomaly~Year, data=WGOM_growth_fall, xlab="Year", type="l",col="#ABB400",lwd=3)
lines(age6_anomaly~Year, data=WGOM_growth_fall, xlab="Year", type="l",col="#EACA00",lwd=3)
abline(h=0,col="#6B6B6B",lwd=2.0,lty=2)
legend("topright",inset=c(0.02,0.02), legend=c("Age 1", "Age 2","Age 3", "Age 4","Age 5","Age 6"), col=c("#3B4620", "#407331","#00608A", "#13A49D","#ABB400", "#EACA00"), lty=1,lwd=3.5, cex=0.9)
##WGOM Spring
plot(age1_anomaly~Year, data=WGOM_growth_spring, main="Weight at Age Anomalies: WGOM Spring",
     xlab="Year",ylab="Weight at Age Anomaly (Kg)", type="l",lwd=3,col="#3B4620",ylim= c(-4.6,5.5),xlim=c(1982,2019), cex.lab=1.4,cex.axis=1.1)
lines(age7_anomaly~Year, data=WGOM_growth_spring, xlab="Year", type="l",col="#F0713F",lwd=3)
lines(age2_anomaly~Year, data=WGOM_growth_spring, xlab="Year", type="l",col="#407331",lwd=3)
lines(age3_anomaly~Year, data=WGOM_growth_spring, xlab="Year", type="l",col="#00608A",lwd=3)
lines(age4_anomaly~Year, data=WGOM_growth_spring, xlab="Year", type="l",col="#13A49D",lwd=3)
lines(age5_anomaly~Year, data=WGOM_growth_spring, xlab="Year", type="l",col="#ABB400",lwd=3)
lines(age6_anomaly~Year, data=WGOM_growth_spring, xlab="Year", type="l",col="#EACA00",lwd=3)

abline(h=0,col="#6B6B6B",lwd=2.0,lty=2)
legend("topright",inset=c(0.02,0.02), legend=c("Age 1", "Age 2","Age 3", "Age 4","Age 5","Age 6","Age7"), col=c("#3B4620", "#407331","#00608A", "#13A49D","#ABB400", "#EACA00","#F0713F"), lty=1,lwd=3.5, cex=0.9)

##GBK Fall
plot(age1_anomaly~Year, data=GBK_growth_fall, main="Weight at Age Anomalies: GBK Fall",
     xlab="Year",ylab="Weight at Age Anomaly (Kg)", type="l",lwd=3,col="#3B4620",ylim= c(-4.6,5.5),xlim=c(1982,2019), cex.lab=1.4,cex.axis=1.1)
lines(age2_anomaly~Year, data=GBK_growth_fall, xlab="Year", type="l",col="#407331",lwd=3)
lines(age3_anomaly~Year, data=GBK_growth_fall, xlab="Year", type="l",col="#00608A",lwd=3)
lines(age4_anomaly~Year, data=GBK_growth_fall, xlab="Year", type="l",col="#13A49D",lwd=3)
lines(age5_anomaly~Year, data=GBK_growth_fall, xlab="Year", type="l",col="#ABB400",lwd=3)
abline(h=0,col="#6B6B6B",lwd=2.0,lty=2)
legend("topright",inset=c(0.02,0.02), legend=c("Age 1", "Age 2","Age 3", "Age 4","Age 5"), col=c("#3B4620", "#407331","#00608A", "#13A49D","#ABB400"), lty=1,lwd=3.5, cex=0.9)

##GBK Spring
plot(age1_anomaly~Year, data=GBK_growth_spring, main="Weight at Age Anomalies: GBK Spring",
     xlab="Year",ylab="Weight at Age Anomaly (Kg)", type="l",lwd=3,col="#3B4620",ylim= c(-4.6,5.5),xlim=c(1982,2019), cex.lab=1.4,cex.axis=1.1)
lines(age7_anomaly~Year, data=GBK_growth_spring, xlab="Year", type="l",col="#F0713F",lwd=3)
lines(age2_anomaly~Year, data=GBK_growth_spring, xlab="Year", type="l",col="#407331",lwd=3)
lines(age3_anomaly~Year, data=GBK_growth_spring, xlab="Year", type="l",col="#00608A",lwd=3)
lines(age4_anomaly~Year, data=GBK_growth_spring, xlab="Year", type="l",col="#13A49D",lwd=3)
lines(age5_anomaly~Year, data=GBK_growth_spring, xlab="Year", type="l",col="#ABB400",lwd=3)
lines(age6_anomaly~Year, data=GBK_growth_spring, xlab="Year", type="l",col="#EACA00",lwd=3)

abline(h=0,col="#6B6B6B",lwd=2.0,lty=2)
legend("topright",inset=c(0.02,0.02), legend=c("Age 1", "Age 2","Age 3", "Age 4","Age 5","Age 6","Age 7"), col=c("#3B4620", "#407331","#00608A", "#13A49D","#ABB400","#EACA00","#F0713F"), lty=1,lwd=3.5, cex=0.9)
dev.off()

################ GAM LOOP FUNCTION#####
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
#not enough data
targets <- c("age1_anomaly","K_rel")
predictors <- colnames(EGOM_growth_spring)[!(colnames(EGOM_growth_spring) %in% c("Year","K_rel","age1_anomaly"))]
#correlated_vars<-c("bt_anomaly","sst_anomaly")

GAM_LOOP_FUN(Edata=EGOM_growth_spring,k="k=3",correlated_vars1="NA",correlated_vars2="NA",correlated_vars3="NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=gaussian()",number_vars_in_mod= (length(predictors)-4))
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/growth/EGOM_spring_growth.png",height= 23*nrow(hypergrid_gaus), width = 138*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()

####EGOM FALL#####
targets <- c("age2_anomaly","K_rel")
predictors <- colnames(EGOM_growth_fall)[!(colnames(EGOM_growth_fall) %in% c("Year","K_rel","age2_anomaly","SSB"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","Avg_GSI","EGOM_hw")

GAM_LOOP_FUN(Edata=EGOM_growth_fall,k="k=5",correlated_vars1= correlated_vars[3],correlated_vars2= correlated_vars[1],correlated_vars3="NA",correlated_vars4="NA",correlated_vars5=correlated_vars[4],correlated_vars6=correlated_vars[2],folder_name="recruitment",familyXYZ= "family=gaussian()",number_vars_in_mod= (length(predictors)-4))
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/growth/EGOM_fall_growth.png",height= 24*nrow(hypergrid_gaus), width = 120*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()
######WGOM SPRING#####
targets <- c("age1_anomaly","age2_anomaly","age3_anomaly","age4_anomaly","age5_anomaly","age6_anomaly","age7_anomaly","K_rel")
predictors <- colnames(WGOM_growth_spring)[!(colnames(WGOM_growth_spring) %in% c("Year","K_rel","age1_anomaly","age2_anomaly","age3_anomaly","age4_anomaly","age5_anomaly","age6_anomaly","age7_anomaly"))]
#correlated_vars<-c("bt_anomaly","sst_anomaly")

GAM_LOOP_FUN(Edata=WGOM_growth_spring,k="k=7",correlated_vars1="NA",correlated_vars2="NA",correlated_vars3= "NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=gaussian()",number_vars_in_mod= (length(predictors)-4))
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/growth/WGOM_spring_growth.png",height= 23*nrow(hypergrid_gaus), width = 160*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()
####WGOM FALL#####
targets <- c("age1_anomaly","age2_anomaly","age3_anomaly","age4_anomaly","age5_anomaly","age6_anomaly","K_rel")
predictors <- colnames(WGOM_growth_fall)[!(colnames(WGOM_growth_fall) %in% c("Year","K_rel","age1_anomaly","age2_anomaly","age3_anomaly","age4_anomaly","age5_anomaly","age6_anomaly"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","Avg_GSI","EGOM_hw")

GAM_LOOP_FUN(Edata=WGOM_growth_fall,k="k=6",correlated_vars1= correlated_vars[3],correlated_vars2= correlated_vars[1],correlated_vars3="NA",correlated_vars4="NA",correlated_vars5=correlated_vars[4],correlated_vars6=correlated_vars[2],folder_name="recruitment",familyXYZ= "family=gaussian()",number_vars_in_mod= (length(predictors)-4))
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/growth/WGOM_fall_growth.png",height= 22*nrow(hypergrid_gaus), width =150*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()
######GBK SPRING#####
targets <- c("age1_anomaly","age2_anomaly","age3_anomaly","age4_anomaly","age5_anomaly","age6_anomaly","age7_anomaly","K_rel")
predictors <- colnames(GBK_growth_spring)[!(colnames(GBK_growth_spring) %in% c("Year","K_rel","age1_anomaly","age2_anomaly","age3_anomaly","age4_anomaly","age5_anomaly","age6_anomaly","age7_anomaly"))]
#correlated_vars<-c("bt_anomaly","sst_anomaly")

GAM_LOOP_FUN(Edata=GBK_growth_spring,k="k=7",correlated_vars1="NA",correlated_vars2="NA",correlated_vars3= "NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=gaussian()",number_vars_in_mod= (length(predictors)-4))
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/growth/GBK_spring_growth.png",height= 22*nrow(hypergrid_gaus), width = 150*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()

####GBK FALL#####
targets <- c("age1_anomaly","age2_anomaly","age3_anomaly","age4_anomaly","age5_anomaly","K_rel")
predictors <- colnames(GBK_growth_fall)[!(colnames(GBK_growth_fall) %in% c("Year","K_rel","age1_anomaly","age2_anomaly","age3_anomaly","age4_anomaly","age5_anomaly"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","Avg_GSI")

GAM_LOOP_FUN(Edata=GBK_growth_fall,k="k=7",correlated_vars1= correlated_vars[3],correlated_vars2= correlated_vars[1],correlated_vars3="NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=gaussian()",number_vars_in_mod= (length(predictors)-4))
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/growth/GBK_fall_growth.png",height= 22*nrow(hypergrid_gaus), width =150*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()

######SNE SPRING#####
targets <- c("K_rel")
predictors <- colnames(SNE_growth_spring)[!(colnames(SNE_growth_spring) %in% c("Year","K_rel"))]
#correlated_vars<-c("bt_anomaly","sst_anomaly")

GAM_LOOP_FUN(Edata=SNE_growth_spring,k="k=4",correlated_vars1="NA",correlated_vars2="NA",correlated_vars3= "NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=gaussian()",number_vars_in_mod= (length(predictors)-4))
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/growth/SNE_spring_growth.png",height= 22*nrow(hypergrid_gaus), width = 150*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()
############### PLOT SIGNIFICANT WAA GAM CURVES #######################
##### WGOM SPRING####
#age1
#nothing significant
#age2
wgomWAA2<-gam(age2_anomaly ~ s(SSB, k=8)+s(calfin_100m3, k=8)+s(pseudo_100m3,k=8), family=gaussian(),method = "REML",data=WGOM_growth_spring)
summary(wgomWAA2)
wgomWAA2$aic
png("Figures/residual_plots/growth/WGOM_spring_age2.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomWAA2,pch=20, cex=1,cex.lab=1.3)
dev.off()
png("Figures/GAM_curves/growth/WGOM_spring_age2.png",width = 898, height =749, units = "px")
layout(matrix(1:4, ncol=2, byrow=FALSE))
GAM_CURVE_FUN(wgomWAA2,WGOM_growth_spring$SSB,x_lab="SSB (kg/tow)",y_lab="PE on WAA Anomaly",select1=1,data_Year = WGOM_growth_spring$Year,position = "topleft",title="WGOM WAA2 Spring")
GAM_CURVE_FUN(wgomWAA2,WGOM_growth_spring$calfin_100m3,x_lab="Calanus (/100m3)",y_lab="PE on WAA Anomaly",select1=2,data_Year = WGOM_growth_spring$Year,position = "topleft",title="WGOM WAA2 Spring")
GAM_CURVE_FUN(wgomWAA2,WGOM_growth_spring$pseudo_100m3,x_lab="Pseudocalanus (/100m3)",y_lab="PE on WAA Anomaly",select1=3,data_Year = WGOM_growth_spring$Year,position = "topleft",title="WGOM WAA2 Spring")
dev.off()
#age3
wgomWAA3<-gam(age3_anomaly ~ s(WGOM_hw, k=8)+s(calfin_100m3, k=8)+s(pseudo_100m3,k=8), family=gaussian(),method = "REML",data=WGOM_growth_spring)
summary(wgomWAA3)
wgomWAA3$aic
png("Figures/residual_plots/growth/WGOM_spring_age3.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomWAA3,pch=20, cex=1,cex.lab=1.3)
dev.off()
png("Figures/GAM_curves/growth/WGOM_spring_age3.png",width = 898, height =749, units = "px")
layout(matrix(1:4, ncol=2, byrow=FALSE))
GAM_CURVE_FUN(wgomWAA3,WGOM_growth_spring$WGOM_hw,x_lab="Mean Cumulative Heatwave (C)",y_lab="PE on WAA Anomaly",select1=1,data_Year = WGOM_growth_spring$Year,position = "topleft",title="WGOM WAA3 Spring")
GAM_CURVE_FUN(wgomWAA3,WGOM_growth_spring$calfin_100m3,x_lab="Calanus (/100m3)",y_lab="PE on WAA Anomaly",select1=2,data_Year = WGOM_growth_spring$Year,position = "topleft",title="WGOM WAA3 Spring")
GAM_CURVE_FUN(wgomWAA3,WGOM_growth_spring$pseudo_100m3,x_lab="Pseudocalanus (/100m3)",y_lab="PE on WAA Anomaly",select1=3,data_Year = WGOM_growth_spring$Year,position = "topleft",title="WGOM WAA3 Spring")
dev.off()
#age4
wgomWAA4<-gam(age4_anomaly ~ s(Avg_GSI,k=10), family=gaussian(),method = "REML",data=WGOM_growth_spring)
summary(wgomWAA4)
wgomWAA4$aic
png("Figures/residual_plots/growth/WGOM_spring_age4.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomWAA4,pch=20, cex=1,cex.lab=1.3)
dev.off()
png("Figures/GAM_curves/growth/WGOM_spring_age4.png",width = 449, height = 374.5, units = "px")
GAM_CURVE_FUN(wgomWAA4,WGOM_growth_spring$Avg_GSI,x_lab="GSI (Δ Deg Lat)",y_lab="PE on WAA Anomaly",select1=1,data_Year = WGOM_growth_spring$Year,position = "bottomleft",title="WGOM WAA4 Spring")
dev.off()
#age5
wgomWAA5<-gam(age5_anomaly ~ s(WGOM_hw, k=10), family=gaussian(),method = "REML",data=WGOM_growth_spring)
summary(wgomWAA5)
wgomWAA5$aic
png("Figures/residual_plots/growth/WGOM_spring_age5.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomWAA5,pch=20, cex=1,cex.lab=1.3)
dev.off()
png("Figures/GAM_curves/growth/WGOM_spring_age5.png",width = 449, height = 374.5, units = "px")
layout(matrix(1:1, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(wgomWAA5,WGOM_growth_spring$WGOM_hw,x_lab="Mean Cumulative Heatwave (C)",y_lab="PE on WAA Anomaly",select1=1,data_Year = WGOM_growth_spring$Year,position = "bottomleft",title="WGOM WAA5 Spring")
dev.off()
#age6
wgomWAA6<-gam(age6_anomaly ~ s(WGOM_hw,k=10), family=gaussian(),method = "REML",data=WGOM_growth_spring)
summary(wgomWAA6)
wgomWAA6$aic
png("Figures/residual_plots/growth/WGOM_spring_age6.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomWAA6,pch=20, cex=1,cex.lab=1.3)
dev.off()
png("Figures/GAM_curves/growth/WGOM_spring_age6.png",width = 449, height = 374.5, units = "px")
GAM_CURVE_FUN(wgomWAA6,WGOM_growth_spring$WGOM_hw,x_lab="CMean Cumulative Heatwave (C)",y_lab="PE on WAA Anomaly",select1=1,data_Year = WGOM_growth_spring$Year,position = "bottomleft",title="WGOM WAA6 Spring")
dev.off()
##### WGOM FALL####
#age1
wgomWAA1<-gam(age1_anomaly ~ s(Avg_GSI,k=10), family=gaussian(),method = "REML",data=WGOM_growth_fall)
summary(wgomWAA1)
wgomWAA1$aic
png("Figures/residual_plots/growth/WGOM_fall_WAA1.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomWAA1,pch=20, cex=1,cex.lab=1.3)
dev.off()
png("Figures/GAM_curves/growth/WGOM_fall_age1.png",width = 449, height = 374.5, units = "px")
layout(matrix(1:1, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(wgomWAA1,WGOM_growth_fall$Avg_GSI,x_lab="Mean GSI (Δ Deg Lat)",y_lab="PE on WAA Anomaly",select1=1,data_Year = WGOM_growth_fall$Year,position = "topleft",title="WGOM WAA1 Fall")
dev.off()
#age5
wgomWAA5<-gam(age5_anomaly ~ s(WGOM_hw, k=10), family=gaussian(),method = "REML",data=WGOM_growth_fall)
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
GAM_CURVE_FUN(wgomWAA5,WGOM_growth_fall$WGOM_hw,x_lab="Mean Cumulative Heatwave (C)",y_lab="PE on WAA Anomaly",select1=1,data_Year = WGOM_growth_fall$Year,position = "bottomleft",title="WGOM WAA5 Fall")
dev.off()
#####GSI####
plot(wgomWAA1, select =1, scale =0,ylab = expression(bold("PE on WAA Anomaly")), xlab = expression(bold("Gulf Stream Index (Deg Lat)")), cex.lab=1.6,cex.axis=1.3,col="#00608A",shade = TRUE,shade.col=t_col("#00608A",55,"plot_ylwt"),lwd = 4,lty=2,xlim = c(-0.9,2),ylim = c(-0.5,1.2),rug=FALSE) #plot age 2
rug(WGOM_growth_fall$Avg_GSI, ticksize = 0.05, side = 1, lwd = 2.5, col = "black")
abline(h=0, lty=2, col="black", lwd=2.0)
legend("topleft", inset=0.04, # position
legend = c("Age 1"),col = c("#00608A"),
cex = 1,lwd = c(4,4,4),lty = c(2,2,2),text.col = "black",
box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
#####SSB
plot(wgomWAA1, select =2, scale =0,ylab = expression(bold("PE on WAA Anomaly")), xlab = expression(bold("Atlantic Cod SSB (kg/tow)")), cex.lab=1.6,cex.axis=1.3,col="#00608A",shade = TRUE,shade.col=t_col("#00608A",55,"plot_ylwt"),lwd = 4,lty=2,xlim = c(0,7),ylim = c(-0.2,0.25),rug=FALSE) #plot age 2
rug(WGOM_growth_fall$SSB, ticksize = 0.05, side = 1, lwd = 2.5, col = "black")
abline(h=0, lty=2, col="black", lwd=2.0)
legend("topleft", inset=0.04, # position
       legend = c("Age 1"),col = c("#00608A"),
       cex = 1,lwd = c(4,4,4),lty = c(2,2,2),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
#####Heatwave
plot(wgomWAA5, select =1, scale =0,ylab = expression(bold("PE on WAA Anomaly")), xlab = expression(bold("Mean Cumulative Heatwave (°C)")), cex.lab=1.6,cex.axis=1.3,col="#00608A",shade = TRUE,shade.col=t_col("#00608A",55,"plot_ylwt"),lwd = 4,lty=2,xlim = c(0,850),ylim = c(-3,2),rug=FALSE) #plot age 2
rug(WGOM_growth_fall$WGOM_hw, ticksize = 0.05, side = 1, lwd = 2.5, col = "black")
abline(h=0, lty=2, col="black", lwd=2.0)
legend("topright", inset=0.04, # position
       legend = c("Age 5"),col = c("#00608A"),
       cex = 1,lwd = c(4,4,4),lty = c(2,2,2),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
dev.off()



##### GBK SPRING####
#age2
gbkWAA2<-gam(age2_anomaly ~ s(Avg_GSI, k=10), family=gaussian(),method = "REML",data=GBK_growth_spring)
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
GAM_CURVE_FUN(gbkWAA2,GBK_growth_spring$Avg_GSI,x_lab="GSI (Δ Deg Lat)",y_lab="PE on WAA Anomaly",select1=1,data_Year = GBK_growth_spring$Year,position = "bottomleft",title="GBK WAA2 Spring")
dev.off()
#age3
gbkWAA3<-gam(age3_anomaly ~ s(GB_hw, k=10), family=gaussian(),method = "REML",data=GBK_growth_spring)
summary(gbkWAA3)
gbkWAA3$aic
png("Figures/residual_plots/growth/GBK_spring_WAA3.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(gbkWAA3,pch=20, cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/growth/GBK_spring_WAA3.png",width =449, height = 374.5, units = "px")
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN(gbkWAA3,GBK_growth_spring$GB_hw,x_lab="Mean Cumulative Heatwave (C)",y_lab="PE on WAA Anomaly",select1=1,data_Year = GBK_growth_spring$Year,position = "bottomleft",title="GBK WAA3 Spring")
dev.off()
#age4
gbkWAA4<-gam(age4_anomaly ~ s(GB_hw,k=10), family=gaussian(),method = "REML",data=GBK_growth_spring)
summary(gbkWAA4)
gbkWAA4$aic
png("Figures/residual_plots/growth/GBK_spring_WAA4.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(gbkWAA4,pch=20, cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/growth/GBK_spring_WAA4.png",width =449, height = 374.5, units = "px")
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN(gbkWAA4,GBK_growth_spring$GB_hw,x_lab="Mean Cumulative Heatwave (C)",y_lab="PE on WAA Anomaly",select1=1,data_Year = GBK_growth_spring$Year,position = "bottomleft",title="GBK WAA4 Spring")
dev.off()
#age5
gbkWAA5<-gam(age5_anomaly ~ s(GB_hw, k=10)+s(calfin_100m3,k=10), family=gaussian(),method = "REML",data=GBK_growth_spring)
summary(gbkWAA5)
gbkWAA5$aic
png("Figures/residual_plots/growth/GBK_spring_WAA5.png",width = 449, height = 374.5, units = "px")
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(gbkWAA5,pch=20, cex=1,cex.lab=1.3)
dev.off()
png("Figures/GAM_curves/growth/GBK_spring_WAA5.png",width = 898, height = 374.5, units = "px")
layout(matrix(1:2, ncol=2, byrow=TRUE))
GAM_CURVE_FUN(gbkWAA5,GBK_growth_spring$GB_hw,x_lab="Mean Cumulative Heatwave (C)",y_lab="PE on WAA Anomaly",select1=1,data_Year = GBK_growth_spring$Year,position = "bottomleft",title="GBK WAA5 Spring")
GAM_CURVE_FUN(gbkWAA5,GBK_growth_spring$calfin_100m3,x_lab="Calanus Density (/100m3)",y_lab="PE on WAA Anomaly",select1=2,data_Year = GBK_growth_spring$Year,position = "bottomleft",title="GBK WAA5 Spring")
dev.off()

#age6
gbkWAA6<-gam(age6_anomaly ~ s(calfin_100m3,k=10), family=gaussian(),method = "REML",data=GBK_growth_spring)
summary(gbkWAA6)
gbkWAA6$aic
png("Figures/residual_plots/growth/GBK_spring_WAA6.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(gbkWAA6,pch=20, cex=1,cex.lab=1.3)
dev.off()
png("Figures/GAM_curves/growth/GBK_spring_WAA6.png",width = 449, height = 374.5, units = "px")
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN(gbkWAA6,GBK_growth_spring$calfin_100m3,x_lab="Calanus Density (/100m3)",y_lab="PE on WAA Anomaly",select1=1,data_Year = GBK_growth_spring$Year,position = "bottomleft",title="GBK WAA6 Spring")
dev.off()
#####################
png("Figures/GAM_curves/growth/GBK_spring_WAA.png",width = 898, height = 749, units = "px")
par(mar=c(4.5,4.5,0.6,1))
layout(matrix(1:4, ncol=2, byrow=TRUE))
#####GSI
plot(gbkWAA3, select =2, scale =0,ylab = expression(bold("PE on WAA Anomaly")), xlab = expression(bold("Gulf Stream Index (Deg Lat)")), cex.lab=1.6,cex.axis=1.3,col="#407331",shade = TRUE,shade.col=t_col("#407331",40,"plot_ylwt"),lwd = 4,lty=2,xlim = c(-0.9,1),ylim = c(-0.2,1),rug=FALSE) #plot age 2
rug(GBK_growth_spring$Avg_GSI, ticksize = 0.05, side = 1, lwd = 2.5, col = "black")
abline(h=0, lty=2, col="black", lwd=2.0)
par(new = TRUE) #plot age 3
plot(gbkWAA2, select =1, scale =0,ylab = "", xlab = "",col="#00608A",axes = FALSE,shade = TRUE,
     shade.col=t_col("#00608A",55,"plot_red"),lwd = 4,lty=2,xlim = c(-0.9,1),ylim = c(-0.2,1),rug=FALSE)
legend("topright", inset=0.04, # position
       legend = c("Age 2","Age 3"),col = c("#00608A","#407331"),
       cex = 1,lwd = c(4,4,4),lty = c(2,2,2),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
#####Heatwave
plot(gbkWAA5, select =1, scale =0,ylab = expression(bold("PE on WAA Anomaly")), xlab = expression(bold("Mean Cumulative Heatwave (°C)")), cex.lab=1.6,cex.axis=1.3,col="#13A49D",shade = TRUE,shade.col=t_col("#13A49D",70,"plot_ylwt"),lwd = 4,lty=2,xlim = c(0,650),ylim = c(-2,1.75),rug=FALSE) #plot age 2
rug(GBK_growth_spring$GB_hw, ticksize = 0.05, side = 1, lwd = 2.5, col = "black")
abline(h=0, lty=2, col="black", lwd=2.0)
par(new = TRUE) #plot age 3
plot(gbkWAA4, select =1, scale =0,ylab = "", xlab = "",col="#00608A",axes = FALSE,shade = TRUE,
     shade.col=t_col("#00608A",55,"plot_red"),lwd = 4,lty=2,xlim = c(0,650),ylim = c(-2,1.75),rug=FALSE)
par(new = TRUE) #plot age 3
plot(gbkWAA3, select =1, scale =0,ylab = "", xlab = "",col="#407331",axes = FALSE,shade = TRUE,
     shade.col=t_col("#407331",40,"plot_red"),lwd = 4,lty=2,xlim = c(0,650),ylim = c(-2,1.75),rug=FALSE)
legend("topright", inset=0.04, # position
       legend = c("Age 3","Age 4","Age 5"),col = c("#407331","#00608A","#13A49D"),
       cex = 1,lwd = c(4,4,4),lty = c(2,2,2),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
#####SST
plot(gbkWAA5, select =2, scale =0,ylab = expression(bold("PE on WAA Anomaly")), xlab = expression(bold("SST Anomaly (°C)")), cex.lab=1.6,cex.axis=1.3,col="#00608A",shade = TRUE,shade.col=t_col("#00608A",55,"plot_ylwt"),lwd = 4,lty=2,xlim = c(-1.5,1),ylim = c(-1.5,1),rug=FALSE) #plot age 2
rug(GBK_growth_spring$sst_anomaly, ticksize = 0.05, side = 1, lwd = 2.5, col = "black")
abline(h=0, lty=2, col="black", lwd=2.0)
legend("topright", inset=0.04, # position
       legend = c("Age 5"),col = c("#00608A"),
       cex = 1,lwd = c(4,4,4),lty = c(2,2,2),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
dev.off()

##### GBK FALL####
#age1
#nothing significant
#age2
gbkWAA2<-gam(age2_anomaly ~ s(calfin_100m3, k=10)+s(sst_anomaly,k=10), family=gaussian(),method = "REML",data=GBK_growth_fall)
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
GAM_CURVE_FUN(gbkWAA2,GBK_growth_fall$calfin_100m3,x_lab="Calanus (/100m3)",y_lab="PE on WAA Anomaly",select1=1,data_Year = GBK_growth_fall$Year,position = "bottomleft",title="GBK WAA2 Fall")
GAM_CURVE_FUN(gbkWAA2,GBK_growth_fall$sst_anomaly,x_lab="SST Anomaly (C)",y_lab="PE on WAA Anomaly",select1=2,data_Year = GBK_growth_fall$Year,position = "bottomleft",title="GBK WAA2 Fall")
dev.off()
#age3
gbkWAA3<-gam(age3_anomaly ~ s(GB_hw,k=10), family=gaussian(),method = "REML",data=GBK_growth_fall)
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
GAM_CURVE_FUN(gbkWAA3,GBK_growth_fall$GB_hw,x_lab="Mean Cumulative Heatwave (C)",y_lab="PE on WAA Anomaly",select1=1,data_Year = GBK_growth_fall$Year,position = "bottomleft",title="GBK WAA3 Fall")
dev.off()
#age4
gbkWAA4<-gam(age4_anomaly ~ s(pseudo_100m3,k=10)+s(sst_anomaly,k=10), family=gaussian(),method = "REML",data=GBK_growth_fall)
summary(gbkWAA4)
gbkWAA4$aic
png("Figures/residual_plots/growth/GBK_fall_WAA4.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(gbkWAA4,pch=20, cex=1,cex.lab=1.3)
dev.off()
png("Figures/GAM_curves/growth/GBK_fall_WAA4.png",width =898, height = 374.5, units = "px")
par(mar=c(4.5,4.5,0.6,1))
layout(matrix(1:2, ncol=2, byrow=TRUE))
GAM_CURVE_FUN(gbkWAA4,GBK_growth_fall$pseudo_100m3,x_lab="Pseudocalanus (/100m3)",y_lab="PE on WAA Anomaly",select1=1,data_Year = GBK_growth_fall$Year,position = "topleft",title="GBK WAA4 Fall")
GAM_CURVE_FUN(gbkWAA4,GBK_growth_fall$sst_anomaly,x_lab="SST Anomaly (C)",y_lab="PE on WAA Anomaly",select1=2,data_Year = GBK_growth_fall$Year,position = "topleft",title="GBK WAA4 Fall")
dev.off()
################
png("Figures/GAM_curves/growth/GBK_fall_WAA.png",width = 898, height = 749, units = "px")
par(mar=c(4.5,4.5,0.6,1))
layout(matrix(1:4, ncol=2, byrow=TRUE))
#####GSI
plot(gbkWAA1, select =4, scale =0,ylab = expression(bold("PE on WAA Anomaly")), xlab = expression(bold("Gulf Stream Index (Deg Lat)")), cex.lab=1.6,cex.axis=1.3,col="#00608A",shade = TRUE,shade.col=t_col("#00608A",55,"plot_ylwt"),lwd = 4,lty=2,xlim = c(-0.9,2),ylim = c(-0.5,1.2),rug=FALSE) #plot age 2
rug(GBK_growth_fall$Avg_GSI, ticksize = 0.05, side = 1, lwd = 2.5, col = "black")
abline(h=0, lty=2, col="black", lwd=2.0)
legend("topleft", inset=0.04, # position
       legend = c("Age 1"),col = c("#00608A"),
       cex = 1,lwd = c(4,4,4),lty = c(2,2,2),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
#####SSB
plot(gbkWAA1, select =2, scale =0,ylab = expression(bold("PE on WAA Anomaly")), xlab = expression(bold("Atlantic Cod SSB (kg/tow)")), cex.lab=1.6,cex.axis=1.3,col="#00608A",shade = TRUE,shade.col=t_col("#00608A",55,"plot_ylwt"),lwd = 4,lty=2,xlim = c(0,7),ylim = c(-0.30,0.2),rug=FALSE) #plot age 2
rug(GBK_growth_fall$SSB, ticksize = 0.05, side = 1, lwd = 2.5, col = "black")
abline(h=0, lty=2, col="black", lwd=2.0)
legend("topleft", inset=0.04, # position
       legend = c("Age 1"),col = c("#00608A"),
       cex = 1,lwd = c(4,4,4),lty = c(2,2,2),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
#####SST
plot(gbkWAA1, select =1, scale =0,ylab = expression(bold("PE on WAA Anomaly")), xlab = expression(bold("SST Anomaly (°C)")), cex.lab=1.6,cex.axis=1.3,col="#00608A",shade = TRUE,shade.col=t_col("#00608A",55,"plot_ylwt"),lwd = 4,lty=2,xlim = c(0,7),ylim = c(-1.30,0.2),rug=FALSE)
rug(GBK_growth_fall$sst_anomaly, ticksize = 0.05, side = 1, lwd = 2.5, col = "black")
abline(h=0, lty=2, col="black", lwd=2.0)
legend("topleft", inset=0.04, # position
       legend = c("Age 1"),col = c("#00608A"),
       cex = 1,lwd = c(4,4,4),lty = c(2,2,2),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
#####Heatwave
plot(gbkWAA3, select =1, scale =0,ylab = expression(bold("PE on WAA Anomaly")), xlab = expression(bold("Mean Cumulative Heatwave (°C)")), cex.lab=1.6,cex.axis=1.3,col="#13A49D",shade = TRUE,shade.col=t_col("#13A49D",55,"plot_ylwt"),lwd = 4,lty=2,xlim = c(0,850),ylim = c(-3,2),rug=FALSE) #plot age 2
rug(GBK_growth_fall$GB_hw, ticksize = 0.05, side = 1, lwd = 2.5, col = "black")
abline(h=0, lty=2, col="black", lwd=2.0)
par(new = TRUE) #plot age 3
plot(gbkWAA2, select =1, scale =0,ylab = "", xlab = "",col="#00608A",axes = FALSE,shade = TRUE,
     shade.col=t_col("#00608A",55,"plot_red"),lwd = 4,lty=2,xlim = c(0,850),ylim = c(-1.5,2),rug=FALSE)
par(new = TRUE) #plot age 3
plot(gbkWAA1, select =3, scale =0,ylab = "", xlab = "",col="#407331",axes = FALSE,shade = TRUE,
     shade.col=t_col("#407331",40,"plot_red"),lwd = 4,lty=2,xlim = c(0,850),ylim = c(-1.5,2),rug=FALSE)
legend("topright", inset=0.04, # position
       legend = c("Age 1","Age 2","Age 3"),col = c("#407331","#00608A","#13A49D"),
       cex = 1,lwd = c(4,4,4),lty = c(2,2,2),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
dev.off()




############# PLOT SIGNIFICANT GAM CURVES #######################
##### K_rel (SPRING) vs. potential environmental influences###########
##### EGOM ####
#nothing significant for spring
##### WGOM ####
wgomKsp<-gam(K_rel ~ s(Avg_GSI,k=10), family=gaussian(),method = "REML",data=WGOM_growth_spring)
summary(wgomKsp)
wgomKsp$aic

png("Figures/residual_plots/growth/WGOM_spring_relk.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomKsp,pch=20, cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/growth/WGOM_spring_relk.png",width =449, height = 374.5, units = "px")
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN(wgomKsp,WGOM_growth_spring$Avg_GSI,x_lab="Mean GSI (Δ Deg Lat)",y_lab="PE on Relative Condition",select1=1,data_Year = WGOM_growth_spring$Year,position = "topleft",title="WGOM Spring")
dev.off()
##### GBK  ####
gbkK<-gam(K_rel ~ s(pseudo_100m3,k=10)+s(sst_anomaly,k=10), family=gaussian(),method = "REML",data=GBK_growth_spring)
summary(gbkK)
gbkK$aic

png("Figures/residual_plots/growth/GBK_spring_relk.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(gbkK,pch=20, cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/growth/GBK_spring_relk.png",width = 898, height = 374.5, units = "px")
layout(matrix(1:2, ncol=2, byrow=TRUE))
GAM_CURVE_FUN(gbkK,GBK_growth_spring$pseudo_100m3,x_lab="Pseudocalanus Density (/100m3)",y_lab="PE on Mean Relative Condition",select1=1,data_Year = GBK_growth_spring$Year,position = "topleft",title="GBK Spring")
GAM_CURVE_FUN(gbkK,GBK_growth_spring$sst_anomaly,x_lab="SST Anomaly (C)",y_lab="PE on Mean Relative Condition",select1=2,data_Year = GBK_growth_spring$Year,position = "topleft",title="GBK Spring")
dev.off()
##### SNE  ####
#nothing significant
##### K_rel  (FALL) vs. potential environmental influences##########
##### EGOM ####
egomK<-gam(K_rel ~ s(bt_anomaly, k=10), family=gaussian(),method = "REML",data=EGOM_growth_fall)
summary(egomK)
egomK$aic

png("Figures/residual_plots/growth/EGOM_fall_relk.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(egomK,pch=20, cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/growth/EGOM_fall_relk.png",width = 449, height = 374.5, units = "px")
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN(egomK,EGOM_growth_fall$bt_anomaly,x_lab="Bottom Temperature Anomaly (C)",y_lab="PE on Relative Condition",select1=1,data_Year = EGOM_growth_fall$Year,position = "bottomleft",title="EGOM Fall")
dev.off()
##### WGOM ####
Wgomk<-gam(K_rel ~ s(Avg_GSI,k=10), family=gaussian(),method = "REML",data=WGOM_growth_fall)
summary(Wgomk)
Wgomk$aic
png("Figures/residual_plots/growth/WGOM_fall_relk.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(Wgomk,pch=20, cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/growth/WGOM_fall_relk.png",width = 449, height = 374.5, units = "px")
par(mar=c(4.5,4.5,0.6,1))
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN(Wgomk,WGOM_growth_fall$Avg_GSI,x_lab="Mean GSI (Δ Deg Lat)",y_lab="PE on Relative Condition",select1=1,data_Year = WGOM_growth_fall$Year,position = "topleft",title="WGOM Fall")
dev.off()

##### GBK  ####
GBKk<-gam(K_rel ~ s(GB_hw,k=10), family=gaussian(),method = "REML",data=GBK_growth_fall)
summary(GBKk)
GBKk$aic
png("Figures/residual_plots/growth/GBK_fall_relk.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(GBKk,pch=20, cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/growth/GBK_fall_relk.png",width = 449, height = 374.5, units = "px")
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN(GBKk,GBK_growth_fall$GB_hw,x_lab="Mean Cumulative Heatwave (C)",y_lab="PE on Relative Condition",select1=1,data_Year = GBK_growth_fall$Year,position = "topleft",title="GBK Fall")
dev.off()
##### SNE  ####
#Nothing Significant
#### spring/fall condition GSI####
png("Figures/GAM_curves/growth/wgom_GSI_relk.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4.5,4.5,2.5,1))
plot(Wgomk, select =1, scale =0,ylab = expression("PE on Relative Condition"), xlab = expression("Mean GSI (Δ Deg Lat)"), cex.lab=1.6,cex.axis=1.3,col="#00608A",shade = TRUE,shade.col=t_col("#00608A",70,"plot_ylwt"),lwd = 4,lty=1,xlim = c(-1,1.5),ylim = c(-0.05,0.05),rug=FALSE,main="GSI Effects on Cod Relative Condition")
rug(WGOM_growth_fall$Avg_GSI, ticksize = 0.05, side = 1, lwd = 2.5, col = "black")
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
rug(WGOM_growth_spring$pseudo_100m3, ticksize = 0.05, side = 1, lwd = 2.5, col = "#00608A")
abline(h=0, lty=2, col="black", lwd=2.0)
par(new = TRUE)
plot(wgomWAA3, select =1, scale =0,ylab = "", xlab = "",col="#535353",axes = FALSE,shade = TRUE,
     shade.col=t_col("#535353",70,"plot_red"),lwd = 4,lty=1,xlim = c(-1.5,1),ylim = c(-1,1),rug=FALSE)
rug(WGOM_growth_fall$pseudo_100m3, ticksize = 0.05, side = 1, lwd = 2.5, col = "#535353")
legend("topleft", inset=0.04,
       legend = c("Spring Age 1","Fall Age 3"),col = c("#00608A","#535353"),
       cex = 1,lwd = c(4,4),lty = c(2,2),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
dev.off()
#### spring WAA 5&6 calanus####
png("Figures/GAM_curves/growth/wgom_calanus_WAA56.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4.5,4.5,2.5,1))
plot(wgomWAA5, select =1, scale =0,ylab = expression("PE on WAA Anomaly"), xlab = expression("Calanus Density (/100m3)"), cex.lab=1.6,cex.axis=1.3,col="#00608A",shade = TRUE,shade.col=t_col("#00608A",70,"plot_ylwt"),lwd = 4,lty=1,xlim = c(-0.6,0.4),ylim = c(-1.5,4),rug=FALSE,main="Zooplankton Effects on Cod WAA Anomaly")
rug(WGOM_growth_spring$calfin_100m3, ticksize = 0.05, side = 1, lwd = 2.5, col = "black")
abline(h=0, lty=2, col="black", lwd=2.0)
par(new = TRUE)
plot(wgomWAA6, select =1, scale =0,ylab = "", xlab = "",col="#535353",axes = FALSE,shade = TRUE,
     shade.col=t_col("#535353",70,"plot_red"),lwd = 4,lty=1,xlim = c(-0.6,0.4),ylim = c(-1.5,4),rug=FALSE)
legend("topleft", inset=0.04,
       legend = c("Spring Age 5","Spring Age 6"),col = c("#00608A","#535353"),
       cex = 1,lwd = c(4,4),lty = c(2,2),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
dev.off()