library(cowplot)
source(here("Code/Env_data_by_stock.R"))
#### load .csv files #####
EGOM_K<-read.csv(here("data/rel_condition/ADIOS_SV_164712_EGOM_NONE_relative_k.csv"))
WGOM_K<-read.csv(here("data/rel_condition/ADIOS_SV_164712_WGOM_NONE_relative_k.csv"))
GBK_K<-read.csv(here("data/rel_condition/ADIOS_SV_164712_GBK_NONE_relative_k.csv"))
SNEMA_K<-read.csv(here("data/rel_condition/ADIOS_SV_164712_SNEMA_NONE_relative_k.csv"))

EGOM_WAA<-read.csv(here("data/WAA/EGOM_mean_weight_at_age.csv"))
names(EGOM_WAA)[7]<-"Year"
WGOM_WAA<-read.csv(here("data/WAA/WGOM_mean_weight_at_age.csv"))
names(WGOM_WAA)[7]<-"Year"
GBK_WAA<-read.csv(here("data/WAA/GBK_mean_weight_at_age.csv"))
names(GBK_WAA)[7]<-"Year"
SNE_WAA<-read.csv(here("data/WAA/SNEMA_mean_weight_at_age.csv"))
names(SNE_WAA)[7]<-"Year"
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
### Reorganize mean WAA data ####
EGOM_WAA<- EGOM_WAA[c(6,7,8,10)]
WGOM_WAA<- WGOM_WAA[c(6,7,8,10)]
GBK_WAA<- GBK_WAA[c(6,7,8,10)]
SNE_WAA<- SNE_WAA[c(6,7,8,10)]

EGOM_WAA= EGOM_WAA[!EGOM_WAA$Year > 2019,]
EGOM_WAA= EGOM_WAA[!EGOM_WAA$Year < 1982,]
WGOM_WAA= WGOM_WAA[!WGOM_WAA$Year > 2019,]
WGOM_WAA= WGOM_WAA[!WGOM_WAA$Year < 1982,]
GBK_WAA= GBK_WAA[!GBK_WAA$Year > 2019,]
GBK_WAA= GBK_WAA[!GBK_WAA$Year < 1982,]
SNE_WAA= SNE_WAA[!SNE_WAA$Year > 2019,]
SNE_WAA= SNE_WAA[!SNE_WAA$Year < 1982,]

df<-data.frame(Year=c(1982:2019))

WAA_transform<-function(data,season,newdf){
newdf<-data.frame(Year = c(1982:2019),
                        Age1 = merge(data[ which(data$AGE==1 & data$SEASON == season),],df,by="Year",all=TRUE)[4],
                        Age2 = merge(data[ which(data$AGE==2 & data$SEASON == season),],df,by="Year",all=TRUE)[4],
                        Age3 = merge(data[ which(data$AGE==3 & data$SEASON == season),],df,by="Year",all=TRUE)[4],
                        Age4 = merge(data[ which(data$AGE==4 & data$SEASON == season),],df,by="Year",all=TRUE)[4],
                        Age5 = merge(data[ which(data$AGE==5 & data$SEASON == season),],df,by="Year",all=TRUE)[4],
                        Age6 = merge(data[ which(data$AGE==6 & data$SEASON == season),],df,by="Year",all=TRUE)[4],
                        Age7 = merge(data[ which(data$AGE==7 & data$SEASON == season),],df,by="Year",all=TRUE)[4],
                        Age8 = merge(data[ which(data$AGE==8 & data$SEASON == season),],df,by="Year",all=TRUE)[4],
                        Age9plus = merge((aggregate(MEAN~Year,(merge(data[ which(data$AGE>=9 & data$SEASON == "FALL"),],df,by="Year",all=TRUE)),mean)),df,by="Year",all=TRUE)[2])
colnames(newdf)[2:10]<-c("Age1WAA","Age2WAA","Age3WAA","Age4WAA","Age5WAA","Age6WAA","Age7WAA","Age8WAA","Age9plusWAA")
return(newdf)
}

WAA_EGOM_FL<-WAA_transform(EGOM_WAA,"FALL",WAA_EGOM_FL)
WAA_EGOM_SP<-WAA_transform(EGOM_WAA,"SPRING",WAA_EGOM_SP)
WAA_WGOM_FL<-WAA_transform(WGOM_WAA,"FALL",WAA_WGOM_FL)
WAA_WGOM_SP<-WAA_transform(WGOM_WAA,"SPRING",WAA_WGOM_SP)
WAA_GBK_FL<-WAA_transform(GBK_WAA,"FALL",GBK_WGOM_FL)
WAA_GBK_SP<-WAA_transform(GBK_WAA,"SPRING",GBK_WGOM_SP)

WAA_transform_SNE<-function(data,season,newdf){
  newdf<-data.frame(Year = c(1982:2019),
                    Age1 = merge(data[ which(data$AGE==1 & data$SEASON == season),],df,by="Year",all=TRUE)[4],
                    Age2 = merge(data[ which(data$AGE==2 & data$SEASON == season),],df,by="Year",all=TRUE)[4],
                    Age3 = merge(data[ which(data$AGE==3 & data$SEASON == season),],df,by="Year",all=TRUE)[4],
                    Age4 = merge(data[ which(data$AGE==4 & data$SEASON == season),],df,by="Year",all=TRUE)[4],
                    Age5 = merge(data[ which(data$AGE==5 & data$SEASON == season),],df,by="Year",all=TRUE)[4],
                    Age6 = merge(data[ which(data$AGE==6 & data$SEASON == season),],df,by="Year",all=TRUE)[4],
                    Age7 = merge(data[ which(data$AGE==7 & data$SEASON == season),],df,by="Year",all=TRUE)[4],
                    Age8 = merge(data[ which(data$AGE==8 & data$SEASON == season),],df,by="Year",all=TRUE)[4],
                    Age9plus = merge(data[ which(data$AGE>=9 & data$SEASON == season),],df,by="Year",all=TRUE)[4])
  colnames(newdf)[2:10]<-c("Age1WAA","Age2WAA","Age3WAA","Age4WAA","Age5WAA","Age6WAA","Age7WAA","Age8WAA","Age9plusWAA")
  return(newdf)
}
WAA_SNE_FL<-WAA_transform_SNE(SNE_WAA,"FALL",SNE_WGOM_FL)
WAA_SNE_SP<-WAA_transform_SNE(SNE_WAA,"SPRING",SNE_WGOM_SP)
### turn recruitment data into growth appropriate data frames####
EGOM_growth_fall<-merge(EGOM_recruitment_fall[c(1,2,5,8:10)],EGOM_K_FL,by="Year",all=TRUE)
EGOM_growth_spring<-merge(EGOM_recruitment_spring[c(1,2,5,8:10)],EGOM_K_SP,by="Year",all=TRUE)
WGOM_growth_fall<-merge(WGOM_recruitment_fall[c(1,2,5,8:10)],WGOM_K_FL,by="Year",all=TRUE)
WGOM_growth_spring<-merge(WGOM_recruitment_spring[c(1,2,5,8:10)],WGOM_K_SP,by="Year",all=TRUE)
GBK_growth_fall<-merge(GBK_recruitment_fall[c(1,2,5,8:10)],GBK_K_FL,by="Year",all=TRUE)
GBK_growth_spring<-merge(GBK_recruitment_spring[c(1,2,5,8:10)],GBK_K_SP,by="Year",all=TRUE)
SNE_growth_fall<-merge(SNE_recruitment_fall[c(1,2,5,8:10)],SNEMA_K_FL,by="Year",all=TRUE)
SNE_growth_spring<-merge(SNE_recruitment_spring[c(1,2,5,8:10)],SNEMA_K_SP,by="Year",all=TRUE)

EGOM_growth_fall<-merge(EGOM_growth_fall,WAA_EGOM_FL,by="Year",all=TRUE)
EGOM_growth_spring<-merge(EGOM_growth_spring,WAA_EGOM_SP,by="Year",all=TRUE)
WGOM_growth_fall<-merge(WGOM_growth_fall,WAA_WGOM_FL,by="Year",all=TRUE)
WGOM_growth_spring<-merge(WGOM_growth_spring,WAA_WGOM_SP,by="Year",all=TRUE)
GBK_growth_fall<-merge(GBK_growth_fall,WAA_GBK_FL,by="Year",all=TRUE)
GBK_growth_spring<-merge(GBK_growth_spring,WAA_GBK_SP,by="Year",all=TRUE)
SNE_growth_fall<-merge(SNE_growth_fall,WAA_SNE_FL,by="Year",all=TRUE)
SNE_growth_spring<-merge(SNE_growth_spring,WAA_SNE_SP,by="Year",all=TRUE)
#####remove dfs I don't Need#####
rm(EGOM_K_FL,EGOM_K_SP,WGOM_K_FL,WGOM_K_SP,GBK_K_FL,GBK_K_SP,SNEMA_K_FL,SNEMA_K_SP,EGOM_recruitment_fall,EGOM_recruitment_spring,WGOM_recruitment_fall,WGOM_recruitment_spring,GBK_recruitment_fall,GBK_recruitment_spring,SNE_recruitment_fall,SNE_recruitment_spring,EGOM_WAA,WGOM_WAA,GBK_WAA,SNE_WAA,WAA_EGOM_FL,WAA_GBK_FL,WAA_GBK_SP,WAA_SNE_FL,WAA_SNE_SP,df,WAA_WGOM_SP,WAA_WGOM_FL,WAA_EGOM_SP)
##### Start here ######
source(here("Code/Gam_data_exploration.R"))
#put dfs in list to apply across functions
df.list <- list(EGOM_growth_fall[2:11],EGOM_growth_spring[2:11],WGOM_growth_fall[2:11],WGOM_growth_spring[2:11],GBK_growth_fall[2:11],GBK_growth_spring[2:11],SNE_growth_fall[2:11],SNE_growth_spring[2:11])

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
                  ylim=c(0.9,1.3))
lineplot_seasonal(springdata=WGOM_growth_spring,
                  falldata=WGOM_growth_fall,
                  springY=WGOM_growth_spring$K_rel,
                  fallY=WGOM_growth_fall$K_rel,
                  fallX= WGOM_growth_fall$Year,
                  springX= WGOM_growth_spring$Year,
                  main="NEFSC Trawl Survey Relative Condition: WGOM",
                  ylab="Relative Condition (K)",
                  ylim=c(0.9,1.3))
lineplot_seasonal(springdata=GBK_growth_spring,
                  falldata=GBK_growth_fall,
                  springY=GBK_growth_spring$K_rel,
                  fallY=GBK_growth_fall$K_rel,
                  fallX= GBK_growth_fall$Year,
                  springX= GBK_growth_spring$Year,
                  main="NEFSC Trawl Survey Relative Condition: GBK",
                  ylab="Relative Condition (K)",
                  ylim=c(0.9,1.3))
lineplot_seasonal(springdata=SNE_growth_spring,
                  falldata=SNE_growth_fall,
                  springY=SNE_growth_spring$K_rel,
                  fallY=SNE_growth_fall$K_rel,
                  fallX= SNE_growth_fall$Year,
                  springX= SNE_growth_spring$Year,
                  main="NEFSC Trawl Survey Relative Condition: SNE",
                  ylab="Relative Condition (K)",
                  ylim=c(0.9,1.3))



################ GAM LOOP FUNCTION#####
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
####Testing Relative K models######
############Gussian###################
####EGOM SPRING#####
targets <- c("K_rel")
predictors <- colnames(EGOM_growth_spring)[!(colnames(EGOM_growth_spring) %in% c("Year","K_rel","Age1WAA","Age2WAA","Age3WAA","Age4WAA","Age5WAA","Age6WAA","Age7WAA","Age8WAA","Age9plusWAA"))]
correlated_vars<-c("bt_anomaly","sst_anomaly")

GAM_LOOP_FUN(Edata=EGOM_growth_spring,k="k=3",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3= "NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=gaussian")
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/EGOM_spring_growth_NEFSC.png",height= 23*nrow(hypergrid_gaus), width = 138*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()
######WGOM SPRING#####
targets <- c("K_rel")
predictors <- colnames(WGOM_growth_spring)[!(colnames(WGOM_growth_spring) %in% c("Year","K_rel","Age1WAA","Age2WAA","Age3WAA","Age4WAA","Age5WAA","Age6WAA","Age7WAA","Age8WAA","Age9plusWAA"))]
correlated_vars<-c("bt_anomaly","sst_anomaly")

GAM_LOOP_FUN(Edata=WGOM_growth_spring,k="k=6",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3= "NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=gaussian()")
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/WGOM_spring_growth_NEFSC.png",height= 23*nrow(hypergrid_gaus), width = 138*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()
######GBK SPRING#####
targets <- c("K_rel")
predictors <- colnames(GBK_growth_spring)[!(colnames(GBK_growth_spring) %in% c("Year","K_rel","Age1WAA","Age2WAA","Age3WAA","Age4WAA","Age5WAA","Age6WAA","Age7WAA","Age8WAA","Age9plusWAA"))]
correlated_vars<-c("bt_anomaly","sst_anomaly")

GAM_LOOP_FUN(Edata=GBK_growth_spring,k="k=6",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3= "NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=gaussian()")
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/GBK_spring_growth_NEFSC.png",height= 23*nrow(hypergrid_gaus), width = 138*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()

######SNE SPRING#####
targets <- c("K_rel")
predictors <- colnames(SNE_growth_spring)[!(colnames(SNE_growth_spring) %in% c("Year","K_rel","Age1WAA","Age2WAA","Age3WAA","Age4WAA","Age5WAA","Age6WAA","Age7WAA","Age8WAA","Age9plusWAA"))]
correlated_vars<-c("bt_anomaly","sst_anomaly")

GAM_LOOP_FUN(Edata=SNE_growth_spring,k="k=3",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3= "NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=gaussian()")
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/SNE_spring_growth_NEFSC.png",height= 23*nrow(hypergrid_gaus), width = 138*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()

####EGOM FALL#####
targets <- c("K_rel")
predictors <- colnames(EGOM_growth_fall)[!(colnames(EGOM_growth_fall) %in% c("Year","K_rel","Age1WAA","Age2WAA","Age3WAA","Age4WAA","Age5WAA","Age6WAA","Age7WAA","Age8WAA","Age9plusWAA"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","Avg_GSI","EGOM_hw")

GAM_LOOP_FUN(Edata=EGOM_growth_fall,k="k=5",correlated_vars1= correlated_vars[3],correlated_vars2= correlated_vars[1],correlated_vars3=correlated_vars[1],correlated_vars4=correlated_vars[2],correlated_vars5=correlated_vars[4],correlated_vars6=correlated_vars[2],folder_name="recruitment",familyXYZ= "family=gaussian()")
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/EGOM_fall_growth_NEFSC.png",height= 24*nrow(hypergrid_gaus), width = 138*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()


####WGOM FALL#####
targets <- c("K_rel")
predictors <- colnames(WGOM_growth_fall)[!(colnames(WGOM_growth_fall) %in% c("Year","K_rel","Age1WAA","Age2WAA","Age3WAA","Age4WAA","Age5WAA","Age6WAA","Age7WAA","Age8WAA","Age9plusWAA"))]
correlated_vars<-c("bt_anomaly","sst_anomaly")

GAM_LOOP_FUN(Edata=WGOM_growth_fall,k="k=6",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3="NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=gaussian()")
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/WGOM_fall_growth_NEFSC.png",height= 24*nrow(hypergrid_gaus), width = 138*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()
####GBK FALL#####
targets <- c("K_rel")
predictors <- colnames(GBK_growth_fall)[!(colnames(GBK_growth_fall) %in% c("Year","K_rel","Age1WAA","Age2WAA","Age3WAA","Age4WAA","Age5WAA","Age6WAA","Age7WAA","Age8WAA","Age9plusWAA"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","Avg_GSI")

GAM_LOOP_FUN(Edata=GBK_growth_fall,k="k=6",correlated_vars1= correlated_vars[3],correlated_vars2= correlated_vars[1],correlated_vars3=correlated_vars[1],correlated_vars4=correlated_vars[2],correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=gaussian()")
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/GBK_fall_growth_NEFSC.png",height= 24*nrow(hypergrid_gaus), width = 138*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()

####SNE FALL#####
targets <- c("K_rel")
predictors <- colnames(SNE_growth_fall)[!(colnames(SNE_growth_fall) %in% c("Year","K_rel","Age1WAA","Age2WAA","Age3WAA","Age4WAA","Age5WAA","Age6WAA","Age7WAA","Age8WAA","Age9plusWAA","SSB"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","Avg_GSI")

GAM_LOOP_FUN(Edata=SNE_growth_fall,k="k=3",correlated_vars1= correlated_vars[3],correlated_vars2= correlated_vars[1],correlated_vars3=correlated_vars[1],correlated_vars4=correlated_vars[2],correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=gaussian()")
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/SNE_fall_growth_NEFSC.png",height= 24*nrow(hypergrid_gaus), width = 138*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()

#test<-gam(K_rel ~ s(SSB, k=2), family=gaussian(),method = "REML",data=SNE_growth_fall)
#summary(test)
#test$aic

############# PLOT SIGNIFICANT GAM CURVES #######################
##### K_rel (SPRING) vs. potential environmental influences###########
##### EGOM ####
#nothing significant
##### WGOM ####
wgomK<-gam(K_rel ~ s(Avg_GSI,k=6)+s(WGOM_hw, k=6)+s(SSB, k=6)+s(bt_anomaly, k=6), family=gaussian(),method = "REML",data=WGOM_growth_spring)
summary(wgomK)
wgomK$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomK,pch=20, cex=1.2,cex.lab=1.5)

par(mar=c(4.5,4.5,0.6,1))
layout(matrix(1:4, ncol=2, byrow=TRUE))
GAM_CURVE_FUN_spring(wgomK,WGOM_growth_spring$Avg_GSI,x_lab="Mean GSI",y_lab="PE on Relative Condition",select1=1)
GAM_CURVE_FUN_spring(wgomK,WGOM_growth_spring$WGOM_hw,x_lab="Mean Cumulative Heatwave (Deg C)",y_lab="PE on Mean Relative Condition",select1=2)
GAM_CURVE_FUN_spring(wgomK,WGOM_growth_spring$SSB,x_lab="SSB (kg/tow)",y_lab="PE on Mean Relative Condition",select1=3)
GAM_CURVE_FUN_spring(wgomK,WGOM_growth_spring$bt_anomaly,x_lab="Bottom Temperature Anomaly (Deg C)",y_lab="PE on Mean Relative Condition",select1=4)
##### GBK  ####
gbkK<-gam(K_rel ~ s(Avg_GSI,k=6)+s(GB_hw, k=6), family=gaussian(),method = "REML",data=GBK_growth_spring)
summary(gbkK)
gbkK$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(gbkK,pch=20, cex=1.2,cex.lab=1.5)

par(mar=c(4.5,4.5,0.6,1))
layout(matrix(1:4, ncol=2, byrow=TRUE))
GAM_CURVE_FUN_spring(gbkK,GBK_growth_spring$Avg_GSI,x_lab="Mean GSI",y_lab="PE on Relative Condition",select1=1)
GAM_CURVE_FUN_spring(gbkK,GBK_growth_spring$GB_hw,x_lab="Mean Cumulative Heatwave (Deg C)",y_lab="PE on Mean Relative Condition",select1=2)
##### SNE  ####
#nothing significant
##### K_rel  (FALL) vs. potential environmental influences##########
##### EGOM ####
EgomK<-gam(K_rel ~ s(bt_anomaly, k=10), family=gaussian(),method = "REML",data=EGOM_growth_fall)
summary(EgomK)
EgomK$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(EgomK,pch=20, cex=1.2,cex.lab=1.5)

par(mar=c(4.5,4.5,0.6,1))
layout(matrix(1:4, ncol=2, byrow=TRUE))
GAM_CURVE_FUN_fall(EgomK,EGOM_growth_fall$bt_anomaly,x_lab="Bottom Temperature Anomaly (Deg C)",y_lab="PE on Relative Condition",select1=1)

##### WGOM ####
Wgomk<-gam(K_rel ~ s(Avg_GSI,k=9)+s(WGOM_hw,k=9)+s(sst_anomaly,k=9), family=gaussian(),method = "REML",data=WGOM_growth_fall)
summary(Wgomk)
Wgomk$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(Wgomk,pch=20, cex=1.2,cex.lab=1.5)

par(mar=c(4.5,4.5,0.6,1))
layout(matrix(1:4, ncol=2, byrow=TRUE))
GAM_CURVE_FUN_fall(Wgomk,WGOM_growth_fall$Avg_GSI,x_lab="Mean GSI",y_lab="PE on Relative Condition",select1=1)
GAM_CURVE_FUN_fall(Wgomk,WGOM_growth_fall$WGOM_hw,x_lab="Mean Cumulative Heatwave (Deg C)",y_lab="PE on Relative Condition",select1=2)
GAM_CURVE_FUN_fall(Wgomk,WGOM_growth_fall$sst_anomaly,x_lab="SST Anomaly (Deg C)",y_lab="PE on Relative Condition",select1=3)
##### GBK  ####
GBKk<-gam(K_rel ~ s(Avg_GSI,k=10)+s(GB_hw,k=10)+s(sst_anomaly,k=10), family=gaussian(),method = "REML",data=GBK_growth_fall)
summary(GBKk)
GBKk$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(GBKk,pch=20, cex=1.2,cex.lab=1.5)

par(mar=c(4.5,4.5,0.6,1))
layout(matrix(1:4, ncol=2, byrow=TRUE))
GAM_CURVE_FUN_fall(GBKk,GBK_growth_fall$Avg_GSI,x_lab="Mean GSI",y_lab="PE on Relative Condition",select1=1)
GAM_CURVE_FUN_fall(GBKk,GBK_growth_fall$GB_hw,x_lab="Mean Cumulative Heatwave (Deg C)",y_lab="PE on Relative Condition",select1=2)
GAM_CURVE_FUN_fall(GBKk,GBK_growth_fall$sst_anomaly,x_lab="SST Anomaly (Deg C)",y_lab="PE on Relative Condition",select1=3)
##### SNE  ####
#Nothing Significant