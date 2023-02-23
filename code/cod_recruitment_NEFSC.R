library(pacman)
pacman::p_load(here, readxl,lubridate,stats,graphics,Hmisc,data.table,utils,mgcv,dplyr,purrr,ecodata,kableExtra,gridExtra) 
nm <- list.files(path =here("data/final_env_data/recruitment"), pattern = ".csv", full.names = TRUE)
nm2 <- list.files(path =here("data/final_env_data/recruitment"), pattern = ".csv", full.names =FALSE)
list2env(lapply(setNames(nm, make.names(gsub("*.csv$", "",nm2))),read.csv),envir=.GlobalEnv)
rm(myfiles,nm,nm2)
##### Create R/ssb column####
EGOM_recruitment_fall$RSSB<-EGOM_recruitment_fall$Age.1/lag(EGOM_recruitment_fall[,"SSB"])
EGOM_recruitment_spring$RSSB<-EGOM_recruitment_spring$Age.1/lag(EGOM_recruitment_spring[,"SSB"])
WGOM_recruitment_fall$RSSB<-WGOM_recruitment_fall$Age.1/lag(WGOM_recruitment_fall[,"SSB"])
WGOM_recruitment_spring$RSSB<-WGOM_recruitment_spring$Age.1/lag(WGOM_recruitment_spring[,"SSB"])
GBK_recruitment_fall$RSSB<-GBK_recruitment_fall$Age.1/lag(GBK_recruitment_fall[,"SSB"])
GBK_recruitment_spring$RSSB<-GBK_recruitment_spring$Age.1/lag(GBK_recruitment_spring[,"SSB"])
SNE_recruitment_fall$RSSB<-SNE_recruitment_fall$Age.1/lag(SNE_recruitment_fall[,"SSB"])
SNE_recruitment_spring$RSSB<-SNE_recruitment_spring$Age.1/lag(SNE_recruitment_spring[,"SSB"])
SNE_recruitment_spring[SNE_recruitment_spring == Inf] <- 0 #replace one cell that had Inf value
#### create lAGE1 column#######
EGOM_recruitment_fall$lAGE1<-(EGOM_recruitment_fall$Age.1)
WGOM_recruitment_fall$lAGE1<-(WGOM_recruitment_fall$Age.1)
GBK_recruitment_fall$lAGE1<-(GBK_recruitment_fall$Age.1)
SNE_recruitment_fall$lAGE1<-(SNE_recruitment_fall$Age.1)

EGOM_recruitment_spring$lAGE1<-(EGOM_recruitment_spring$Age.1)
WGOM_recruitment_spring$lAGE1<-(WGOM_recruitment_spring$Age.1)
GBK_recruitment_spring$lAGE1<-(GBK_recruitment_spring$Age.1)
SNE_recruitment_spring$lAGE1<-(SNE_recruitment_spring$Age.1)

#add +1 to all cells to combat cells with 0 values
EGOM_recruitment_fall$lAGE1<-EGOM_recruitment_fall[,"lAGE1"]+1
WGOM_recruitment_fall$lAGE1<-WGOM_recruitment_fall[,"lAGE1"]+1
GBK_recruitment_fall$lAGE1<-GBK_recruitment_fall[,"lAGE1"]+1
SNE_recruitment_fall$lAGE1<-SNE_recruitment_fall[,"lAGE1"]+1

EGOM_recruitment_spring$lAGE1<-EGOM_recruitment_spring[,"lAGE1"]+1
WGOM_recruitment_spring$lAGE1<-WGOM_recruitment_spring[,"lAGE1"]+1
GBK_recruitment_spring$lAGE1<-GBK_recruitment_spring[,"lAGE1"]+1
SNE_recruitment_spring$lAGE1<-SNE_recruitment_spring[,"lAGE1"]+1

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
##### Start here ######
source(here("Code/Gam_data_exploration.R"))
#put dfs in list to apply across functions
df.list <- list(EGOM_recruitment_fall,EGOM_recruitment_spring,WGOM_recruitment_fall,WGOM_recruitment_spring,GBK_recruitment_fall,GBK_recruitment_spring,SNE_recruitment_fall,SNE_recruitment_spring)

#apply functions
lapply(df.list, dotchart_fun_10)
lapply(df.list, hist_fun10)
lapply(df.list, view_boxplot_fun10)
lapply(df.list, shapiro_fun)
#histograms of dependent data
hist(EGOM_recruitment_fall$RSSB)
hist(EGOM_recruitment_spring$RSSB)
hist(WGOM_recruitment_fall$RSSB)
hist(WGOM_recruitment_spring$RSSB)
hist(GBK_recruitment_fall$RSSB)
hist(GBK_recruitment_spring$RSSB)
hist(SNE_recruitment_fall$RSSB)
hist(SNE_recruitment_spring$RSSB)
hist(EGOM_recruitment_fall$lAGE1)
hist(EGOM_recruitment_spring$lAGE1)
hist(WGOM_recruitment_fall$lAGE1)
hist(WGOM_recruitment_spring$lAGE1)
hist(GBK_recruitment_fall$lAGE1)
hist(GBK_recruitment_spring$lAGE1)
hist(SNE_recruitment_fall$lAGE1)
hist(SNE_recruitment_spring$lAGE1)
#pairwise colinearity testing
Mypairs(EGOM_recruitment_fall[c(2:7,9)])
Mypairs(EGOM_recruitment_spring[c(2:7,9)])
Mypairs(WGOM_recruitment_fall[c(2:7,9)])
Mypairs(WGOM_recruitment_spring[c(2:7,9)])
Mypairs(GBK_recruitment_fall[c(2:7,9)])
Mypairs(GBK_recruitment_spring[c(2:7,9)])
Mypairs(SNE_recruitment_fall[c(2:7,9)])
Mypairs(SNE_recruitment_spring[c(2:7,9)])
#VIF testing
corvif(EGOM_recruitment_fall[c(2:7)])
corvif(EGOM_recruitment_fall[c(2,4:7)]) #SST and heatwave

corvif(WGOM_recruitment_fall[c(2:7,9)])
corvif(WGOM_recruitment_spring[c(2,4:7,9)]) #SST and heatwave

corvif(GBK_recruitment_fall[c(2:7,9)]) #all good

corvif(GBK_recruitment_spring[c(2:7,9)])
corvif(GBK_recruitment_spring[c(3:7,9)]) #Bt anomaly

corvif(SNE_recruitment_fall[c(2:7)]) #All good

corvif(SNE_recruitment_spring[c(2:7,9)])
corvif(SNE_recruitment_spring[c(2,4:7,9)]) #SST Anomaly
#### view age 1 timeseries ####
layout(matrix(1:4, ncol=2, byrow=TRUE))
par(mar=c(4.1,4.5,1.5,1), oma=c(1.0,0,1.0,0.1))
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

####View Log R Timeseries#####
layout(matrix(1:4, ncol=2, byrow=TRUE))
par(mar=c(4.1,4.5,1.5,1), oma=c(1.0,0,1.0,0.1))
lineplot_seasonal(springdata=EGOM_recruitment_spring,
                  falldata=EGOM_recruitment_fall,
                  springY=EGOM_recruitment_spring$lAGE1,
                  fallY=EGOM_recruitment_fall$lAGE1,
                  fallX= EGOM_recruitment_fall$Year,
                  springX= EGOM_recruitment_spring$Year,
                  main="Log(Age 1+1): EGOM",
                  ylab="Abundance (numbers/tow)",
                  ylim=c(0,2))
lineplot_seasonal(springdata=WGOM_recruitment_spring,
                  falldata=WGOM_recruitment_fall,
                  springY=WGOM_recruitment_spring$lAGE1,
                  fallY=WGOM_recruitment_fall$lAGE1,
                  fallX= WGOM_recruitment_fall$Year,
                  springX= WGOM_recruitment_spring$Year,
                  main="Log(Age 1+1): WGOM",
                  ylab="Abundance (numbers/tow)",
                  ylim=c(0,2))
lineplot_seasonal(springdata=GBK_recruitment_spring,
                  falldata=GBK_recruitment_fall,
                  springY=GBK_recruitment_spring$lAGE1,
                  fallY=GBK_recruitment_fall$lAGE1,
                  fallX= GBK_recruitment_fall$Year,
                  springX= GBK_recruitment_spring$Year,
                  main="Log(Age 1+1): GBK",
                  ylab="Abundance (numbers/tow)",
                  ylim=c(0,2))
lineplot_seasonal(springdata=SNE_recruitment_spring,
                  falldata=SNE_recruitment_fall,
                  springY=SNE_recruitment_spring$lAGE1,
                  fallY=SNE_recruitment_fall$lAGE1,
                  fallX= SNE_recruitment_fall$Year,
                  springX= SNE_recruitment_spring$Year,
                  main="Log(Age 1+1): SNE",
                  ylab="Abundance (numbers/tow)",
                  ylim=c(0,2))
####View RSSB Timeseries#####
layout(matrix(1:4, ncol=2, byrow=TRUE))
par(mar=c(4.1,4.5,1.5,1), oma=c(1.0,0,1.0,0.1))
lineplot_seasonal(springdata=EGOM_recruitment_spring,
                  falldata=EGOM_recruitment_fall,
                  springY=EGOM_recruitment_spring$RSSB,
                  fallY=EGOM_recruitment_fall$RSSB,
                  fallX= EGOM_recruitment_fall$Year,
                  springX= EGOM_recruitment_spring$Year,
                  main="Log(Age 1): EGOM",
                  ylab="Abundance (numbers/tow)",
                  ylim=c(0,5))
lineplot_seasonal(springdata=WGOM_recruitment_spring,
                  falldata=WGOM_recruitment_fall,
                  springY=WGOM_recruitment_spring$RSSB,
                  fallY=WGOM_recruitment_fall$RSSB,
                  fallX= WGOM_recruitment_fall$Year,
                  springX= WGOM_recruitment_spring$Year,
                  main="Log(Age 1): WGOM",
                  ylab="Abundance (numbers/tow)",
                  ylim=c(0,5))
lineplot_seasonal(springdata=GBK_recruitment_spring,
                  falldata=GBK_recruitment_fall,
                  springY=GBK_recruitment_spring$RSSB,
                  fallY=GBK_recruitment_fall$RSSB,
                  fallX= GBK_recruitment_fall$Year,
                  springX= GBK_recruitment_spring$Year,
                  main="Log(Age 1): GBK",
                  ylab="Abundance (numbers/tow)",
                  ylim=c(0,5))
lineplot_seasonal(springdata=SNE_recruitment_spring,
                  falldata=SNE_recruitment_fall,
                  springY=SNE_recruitment_spring$RSSB,
                  fallY=SNE_recruitment_fall$RSSB,
                  fallX= SNE_recruitment_fall$Year,
                  springX= SNE_recruitment_spring$Year,
                  main="Log(Age 1): SNE",
                  ylab="Abundance (numbers/tow)",
                  ylim=c(0,5))
#############################################
################ GAM FOR-LOOP#####
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
#  predictor_combinations <-predictor_combinations[!grepl("bt_anomaly", predictor_combinations$predictor_combinations)| !grepl("sst_anomaly" ,predictor_combinations$predictor_combinations),]
  
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
####### 
####Testing RSSB models######
###write column names of dependent "target" variables, and independent "predictors" variables will be all column names other than dependent variables, or any other column name you list (I also listed year)
############Tweedie###################
####EGOM SPRING#####
targets <- c("RSSB")
predictors <- colnames(EGOM_recruitment_spring)[!(colnames(EGOM_recruitment_spring) %in% c("Age.1","RSSB", "Year","lAGE1","SSB"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","Heatwave","GSI")

GAM_LOOP_FUN(Edata=EGOM_recruitment_spring,k="k=5",correlated_vars1=correlated_vars[2],correlated_vars2=correlated_vars[3],correlated_vars3=correlated_vars[1],correlated_vars4=correlated_vars[4],correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=tw()",number_vars_in_mod= (length(predictors)-3))
hypergrid_tw<-hypergrid
hypergrid_tw$s.pv<-as.character(hypergrid_tw$s.pv)
hypergrid_tw<-as.data.frame(hypergrid_tw,stringsAsFactors = F)
hypergrid_tw<-hypergrid_tw[ , !names(hypergrid_tw) %in% c("model")]

png("Figures/Model_run_tables/Recruitment_RSSB/EGOM_spring_recruitment_NEFSC.png",height= 23*nrow(hypergrid_tw), width = 150*ncol(hypergrid_tw))
grid.table(hypergrid_tw)
dev.off()
######WGOM SPRING#####
targets <- c("RSSB")
predictors <- colnames(WGOM_recruitment_spring)[!(colnames(WGOM_recruitment_spring) %in% c("Age.1","RSSB", "Year","lAGE1","SSB"))]
correlated_vars<-c("Heatwave","sst_anomaly","bt_anomaly","GSI")

GAM_LOOP_FUN(Edata=WGOM_recruitment_spring,k="k=9",correlated_vars1=correlated_vars[1],correlated_vars2=correlated_vars[2],correlated_vars3=correlated_vars[3],correlated_vars4=correlated_vars[2],correlated_vars5=correlated_vars[3],correlated_vars6=correlated_vars[4],folder_name="recruitment",familyXYZ= "family=tw()",number_vars_in_mod= (length(predictors)-3))
hypergrid_tw<-hypergrid
hypergrid_tw$s.pv<-as.character(hypergrid_tw$s.pv)
hypergrid_tw<-as.data.frame(hypergrid_tw,stringsAsFactors = F)
hypergrid_tw<-hypergrid_tw[ , !names(hypergrid_tw) %in% c("model")]

png("Figures/Model_run_tables/Recruitment_RSSB/WGOM_spring_recruitment_NEFSC.png",height= 23*nrow(hypergrid_tw), width = 160*ncol(hypergrid_tw))
grid.table(hypergrid_tw)
dev.off()
######GBK SPRING#####
targets <- c("RSSB")
predictors <- colnames(GBK_recruitment_spring)[!(colnames(GBK_recruitment_spring) %in% c("Age.1","RSSB", "Year","lAGE1","SSB"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","Heatwave","GSI")

GAM_LOOP_FUN(Edata=GBK_recruitment_spring,k="k=9",correlated_vars1=correlated_vars[1],correlated_vars2=correlated_vars[2],correlated_vars3=correlated_vars[1],correlated_vars4=correlated_vars[3],correlated_vars5=correlated_vars[1],correlated_vars6=correlated_vars[4],folder_name="recruitment",familyXYZ= "family=tw()",number_vars_in_mod= (length(predictors)-3))
hypergrid_tw<-hypergrid
hypergrid_tw$s.pv<-as.character(hypergrid_tw$s.pv)
hypergrid_tw<-as.data.frame(hypergrid_tw,stringsAsFactors = F)
hypergrid_tw<-hypergrid_tw[ , !names(hypergrid_tw) %in% c("model")]

png("Figures/Model_run_tables/Recruitment_RSSB/GBK_spring_recruitment_NEFSC.png",height= 23*nrow(hypergrid_tw), width = 150*ncol(hypergrid_tw))
grid.table(hypergrid_tw)
dev.off()

######SNE SPRING#####
targets <- c("RSSB")
predictors <- colnames(SNE_recruitment_spring)[!(colnames(SNE_recruitment_spring) %in% c("Age.1","RSSB", "Year","lAGE1","SSB"))]
correlated_vars<-c("bt_anomaly","sst_anomaly")

GAM_LOOP_FUN(Edata=SNE_recruitment_spring,k="k=5",correlated_vars1=correlated_vars[1],correlated_vars2=correlated_vars[2],correlated_vars3= "NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=tw()",number_vars_in_mod= (length(predictors)-5))
hypergrid_tw<-hypergrid
hypergrid_tw$s.pv<-as.character(hypergrid_tw$s.pv)
hypergrid_tw<-as.data.frame(hypergrid_tw,stringsAsFactors = F)
hypergrid_tw<-hypergrid_tw[ , !names(hypergrid_tw) %in% c("model")]

png("Figures/Model_run_tables/Recruitment_RSSB/SNE_spring_recruitment_NEFSC.png",height= 24*nrow(hypergrid_tw), width = 138*ncol(hypergrid_tw))
grid.table(hypergrid_tw)
dev.off()

####EGOM FALL#####
#not enough rssb data
targets <- c("RSSB")
predictors <- colnames(EGOM_recruitment_fall)[!(colnames(EGOM_recruitment_fall) %in% c("Age.1","RSSB", "Year","lAGE1","SSB"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","Heatwave")

GAM_LOOP_FUN(Edata=EGOM_recruitment_fall,k="k=4",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3=correlated_vars[2],correlated_vars4=correlated_vars[3],correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=tw()",number_vars_in_mod= (length(predictors)-4))
hypergrid_tw<-hypergrid
hypergrid_tw$s.pv<-as.character(hypergrid_tw$s.pv)
hypergrid_tw<-as.data.frame(hypergrid_tw,stringsAsFactors = F)
hypergrid_tw<-hypergrid_tw[ , !names(hypergrid_tw) %in% c("model")]

png("Figures/Model_run_tables/Recruitment_RSSB/EGOM_fall_recruitment_NEFSC.png",height= 24*nrow(hypergrid_tw), width = 136*ncol(hypergrid_tw))
grid.table(hypergrid_tw)
dev.off()

####WGOM FALL#####
targets <- c("RSSB")
predictors <- colnames(WGOM_recruitment_fall)[!(colnames(WGOM_recruitment_fall) %in% c("Age.1","RSSB", "Year","lAGE1","SSB"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","Heatwave")

GAM_LOOP_FUN(Edata=WGOM_recruitment_fall,k="k=8",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3= correlated_vars[2],correlated_vars4= correlated_vars[3],correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=tw()",number_vars_in_mod= (length(predictors)-3))
hypergrid_tw<-hypergrid
hypergrid_tw$s.pv<-as.character(hypergrid_tw$s.pv)
hypergrid_tw<-as.data.frame(hypergrid_tw,stringsAsFactors = F)
hypergrid_tw<-hypergrid_tw[ , !names(hypergrid_tw) %in% c("model")]

png("Figures/Model_run_tables/Recruitment_RSSB/WGOM_fall_recruitment_NEFSC.png",height= 24*nrow(hypergrid_tw), width = 160*ncol(hypergrid_tw))
grid.table(hypergrid_tw)
dev.off()
####GBK FALL#####
targets <- c("RSSB")
predictors <- colnames(GBK_recruitment_fall)[!(colnames(GBK_recruitment_fall) %in% c("Age.1","RSSB", "Year","lAGE1","SSB"))]
correlated_vars<-c("bt_anomaly","sst_anomaly")

GAM_LOOP_FUN(Edata=GBK_recruitment_fall,k="k=9",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3="NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=tw()",number_vars_in_mod= (length(predictors)-3))
hypergrid_tw<-hypergrid
hypergrid_tw$s.pv<-as.character(hypergrid_tw$s.pv)
hypergrid_tw<-as.data.frame(hypergrid_tw,stringsAsFactors = F)
hypergrid_tw<-hypergrid_tw[ , !names(hypergrid_tw) %in% c("model")]

png("Figures/Model_run_tables/Recruitment_RSSB/GBK_fall_recruitment_NEFSC.png",height= 24*nrow(hypergrid_tw), width = 160*ncol(hypergrid_tw))
grid.table(hypergrid_tw)
dev.off()
####SNE FALL#####
#not enough data
############ Testing Log(AGE1) models#######
############Tweedie###################
####EGOM SPRING#####
targets <- c("lAGE1")
predictors <- colnames(EGOM_recruitment_spring)[!(colnames(EGOM_recruitment_spring) %in% c("Age.1","RSSB", "Year","lAGE1"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","Heatwave","GSI")

GAM_LOOP_FUN(Edata=EGOM_recruitment_spring,k="k=5",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3= correlated_vars[2],correlated_vars4= correlated_vars[3],correlated_vars5= correlated_vars[1],correlated_vars6= correlated_vars[4],folder_name="recruitment",familyXYZ= "family=tw()",number_vars_in_mod= (length(predictors)-4))
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/Recruitment_logR1/EGOM_spring_recruitment_NEFSC.png",height= 23*nrow(hypergrid_gaus), width = 150*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()
######WGOM SPRING#####
targets <- c("lAGE1")
predictors <- colnames(WGOM_recruitment_spring)[!(colnames(WGOM_recruitment_spring) %in% c("Age.1","RSSB", "Year","lAGE1"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","Heatwave","GSI")

GAM_LOOP_FUN(Edata=WGOM_recruitment_spring,k="k=9",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3= correlated_vars[2],correlated_vars4= correlated_vars[3],correlated_vars5= correlated_vars[1],correlated_vars6= correlated_vars[4],folder_name="recruitment",familyXYZ= "family=tw()",number_vars_in_mod= (length(predictors)-4))
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/Recruitment_logR1/WGOM_spring_recruitment_NEFSC.png",height= 23*nrow(hypergrid_gaus), width = 160*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()
######GBK SPRING#####
targets <- c("lAGE1")
predictors <- colnames(GBK_recruitment_spring)[!(colnames(GBK_recruitment_spring) %in% c("Age.1","RSSB", "Year","lAGE1"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","Heatwave","GSI")

GAM_LOOP_FUN(Edata=GBK_recruitment_spring,k="k=9",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3= correlated_vars[1],correlated_vars4= correlated_vars[3],correlated_vars5= correlated_vars[1],correlated_vars6= correlated_vars[4],folder_name="recruitment",familyXYZ= "family=tw()",number_vars_in_mod= (length(predictors)-4))
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/Recruitment_logR1/GBK_spring_recruitment_NEFSC.png",height= 23*nrow(hypergrid_gaus), width = 200*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()

######SNE SPRING#####
targets <- c("lAGE1")
predictors <- colnames(SNE_recruitment_spring)[!(colnames(SNE_recruitment_spring) %in% c("Age.1","RSSB", "Year","lAGE1"))]
correlated_vars<-c("bt_anomaly","sst_anomaly")

GAM_LOOP_FUN(Edata=SNE_recruitment_spring,k="k=5",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3= "NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=tw()",number_vars_in_mod= (length(predictors)-4))
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/Recruitment_logR1/SNE_spring_recruitment_NEFSC.png",height= 23*nrow(hypergrid_gaus), width = 138*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()

####EGOM FALL#####
#not enough data
targets <- c("lAGE1")
predictors <- colnames(EGOM_recruitment_fall)[!(colnames(EGOM_recruitment_fall) %in% c("Age.1","RSSB", "Year","lAGE1"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","Heatwave")

GAM_LOOP_FUN(Edata=EGOM_recruitment_fall,k="k=3",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3=correlated_vars[2],correlated_vars4=correlated_vars[3],correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=tw()",number_vars_in_mod= (length(predictors)-4))
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/Recruitment_logR1/EGOM_fall_recruitment_NEFSC.png",height= 24*nrow(hypergrid_gaus), width = 160*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()

####WGOM FALL#####
targets <- c("lAGE1")
predictors <- colnames(WGOM_recruitment_fall)[!(colnames(WGOM_recruitment_fall) %in% c("Age.1","RSSB", "Year","lAGE1"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","Heatwave")

GAM_LOOP_FUN(Edata=WGOM_recruitment_fall,k="k=9",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3=correlated_vars[2],correlated_vars4=correlated_vars[3],correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=tw()",number_vars_in_mod= (length(predictors)-4))
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/Recruitment_logR1/WGOM_fall_recruitment_NEFSC.png",height= 24*nrow(hypergrid_gaus), width = 170*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()
####GBK FALL#####
targets <- c("lAGE1")
predictors <- colnames(GBK_recruitment_fall)[!(colnames(GBK_recruitment_fall) %in% c("Age.1","RSSB","Year","lAGE1"))]
correlated_vars<-c("bt_anomaly","sst_anomaly")

GAM_LOOP_FUN(Edata=GBK_recruitment_fall,k="k=9",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3="NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=tw()",number_vars_in_mod= (length(predictors)-4))
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[,!names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/Recruitment_logR1/GBK_fall_recruitment_NEFSC.png",height= 24*nrow(hypergrid_gaus), width = 170*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()

####SNE FALL#####
#not enough data

############# PLOT SIGNIFICANT GAM CURVES #######################
##### log(R) (SPRING) vs. potential environmental influences###########
##### EGOM ####
egomLR<-gam(lAGE1 ~ s(sst_anomaly,k=10)+s(calfin_100m3,k=10), family=tw(),method = "REML",data=EGOM_recruitment_spring)
summary(egomLR)
egomLR$aic

png("Figures/residual_plots/recruitment/EGOM_spring_logR_NEFSC.png",width =449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(egomLR,pch=20, cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/recruitment/logR1/EGOM_spring_logR_NEFSC.png",width =898, height =374.5, units = "px")
layout(matrix(1:2, ncol=2, byrow=TRUE))
GAM_CURVE_FUN(egomLR,EGOM_recruitment_spring$sst_anomaly,x_lab="SST Anomaly (C)",y_lab="PE on Log Recrutiment",select1=1,data_Year = EGOM_recruitment_spring$Year,position = "bottomleft",title="EGOM Spring")
GAM_CURVE_FUN(egomLR,EGOM_recruitment_spring$calfin_100m3,x_lab="Calanus Abundance (/100m3)",y_lab="PE on Log Recrutiment",select1=2,data_Year = EGOM_recruitment_spring$Year,position = "bottomleft",title=NULL)
dev.off()
##### WGOM ####

wgomLR<-gam(lAGE1 ~ s(Heatwave,k=10), family=tw(),method = "REML",data=WGOM_recruitment_spring)
summary(wgomLR)
wgomLR$aic
png("Figures/residual_plots/recruitment/WGOM_spring_logR_NEFSC.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomLR,pch=20, cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/recruitment/logR1/WGOM_spring_logR_NEFSC.png",width = 449, height = 374.5, units = "px")
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN(wgomLR,WGOM_recruitment_spring$Heatwave,x_lab="Cumulative Heatwave (C)",y_lab="PE on Log Recrutiment",select1=1,data_Year = WGOM_recruitment_spring$Year,position = "bottomleft",title="WGOM Spring")
dev.off()
##### GBK  ####
#nothing
##### SNE  ####
sneLR<-gam(lAGE1 ~ s(calfin_100m3,k=10), family=tw(),method = "REML",data=SNE_recruitment_spring)
summary(sneLR)
sneLR$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(sneLR,pch=20, cex=1,cex.lab=1.3) #bad residuals

##### log(R) (FALL) vs. potential environmental influences###########
##### EGOM ####
egomLR<-gam(lAGE1 ~ s(sst_anomaly,k=10), family=tw(),method = "REML",data=EGOM_recruitment_fall)
summary(egomLR)
egomLR$aic

png("Figures/residual_plots/recruitment/EGOM_fall_logR_NEFSC.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(egomLR,pch=20, cex=1,cex.lab=1.3) #Residuals not good
dev.off()

png("Figures/GAM_curves/recruitment/logR1/EGOM_fall_logR_NEFSC.png",width = 449, height = 374.5, units = "px")
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN(egomLR,EGOM_recruitment_fall$sst_anomaly,x_lab="SST Anomaly (Deg C)",y_lab="PE on Log Recruitment",select1=1,data_Year = EGOM_recruitment_fall$Year,position = "bottomleft",title="EGOM Fall")
dev.off()
##### WGOM ####
wgomLR<-gam(lAGE1 ~ s(bt_anomaly,k=10), family=tw(),method = "REML",data=WGOM_recruitment_fall)
summary(wgomLR)
wgomLR$aic

png("Figures/residual_plots/recruitment/WGOM_fall_logR_NEFSC.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomLR,pch=20,cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/recruitment/logR1/WGOM_fall_logR_NEFSC.png",width =447, height =374.5, units = "px")
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN(wgomLR,WGOM_recruitment_fall$bt_anomaly,x_lab="BT Anomaly (Deg C)",y_lab="PE on Log Recruitment",select1=1,data_Year = WGOM_recruitment_fall$Year,position = "bottomleft",title="WGOM Fall")
dev.off()
##### GBK  ####
gbkLR<-gam(lAGE1 ~ s(SSB,k=10), family=tw(),method = "REML",data=GBK_recruitment_fall)
summary(gbkLR)
gbkLR$aic

png("Figures/residual_plots/recruitment/GBK_fall_logR_NEFSC.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(gbkLR,pch=20,cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/recruitment/logR1/GBK_fall_logR_NEFSC.png",width =447, height =374.5, units = "px")
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN(gbkLR,GBK_recruitment_fall$SSB,x_lab="SSB (kg/tow)",y_lab="PE on Log Recruitment",select1=1,data_Year = GBK_recruitment_fall$Year,position = "bottomleft",title="GBK Fall")
dev.off()
##### SNE  ####
#nothing significant

##### RSSB (SPRING) vs. potential environmental influences###########
##### EGOM ####
egomRSSB<-gam(RSSB ~ s(sst_anomaly,k=10), family=tw(),method = "REML",data=EGOM_recruitment_spring)
summary(egomRSSB)
egomRSSB$aic

png("Figures/residual_plots/recruitment/EGOM_spring_RSSB.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(egomRSSB,pch=20,cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/recruitment/RSSB/EGOM_spring_RSSB_NEFSC.png",width =449, height =374.5, units = "px")
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN(egomRSSB,EGOM_recruitment_spring$sst_anomaly,x_lab="SST Anomaly (Deg C)",y_lab="PE on RSSB",select1=1,data_Year = EGOM_recruitment_spring$Year,position = "topleft",title="EGOM Spring")
dev.off()
##### WGOM ####
wgomRSSB<-gam(RSSB ~ s(bt_anomaly,k=10), family=tw(),method = "REML",data=WGOM_recruitment_spring)
summary(wgomRSSB)
wgomRSSB$aic

png("Figures/residual_plots/recruitment/WGOM_spring_RSSB.png",width=449, height=374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomRSSB,pch=20, cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/recruitment/RSSB/WGOM_spring_RSSB_NEFSC.png",width = 449, height = 374.5, units = "px")
par(mar=c(4.5,4.5,0.6,1))
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN(wgomRSSB,WGOM_recruitment_spring$bt_anomaly,x_lab="Bt Anomaly (Deg C)",y_lab="PE on R/SSB",select1=1,data_Year = WGOM_recruitment_spring$Year,position = "bottomleft",title="WGOM Spring")
dev.off()
##### GBK  ####
##Nothing
##### SNE  ####
##Nothing
##### RSSB (FALL) vs. potential environmental influences###########
##### EGOM ####
#nothing significant
##### WGOM ####
wgomRSSB<-gam(RSSB ~ s(sst_anomaly,k=10), family=tw(),method = "REML",data=WGOM_recruitment_fall)
summary(wgomRSSB)
wgomRSSB$aic

png("Figures/residual_plots/recruitment/WGOM_fall_RSSB.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomRSSB,pch=20, cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/recruitment/RSSB/WGOM_fall_RSSB_NEFSC.png",width = 449, height = 374.5, units = "px")
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN(wgomRSSB,WGOM_recruitment_fall$sst_anomaly,x_lab="SST Anomaly (Deg C)",y_lab="PE on RSSB",select1=1,data_Year = WGOM_recruitment_fall$Year,position = "bottomleft",title="WGOM Fall")
dev.off()
##### GBK  ####
#nothing significant
##### SNE  ####
##Nothing