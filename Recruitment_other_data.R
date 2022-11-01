library(here)
source(here("Code/Env_data_by_stock.R"))
#### load new dfs#####
Cod_NAA<-read.csv(here("data/cod_NAA.csv"))
Cod_NAA<-Cod_NAA[,c(2:3,7,10:15,17:18)]
MENH_WGOM = Cod_NAA[Cod_NAA$SURVEY == "MENH_WGOM",]
MENH_WGOM = MENH_WGOM[!MENH_WGOM$YEAR  < 1982,]
MENH_WGOM = MENH_WGOM[!MENH_WGOM$YEAR  > 2019,]
codage1<-MENH_WGOM[,c(1:3)]
names(codage1)[1]<-"Year"
MENH_WGOM<-MENH_WGOM[,c(1:2,4:11)]
SSB_Fall_WGOM_MENH<-read.csv(here("data/SSB_estimates/SSB_Fall_WGOM_MENH.csv"))
names(SSB_Fall_WGOM_MENH)[1]<-"Year"
SSB_Spring_WGOM_MENH<-read.csv(here("data/SSB_estimates/SSB_Spring_WGOM_MENH.csv"))
names(SSB_Spring_WGOM_MENH)[1]<-"Year"
#### remove other stock areas for now####
rm(EGOM_recruitment_fall,EGOM_recruitment_spring,GBK_recruitment_fall,GBK_recruitment_spring,SNE_recruitment_fall,SNE_recruitment_spring,Cod_NAA)
#### keep only anomaly temperature columns####
WGOM_recruitment_fall<-WGOM_recruitment_fall[c(1:2,5,7,9:10)]
WGOM_recruitment_spring<-WGOM_recruitment_spring[c(1:2,5,7,9:10)]
#### replace Age.1 and SSB columns #######
###add ssb column
WGOM_recruitment_fall<-merge(WGOM_recruitment_fall,SSB_Fall_WGOM_MENH[SSB_Fall_WGOM_MENH$SEASON == "FALL",c(1,3)],by="Year",all = TRUE)
WGOM_recruitment_spring<-merge(WGOM_recruitment_spring,SSB_Spring_WGOM_MENH[SSB_Spring_WGOM_MENH$SEASON == "SPRING",c(1,3)],by="Year",all = TRUE)

###add Age.1 column
WGOM_recruitment_fall<-merge(WGOM_recruitment_fall,codage1[codage1$SEASON == "FALL",c(1,3)],by="Year",all = TRUE)
WGOM_recruitment_spring<-merge(WGOM_recruitment_spring,codage1[codage1$SEASON == "SPRING",c(1,3)],by="Year",all = TRUE)

##### Create R/ssb column####

WGOM_recruitment_fall$RSSB<-WGOM_recruitment_fall$Age.1/WGOM_recruitment_fall$SSB
WGOM_recruitment_spring$RSSB<-WGOM_recruitment_spring$Age.1/WGOM_recruitment_spring$SSB

#### create lAGE1 column#######

WGOM_recruitment_fall$lAGE1<-(WGOM_recruitment_fall$Age.1)
WGOM_recruitment_spring$lAGE1<-(WGOM_recruitment_spring$Age.1)

#add small positive value to all cells to combat cells with 0 values
#EGOM_recruitment_fall$lAGE1<-EGOM_recruitment_fall[,"lAGE1"]+0.00001

WGOM_recruitment_fall$lAGE1<-WGOM_recruitment_fall[,"lAGE1"]+1
WGOM_recruitment_spring$lAGE1<-WGOM_recruitment_spring[,"lAGE1"]+1

WGOM_recruitment_fall$lAGE1<-log(WGOM_recruitment_fall$lAGE1)
WGOM_recruitment_spring$lAGE1<-log(WGOM_recruitment_spring$lAGE1)

#### remove season columns #####
WGOM_recruitment_fall<-within(WGOM_recruitment_fall,rm("SEASON"))
WGOM_recruitment_spring<-within(WGOM_recruitment_spring,rm("SEASON"))
#### remove data before 2000#####
WGOM_recruitment_fall<-WGOM_recruitment_fall[!WGOM_recruitment_fall$Year  < 2000,]
WGOM_recruitment_spring<-WGOM_recruitment_spring[!WGOM_recruitment_spring$Year  < 2000,]
##### replace inf value cells with NA
WGOM_recruitment_fall[WGOM_recruitment_fall== Inf] <- NA
WGOM_recruitment_spring[WGOM_recruitment_spring== Inf] <- NA
##### Start here ######
source(here("Code/Gam_data_exploration.R"))
#put dfs in list to apply across functions
df.list <- list(WGOM_recruitment_fall,WGOM_recruitment_spring)

#apply functions
lapply(df.list, dotchart_fun_10)
lapply(df.list, hist_fun10)
lapply(df.list, view_boxplot_fun10)
lapply(df.list, shapiro_fun)
lapply(df.list,Mypairs)
#####Line plot spring/fall together on same plot#####
lineplot_seasonal<- function (springdata, falldata, springY, fallY,springX,fallX,main,ylab,ylim,xlim){
  
  plot(fallY ~fallX, data=falldata, main=main, xlab="Year",ylab=ylab, type="l",lwd=3,col="#00608A",ylim= ylim,xlim=xlim, cex.lab=1.4,cex.axis=1.1)
  abline(lm(fallY~fallX),col="#00608A",lwd=2.5,lty=2)
  
  lines(springY ~springX, data=springdata, xlab="Year", type="l",col="#EA4F12",lwd=3)
  abline(lm(springY~springX),col="#EA4F12",lwd=2.5,lty=2)
  legend("topleft",inset=c(0.03,0.03), legend=c("Spring", "Fall"), col=c("#EA4F12", "#00608A"), lty=1,lwd=3, cex=1.0)
}
#### view age 1 timeseries ####
layout(matrix(1:4, ncol=2, byrow=TRUE))
par(mar=c(4.1,4.5,1.5,1), oma=c(1.0,0,1.0,0.1))
lineplot_seasonal(springdata=WGOM_recruitment_spring,
                  falldata=WGOM_recruitment_fall,
                  springY=WGOM_recruitment_spring$Age.1,
                  fallY=WGOM_recruitment_fall$Age.1,
                  fallX= WGOM_recruitment_fall$Year,
                  springX= WGOM_recruitment_spring$Year,
                  main="MENH Trawl Survey Numbers at Age 1: WGOM",
                  ylab="Abundance (numbers/tow)",
                  ylim=c(0,3.9),
                  xlim = c(2000,2019))
lineplot_seasonal(springdata=WGOM_recruitment_spring,
                  falldata=WGOM_recruitment_fall,
                  springY=WGOM_recruitment_spring$lAGE1,
                  fallY=WGOM_recruitment_fall$lAGE1,
                  fallX= WGOM_recruitment_fall$Year,
                  springX= WGOM_recruitment_spring$Year,
                  main="MENH Log(Age 1+1): WGOM",
                  ylab="Abundance (numbers/tow)",
                  ylim=c(0,2),
                  xlim = c(2000,2019))
lineplot_seasonal(springdata=WGOM_recruitment_spring,
                  falldata=WGOM_recruitment_fall,
                  springY=WGOM_recruitment_spring$RSSB,
                  fallY=WGOM_recruitment_fall$RSSB,
                  fallX= WGOM_recruitment_fall$Year,
                  springX= WGOM_recruitment_spring$Year,
                  main="MENH R/SSB: WGOM",
                  ylab="Abundance (numbers/tow)",
                  ylim=c(0,5),
                  xlim = c(2000,2019))
################ GAM FOR-LOOP#####
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
####Testing RSSB models######
###write column names of dependent "target" variables, and independent "predictors" variables will be all column names other than dependent variables, or any other column name you list (I also listed year)
############Tweedie###################
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

png("Figures/Model_run_tables/WGOM_spring_recruitment_MENH.png",height= 23*nrow(hypergrid_tw), width = 138*ncol(hypergrid_tw))
grid.table(hypergrid_tw)
dev.off()
####WGOM FALL#####
targets <- c("RSSB")
predictors <- colnames(WGOM_recruitment_fall)[!(colnames(WGOM_recruitment_fall) %in% c("Age.1","RSSB", "Year","SEASON","lAGE1"))]
correlated_vars<-c("bt_anomaly","sst_anomaly")

GAM_LOOP_FUN(Edata=WGOM_recruitment_fall,k="k=4",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3="NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=tw()")
hypergrid_tw<-hypergrid
hypergrid_tw$s.pv<-as.character(hypergrid_tw$s.pv)
hypergrid_tw<-as.data.frame(hypergrid_tw,stringsAsFactors = F)
hypergrid_tw<-hypergrid_tw[ , !names(hypergrid_tw) %in% c("model")]

png("Figures/Model_run_tables/WGOM_fall_recruitment_MENH.png",height= 24*nrow(hypergrid_tw), width = 138*ncol(hypergrid_tw))
grid.table(hypergrid_tw)
dev.off()
############ Testing Log(AGE1) models#######
############Tweedie###################
######WGOM SPRING#####
targets <- c("lAGE1")
predictors <- colnames(WGOM_recruitment_spring)[!(colnames(WGOM_recruitment_spring) %in% c("Age.1","RSSB", "Year","SEASON","lAGE1"))]
correlated_vars<-c("bt_anomaly","sst_anomaly")

GAM_LOOP_FUN(Edata=WGOM_recruitment_spring,k="k=5",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3= "NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=tw()")
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/WGOM_spring_recruitment_MENH_logr.png",height= 23*nrow(hypergrid_gaus), width = 138*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()
####WGOM FALL#####
targets <- c("lAGE1")
predictors <- colnames(WGOM_recruitment_fall)[!(colnames(WGOM_recruitment_fall) %in% c("Age.1","RSSB", "Year","SEASON","lAGE1"))]
correlated_vars<-c("bt_anomaly","sst_anomaly")

GAM_LOOP_FUN(Edata=WGOM_recruitment_fall,k="k=5",correlated_vars1= correlated_vars[2],correlated_vars2= correlated_vars[1],correlated_vars3="NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=tw()")
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/WGOM_fall_recruitment_MENH_logr.png",height= 24*nrow(hypergrid_gaus), width = 138*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()
############# PLOT SIGNIFICANT GAM CURVES #######################
##### log(R) (SPRING) vs. potential environmental influences###########
##### WGOM ####
wgomLR<-gam(lAGE1 ~ s(Avg_GSI,k=10)+s(SSB,k=10), family=tw(),method = "REML",data=WGOM_recruitment_spring)
summary(wgomLR)
wgomLR$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomLR,pch=20, cex=1.2,cex.lab=1.5)

png("Figures/GAM_curves/recruitment/WGOM_spring_logR_MENH.png",width = 898, height = 374.5, units = "px")
par(mar=c(4.5,4.5,0.6,1))
layout(matrix(1:2, ncol=2, byrow=TRUE))
GAM_CURVE_FUN_spring(wgomLR,WGOM_recruitment_spring$Avg_GSI,x_lab="Mean GSI (Deg Lat)",y_lab="PE on Log Recrutiment",select1=1)
GAM_CURVE_FUN_spring(wgomLR,WGOM_recruitment_spring$SSB,x_lab="Mean SSB (Kg/Tow)",y_lab="PE on Log Recrutiment",select1=2)
dev.off()
##### log(R) (FALL) vs. potential environmental influences###########
##### WGOM ####
wgomLR<-gam(lAGE1 ~ s(sst_anomaly,k=10)+s(SSB,k=10), family=tw(),method = "REML",data=WGOM_recruitment_fall)
summary(wgomLR)
wgomLR$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomLR,pch=20, cex=1.2,cex.lab=1.5)

png("Figures/GAM_curves/recruitment/WGOM_fall_logR_MENH.png",width = 898, height = 374.5, units = "px")
par(mar=c(4.5,4.5,0.6,1))
layout(matrix(1:2, ncol=2, byrow=TRUE))
GAM_CURVE_FUN_fall(wgomLR,WGOM_recruitment_fall$sst_anomaly,x_lab="SST Anomaly (Deg C)",y_lab="PE on Log Recruitment",select1=1)
GAM_CURVE_FUN_fall(wgomLR,WGOM_recruitment_fall$SSB,x_lab="Mean SSB (Kg/Tow)",y_lab="PE on Log Recruitment",select1=2)
dev.off()
##### RSSB (SPRING) vs. potential environmental influences###########
##### WGOM ####
wgomRSSB<-gam(RSSB ~ s(bt_anomaly,k=8)+s(SSB,k=8), family=tw(),method = "REML",data=WGOM_recruitment_spring)
summary(wgomRSSB)
wgomRSSB$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomRSSB,pch=20, cex=1.2,cex.lab=1.5)

png("Figures/GAM_curves/recruitment/WGOM_spring_RSSB_MENH.png",width = 898, height = 374.5, units = "px")
par(mar=c(4.5,4.5,0.6,1))
layout(matrix(1:2, ncol=2, byrow=TRUE))
GAM_CURVE_FUN_spring(wgomRSSB,WGOM_recruitment_spring$bt_anomaly,x_lab="Bt Anomaly (Deg C)",y_lab="PE on Log Recrutiment",select1=1)
GAM_CURVE_FUN_spring(wgomRSSB,WGOM_recruitment_spring$SSB,x_lab="Mean SSB (Kg/Tow)",y_lab="PE on Log Recrutiment",select1=2)
dev.off()
##### RSSB (FALL) vs. potential environmental influences###########
##### WGOM ####
wgomRSSB<-gam(RSSB ~ s(Avg_GSI,k=10), family=tw(),method = "REML",data=WGOM_recruitment_fall)
summary(wgomRSSB)
wgomRSSB$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomRSSB,pch=20, cex=1.2,cex.lab=1.5)

png("Figures/GAM_curves/recruitment/WGOM_fall_RSSB_MENH.png",width = 449, height = 374.5, units = "px")
par(mar=c(4.5,4.5,0.6,1))
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN_fall(wgomRSSB,WGOM_recruitment_spring$Avg_GSI,x_lab="Mean GSI",y_lab="PE on RSSB",select1=1)
dev.off()
