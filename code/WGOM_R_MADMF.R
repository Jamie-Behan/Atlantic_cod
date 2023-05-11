library(pacman)
pacman::p_load(here, readxl,lubridate,stats,graphics,Hmisc,data.table,utils,mgcv,dplyr,purrr,ecodata,kableExtra,gridExtra) 
#### load WGOM files that conatin environmental data:
#nm <- list.files(path =here("data/final_env_data/recruitment"), pattern = ".csv", full.names = TRUE)
#nm2 <- list.files(path =here("data/final_env_data/recruitment"), pattern = ".csv", full.names =FALSE)
#list2env(lapply(setNames(nm, make.names(gsub("*.csv$", "",nm2))),read.csv),envir=.GlobalEnv)
#rm(nm,nm2,EGOM_recruitment_fall,EGOM_recruitment_spring,GBK_recruitment_fall,GBK_recruitment_spring,SNE_recruitment_fall,SNE_recruitment_spring)
source(here("Code/Envdata2_MADMF.R")) #data with env data specific to start of MADMF age 0 recruitment
####load MADMF data####
MADMFdata <- read_excel(here("data/final_env_data/recruitment/MADFM_Inshore_data_template_2022_05_10.xlsx"),sheet = "Survey Data")
names(MADMFdata)[5]<-"Year"
####clean winter spawner Data####
MADMF_may<-subset(MADMFdata, SEASON == 'SPRING' & STOCK == 'WGOM') #subset spring and WGOM
MADMF_may<-MADMF_may[,c(5,15)] #subset to only Year and N age 0 columns
MADMF_may<-aggregate(NCOD_A0 ~ Year, data=MADMF_may, mean) #aggregate by year, mean number of age0 by year
plot(MADMF_may$NCOD_A0~MADMF_may$Year, type="l") #plot for quick look at timeseries
####clean spring spawner Data####
MADMF_sep<-subset(MADMFdata, SEASON == 'FALL' & STOCK == 'WGOM') #subset fall and WGOM
MADMF_sep<-MADMF_sep[,c(5,15)] #subset to only Year and N age 0 columns
MADMF_sep<-aggregate(NCOD_A0 ~ Year, data=MADMF_sep, mean) #aggregate by year, mean number of age0 by year
plot(MADMF_sep$NCOD_A0~MADMF_sep$Year, type="l") #plot for quick look at timeseries
#### create lAGE1 column#######
MADMF_may$lAGE1<-(MADMF_may$NCOD_A0)
MADMF_sep$lAGE1<-(MADMF_sep$NCOD_A0)
#add +1 to all cells to combat cells with 0 values (only the MADMF_sep df contains 0s but doing it to all dfs for consistency)
MADMF_may$lAGE1<-MADMF_may[,"lAGE1"]+1
MADMF_sep$lAGE1<-MADMF_sep[,"lAGE1"]+1
MADMF_may$lAGE1<-log(MADMF_may$lAGE1)
MADMF_sep$lAGE1<-log(MADMF_sep$lAGE1)

plot(MADMF_may$lAGE1~MADMF_may$Year, type="l") #plot for quick look at timeseries
plot(MADMF_sep$lAGE1~MADMF_sep$Year, type="l") #plot for quick look at timeseries
#### view age 1 timeseries ####
lineplot_seasonal<- function (springdata, falldata, springY, fallY,springX,fallX,main,ylab,ylim){
  
  plot(fallY ~fallX, data=falldata, main=main, xlab="Year",ylab=ylab, type="l",lwd=3,col="#00608A",ylim= ylim, cex.lab=1.4,cex.axis=1.1)
  #abline(lm(fallY~fallX),col="#00608A",lwd=2.5,lty=2)
  
  lines(springY ~springX, data=springdata, xlab="Year", type="l",col="#EA4F12",lwd=3)
  #abline(lm(springY~springX),col="#EA4F12",lwd=2.5,lty=2)
  legend("topleft",inset=c(0.03,0.03), legend=c("Spring", "Fall"), col=c("#EA4F12", "#00608A"), lty=1,lwd=3, cex=1.0)
}
layout(matrix(1:1, ncol=1, byrow=TRUE))
par(mar=c(4.1,4.5,1.5,1), oma=c(1.0,0,1.0,0.1))
lineplot_seasonal(springdata=MADMF_may,
                  falldata=MADMF_sep,
                  springY=MADMF_may$NCOD_A0,
                  fallY=MADMF_sep$NCOD_A0,
                  fallX= MADMF_sep$Year,
                  springX= MADMF_may$Year,
                  main="MADMF Trawl Survey Mean Numbers at Age 0: WGOM",
                  ylab="Mean Abundance (numbers/tow)",
                  ylim=c(0,750))

### add env data to MADMF dfs ####
MADMF_may<-merge(MADMF_may,WGOM_recruitment_spring[1:7],by="Year")
MADMF_sep<-merge(MADMF_sep,WGOM_recruitment_fall[1:7],by="Year")
##### examining data ######
source(here("Code/Gam_data_exploration.R"))
#put dfs in list to apply across functions
df.list <- list(MADMF_may,MADMF_sep)

#apply functions
lapply(df.list, dotchart_fun_10)
lapply(df.list, hist_fun10)
lapply(df.list, view_boxplot_fun10)
lapply(df.list, shapiro_fun)
#histograms of dependent data
hist(MADMF_may$lAGE1)
hist(MADMF_sep$lAGE1) #still zero inflated
hist(MADMF_all$lAGE1)

#pairwise colinearity testing
Mypairs(MADMF_may[c(4:9)]) #bt&sst, sst and heatwave,calfin and pseudo
Mypairs(MADMF_sep[c(4:9)]) #bt and heatwave, bt and GSI

#VIF testing
corvif(MADMF_may[c(4:9)])
corvif(MADMF_may[c(4:8)]) #calfin and pseudo
corvif(MADMF_may[c(4,5,7:9)])

corvif(MADMF_sep[c(4:9)])
corvif(MADMF_sep[c(5:9)]) #bt

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
######WGOM SPRING (MADMF_may lAGE0)#####
targets <- c("lAGE1")
predictors <- colnames(MADMF_may)[!(colnames(MADMF_may) %in% c("NCOD_A0","Year","lAGE1"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","Heatwave","calfin_100m3","pseudo_100m3")

GAM_LOOP_FUN(Edata=MADMF_may,k="k=7",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3= correlated_vars[2],correlated_vars4= correlated_vars[3],correlated_vars5=correlated_vars[4],correlated_vars6=correlated_vars[5],folder_name="recruitment",familyXYZ= "family=gaussian()",number_vars_in_mod= (length(predictors)-3))
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/Recruitment_logR1/WGOM_MADMF_may_lage0.png",height= 23*nrow(hypergrid_gaus), width = 160*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()
######WGOM SPRING (MADMF_may, NCOD_A0)#####
targets <- c("NCOD_A0")
predictors <- colnames(MADMF_may)[!(colnames(MADMF_may) %in% c("NCOD_A0","Year","lAGE1"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","Heatwave","calfin_100m3","pseudo_100m3")

GAM_LOOP_FUN(Edata=MADMF_may,k="k=7",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3= correlated_vars[2],correlated_vars4= correlated_vars[3],correlated_vars5=correlated_vars[4],correlated_vars6=correlated_vars[5],folder_name="recruitment",familyXYZ= "family=tw()",number_vars_in_mod= (length(predictors)-3))
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/Recruitment_logR1/WGOM_MADMF_may_R.png",height= 23*nrow(hypergrid_gaus), width = 160*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()

######WGOM FALL (MADMF_sep lAGE0 and NCOD_A0 bc theyre both zero inflated, so both use tw distribution)#####
targets <- c("lAGE1","NCOD_A0")
predictors <- colnames(MADMF_sep)[!(colnames(MADMF_sep) %in% c("NCOD_A0","Year","lAGE1"))]
correlated_vars<-c("bt_anomaly","Heatwave","GSI")

GAM_LOOP_FUN(Edata=MADMF_sep,k="k=7",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3=correlated_vars[1],correlated_vars4=correlated_vars[3],correlated_vars5="NA",correlated_vars6="NA",folder_name="recruitment",familyXYZ= "family=tw()",number_vars_in_mod= (length(predictors)-2))
hypergrid_gaus<-hypergrid
hypergrid_gaus$s.pv<-as.character(hypergrid_gaus$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid_gaus,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]

png("Figures/Model_run_tables/Recruitment_logR1/WGOM_MADMF_sep.png",height= 23*nrow(hypergrid_gaus), width = 160*ncol(hypergrid_gaus))
grid.table(hypergrid_gaus)
dev.off()

#### Plot Significant GAM Curves ####
##### WGOM SPRING (MADMF_may lAGE0)####
wgomLR<-gam(lAGE1 ~ s(calfin_100m3,k=10), family=gaussian(),method = "REML",data=MADMF_may)
summary(wgomLR)
wgomLR$aic
png("Figures/residual_plots/recruitment/WGOM_MADMF_may_lage0.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomLR,pch=20, cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/recruitment/logR1/WGOM_MADMF_may_lage0.png",width = 449, height = 374.5, units = "px")
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN(wgomLR,MADMF_may$calfin_100m3,x_lab="Calanus Abundance Anomaly (100m3)",y_lab="PE on log Recrutiment",select1=1,data_Year = MADMF_may$Year,position = "topleft",title="WGOM Spring")
dev.off()
##### WGOM SPRING (MADMF_may R)####
wgomR<-gam(NCOD_A0 ~ s(sst_anomaly,k=7)+s(calfin_100m3,k=7), family=tw(),method = "REML",data=MADMF_may)
summary(wgomR)
wgomR$aic
png("Figures/residual_plots/recruitment/WGOM_MADMF_may_age0.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomR,pch=20, cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/recruitment/logR1/WGOM_MADMF_may_age0.png",width = 898, height = 374.5, units = "px",res=90)
layout(matrix(1:2, ncol=2, byrow=TRUE))
GAM_CURVE_FUN(wgomR,MADMF_may$sst_anomaly,x_lab="SST Anomaly (C)",y_lab="PE on Recrutiment (untransformed)",select1=1,data_Year = MADMF_may$Year,position = "bottomleft",title="WGOM Spring")
#GAM_CURVE_FUN(wgomR,MADMF_may$Heatwave,x_lab="Cumulative Heatwave (C)",y_lab="PE on Recrutiment (untransformed)",select1=2,data_Year = MADMF_may$Year,position = "topleft",title="WGOM Spring")
GAM_CURVE_FUN(wgomR,MADMF_may$calfin_100m3,x_lab="Calanus Abundance Anomaly (100m3)",y_lab="PE on Recrutiment (untransformed)",select1=2,data_Year = MADMF_may$Year,position = "topleft",title="WGOM Spring")
dev.off()

##### WGOM FALL (MADMF_sep lAGE0)####
wgomLR<-gam(lAGE1 ~ s(bt_anomaly,k=10), family=tw(),method = "REML",data=MADMF_sep)
summary(wgomLR)
wgomLR$aic
png("Figures/residual_plots/recruitment/WGOM_MADMF_sep_lage0.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomLR,pch=20, cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/recruitment/logR1/WGOM_MADMF_sep_lage0.png",width = 449, height = 374.5, units = "px")
layout(matrix(1:1, ncol=1, byrow=TRUE))
GAM_CURVE_FUN(wgomLR,MADMF_sep$bt_anomaly,x_lab="Bt Anomaly (C)",y_lab="PE on log Recrutiment",select1=1,data_Year = MADMF_sep$Year,position = "bottomleft",title="WGOM FALL")
dev.off()
##### WGOM FALL (MADMF_sep R)####
wgomR<-gam(NCOD_A0 ~ s(Heatwave,k=10)+s(GSI,k=10), family=tw(),method = "REML",data=MADMF_sep)
summary(wgomR)
wgomR$aic
png("Figures/residual_plots/recruitment/WGOM_MADMF_sep_age0.png",width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(wgomR,pch=20, cex=1,cex.lab=1.3)
dev.off()

png("Figures/GAM_curves/recruitment/logR1/WGOM_MADMF_sep_age0.png",width = 898, height =374.5, units = "px",res=90)
layout(matrix(1:2, ncol=2, byrow=TRUE))
GAM_CURVE_FUN(wgomR,MADMF_sep$Heatwave,x_lab="Cumulative Heatwave (C)",y_lab="PE on Recrutiment",select1=1,data_Year = MADMF_sep$Year,position = "bottomleft",title="WGOM Fall")
GAM_CURVE_FUN(wgomR,MADMF_sep$GSI,x_lab="GSI (Deg Lat)",y_lab="PE on Recrutiment",select1=2,data_Year = MADMF_sep$Year,position = "bottomleft",title="WGOM Fall")
dev.off()
