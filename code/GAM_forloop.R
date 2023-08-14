### GAM Loop Function ####
library(pacman)
pacman::p_load(here, readxl,lubridate,stats,graphics,Hmisc,data.table,utils,mgcv,dplyr,purrr,ecodata,kableExtra,gridExtra,DataExplorer) 


####Notes (READ ME:)####
#targets <- c("Name_of_dependent_var_column")  #target (dependent variable column name in df). can write more than one, separated by commas
#predictors <- colnames(df_containing_cols_ofall_vars)[!(colnames(df_containing_cols_ofall_vars) %in% c("Name_of_dependent_var_column", "Year"))] #Predictors = independent variables of interest. Writes all names except those specified in "%in% c("Name_of_dependent_var_column", "Year")" section
#correlated_vars<-c("ind_var1","ind_var2","ind_var3",...) #list which variables have correlations/ you don't want them run in the same model iteration (which ones can't be run together will be specified in function parameters, this is just a list of all column names applicable)

## Description of parameters
#' @param  Edata The Data going into the model; data frame containing both the independent and dependent data, each variable in a separate column by year as rows
#' @param  k    number of knots (k) in GAM. e.g parameter defined as k="k=10" with quotes
#' @param  correlated_vars (1-6) After running correlation/VIF tests, this parameter is used to identify which variables should not be run in any model iteration together. 
#' e.g correlated_vars1=correlated_vars[1],correlated_vars2=correlated_vars[4] would signify that the first and 4th independent variables listed in correlated_vars 
#' list would not both be included together as covariates in any model iteration. Currently there are place holders for up to 3 pairs of correlated variables to exclude (1-2, 3-4, 5-6),
#' more can be added into source code, but if less are needed, then correlated_vars#="NA" can be used in place.
#' @param  folder_name Name of folder to put model results (.RDS and .CSV) files in. Name must be written in quotes "
#' @param  familyXYZ Name of GAM family to put model results (.RDS and .CSV) files in must include family= in quotes. e.g: familyXYZ= "family=gaussian()"
#' @param  number_vars_in_mod Maximum number of covariates to include in any model run.e.g. if you have 6 environmental covariates to test, but only want a maximum of 4 included in model, can use: number_vars_in_mod = (length(predictors)-2))

####Function:####
GAM_LOOP_FUN<-function(Edata,k,correlated_vars1,correlated_vars2,correlated_vars3,correlated_vars4,correlated_vars5,correlated_vars6,correlated_vars7,correlated_vars8,folder_name,familyXYZ,number_vars_in_mod){
  
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
   if(correlated_vars1!="NA"||correlated_vars2!="NA"){
    predictor_combinations <- as.data.frame(predictor_combinations)
    predictor_combinations <-predictor_combinations[!grepl(correlated_vars1, predictor_combinations$predictor_combinations)| !grepl(correlated_vars2,predictor_combinations$predictor_combinations),]
  } 
  if(correlated_vars3!="NA"||correlated_vars4!="NA"){
    predictor_combinations <- as.data.frame(predictor_combinations)
    predictor_combinations <-predictor_combinations[!grepl(correlated_vars3, predictor_combinations$predictor_combinations)| !grepl(correlated_vars4,predictor_combinations$predictor_combinations),]
  }
  if(correlated_vars5!="NA"||correlated_vars6!="NA"){
    predictor_combinations <- as.data.frame(predictor_combinations)
    predictor_combinations <-predictor_combinations[!grepl(correlated_vars5, predictor_combinations$predictor_combinations)| !grepl(correlated_vars6,predictor_combinations$predictor_combinations),]  
  }
  if(correlated_vars7!="NA"||correlated_vars8!="NA"){
    predictor_combinations <- as.data.frame(predictor_combinations)
    predictor_combinations <-predictor_combinations[!grepl(correlated_vars7, predictor_combinations$predictor_combinations)| !grepl(correlated_vars8,predictor_combinations$predictor_combinations),]  
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
    hypergrid[i, AIC := round(rel.model$aic,digits=3)] #AIC
    hypergrid[i, s.pv := list(round(summary(rel.model)[["s.pv"]],digits=3))] #p-value of each coviariate in model
    hypergrid[i, dev.expl := round(summary(rel.model)[["dev.expl"]],digits=3)] #deviance explained of model
    hypergrid[i, family := rel.model$family[1]] #what family was used
  }
  
  #arrange hypergrid and see resulting df showing model diognisc comparisons
  hypergrid<- dplyr::arrange(hypergrid, hypergrid$target, desc(hypergrid$AIC))
  .GlobalEnv$hypergrid <- hypergrid
}

allorsome<-function(all){
##New stuff added to filter hypergrid results based on significant independent variables
checksome <- function(x){ 
  TRUE %in% (x<=0.05)
}
checkall <- function(x){ 
  all(x<=0.05)
}
# Get a vector that tells you if at least one pvalue <= 0.05
hypergrid$some <- lapply(hypergrid$s.pv, checksome) 
hypergrid$all <-  lapply(hypergrid$s.pv, checkall) 
hypergrid$s.pv<-as.character(hypergrid$s.pv)

# Pick significant models
# all=FALSE returns models runs that have at least one significant covariate p<=0.05
# all=TRUE  returns models runs where all covariates are significant at p<=0.05
if (all==TRUE) {
  hypergrid <- hypergrid %>% filter(all==TRUE) %>% as.data.frame() %>% drop_columns(c("all", "some")) 
} else if (all==FALSE){
  hypergrid <- hypergrid %>% filter(some==TRUE) %>% as.data.frame() %>% drop_columns(c("all", "some"))
} else {message("All or some function not working")}

hypergrid<-as.data.frame(hypergrid,stringsAsFactors = F)
hypergrid<-hypergrid[ , !names(hypergrid) %in% c("model")]
.GlobalEnv$hypergrid <- hypergrid
}



### An example function setup###
#targets <- c("depth_Fall")  #target (dependent variable column name in df). can write more than one, separated by commas
#predictors <- colnames(Fall_distribution.df)[!(colnames(Fall_distribution.df) %in% c("depth_Fall","Lat_Fall", "Year"))] #Predictors = independent variables of interest
#correlated_vars<-c("Annual_os_bt_anomaly","month6_bt_anomaly","GSI6","GSI12","NAO12","NAO6","AMO6","AMO12") #list which variables have correlations/ you don't want them run in the same model iteration (which ones can't be run together will be specified in function parameters, this is just a list of all column names applicable)

#GAM_LOOP_FUN(Edata=Fall_distribution.df,k="k=8",correlated_vars1=correlated_vars[1],correlated_vars2=correlated_vars[2],correlated_vars3=correlated_vars[3],correlated_vars4=correlated_vars[4],correlated_vars5=correlated_vars[5],correlated_vars6=correlated_vars[6],correlated_vars7=correlated_vars[7],correlated_vars8=correlated_vars[8],folder_name="plaice_fall_depth",familyXYZ= "family=tw()",number_vars_in_mod = (length(predictors))) #example of function with all parameters specified
#all_hypergrid<-allorsome(all=TRUE) 
