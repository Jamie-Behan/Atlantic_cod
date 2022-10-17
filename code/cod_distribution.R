###### American plaice stock assessment data GAM work####
library(pacman)
pacman::p_load(here, readxl,lubridate,stats,graphics,Hmisc,data.table,utils,mgcv,dplyr,purrr,ecodata,gridExtra) 
here()
#### load .csv files #####
Cod_distribution<-read.csv(here("data/Cod_distribution.csv"))
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
distribution_fall <- list(Cod_distribution[,c(1,4,5)], annual_GSI, Bottom_temp_fall[,c(1,6)],Friedland_OISST_fall[,c(1,6)],cod_heatwave[,c(1,6)],SSB_Fall_all[,c(1,3)])
#merge all data frames in list
distribution_fall<-distribution_fall %>% reduce(full_join, by='Year')
names(distribution_fall)[4] <- "Avg_GSI"

distribution_spring <- list(Cod_distribution[,c(1,2,3)], annual_GSI, Bottom_temp_spring[,c(1,6)],Friedland_OISST_spring[,c(1,6)],cod_heatwave[,c(1,6)],SSB_Spring_all[,c(1,3)])
distribution_spring<-distribution_spring %>% reduce(full_join, by='Year')
names(distribution_spring)[4] <- "Avg_GSI"
### remove data I don't need ####
rm(annual_GSI,Bottom_temp_fall,Bottom_temp_spring,Friedland_OISST_fall,Friedland_OISST_spring,Cod_distribution,cod_heatwave,SSB_Fall_all,SSB_Spring_all)

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
distribution_fall<-distribution_fall[,c(1:4,7:10)]
distribution_fall$COG_depth_fall<-abs(distribution_fall$COG_depth_fall)
distribution_spring<-distribution_spring[,c(1:4,7:10)]
distribution_spring$COG_depth_spring<-abs(distribution_spring$COG_depth_spring)
#########################
############ START ANALYSIS ##################



####### Check Outliars######
summary(distribution_fall)
summary(distribution_spring)

#SPRING
par(mar=c(2,2,0,0), mfrow=c(2,3))
dotchart(distribution_spring[,4])
dotchart(distribution_spring[,5])
dotchart(distribution_spring[,6])
dotchart(distribution_spring[,7])
dotchart(distribution_spring[,8])

#Fall
par(mar=c(2,2,0,0), mfrow=c(2,3))
dotchart(distribution_fall[,4])
dotchart(distribution_fall[,5])
dotchart(distribution_fall[,6])
dotchart(distribution_fall[,7])
dotchart(distribution_fall[,8])


#####PLOT Seasonal Depth changes together on same plot#####
layout(matrix(1:2, ncol=2, byrow=FALSE))
par(mar=c(4.1,4.5,1.5,1), oma=c(1.0,0,1.0,0.1))
plot(COG_depth_fall ~ Year, data=distribution_fall, main="Mean Depth of Cod Occurance Overtime", xlab="Year",ylab="Mean Depth (m)", type="l",lwd=3,col="#00608A",ylim= c(140,80), cex.lab=1.4,cex.axis=1.1)
abline(lm(distribution_fall$COG_depth_fall~distribution_fall$Year),col="#00608A",lwd=2.5,lty=2)
lines(COG_depth_spring ~ Year, data=distribution_spring, xlab="Year", type="l",col="#EA4F12",lwd=3)
abline(lm(distribution_spring$COG_depth_spring~distribution_spring$Year),col="#EA4F12",lwd=2.5,lty=2)
legend(2010, 125, legend=c("Spring", "Fall"),
       col=c("#EA4F12", "#00608A"), lty=1,lwd=3, cex=1.0)
#PLOT Seasonal Latitude changes together on same plot
plot(COG_Lat_fall ~ Year, data=distribution_fall, main="Mean Latitude of Cod Occurance Overtime", xlab="Year",ylab="Mean Latitude (km)", type="l",lwd=3,col="#00608A", cex.lab=1.4,cex.axis=1.1,ylim= c(41.8,42.7))
abline(lm(distribution_fall$COG_Lat_fall~distribution_fall$Year),col="#00608A",lwd=2.5,lty=2)
lines(COG_Lat_spring ~ Year, data=distribution_spring, xlab="Year", type="l",col="#EA4F12",lwd=3)
abline(lm(distribution_spring$COG_Lat_spring~distribution_spring$Year),col="#EA4F12",lwd=2.5,lty=2)
legend(2010, 42.0, legend=c("Spring", "Fall"),
       col=c("#EA4F12", "#00608A"), lty=1,lwd=3, cex=1.0)
######Boxplots######
view_boxplot_fun<- function (data){
  layout(matrix(1:8, ncol=4, byrow=TRUE))
  boxplot(data[2],varwidth = TRUE, xlab = "Avg Depth", ylab = "Meters", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[3],varwidth = TRUE, xlab = "Avg Latitude", ylab = "Degrees Lat", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[4],varwidth = TRUE, xlab = "Avg GSI", ylab = "Degrees Lat", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[6],varwidth = TRUE, xlab = "Avg SSB all stocks", ylab = "Kg", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[7],varwidth = TRUE, xlab = "Bt Anomaly", ylab = "Deg C", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[8],varwidth = TRUE, xlab = "SST Anomaly", ylab = "Deg C", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[5],varwidth = TRUE, xlab = "Mean Cumulative Heatwave", ylab = "Deg C", data = data,cex.lab=1.5, cex.axis=1.5)
}
view_boxplot_fun(distribution_fall)
view_boxplot_fun(distribution_spring)

##### check distribtuion ####
#SPRING
par(mar=c(2,2,2,0), mfrow=c(2,4))
hist(distribution_spring$COG_depth_spring)
hist(distribution_spring$COG_Lat_spring)
hist(distribution_spring[,4])
hist(distribution_spring[,5])
hist(distribution_spring[,6])
hist(distribution_spring[,7])
hist(distribution_spring[,8])

#Fall
par(mar=c(2,2,2,0), mfrow=c(2,4))
hist(distribution_fall$COG_depth_fall)
hist(distribution_fall$COG_Lat_fall)
hist(distribution_fall[,4])
hist(distribution_fall[,5])
hist(distribution_fall[,6])
hist(distribution_fall[,7])
hist(distribution_fall[,8])

#####shapiro test for normality#####
#if p >0.05, we can assume normality
#SPRING
shapiro.test(distribution_spring$COG_depth_spring)#not normal
shapiro.test(distribution_spring$COG_Lat_spring)#not normal
shapiro.test(distribution_spring[,4])
shapiro.test(distribution_spring[,5])#not normal
shapiro.test(distribution_spring[,6])#not normal
shapiro.test(distribution_spring[,7])
shapiro.test(distribution_spring[,8])

#Fall
shapiro.test(distribution_fall$COG_depth_fall)
shapiro.test(distribution_fall$COG_Lat_fall)
shapiro.test(distribution_fall[,4])
shapiro.test(distribution_fall[,5])#not normal
shapiro.test(distribution_fall[,6])#not normal
shapiro.test(distribution_fall[,7])
shapiro.test(distribution_fall[,8])

####### Check Correlation matrix######
############Correlation Coefficient Test#############
myvif <- function(mod) {
  v <- vcov(mod)
  assign <- attributes(model.matrix(mod))$assign
  if (names(coefficients(mod)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  } else warning("No intercept: vifs may not be sensible.")
  terms <- labels(terms(mod))
  n.terms <- length(terms)
  if (n.terms < 2) stop("The model contains fewer than 2 terms")
  if (length(assign) > dim(v)[1] ) {
    diag(tmp_cor)<-0
    if (any(tmp_cor==1.0)){
      return("Sample size is too small, 100% collinearity is present")
    } else {
      return("Sample size is too small")
    }
  }
  R <- cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/2Df)")
  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs])) / detR
    result[term, 2] <- length(subs)
  }
  if (all(result[, 2] == 1)) {
    result <- data.frame(GVIF=result[, 1])
  } else {
    result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  }
  invisible(result)
}
corvif <- function(dataz) {
  dataz <- as.data.frame(dataz)
  
  #vif part
  form    <- formula(paste("fooy ~ ",paste(strsplit(names(dataz)," "),collapse=" + ")))
  dataz   <- data.frame(fooy=1 + rnorm(nrow(dataz)) ,dataz)
  lm_mod  <- lm(form,dataz)
  
  cat("\n\nVariance inflation factors\n\n")
  print(myvif(lm_mod))
}

panel.cor <- function(x, y, digits=1, prefix="", cex.cor = 6){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1=cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) { cex <- 0.9/strwidth(txt) } else {
    cex = cex.cor}
  text(0.5, 0.5, txt, cex = cex * r)
}
Mypairs <- function(Z) {
  MyVarx <- colnames(Z)
  pairs(Z, labels = MyVarx,
        cex.labels =  2,
        lower.panel = function(x, y, digits=2, prefix="", cex.cor = 7) {
          panel.cor(x, y, digits, prefix, cex.cor)},
        upper.panel =  function(x, y) points(x, y,
                                             pch = 16, cex = 0.8,
                                             col = gray(0.1)))
  #print(P)
}

Mypairs(distribution_fall[c(4:8)])
Mypairs(distribution_spring[c(4:8)])

####Making transparent colors for plots below: ####
t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}
###Plot GAM Response Curves------ FUNCTION ####
GAM_CURVE_FUN<- function(gam_name,data_column,data_Year,x_lab,y_lab,select1,title,position){
  par(mar=c(5,4.5,3,1))
  plot.gam(gam_name,xlab=paste(x_lab),ylab=paste(y_lab), select=select1,cex.lab=1.5,cex.axis=1.4,rug=FALSE,shade = TRUE,col = "black",shade.col=t_col("lightgray",50,"plot_gray"),lwd = 2.5)
  rug(subset(data_column, data_Year <=1989), ticksize=0.03, side=1, lwd=2.5,col="blue")
  rug(subset(data_column, data_Year <=1999 & data_Year >=1990), ticksize=0.05, side=1, lwd=2.5,col="green")
  rug(subset(data_column, data_Year <=2009 & data_Year >=2000), ticksize=0.05, side=1, lwd=2.5,col="orange")
  rug(subset(data_column, data_Year <=2019 & data_Year >=2010), ticksize=0.03, side=1, lwd=2.5,col="red")
  legend(paste(position),inset=c(0.05,0.05), legend =c('1977-1989', '1990-1999','2000-2009', '2010-2019'), pch=16, pt.cex=1.5, cex=1.2, bty='n',
         col = c("blue", "green","orange", "red"),title="Decade")
  abline(h=0, lty=2, col="tomato3", lwd=2.0)
  title(main=paste(title),cex.main=1.8)
}
GAM_CURVE_FUN_fall<- function(gam_name,data_column,x_lab,y_lab,select1){
  plot.gam(gam_name,xlab=paste(x_lab),ylab=paste(y_lab), select=select1,cex.lab=1.5,cex.axis=1.4,rug=FALSE,shade = TRUE,col = "#00608A",shade.col=t_col("#00608A",70,"plot_gray"),lwd = 3.5,lty=2)
  rug(data_column, ticksize=0.03, side=1, lwd=2.5,col="#00608A")
  abline(h=0, lty=2, col="black", lwd=2.0)
}
GAM_CURVE_FUN_spring<- function(gam_name,data_column,x_lab,y_lab,select1){
  plot.gam(gam_name,xlab=paste(x_lab),ylab=paste(y_lab), select=select1,cex.lab=1.5,cex.axis=1.4,rug=FALSE,shade = TRUE,col = "#EA4F12",shade.col=t_col("#EA4F12",75,"plot_gray"),lwd = 3.5,lty=2)
  rug(data_column, ticksize=0.03, side=1, lwd=2.5,col="#EA4F12")
  abline(h=0, lty=2, col="black", lwd=2.0)
}
#############################################
################ Exploring to see if GAM loop works to test more combinations of gam varibales quickly#####
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

####Testing Fall depth models######
###write column names of dependent "target" variables, and independent "predictors" variables will be all column names other than dependent variables, or any other column name you list (I also listed year)
############Gaussian###################
targets <- c("COG_depth_fall")
predictors <- colnames(distribution_fall)[!(colnames(distribution_fall) %in% c("COG_depth_fall","COG_Lat_fall", "Year"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","mean_c_heatwave","Avg_GSI")

GAM_LOOP_FUN(Edata=distribution_fall,k="k=10",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3= correlated_vars[2],correlated_vars4= correlated_vars[3],correlated_vars5= correlated_vars[1],correlated_vars6= correlated_vars[4],folder_name="cod_fall_depth",familyXYZ= "family=gaussian()")
hypergrid$s.pv<-as.character(hypergrid$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]
grid.newpage(grid.table(hypergrid_gaus))

############Tweedie###################
targets <- c("COG_depth_fall")
predictors <- colnames(distribution_fall)[!(colnames(distribution_fall) %in% c("COG_depth_fall","COG_Lat_fall", "Year"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","mean_c_heatwave","Avg_GSI")

GAM_LOOP_FUN(Edata=distribution_fall,k="k=10",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3= correlated_vars[2],correlated_vars4= correlated_vars[3],correlated_vars5= correlated_vars[1],correlated_vars6= correlated_vars[4],folder_name="cod_fall_depth",familyXYZ= "family=tw()")
hypergrid_tw<-hypergrid
hypergrid_tw$s.pv<-as.character(hypergrid_tw$s.pv)
hypergrid_tw<-as.data.frame(hypergrid_tw,stringsAsFactors = F)
hypergrid_tw<-hypergrid_tw[ , !names(hypergrid_tw) %in% c("model")]
grid.newpage(grid.table(hypergrid_tw))
#### Testing Spring depth models########
############Gaussian###################
targets <- c("COG_depth_spring")
predictors <- colnames(distribution_spring)[!(colnames(distribution_spring) %in% c("COG_depth_spring","COG_Lat_spring", "Year"))]
correlated_vars<-c("bt_anomaly","sst_anomaly")

GAM_LOOP_FUN(Edata=distribution_spring,k="k=5",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3= "NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="cod_spring_depth",familyXYZ= "family=gaussian()")
hypergrid$s.pv<-as.character(hypergrid$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]
grid.newpage(grid.table(hypergrid_gaus))
############Tweedie###################
targets <- c("COG_depth_spring")
predictors <- colnames(distribution_spring)[!(colnames(distribution_spring) %in% c("COG_depth_spring","COG_Lat_spring", "Year"))]
correlated_vars<-c("bt_anomaly","sst_anomaly")

GAM_LOOP_FUN(Edata=distribution_spring,k="k=5",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3= "NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="cod_spring_depth",familyXYZ= "family=tw()")
hypergrid_tw<-hypergrid
hypergrid_tw$s.pv<-as.character(hypergrid_tw$s.pv)
hypergrid_tw<-as.data.frame(hypergrid_tw,stringsAsFactors = F)
hypergrid_tw<-hypergrid_tw[ , !names(hypergrid_tw) %in% c("model")]
grid.newpage(grid.table(hypergrid_tw))
####Testing Fall Latitude models######
############Gaussian###################
targets <- c("COG_Lat_fall")
predictors <- colnames(distribution_fall)[!(colnames(distribution_fall) %in% c("COG_depth_fall","COG_Lat_fall", "Year"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","mean_c_heatwave","Avg_GSI")

GAM_LOOP_FUN(Edata=distribution_fall,k="k=10",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3= correlated_vars[2],correlated_vars4= correlated_vars[3],correlated_vars5= correlated_vars[1],correlated_vars6= correlated_vars[4],folder_name="cod_fall_lat",familyXYZ= "family=gaussian()")
hypergrid$s.pv<-as.character(hypergrid$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]
grid.newpage(grid.table(hypergrid_gaus))

############Tweedie###################
targets <- c("COG_Lat_fall")
predictors <- colnames(distribution_fall)[!(colnames(distribution_fall) %in% c("COG_depth_fall","COG_Lat_fall", "Year"))]
correlated_vars<-c("bt_anomaly","sst_anomaly","mean_c_heatwave","Avg_GSI")

GAM_LOOP_FUN(Edata=distribution_fall,k="k=10",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3= correlated_vars[2],correlated_vars4= correlated_vars[3],correlated_vars5= correlated_vars[1],correlated_vars6= correlated_vars[4],folder_name="cod_fall_lat",familyXYZ= "family=tw()")
hypergrid_tw<-hypergrid
hypergrid_tw$s.pv<-as.character(hypergrid_tw$s.pv)
hypergrid_tw<-as.data.frame(hypergrid_tw,stringsAsFactors = F)
hypergrid_tw<-hypergrid_tw[ , !names(hypergrid_tw) %in% c("model")]
grid.newpage(grid.table(hypergrid_tw))
#### Testing Spring depth models########
############Gaussian###################
targets <- c("COG_Lat_spring")
predictors <- colnames(distribution_spring)[!(colnames(distribution_spring) %in% c("COG_depth_spring","COG_Lat_spring", "Year"))]
correlated_vars<-c("bt_anomaly","sst_anomaly")

GAM_LOOP_FUN(Edata=distribution_spring,k="k=5",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3= "NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="cod_spring_lat",familyXYZ= "family=gaussian()")
hypergrid$s.pv<-as.character(hypergrid$s.pv)
hypergrid_gaus<-as.data.frame(hypergrid,stringsAsFactors = F)
hypergrid_gaus<-hypergrid_gaus[ , !names(hypergrid_gaus) %in% c("model")]
grid.newpage(grid.table(hypergrid_gaus))
############Tweedie###################
targets <- c("COG_Lat_spring")
predictors <- colnames(distribution_spring)[!(colnames(distribution_spring) %in% c("COG_depth_spring","COG_Lat_spring", "Year"))]
correlated_vars<-c("bt_anomaly","sst_anomaly")

GAM_LOOP_FUN(Edata=distribution_spring,k="k=5",correlated_vars1= correlated_vars[1],correlated_vars2= correlated_vars[2],correlated_vars3= "NA",correlated_vars4="NA",correlated_vars5="NA",correlated_vars6="NA",folder_name="cod_spring_lat",familyXYZ= "family=tw()")
hypergrid_tw<-hypergrid
hypergrid_tw$s.pv<-as.character(hypergrid_tw$s.pv)
hypergrid_tw<-as.data.frame(hypergrid_tw,stringsAsFactors = F)
hypergrid_tw<-hypergrid_tw[ , !names(hypergrid_tw) %in% c("model")]
grid.newpage(grid.table(hypergrid_tw))
############# PLOT SIGNIFICANT GAM CURVES #######################
##### DEPTH (Fall) vs. potential environmental influences###########

#nothing significant

##### DEPTH (Spring tow) vs. potential environmental influences##########
SP_Depth<-gam(abs(COG_depth_spring) ~ s(SSB, k=5), family=tw(),method = "REML",data=distribution_spring)
summary(SP_Depth)
SP_Depth$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(SP_Depth,pch=20, cex=1.2,cex.lab=1.5)

layout(matrix(1:1, ncol=1, byrow=FALSE))
GAM_CURVE_FUN_spring(SP_Depth,distribution_spring$SSB,x_lab="SSB (kg/tow)",y_lab="PE on Mean Depth",select1=1)

##### LATITUDE (Fall) vs. potential environmental influences###########
FL_Lat<-gam((COG_Lat_fall) ~ s(SSB, k=10), family=Gamma(),method = "REML",data=distribution_fall) # Build GAM with all possible variables
summary(FL_Lat) # Find significant variables based on p-value
FL_Lat$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(FL_Lat,pch=20, cex=1.2,cex.lab=1.5)

###Plot GAM
layout(matrix(1:1, ncol=1, byrow=FALSE))
GAM_CURVE_FUN_fall(FL_Lat,distribution_fall$SSB,x_lab="SSB (kg/tow)",y_lab="PE on Mean Latitude",select1=1)


##### Latitude (Spring tow) vs. potential environmental influences##########
SP_numtow<-gam((COG_Lat_spring) ~ s(mean_c_heatwave, k=10), family=tw(),method = "REML",data=distribution_spring) # Build GAM with all possible variables
summary(SP_numtow) # Find significant variables based on p-value
SP_numtow$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(SP_numtow,pch=20, cex=1.2,cex.lab=1.5)

###Plot GAM
layout(matrix(1:1, ncol=1, byrow=FALSE))
GAM_CURVE_FUN_spring(SP_numtow,distribution_spring$mean_c_heatwave,x_lab="GSI",y_lab="PE on Mean Latitude",select1=1)
##############
