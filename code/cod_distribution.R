###### American plaice stock assessment data GAM work
library(pacman)
pacman::p_load(here, readxl,lubridate,stats,graphics,Hmisc,data.table,utils,mgcv,dplyr,purrr,ecodata) 
here()
#### load .csv files #####
Cod_distribution<-read.csv(here("data/Cod_distribution.csv"))
annual_GSI<-read.csv(here("data/annual_GSI.csv"))
Bottom_temp_fall<-read.csv(here("data/Friedland_fall_mean_bottom_temp_by_stock.csv"))
Bottom_temp_spring<-read.csv(here("data/Friedland_spring_mean_bottom_temp_by_stock.csv"))
Friedland_OISST_fall<-read.csv(here("data/Friedland_OISST_fall.csv"))
Friedland_OISST_spring<-read.csv(here("data/Friedland_OISST_spr.csv"))
EGOM_K<-read.csv(here("data/rel_condition/ADIOS_SV_164712_EGOM_NONE_relative_k.csv"))
WGOM_K<-read.csv(here("data/rel_condition/ADIOS_SV_164712_WGOM_NONE_relative_k.csv"))
GBK_K<-read.csv(here("data/rel_condition/ADIOS_SV_164712_GBK_NONE_relative_k.csv"))
SNEMA_K<-read.csv(here("data/rel_condition/ADIOS_SV_164712_SNEMA_NONE_relative_k.csv"))
#AMO_NAO<-read.csv(here("data/Full_GSI_AMO_NAO.csv"))
### Reorganize Fulton's K data####
##fall data
FultonK_fall<-FultonKOutput[FultonKOutput$source == "NMFS Fall", ]

FultonK_fall<-aggregate(FultonK~Year+Stock,FultonK_fall,FUN=mean)

EGOM_FK<-FultonK_fall[FultonK_fall$Stock == "Eastern GOM", ]
names(EGOM_FK)[3] <- "EGOM_FK"
WGOM_FK<-FultonK_fall[FultonK_fall$Stock == "Western GOM", ]
names(WGOM_FK)[3] <- "WGOM_FK"
GB_FK<-FultonK_fall[FultonK_fall$Stock == "Georges Bank", ]
names(GB_FK)[3] <- "GB_FK"

FultonK_fall <- list(EGOM_FK[,c(1,3)], WGOM_FK[,c(1,3)], GB_FK[,c(1,3)])
FultonK_fall <-FultonK_fall %>% reduce(full_join, by='Year')
FultonK_fall$Avg_FK <-rowMeans(FultonK_fall[,c(2:4)],na.rm=TRUE)
##spring data
FultonK_spring<-FultonKOutput[FultonKOutput$source == "NMFS Spring", ]
FultonK_spring<-aggregate(FultonK~Year+Stock,FultonK_spring,FUN=mean)

EGOM_FK<-FultonK_spring[FultonK_spring$Stock == "Eastern GOM", ]
names(EGOM_FK)[3] <- "EGOM_FK"
WGOM_FK<-FultonK_spring[FultonK_spring$Stock == "Western GOM", ]
names(WGOM_FK)[3] <- "WGOM_FK"
GB_FK<-FultonK_spring[FultonK_spring$Stock == "Georges Bank", ]
names(GB_FK)[3] <- "GB_FK"

FultonK_spring <- list(EGOM_FK[,c(1,3)], WGOM_FK[,c(1,3)], GB_FK[,c(1,3)])
FultonK_spring <-FultonK_spring %>% reduce(full_join, by='Year')
FultonK_spring$Avg_FK <-rowMeans(FultonK_spring[,c(2:4)],na.rm=TRUE)
### remove data I don't need ####
rm(EGOM_FK,WGOM_FK,GB_FK,FultonKOutput)
####Combine data into separate spring and fall data frames####
#put all data frames into list
distribution_fall <- list(Cod_distribution[,c(1,4,5)], annual_GSI, Bottom_temp_fall[,c(1,6)],Friedland_OISST_fall[,c(1,6)],FultonK_fall[,c(1,5)])
#merge all data frames in list
distribution_fall<-distribution_fall %>% reduce(full_join, by='Year')
names(distribution_fall)[4] <- "Avg_GSI"

distribution_spring <- list(Cod_distribution[,c(1,2,3)], annual_GSI, Bottom_temp_spring[,c(1,6)],Friedland_OISST_spring[,c(1,6)],FultonK_spring[,c(1,5)])
distribution_spring<-distribution_spring %>% reduce(full_join, by='Year')
names(distribution_spring)[4] <- "Avg_GSI"
### remove data I don't need ####
rm(annual_GSI)
rm(Bottom_temp_fall)
rm(Bottom_temp_spring)
rm(Friedland_OISST_fall)
rm(Friedland_OISST_spring)
rm(FultonK_fall)
rm(FultonK_spring)
###clip to years with most data###
distribution_fall = distribution_fall[!distribution_fall$Year > 2019,]
distribution_fall = distribution_fall[!distribution_fall$Year < 1977,]
distribution_spring = distribution_spring[!distribution_spring$Year > 2019,]
distribution_spring = distribution_spring[!distribution_spring$Year < 1977,]
###reorder by year###
distribution_fall<-distribution_fall %>% arrange(Year)
distribution_spring<-distribution_spring %>% arrange(Year)

###### Anomaly Base Period########
### using 1981-2010 as baseline anomaly period as NOAA does####

bt_fall_bp<-mean(distribution_fall[5:34,5])
bt_spring_bp<-mean(distribution_spring[5:34,5])
sst_fall_bp<-mean(distribution_fall[5:34,6])
sst_spring_bp<-mean(distribution_spring[5:34,6])
#####
##### Calculate temperature anomaly columns#####

#Fall
distribution_fall$bt_anomaly<- distribution_fall$Avg_bt  - bt_fall_bp
distribution_fall$sst_anomaly<- distribution_fall$Avg_oisst  - sst_fall_bp

#Spring
distribution_spring$bt_anomaly<- distribution_spring$Avg_bt  - bt_spring_bp
distribution_spring$sst_anomaly<- distribution_spring$Avg_oisst  - sst_spring_bp

##### Get cod heatwave data ####
cod_heatwave<-as.data.frame(ecodata::ESP_heatwave_cod)

EGOM_chw<-cod_heatwave[(cod_heatwave$stock_id == "EGOM") & (cod_heatwave$Var == "cumulative intensity"), ]
names(EGOM_chw)[3] <- "EGOM_hw"
EGOM_chw <- EGOM_chw[, -c(2,4:5)]
WGOM_chw<-cod_heatwave[(cod_heatwave$stock_id == "WGOM") & (cod_heatwave$Var == "cumulative intensity"), ]
names(WGOM_chw)[3] <- "WGOM_hw"
WGOM_chw <- WGOM_chw[, -c(2,4:5)]
GB_chw<-cod_heatwave[(cod_heatwave$stock_id == "GBK") & (cod_heatwave$Var == "cumulative intensity"), ]
names(GB_chw)[3] <- "GB_hw"
GB_chw <- GB_chw[, -c(2,4:5)]
SNE_chw<-cod_heatwave[(cod_heatwave$stock_id == "SNE") & (cod_heatwave$Var == "cumulative intensity"), ]
names(SNE_chw)[3] <- "SNE_hw"
SNE_chw <- SNE_chw[, -c(2,4:5)]

c_od_heatwave<-merge(EGOM_chw,WGOM_chw,merge="Time",all=TRUE)
c_od_heatwave<-merge(c_od_heatwave,GB_chw,merge="Time",all=TRUE)
c_od_heatwave<-merge(c_od_heatwave,SNE_chw,merge="Time",all=TRUE)

c_od_heatwave$mean_c_heatwave <- rowMeans(c_od_heatwave[,2:5],na.rm=TRUE)
names(c_od_heatwave)[1] <- "Year"
###### add heatwave, NAO, AMO columns ####
distribution_fall<-merge(distribution_fall,c_od_heatwave[,c(1,6)],merge="Year",all=TRUE)
distribution_fall<-merge(distribution_fall,AMO_NAO[30:74,c(1,3:4)],merge="Year",all=TRUE)
distribution_spring<-merge(distribution_spring,c_od_heatwave[,c(1,6)],merge="Year",all=TRUE)
distribution_spring<-merge(distribution_spring,AMO_NAO[30:74,c(1,3:4)],merge="Year",all=TRUE)

distribution_fall <- distribution_fall[-45, ]
distribution_spring <- distribution_spring[-45, ]

rm(EGOM_chw,WGOM_chw,GB_chw,SNE_chw,cod_heatwave,AMO_NAO)
####### Check Outliars######
summary(distribution_fall)
summary(distribution_spring)

#SPRING
par(mar=c(2,2,0,0), mfrow=c(3,4))
dotchart(distribution_spring[,4])
dotchart(distribution_spring[,5])
dotchart(distribution_spring[,6])
dotchart(distribution_spring[,7])
dotchart(distribution_spring[,8])
dotchart(distribution_spring[,9])
dotchart(distribution_spring[,10])
dotchart(distribution_spring[,11])
dotchart(distribution_spring[,12])
#Fall
par(mar=c(2,2,0,0), mfrow=c(3,4))
dotchart(distribution_fall[,4])
dotchart(distribution_fall[,5])
dotchart(distribution_fall[,6])
dotchart(distribution_fall[,7])
dotchart(distribution_fall[,8])
dotchart(distribution_fall[,9])
dotchart(distribution_fall[,10])
dotchart(distribution_fall[,11])
dotchart(distribution_fall[,12])
#####PLOT Seasonal Depth changes together on same plot#####
layout(matrix(1:2, ncol=2, byrow=FALSE))
par(mar=c(4.1,4.5,1.5,1), oma=c(1.0,0,1.0,0.1))
plot(COG_depth_fall ~ Year, data=distribution_fall, main="Mean Depth of Cod Occurance Overtime", xlab="Year",ylab="Mean Depth (m)", type="l",lwd=3,col="#00608A",ylim= c(-140,-80), cex.lab=1.4,cex.axis=1.1)
abline(lm(distribution_fall$COG_depth_fall~distribution_fall$Year),col="#00608A",lwd=2.5,lty=2)
lines(COG_depth_spring ~ Year, data=distribution_spring, xlab="Year", type="l",col="#EA4F12",lwd=3)
abline(lm(distribution_spring$COG_depth_spring~distribution_spring$Year),col="#EA4F12",lwd=2.5,lty=2)
legend(2010, -125, legend=c("Spring", "Fall"),
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
  layout(matrix(1:10, ncol=5, byrow=TRUE))
  boxplot(data[2],varwidth = TRUE, xlab = "Avg Depth", ylab = "Meters", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[3],varwidth = TRUE, xlab = "Avg Latitude", ylab = "Degrees Lat", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[4],varwidth = TRUE, xlab = "Avg GSI", ylab = "Degrees Lat", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[7],varwidth = TRUE, xlab = "Avg Fulton's K", ylab = "Condition", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[8],varwidth = TRUE, xlab = "Bt Anomaly", ylab = "Deg C", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[9],varwidth = TRUE, xlab = "SST Anomaly", ylab = "Deg C", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[10],varwidth = TRUE, xlab = "Mean Cumulative Heatwave", ylab = "Deg C", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[11],varwidth = TRUE, xlab = "AMO", ylab = "Deg C", data = data,cex.lab=1.5, cex.axis=1.5)
  boxplot(data[12],varwidth = TRUE, xlab = "NAO", ylab = "Deg C", data = data,cex.lab=1.5, cex.axis=1.5)
}
view_boxplot_fun(distribution_fall)
view_boxplot_fun(distribution_spring)

##### check distribtuion ####
#SPRING
par(mar=c(2,2,2,0), mfrow=c(2,4))
hist(distribution_spring$COG_depth_spring)
hist(distribution_spring$COG_Lat_spring)
hist(distribution_spring[,4])
hist(distribution_spring[,7])
hist(distribution_spring[,8])
hist(distribution_spring[,9])
hist(distribution_spring[,10])
hist(distribution_spring[,11])
hist(distribution_spring[,12])
#Fall
par(mar=c(2,2,2,0), mfrow=c(2,4))
hist(distribution_fall$COG_depth_fall)
hist(distribution_fall$COG_Lat_fall)
hist(distribution_fall[,4])
hist(distribution_fall[,7])
hist(distribution_fall[,8])
hist(distribution_fall[,9])
hist(distribution_fall[,10])
hist(distribution_fall[,11])
hist(distribution_fall[,12])
#####shapiro test for normality#####
#if p >0.05, we can assume normality
#SPRING
shapiro.test(distribution_spring$COG_depth_spring)#not normal
shapiro.test(distribution_spring$COG_Lat_spring)#not normal
shapiro.test(distribution_spring[,4])
shapiro.test(distribution_spring[,7])
shapiro.test(distribution_spring[,8])
shapiro.test(distribution_spring[,9])
shapiro.test(distribution_spring[,10])#not normal
shapiro.test(distribution_spring[,11])
shapiro.test(distribution_spring[,12])
#Fall
shapiro.test(distribution_fall$COG_depth_fall)
shapiro.test(distribution_fall$COG_Lat_fall)
shapiro.test(distribution_fall[,4])
shapiro.test(distribution_fall[,7])
shapiro.test(distribution_fall[,8])
shapiro.test(distribution_fall[,9])
shapiro.test(distribution_fall[,10])#not normal
shapiro.test(distribution_fall[,11])
shapiro.test(distribution_fall[,12])
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

Mypairs(distribution_fall[c(4,7:12)])
Mypairs(distribution_spring[c(4,7:12)])

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
############# EXPLORATORY GAMs #######################
##### DEPTH (Fall) vs. potential environmental influences###########
FL_Depth<-gam(abs(COG_depth_fall) ~ s(Avg_GSI, k=5)+s(Avg_FK, k=5)+s(bt_anomaly, k=5)+s(sst_anomaly, k=5)+s(mean_c_heatwave, k=5)+s(AMO,k=5)+s(NAO,k=5), family=gaussian(),method = "REML",data=distribution_fall)
summary(FL_Depth)
FL_Depth$aic

#full model, duplicates removed:
FL_Depth<-gam(abs(COG_depth_fall) ~ s(Avg_GSI, k=5)+s(Avg_FK, k=5)+s(bt_anomaly, k=5)+s(sst_anomaly, k=5)+s(mean_c_heatwave, k=5)+s(AMO,k=5)+s(NAO,k=5), family=gaussian(),method = "REML",data=distribution_fall)
summary(FL_Depth)
FL_Depth$aic

#reduced model:
FL_Depth<-gam(abs(COG_depth_fall) ~ s(Avg_GSI, k=5)+s(Avg_FK, k=5)+s(bt_anomaly, k=5)+s(sst_anomaly, k=5)+s(mean_c_heatwave, k=5)+s(AMO,k=5)+s(NAO,k=5), family=gaussian(),method = "REML",data=distribution_fall)
summary(FL_Depth)
FL_Depth$aic


###Plot GAM
layout(matrix(1:1, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(FL_Depth,distribution_fall$sst_anomaly,distribution_fall$Year,x_lab="OISST Anomaly (Deg C)",y_lab="PE on Mean Depth",select1=1,title="Environmental Effects on Fall Mean Depth",position="topright")


par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(FL_Depth,pch=20, cex=1.2,cex.lab=1.5)


##### DEPTH (Spring tow) vs. potential environmental influences##########
SP_Depth<-gam(abs(COG_depth_spring) ~ s(Avg_GSI, k=5)+s(Avg_FK, k=5)+s(bt_anomaly, k=5)+s(sst_anomaly, k=5)+s(mean_c_heatwave, k=5)+s(AMO,k=5)+s(NAO,k=5), family=Gamma(),method = "REML",data=distribution_spring)
summary(SP_Depth)
SP_Depth$aic
##full model with duplicates removed:
SP_Depth<-gam(abs(COG_depth_spring) ~ s(Avg_GSI, k=5)+s(Avg_FK, k=5)+s(bt_anomaly, k=5)+s(sst_anomaly, k=5)+s(mean_c_heatwave, k=5)+s(AMO,k=5)+s(NAO,k=5), family=Gamma(),method = "REML",data=distribution_spring)
summary(SP_Depth)
SP_Depth$aic
#reduced model:
SP_Depth<-gam(abs(COG_depth_spring) ~ s(Avg_GSI, k=5)+s(Avg_FK, k=5)+s(bt_anomaly, k=5)+s(sst_anomaly, k=5)+s(mean_c_heatwave, k=5)+s(AMO,k=5)+s(NAO,k=5), family=Gamma(),method = "REML",data=distribution_spring)
summary(SP_Depth)
SP_Depth$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(SP_Depth,pch=20, cex=1.2,cex.lab=1.5)

layout(matrix(1:1, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(SP_Depth,distribution_spring$sst_anomaly,distribution_spring$Year,x_lab="Gulf Stream Index (Deg Lat)",y_lab="PE on Mean Depth",select1=1,title="Environmental Effects on Spring Mean Depth",position="topleft")

##### LATITUDE (Fall) vs. potential environmental influences###########
FL_Lat<-gam((COG_Lat_fall) ~ s(Avg_GSI, k=5)+s(Avg_FK, k=5)+s(bt_anomaly, k=5)+s(sst_anomaly, k=5)+s(mean_c_heatwave, k=5)+s(AMO,k=5)+s(NAO,k=5), family=gaussian(),method = "REML",data=distribution_fall) # Build GAM with all possible variables
summary(FL_Lat) # Find significant variables based on p-value
FL_Lat$aic

#full model, duplicates removed:
FL_Lat<-gam((COG_Lat_fall) ~ s(Avg_GSI, k=5)+s(Avg_FK, k=5)+s(bt_anomaly, k=5)+s(sst_anomaly, k=5)+s(mean_c_heatwave, k=5)+s(AMO,k=5)+s(NAO,k=5), family=gaussian(),method = "REML",data=distribution_fall) # Build GAM with all possible variables
summary(FL_Lat) # Find significant variables based on p-value
FL_Lat$aic

#reduced model:
FL_Lat<-gam((COG_Lat_fall) ~ s(Avg_GSI, k=5)+s(Avg_FK, k=5)+s(bt_anomaly, k=5)+s(sst_anomaly, k=5)+s(mean_c_heatwave, k=5)+s(AMO,k=5)+s(NAO,k=5), family=gaussian(),method = "REML",data=distribution_fall) # Build GAM with all possible variables
summary(FL_Lat) # Find significant variables based on p-value
FL_Lat$aic


###Plot GAM
layout(matrix(1:3, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(FL_Lat,distribution_fall$AMO6,distribution_fall$Year,x_lab="Atlantic Multidecadal Oscillation (Δ SST °C)",y_lab="PE on Mean Latitude",select1=1,title="Environmental Effects on Fall Mean Latitude",position="topleft")
GAM_CURVE_FUN(FL_Lat,distribution_fall$Annual_os_bt_anomaly,distribution_fall$Year,x_lab="Annual Mean Bottom Temperature Anomlay",y_lab="PE on Mean Latitude",select1=2,title=NULL,position="topleft")
GAM_CURVE_FUN(FL_Lat,distribution_fall$SSB,distribution_fall$Year,x_lab="SSB (kg/tow)",y_lab="PE on Mean Latitude",select1=3,title=NULL,position="topleft")

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(FL_Lat,pch=20, cex=1.2,cex.lab=1.5)


##### Latitude (Spring tow) vs. potential environmental influences##########
SP_numtow<-gam((Lat_Spring) ~ s(Avg_GSI, k=5)+s(Avg_FK, k=5)+s(bt_anomaly, k=5)+s(sst_anomaly, k=5)+s(mean_c_heatwave, k=5)+s(AMO,k=5)+s(NAO,k=5), family=gaussian(),method = "REML",data=distribution_spring) # Build GAM with all possible variables
summary(SP_numtow) # Find significant variables based on p-value
SP_numtow$aic
##full model with duplicates removed:
SP_numtow<-gam((Lat_Spring) ~ s(Avg_GSI, k=5)+s(Avg_FK, k=5)+s(bt_anomaly, k=5)+s(sst_anomaly, k=5)+s(mean_c_heatwave, k=5)+s(AMO,k=5)+s(NAO,k=5), family=gaussian(),method = "REML",data=distribution_spring) # Build GAM with all possible variables
summary(SP_numtow) # Find significant variables based on p-value
SP_numtow$aic
#reduced model:
SP_numtow<-gam((Lat_Spring) ~ s(Avg_GSI, k=5)+s(Avg_FK, k=5)+s(bt_anomaly, k=5)+s(sst_anomaly, k=5)+s(mean_c_heatwave, k=5)+s(AMO,k=5)+s(NAO,k=5), family=gaussian(),method = "REML",data=distribution_spring) # Build GAM with all possible variables
summary(SP_numtow) # Find significant variables based on p-value
SP_numtow$aic

par(mar=c(4,4,1,1))
layout(matrix(1:4, ncol=2, byrow=FALSE))
gam.check(SP_numtow,pch=20, cex=1.2,cex.lab=1.5)

###Plot GAM
layout(matrix(1:3, ncol=1, byrow=FALSE))
GAM_CURVE_FUN(SP_numtow,distribution_spring$NAO6,distribution_spring$Year,x_lab="North Atlantic Oscillation (hPa)",y_lab="PE on Mean Latitude",select1=1,title="Environmental Effects on Spring Mean Latitude",position="topleft")
GAM_CURVE_FUN(SP_numtow,distribution_spring$Annual_mf_bt_anomaly,distribution_spring$Year,x_lab="Annual Mean Bottom Temperature Anomlay",y_lab="PE on Mean Latitude",select1=2,title=NULL,position="topleft")
GAM_CURVE_FUN(SP_numtow,distribution_spring$SSB,distribution_spring$Year,x_lab="SSB (kg/tow)",y_lab="PE on Mean Latitude",select1=3,title=NULL,position="topleft")
##############
##### Make figures for RMarkdown#####
par(mar=c(4.5,4.5,1.5,1.5))
layout(matrix(1:6, ncol=2, byrow=TRUE))
qq.gam(FL_Depth, rep = 0, level = 0.9, type = "deviance", rl.col = 2, 
       rep.col = "gray80",pch=20, cex=1.2,cex.lab=1.2,main = "Fall Depth QQ")
hist(residuals(FL_Depth, type ="deviance"), xlab = "Residuals", main = "Fall Depth Histogram of residuals",cex.lab=1.2)
qq.gam(FL_Lat, rep = 0, level = 0.9, type = "deviance", rl.col = 2, 
       rep.col = "gray80",pch=20, cex=1.2,cex.lab=1.2,main = "Fall Latitude QQ")
hist(residuals(FL_Lat, type ="deviance"), xlab = "Residuals", main = "Fall Latitude Histogram of residuals",cex.lab=1.2)
qq.gam(SP_numtow, rep = 0, level = 0.9, type ="deviance", rl.col = 2, 
       rep.col = "gray80",pch=20, cex=1.2,cex.lab=1.2,main = "Spring Latitude QQ")
hist(residuals(SP_numtow, type ="deviance"), xlab = "Residuals", main = "Spring Latitude Histogram of residuals",cex.lab=1.2)

##### Combine NAO and bottom temp curves for Fall Depth ####
layout(matrix(1:3, ncol=1, byrow=TRUE))
par("mar"=c(4, 5, 1, 1))
plot.gam(FL_Depth, select =1, scale =0,ylab = expression(bold("PE on Mean Depth")), xlab = expression(bold("Mean Anomalies")), cex.lab=1.6,cex.axis=1.3,col = "#075fb8",shade = TRUE,shade.col=t_col("#075fb8",70,"plot_blt"),lwd = 4, lty=2,rug=FALSE,xlim = c(-1.025,1.025),ylim = c(-0.1,0.1))
rug(distribution_fall$NAO12, ticksize = 0.07, side = 1, lwd = 2.7, col = "blue")
abline(h=0, lty=2, col="black", lwd=2.0)
par(new = TRUE) #plot spring
plot(FL_Depth, select =2, scale =0,ylab = "", xlab = "",col="#0603cD",axes = FALSE,shade = TRUE,
     shade.col=t_col("#0603cD",45,"plot_blue"),lwd = 4,lty=2,xlim = c(-1.025,1.025),ylim = c(-0.1,0.1),rug=FALSE)
rug(distribution_spring$month6_bt_anomaly, ticksize = 0.05, side = 1, lwd = 2.9, col = "#0603cD")
legend("topleft", inset=0.04, # position
       legend = c("Fall Annual NAO Anomaly (hPa)","Fall 6-Month Bt Anomaly (°C)"), col = c("#075fb8","#0603cD"),
       cex = 1.3,lwd = c(4),lty = c(2),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
#### Second plot
plot.gam(FL_Lat, select =2, scale =0,ylab = expression(bold("PE on Mean Latitude")), xlab = expression(bold("Annual Mean Bottom Temperature Anomalies (°C)")), cex.lab=1.6,cex.axis=1.6,col = "#075fb8",shade = TRUE,shade.col=t_col("#075fb8",70,"plot_blt"),lwd = 4, lty=2,rug=FALSE,xlim = c(-0.55,1.4),ylim = c(-0.0025,0.009))
rug(distribution_fall$Annual_os_bt_anomaly, ticksize = 0.07, side = 1, lwd = 2.7, col = "blue")
abline(h=0, lty=2, col="black", lwd=2.0)
par(new = TRUE) #plot spring
plot(SP_numtow, select =2, scale =0,ylab = "", xlab = "",col="#FF0000",axes = FALSE,shade = TRUE,
     shade.col=t_col("#FFADAD",45,"plot_red"),lwd = 4,lty=2,xlim = c(-0.55,1.4),ylim = c(-0.0025,0.009),rug=FALSE)
rug(distribution_spring$Annual_mf_bt_anomaly, ticksize = 0.05, side = 1, lwd = 2.9, col = "#FF0000")
legend("topleft", inset=0.04, # position
       legend = c("Fall","Spring"), col = c("#075fb8","#FF0000"),
       cex = 1.3,lwd = c(4),lty = c(2),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border
#### Third plot

plot.gam(FL_Lat, select =3, scale =0,ylab = expression(bold("PE on Mean Latitude")), xlab = expression(bold("Am Plaice Mean SSB (kg/tow)")), cex.lab=1.6,cex.axis=1.3,col = "#075fb8",shade = TRUE,shade.col=t_col("#075fb8",70,"plot_blt"),lwd = 4, lty=2,rug=FALSE,xlim = c(0.7,6),ylim = c(-0.0025,0.009))
rug(distribution_fall$SSB, ticksize = 0.07, side = 1, lwd = 2.7, col = "blue")
abline(h=0, lty=2, col="black", lwd=2.0)
par(new = TRUE) #plot spring
plot(SP_numtow, select =3, scale =0,ylab = "", xlab = "",col="#FF0000",axes = FALSE,shade = TRUE,
     shade.col=t_col("#FFADAD",45,"plot_red"),lwd = 4,lty=2,xlim = c(0.7,6),ylim = c(-0.0025,0.009),rug=FALSE)
rug(distribution_spring$SSB, ticksize = 0.05, side = 1, lwd = 2.9, col = "#FF0000")
legend("topleft", inset=0.04, # position
       legend = c("Fall","Spring"), col = c("#075fb8","#FF0000"),
       cex = 1.3,lwd = c(4),lty = c(2),text.col = "black",
       box.col = "black",box.lty=1, box.lwd=1,bty = "o",bg="gray95") # border

