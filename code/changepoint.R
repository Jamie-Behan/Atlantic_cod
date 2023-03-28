#changepoint analysis using EnvCpt package
library(EnvCpt)
library(here)
#to make plot figures:
library(png)
library(grid)
library(gridExtra)
here()
#####################change point analysis for Age 1 recruitment data for cod####
nm <- list.files(path =here("data/Final_Data_for_Modelers/Recruitment"), pattern = ".csv", full.names = TRUE)
nm2 <- list.files(path =here("data/Final_Data_for_Modelers/Recruitment"), pattern = ".csv", full.names =FALSE)
list2env(lapply(setNames(nm, make.names(gsub("*.csv$", "",nm2))),read.csv),envir=.GlobalEnv)
rm(nm,nm2)

### WGOM Fall CP=2009; DEFAULT length####
fit_envcpt = envcpt(na.omit(WGOM_recruitment_fall$Age.1))  # Fit all models at once
fit_envcpt$summary  # Show log-likelihoods
plot(fit_envcpt,type='fit')

plot(fit_envcpt,type='aic') # plots the aic values
AIC(fit_envcpt)
fit_envcpt$meancpt
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

WGOM_recruitment_fall[28,] #get changepoint year
model=envcpt(na.omit(WGOM_recruitment_fall$Age.1),models="meancpt")
model=model$meancpt
#make plot
png(here("Figures/Raw_data_trends/Changepoint/wgomfall.png"),width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4.5,2,0.5))
plot(model,main="WGOM Fall Numbers at Age 1",ylab="Cod Recruitment Age 1", xlab="Year",type="l",lwd=3,col="#00608A",cex.lab=1.5,cex.axis=1.5,xaxt='n')
axis(side=1,at=c(1,28.5,38),labels=c("1982","2009","2019"))
abline(v=28.5,col="red",lwd=3,lty=2)
legend("topright",inset=c(0.03,0.03), legend=c("Recruitment Data", "Segmented Mean","Changepoint"), col=c("#00608A", "red", "red"), lty=c(1,1,2),lwd=c(3,1,3), cex=1.0)
dev.off()
### WGOM Spring CP=2010; DEFAULT length####
fit_envcpt = envcpt(na.omit(WGOM_recruitment_spring$Age.1))  # Fit all models at once
fit_envcpt$summary  # Show log-likelihoods
plot(fit_envcpt,type='fit')

plot(fit_envcpt,type='aic') # plots the aic values
AIC(fit_envcpt)
fit_envcpt$meancpt
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

WGOM_recruitment_spring[29,] #get changepoint year
model=envcpt(na.omit(WGOM_recruitment_spring$Age.1),models="meancpt")
model=model$meancpt
#make plot
png(here("Figures/Raw_data_trends/Changepoint/wgomspring.png"),width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4.5,2,0.5))
plot(model,main="WGOM Spring Numbers at Age 1",ylab="Cod Recruitment Age 1", xlab="Year",type="l",lwd=3,col="#00608A",cex.lab=1.5,cex.axis=1.5,xaxt='n')
axis(side=1,at=c(1,29.5,38),labels=c("1982","2010","2019"))
abline(v=29.5,col="red",lwd=3,lty=2)
legend("topright",inset=c(0.03,0.03), legend=c("Recruitment Data", "Segmented Mean","Changepoint"), col=c("#00608A", "red", "red"), lty=c(1,1,2),lwd=c(3,1,3), cex=1.0)
dev.off()
### EGOM Fall CP=1992 & 2014; DEFAULT length ####
fit_envcpt = envcpt(na.omit(EGOM_recruitment_fall$Age.1))  # Fit all models at once
fit_envcpt$summary  # Show log-likelihoods
plot(fit_envcpt,type='fit')

plot(fit_envcpt,type='aic') # plots the aic values
AIC(fit_envcpt)
fit_envcpt$meancpt
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

EGOM_recruitment_fall[c(11,33),] #get changepoint year
model=envcpt(na.omit(EGOM_recruitment_fall$Age.1),models="meancpt")
model=model$meancpt
#make plot
png(here("Figures/Raw_data_trends/Changepoint/recruitment/egomfall.png"),width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4.5,2,0.5))
plot(model,main="EGOM Fall Numbers at Age 1",ylab="Cod Recruitment Age 1", xlab="Year",type="l",lwd=3,col="#00608A",cex.lab=1.5,cex.axis=1.5,xaxt='n')
axis(side=1,at=c(1,11.5,33.5,38),labels=c("1982","1992","2014","2019"))
abline(v=11.5,col="red",lwd=3,lty=2)
abline(v=33.5,col="red",lwd=3,lty=2)
legend("topright",inset=c(0.03,0.03), legend=c("Recruitment Data", "Segmented Mean","Changepoint"), col=c("#00608A", "red", "red"), lty=c(1,1,2),lwd=c(3,1,3), cex=1.0)
dev.off()
### EGOM Spring CP=2004; DEFAULT length####
fit_envcpt = envcpt(na.omit(EGOM_recruitment_spring$Age.1))  # Fit all models at once
fit_envcpt$summary  # Show log-likelihoods
plot(fit_envcpt,type='fit')

plot(fit_envcpt,type='aic') # plots the aic values
AIC(fit_envcpt)
fit_envcpt$meancpt
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

EGOM_recruitment_spring[23,] #get changepoint year
model=envcpt(na.omit(EGOM_recruitment_spring$Age.1),models="meancpt")
model=model$meancpt
#make plot
png(here("Figures/Raw_data_trends/Changepoint/egomspring.png"),width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4.5,2,0.5))
plot(model,main="EGOM Spring Numbers at Age 1",ylab="Cod Recruitment Age 1", xlab="Year",type="l",lwd=3,col="#00608A",cex.lab=1.5,cex.axis=1.5,xaxt='n')
axis(side=1,at=c(1,23.5,38),labels=c("1982","2004","2019"))
abline(v=23.5,col="red",lwd=3,lty=2)
legend("topright",inset=c(0.03,0.03), legend=c("Recruitment Data", "Segmented Mean","Changepoint"), col=c("#00608A", "red", "red"), lty=c(1,1,2),lwd=c(3,1,3), cex=1.0)
dev.off()
### GBK Fall CP=1990 & 2010; DEFAULT length####
fit_envcpt = envcpt(na.omit(GBK_recruitment_fall$Age.1))  # Fit all models at once
fit_envcpt$summary  # Show log-likelihoods
plot(fit_envcpt,type='fit')

plot(fit_envcpt,type='aic') # plots the aic values
AIC(fit_envcpt)
fit_envcpt$meancpt
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

GBK_recruitment_fall[c(9,29),] #get changepoint year
model=envcpt(na.omit(GBK_recruitment_fall$Age.1),models="meancpt")
model=model$meancpt
#make plot
png(here("Figures/Raw_data_trends/Changepoint/gbkfall.png"),width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4.5,2,0.5))
plot(model,main="GBK Fall Numbers at Age 1",ylab="Cod Recruitment Age 1", xlab="Year",type="l",lwd=3,col="#00608A",cex.lab=1.5,cex.axis=1.5,xaxt='n')
axis(side=1,at=c(1,9.5,29.5,38),labels=c("1982","1990","2010","2019"))
abline(v=9.5,col="red",lwd=3,lty=2)
abline(v=29.5,col="red",lwd=3,lty=2)
legend("topright",inset=c(0.03,0.03), legend=c("Recruitment Data", "Segmented Mean","Changepoint"), col=c("#00608A", "red", "red"), lty=c(1,1,2),lwd=c(3,1,3), cex=1.0)
dev.off()
### GBK Spring CP=1991 & 2003, Segment min length =8####
fit_envcpt = envcpt(na.omit(GBK_recruitment_spring$Age.1,minseglen=8))  # Fit all models at once
fit_envcpt$summary  # Show log-likelihoods
plot(fit_envcpt,type='fit')

plot(fit_envcpt,type='aic') # plots the aic values
AIC(fit_envcpt)
fit_envcpt$meancpt
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est

GBK_recruitment_spring[c(10,22),] #get changepoint year
model=envcpt(na.omit(GBK_recruitment_spring$Age.1),minseglen=8,models="meancpt")
model=model$meancpt
#make plot
png(here("Figures/Raw_data_trends/Changepoint/gbkspring.png"),width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4.5,2,0.5))
plot(model,main="GBK Spring Numbers at Age 1",ylab="Cod Recruitment Age 1", xlab="Year",type="l",lwd=3,col="#00608A",cex.lab=1.5,cex.axis=1.5,xaxt='n')
axis(side=1,at=c(1,10.5,22.5,38),labels=c("1982","1991","2003","2019"))
abline(v=10.5,col="red",lwd=3,lty=2)
abline(v=22.5,col="red",lwd=3,lty=2)
legend("topright",inset=c(0.03,0.03), legend=c("Recruitment Data", "Segmented Mean","Changepoint"), col=c("#00608A", "red", "red"), lty=c(1,1,2),lwd=c(3,1,3), cex=1.0)
dev.off()
### PLOT ####
png(here("Figures/Raw_data_trends/Changepoint/Age1.png"),width = 1347,height=749, units = "px",res=130)
img1 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/wgomfall.png")), interpolate = FALSE)
img2 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/wgomspring.png")), interpolate = FALSE)
img3 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/egomfall.png")), interpolate = FALSE)
img4 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/egomspring.png")), interpolate = FALSE)
img5 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/gbkfall.png")), interpolate = FALSE)
img6 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/gbkspring.png")), interpolate = FALSE)
grid.arrange(img1,img3,img5,img2,img4,img6,ncol =3)
dev.off()
###################
###################change point analysis for R/SSB data for cod####
#### made dataframe of year + R/SSB data for every stock + season####
rssbdf<-data.frame(WGOM_recruitment_fall[c(1:38),1],
                   WGOM_recruitment_fall[c(1:38),10],WGOM_recruitment_spring[c(1:38),10],
                   EGOM_recruitment_fall[c(1:38),10],EGOM_recruitment_spring[c(1:38),10],
                   GBK_recruitment_fall[c(1:38),10],GBK_recruitment_spring[c(1:38),10],
                   SNE_recruitment_fall[c(1:38),10],SNE_recruitment_spring[c(1:38),10])
colnames(rssbdf)<-c('Year',
                    'WGOM_Fall_rssb','WGOM_Spring_rssb',
                    'EGOM_Fall_rssb','EGOM_Spring_rssb',
                    'GBK_Fall_rssb','GBK_Spring_rssb',
                    'SNE_Fall_rssb','SNE_Spring_rssb')
#### function for condition index data####
changepoint_fun<- function(data){
  for (k in 2:length(data)){
    fit_envcpt = envcpt(na.omit(data[c(1:28),k]))  # Fit all models at once
    plot(fit_envcpt,type='fit',main=colnames(data[k]))
    plot(fit_envcpt,type='aic',main=colnames(data[k]))
  }}
changepoint_fun(rssbdf)
#changepoint only found in Spring and fall GBK

###re-run Spring GBK model####
fit_envcpt = envcpt(na.omit(rssbdf$GBK_Spring_rssb))  # Fit all models at once
plot(fit_envcpt,type='fit')
plot(fit_envcpt,type='aic')
fit_envcpt$meancpt
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est
cp_year=rssbdf[fit_envcpt$meancpt@cpts,1] #get changepoint year
model=envcpt(na.omit(rssbdf$GBK_Spring_rssb),models="meancpt")
model=model$meancpt
#make plot for GBK Spring rel K
png(here("Figures/Raw_data_trends/Changepoint/recruitment/gbkspring_rssb.png"),width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4.5,2,0.7))
plot(model,main="GBK Spring",ylab="R/SSB", xlab="Year",type="l",lwd=3,col="#00608A",cex.lab=1.5,cex.axis=1.5,xaxt='n')
axis(side=1,at=c(1,(model@cpts[1:2]+.5),38),labels=c("1982",cp_year[1:2],"2019"))
abline(v=(model@cpts[1:2]+.5),col="red",lwd=3,lty=2)
legend("topright",inset=c(0.03,0.03), legend=c("Recruitment Data", "Segmented Mean","Changepoint"), col=c("#00608A", "red", "red"), lty=c(1,1,2),lwd=c(3,1,3), cex=1.0)
dev.off()
###re-run Fall GBK model####
fit_envcpt = envcpt(na.omit(rssbdf$GBK_Fall_rssb))  # Fit all models at once
plot(fit_envcpt,type='fit')
plot(fit_envcpt,type='aic')
fit_envcpt$meancpt
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est
cp_year=rssbdf[fit_envcpt$meancpt@cpts,1] #get changepoint year
model=envcpt(na.omit(rssbdf$GBK_Fall_rssb),models="meancpt")
model=model$meancpt
#make plot for GBK Fall rel K
png(here("Figures/Raw_data_trends/Changepoint/recruitment/gbkfall_rssb.png"),width = 449, height = 374.5, units = "px",res=90)
par(mar=c(4,4.5,2,0.7))
plot(model,main="GBK Fall",ylab="R/SSB", xlab="Year",type="l",lwd=3,col="#00608A",cex.lab=1.5,cex.axis=1.5,xaxt='n')
axis(side=1,at=c(1,(model@cpts[1:2]+.5),38),labels=c("1982",cp_year[1:2],"2019"))
abline(v=(model@cpts[1:2]+.5),col="red",lwd=3,lty=2)
legend("topright",inset=c(0.03,0.03), legend=c("Recruitment Data", "Segmented Mean","Changepoint"), col=c("#00608A", "red", "red"), lty=c(1,1,2),lwd=c(3,1,3), cex=1.0)
dev.off()
### PLOT ALL R/SSB ####
png(here("Figures/Raw_data_trends/Changepoint/recruitment/AllGBK_RSSB.png"),width =898,height=374.5, units = "px",res=100)
img1 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/gbkfall_rssb.png")), interpolate = FALSE)
img2 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/gbkspring_rssb.png")), interpolate = FALSE)
grid.arrange(img1,img2,ncol=2)
dev.off()
###################change point analysis for CONDITION data for cod####
nm <- list.files(path =here("data/Final_Data_for_Modelers/Growth"), pattern = ".csv", full.names = TRUE)
nm2 <- list.files(path =here("data/Final_Data_for_Modelers/Growth"), pattern = ".csv", full.names =FALSE)
list2env(lapply(setNames(nm, make.names(gsub("*.csv$", "",nm2))),read.csv),envir=.GlobalEnv)
rm(nm,nm2)

#### made dataframe of year + condition data for every stock + season####
relkdf<-data.frame(WGOM_Growth_fall[c(11:38),1],
                   WGOM_Growth_fall[c(11:38),9],WGOM_Growth_spring[c(11:38),9],
                   EGOM_Growth_fall[c(11:38),9],EGOM_Growth_spring[c(11:38),9],
                   GBK_Growth_fall[c(11:38),9],GBK_Growth_spring[c(11:38),9],
                   SNE_Growth_fall[c(11:38),9],SNE_Growth_spring[c(11:38),9])
colnames(relkdf)<-c('Year',
                 'WGOM_Fall_K','WGOM_Spring_K',
                 'EGOM_Fall_K','EGOM_Spring_K',
                 'GBK_Fall_K','GBK_Spring_K',
                 'SNE_Fall_K','SNE_Spring_K')
#### function for condition index data####
changepoint_fun<- function(data){
  for (k in 2:length(data)){
  fit_envcpt = envcpt(na.omit(data[c(1:28),k]))  # Fit all models at once
  plot(fit_envcpt,type='fit',main=colnames(data[k]))
  plot(fit_envcpt,type='aic',main=colnames(data[k]))
  }}
changepoint_fun(relkdf)
#changepoint only found in Spring GBK

###re-run Spring GBK model####
fit_envcpt = envcpt(na.omit(relkdf$GBK_Spring_K))  # Fit all models at once
plot(fit_envcpt,type='fit')
plot(fit_envcpt,type='aic')
fit_envcpt$meancpt
fit_envcpt$meancpt@cpts
fit_envcpt$meancpt@param.est
cp_year=relkdf[fit_envcpt$meancpt@cpts,1] #get changepoint year
model=envcpt(na.omit(relkdf$GBK_Spring_K),models="meancpt")
model=model$meancpt
#make plot for GBK Spring rel K
png(here("Figures/Raw_data_trends/Changepoint/growth/gbkspring_relk.png"),width = 449, height = 374.5, units = "px",res=90)
  par(mar=c(4,4.5,2,0.5))
  plot(model,main="GBK Spring",ylab="Relative Condition", xlab="Year",type="l",lwd=3,col="#00608A",cex.lab=1.5,cex.axis=1.5,xaxt='n')
  axis(side=1,at=c(1,(model@cpts+.5),38),labels=c("1982",cp_year,"2019"))
  abline(v=(model@cpts[1:2]+.5),col="red",lwd=3,lty=2)
  legend("topright",inset=c(0.03,0.03), legend=c("Growth Data", "Segmented Mean","Changepoint"), col=c("#00608A", "red", "red"), lty=c(1,1,2),lwd=c(3,1,3), cex=1.0)
dev.off()
#######################
#######################change point analysis for WAA data for cod####
#source weights at age (not anomalies) from another script
sourcePartial <- function(fn, skip=0, n=-1) {
  lines <- scan(fn, what=character(), sep="\n", skip=skip, n=n, quiet=TRUE)
  tc <- textConnection(lines)
  source(tc)
  close(tc)
}
sourcePartial(here("code/get_WAA_anomalies.R"), 1, 108)
#### made dataframe of year + condition data for every stock + season####
WAAdf<- list(WAA_WGOM_fall,WAA_WGOM_spring,WAA_EGOM_fall,WAA_EGOM_spring,WAA_GBK_fall,WAA_GBK_spring)#put all data frames into list
WAAdf<-Reduce(function(x, y) merge(x, y, by="YEAR",all=TRUE),WAAdf)#merge all data frames in list
colnames(WAAdf)<-c('Year',
                    'WGOM_Fall_WAA1','WGOM_Fall_WAA2','WGOM_Fall_WAA3','WGOM_Fall_WAA4','WGOM_Fall_WAA5','WGOM_Fall_WAA6',
                    'WGOM_Spring_WAA1','WGOM_Spring_WAA2','WGOM_Spring_WAA3','WGOM_Spring_WAA4','WGOM_Spring_WAA5','WGOM_Spring_WAA6','WGOM_Spring_WAA7',
                    'EGOM_Fall_WAA2','EGOM_Spring_WAA1',
                    'GBK_Fall_WAA1','GBK_Fall_WAA2','GBK_Fall_WAA3','GBK_Fall_WAA4','GBK_Fall_WAA5',
                    'GBK_Spring_WAA1','GBK_Spring_WAA2','GBK_Spring_WAA3','GBK_Spring_WAA4','GBK_Spring_WAA5','GBK_Spring_WAA6','GBK_Spring_WAA7')
#### function for condition index data####
changepoint_fun<- function(data){
  for (k in 2:length(data)){
    fit_envcpt = envcpt(na.omit(data[c(1:28),k]))  # Fit all models at once
    plot(fit_envcpt,type='fit',main=colnames(data[k]))
    #plot(fit_envcpt,type='aic',main=colnames(data[k]))
  }}
changepoint_fun(WAAdf)
#changepoint found in: 
#WGOM fall waa4
#WGOM spring waa3-5
#GBK fall waa2,3,5
#GBK spring waa3-7

#####make reduced WAA dataframe to rerun analysis with data known to have changepoints#####
reducedWAA<-WAAdf[,c(1,5,10,11,12,18,19,21,24:28)]

### rerun anaylsis
changepoint_fun<- function(data){
  for (k in 2:length(data)){
    fit_envcpt = envcpt(na.omit(data[c(1:28),k]))  # Fit all models at once
    
    fit_envcpt$meancpt
    fit_envcpt$meancpt@cpts
    fit_envcpt$meancpt@param.est
    cp_year=reducedWAA[fit_envcpt$meancpt@cpts,1] #get changepoint year
    model=envcpt(na.omit(data[c(1:28),k]),models="meancpt")
    model=model$meancpt
    #make plot
    png(file=paste("C:/Users/jbehan/Stock_Assessments/Atlantic_cod/Figures/Raw_data_trends/Changepoint/growth","\\", names(reducedWAA)[k] ,".png",sep=""),width = 449, height = 374.5, units = "px",res=90)
    par(mar=c(4,4.5,2,0.5))
    plot(model,main=names(reducedWAA)[k],ylab="Weight at Age (kg)", xlab="Year",type="l",lwd=3,col="#00608A",cex.lab=1.5,cex.axis=1.5,xaxt='n')
    axis(side=1,at=c(1,(model@cpts+.5),38),labels=c("1982",cp_year,"2019"))
    abline(v=(model@cpts[1]+.5),col="red",lwd=3,lty=2)
    legend("topright",inset=c(0.03,0.03), legend=c("Growth Data", "Segmented Mean","Changepoint"), col=c("#00608A", "red", "red"), lty=c(1,1,2),lwd=c(3,1,3), cex=1.0)
    dev.off()
  }}
changepoint_fun(reducedWAA)

### PLOT ALL WAA ####
png(here("Figures/Raw_data_trends/Changepoint/growth/AllWGOM_WAA.png"),width =898,height=749, units = "px",res=100)
img1 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/WGOM_Fall_WAA4.png")), interpolate = FALSE)
img2 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/WGOM_Spring_WAA3.png")), interpolate = FALSE)
img3 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/WGOM_Spring_WAA4.png")), interpolate = FALSE)
img4 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/WGOM_Spring_WAA5.png")), interpolate = FALSE)
grid.arrange(img1,img2,img3,img4,ncol=2)
dev.off()

png(here("Figures/Raw_data_trends/Changepoint/growth/AllGBK_WAA.png"),width =1796,height=749, units = "px",res=100)
img1 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/GBK_Fall_WAA2.png")), interpolate = FALSE)
img2 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/GBK_Fall_WAA3.png")), interpolate = FALSE)
img3 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/GBK_Fall_WAA5.png")), interpolate = FALSE)
img4 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/GBK_Spring_WAA3.png")), interpolate = FALSE)
img5 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/GBK_Spring_WAA4.png")), interpolate = FALSE)
img6 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/GBK_Spring_WAA5.png")), interpolate = FALSE)
img7 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/GBK_Spring_WAA6.png")), interpolate = FALSE)
img8 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/GBK_Spring_WAA7.png")), interpolate = FALSE)
grid.arrange(img1,img2,img3,img4,img5,img6,img7,img8,ncol=4)
dev.off()
#####function to get years and segmented means####
cp_yearmean<- function(data){
  for (k in 2:length(data)){
    fit_envcpt = envcpt(na.omit(data[c(1:28),k]))  # Fit all models at once
    p1<-(fit_envcpt$meancpt@param.est)
    p2<-(cp_year=reducedWAA[fit_envcpt$meancpt@cpts[1],1]) #get changepoint year
    print(p1)
    print(p2)
  }}
cp_yearmean(reducedWAA)
