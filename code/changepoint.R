#changepoint analysis using EnvCpt package
library(EnvCpt)
library(changepoint)
library(here)
#to make plot figures:
library(png)
library(grid)
library(gridExtra)
library(plotrix) #to make cropped abline
here()
#finalnumber<-which(rssbdf$Year==(na.omit(rssbdf[rssbdf[,k]==(dplyr::last(na.omit(rssbdf[c(1:38),k]))),1]))) #row number last non NA data value exists
#finalnumber<-which((rssbdf[,1])==dplyr::last(na.omit(rssbdf[rssbdf[,4]==(dplyr::last(na.omit(rssbdf[c(1:38),4]))),1])))
#### EnvCPT Package #####
#####################change point analysis for Age 1 recruitment data for cod####
nm <- list.files(path =here("data/Final_Data_for_Modelers/Recruitment"), pattern = ".csv", full.names = TRUE)
nm2 <- list.files(path =here("data/Final_Data_for_Modelers/Recruitment"), pattern = ".csv", full.names =FALSE)
list2env(lapply(setNames(nm, make.names(gsub("*.csv$", "",nm2))),read.csv),envir=.GlobalEnv)
rm(nm,nm2)
#### ENVCPT FUNCTION ####
trackyear<-data.frame(matrix(ncol = 1, nrow = 0))
colnames(trackyear) <- c('cpyear')
envcpt_fun<- function(data,dfnames,foldertype,yax,ylabel,datatype){
  for (k in 2:length(data)){
    fit_envcpt = envcpt(na.omit(data[c(1:38),k]),models="meancpt",minseglen=5)  # Fit mean model
    ints = unlist((fit_envcpt$meancpt@param.est[1])) #get mean cpt values
    cp = fit_envcpt$meancpt@cpts #get changepoint year numbers
    cp = cp[!cp %in% c(1,nrow(na.omit(data[k])))]
    cpyear<-na.omit(as.data.frame(data[c(1,k)]))[cp,1] #get changepoint year
trackyear2<-as.data.frame(cpyear)
trackyear<- rbind(trackyear,trackyear2)

    
    startyear<-dplyr::first(na.omit(data[data[,k]==(dplyr::first(na.omit(data[c(1:38),k]))),1])) #get first non NA year of data for every column
    finalyear<-dplyr::last(na.omit(data[data[,k]==(dplyr::last(na.omit(data[c(1:38),k]))),1])) #get final year of data for every column
    #make plot
    png(here(paste0("Figures/Raw_data_trends/Changepoint/",foldertype,"/EnvCPT/",dfnames[k-1],".png")),width = 449, height = 374.5, units = "px",res=90)
    par(mar=c(4,4.5,2,0.5))
    plot(data[,1],data[,k],main=paste0(c(dfnames[k-1],yax),collapse=" "),ylab=ylabel, xlab="Year",type="l",lwd=3,col="#00608A",cex.lab=1.5,cex.axis=1.2,xaxt='n')
    axis(side=1,at=c(startyear,cpyear[1],cpyear[2],finalyear),labels=c(startyear,cpyear[1],cpyear[2],finalyear))
    abline(v=c(cpyear[1],cpyear[2],cpyear[3]),col="red",lwd=3,lty=2)
    if (length(cp)==0){abline(h=ints[1],col="red",lwd=1,lty=1)}else{
      if (length(cp)==1){ablineclip(h=ints[1],col="red",lwd=1,lty=1,x1=startyear-1,x2=cpyear[1])
                          ablineclip(h=ints[2],col="red",lwd=1,lty=1,x1=cpyear[1],x2=finalyear)}else{
        if (length(cp)==2){ablineclip(h=ints[1],col="red",lwd=1,lty=1,x1=startyear-1,x2=cpyear[1])
                            ablineclip(h=ints[2],col="red",lwd=1,lty=1,x1=cpyear[1],x2=cpyear[2])
                            ablineclip(h=ints[3],col="red",lwd=1,lty=1,x1=cpyear[2],x2=finalyear)}else{
        if (length(cp)>=3){ablineclip(h=ints[1],col="red",lwd=1,lty=1,x1=startyear-1,x2=cpyear[1])
                              ablineclip(h=ints[2],col="red",lwd=1,lty=1,x1=cpyear[1],x2=cpyear[2])
                              ablineclip(h=ints[3],col="red",lwd=1,lty=1,x1=cpyear[2],x2=cpyear[3])
                              ablineclip(h=ints[4],col="red",lwd=1,lty=1,x1=cpyear[3],x2=finalyear)}
                              
    }}}
    legend("topright",inset=c(0.03,0.03), legend=c(datatype, "Segmented Mean","Changepoint"), col=c("#00608A", "red", "red"), lty=c(1,1,2),lwd=c(3,1,3), cex=1.0)
    dev.off()
  }
  list2env(trackyear, envir = .GlobalEnv)
}
####### call function for Age 1 recruitment ########
age1df<-data.frame(WGOM_recruitment_fall[c(1:38),1],
                   WGOM_recruitment_fall[c(1:38),8],WGOM_recruitment_spring[c(1:38),8],
                   EGOM_recruitment_fall[c(1:38),8],EGOM_recruitment_spring[c(1:38),8],
                   GBK_recruitment_fall[c(1:38),8],GBK_recruitment_spring[c(1:38),8],
                   SNE_recruitment_fall[c(1:38),8],SNE_recruitment_spring[c(1:38),8])
colnames(age1df)<-c('Year',
                    'WGOM_Fall_age1','WGOM_Spring_age1',
                    'EGOM_Fall_age1','EGOM_Spring_age1',
                    'GBK_Fall_age1','GBK_Spring_age1',
                    'SNE_Fall_age1','SNE_Spring_age1')
age1names<-colnames(age1df[2:7])
envcpt_fun(age1df[-c(8:9)],age1names,foldertype="recruitment",yax="Abundance",ylabel="Recruitment",datatype="Recruitment Data")
R_Envyears<-as.data.frame(cpyear)

### PLOT ALL Age 1 ####
png(here("Figures/Raw_data_trends/Changepoint/recruitment/EnvCPT/All_age1.png"),width =898,height=1123.5, units = "px",res=100)
img1 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/EnvCPT/WGOM_Fall_age1.png")), interpolate = FALSE)
img2 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/EnvCPT/WGOM_Spring_age1.png")), interpolate = FALSE)
img3 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/EnvCPT/EGOM_Fall_age1.png")), interpolate = FALSE)
img4 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/EnvCPT/EGOM_Spring_age1.png")), interpolate = FALSE)
img5 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/EnvCPT/GBK_Fall_age1.png")), interpolate = FALSE)
img6 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/EnvCPT/GBK_Spring_age1.png")), interpolate = FALSE)
grid.arrange(img1,img2,img3,img4,img5,img6,ncol=2)
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
#### call function for R/SSB data####
rssbnames<-colnames(rssbdf[2:7])
envcpt_fun(rssbdf[-c(8:9)],rssbnames,foldertype="recruitment",yax="",ylabel="Cod R/SSB",datatype="Recruitment Data")
RSSB_Envyears<-as.data.frame(cpyear)
### PLOT ALL R/SSB ####
png(here("Figures/Raw_data_trends/Changepoint/recruitment/EnvCPT/All_RSSB.png"),width =898,height=1123.5, units = "px",res=100)
img1 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/EnvCPT/WGOM_Fall_rssb.png")), interpolate = FALSE)
img2 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/EnvCPT/WGOM_Spring_rssb.png")), interpolate = FALSE)
img3 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/EnvCPT/EGOM_Fall_rssb.png")), interpolate = FALSE)
img4 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/EnvCPT/EGOM_Spring_rssb.png")), interpolate = FALSE)
img5 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/EnvCPT/GBK_Fall_rssb.png")), interpolate = FALSE)
img6 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/EnvCPT/GBK_Spring_rssb.png")), interpolate = FALSE)
grid.arrange(img1,img2,img3,img4,img5,img6,ncol=2)
dev.off()
### make recruitment density plot ####
# Density Plot
png(here("Figures/Raw_data_trends/Changepoint/recruitment/EnvCPT/Recruitment_density.png"),width =449,height=374.5, units = "px",res=100)
d <- density(RSSB_Envyears$cpyear,bw=2.8)
plot(d, main="Recruitment CP Year Trends: EnvCpt", xlab="Year",lwd=3,col="#00736D",xlim=c(1977,2023),ylim=c(0,0.1))
lines(density(R_Envyears$cpyear,bw=2.8),lwd=3,col="#ABB400",xlim=c(1977,2023),ylim=c(0,0.1))
legend("topleft", c(paste0("Numbers at Age 1 Data   N=",nrow(R_Envyears)),paste0("R/SSB Data   N=",nrow(RSSB_Envyears))), col=c("#ABB400","#00736D"),lty = c(1,1),lwd = c(3,3),cex = 0.8)
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
#### call function for condition index data####
relknames<-colnames(relkdf[2:9])
envcpt_fun(relkdf[-c(8:9)],relknames,foldertype="growth",yax="",ylabel="Cod Rel. K",datatype="Growth Data")
Relk_Envyears<-as.data.frame(cpyear)
### PLOT ALL relk ####
png(here("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/All_relk.png"),width =898,height=1123.5, units = "px",res=100)
img1 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/WGOM_Fall_K.png")), interpolate = FALSE)
img2 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/WGOM_Spring_K.png")), interpolate = FALSE)
img3 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/EGOM_Fall_K.png")), interpolate = FALSE)
img4 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/EGOM_Spring_K.png")), interpolate = FALSE)
img5 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/GBK_Fall_K.png")), interpolate = FALSE)
img6 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/GBK_Spring_K.png")), interpolate = FALSE)
grid.arrange(img1,img2,img3,img4,img5,img6,ncol=2)
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
#### call function for WAA data####
WAAnames<-colnames(WAAdf[2:28])
envcpt_fun(WAAdf,WAAnames,foldertype="growth",yax="",ylabel="Cod WAA",datatype="Growth Data")
WAA_Envyears<-as.data.frame(cpyear)
##### Density Plot#####
png(here("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/growth_density.png"),width =449,height=374.5, units = "px",res=100)
d <- density(Relk_Envyears$cpyear,bw=2.8)
plot(d, main="Growth CP Year Trends: EnvCpt", xlab="Year",lwd=3,col="#00736D",xlim=c(1977,2023),ylim=c(0,0.1))
lines(density(WAA_Envyears$cpyear,bw=2.8),lwd=3,col="#ABB400",xlim=c(1977,2023),ylim=c(0,0.1))
legend("topleft", c(paste0("Weight at Age Data   N=",nrow(WAA_Envyears)),paste0("Rel. K Data   N=",nrow(Relk_Envyears))), col=c("#ABB400","#00736D"),lty = c(1,1),lwd = c(3,3),cex = 0.8)
dev.off()
### PLOT ALL Fall WGOM WAA ####
png(here("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/AllWGOMfall_WAA.png"),width =898,height=1123.5, units = "px",res=100)
img1 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/WGOM_Fall_WAA1.png")), interpolate = FALSE)
img2 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/WGOM_Fall_WAA2.png")), interpolate = FALSE)
img3 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/WGOM_Fall_WAA3.png")), interpolate = FALSE)
img4 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/WGOM_Fall_WAA4.png")), interpolate = FALSE)
img5 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/WGOM_Fall_WAA5.png")), interpolate = FALSE)
img6 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/WGOM_Fall_WAA6.png")), interpolate = FALSE)
grid.arrange(img1,img2,img3,img4,img5,img6,ncol=2)
dev.off()
### PLOT ALL Spring WGOM WAA ####
png(here("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/AllWGOMspring_WAA.png"),width =898,height=1123.5, units = "px",res=100)
img1 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/WGOM_Spring_WAA2.png")), interpolate = FALSE)
img2 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/WGOM_Spring_WAA3.png")), interpolate = FALSE)
img3 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/WGOM_Spring_WAA4.png")), interpolate = FALSE)
img4 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/WGOM_Spring_WAA5.png")), interpolate = FALSE)
img5 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/WGOM_Spring_WAA6.png")), interpolate = FALSE)
img6 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/WGOM_Spring_WAA7.png")), interpolate = FALSE)
grid.arrange(img1,img2,img3,img4,img5,img6,ncol=2)
dev.off()
### PLOT ALL Fall GBK WAA ####
png(here("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/AllGBKfall_WAA.png"),width =898,height=1123.5, units = "px",res=100)
img1 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/GBK_Fall_WAA1.png")), interpolate = FALSE)
img2 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/GBK_Fall_WAA2.png")), interpolate = FALSE)
img3 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/GBK_Fall_WAA3.png")), interpolate = FALSE)
img4 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/GBK_Fall_WAA4.png")), interpolate = FALSE)
img5 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/GBK_Fall_WAA5.png")), interpolate = FALSE)
grid.arrange(img1,img2,img3,img4,img5,ncol=2)
dev.off()
### PLOT ALL Spring GBK WAA ####
png(here("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/AllGBKspring_WAA.png"),width =898,height=1123.5, units = "px",res=100)
img1 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/GBK_Spring_WAA1.png")), interpolate = FALSE)
img2 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/GBK_Spring_WAA2.png")), interpolate = FALSE)
img3 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/GBK_Spring_WAA3.png")), interpolate = FALSE)
img4 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/GBK_Spring_WAA4.png")), interpolate = FALSE)
img5 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/GBK_Spring_WAA5.png")), interpolate = FALSE)
img6 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/GBK_Spring_WAA6.png")), interpolate = FALSE)
grid.arrange(img1,img2,img3,img4,img5,img6,ncol=2)
dev.off()
### PLOT ALL EGOM (spring +fall) WAA ####
png(here("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/AllEGOM_WAA.png"),width =898,height=374.5, units = "px",res=100)
img1 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/EGOM_Spring_WAA1.png")), interpolate = FALSE)
img2 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/EnvCPT/EGOM_Fall_WAA2.png")), interpolate = FALSE)
grid.arrange(img1,img2,ncol=2)
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
#####
#####
#####
#####
##### Changepoint package #####
####Changepoint Function for Changepoint package####
trackyear<-data.frame(matrix(ncol = 1, nrow = 0))
colnames(trackyear) <- c('cpyear')
changepoint_fun<- function(data,dfnames,foldertype,yax,ylabel,datatype){
  for (k in 2:length(data)){
    fit_cpt = changepoint::cpt.mean(na.omit(data[c(1:38),k]),method = "PELT",penalty="AIC",minseglen = 5)  # Fit mean model
    ints = param.est(fit_cpt)$mean #get mean cpt values
    cp = cpts(fit_cpt) #get changepoint year numbers
    cpyear<-na.omit(as.data.frame(data[c(1,k)]))[cp,1] #get changepoint year
    trackyear2<-as.data.frame(cpyear)
    trackyear<- rbind(trackyear,trackyear2)
    
    startyear<-dplyr::first(na.omit(data[data[,k]==(dplyr::first(na.omit(data[c(1:38),k]))),1])) #get first non NA year of data for every column
    finalyear<-dplyr::last(na.omit(data[data[,k]==(dplyr::last(na.omit(data[c(1:38),k]))),1])) #get final year of data for every column
    #make plot
    png(here(paste0("Figures/Raw_data_trends/Changepoint/",foldertype,"/changepoint/",dfnames[k-1],".png")),width = 449, height = 374.5, units = "px",res=90)
    par(mar=c(4,4.5,2,0.5))
    plot(data[,1],data[,k],main=paste0(c(dfnames[k-1],yax),collapse=" "),ylab=ylabel, xlab="Year",type="l",lwd=3,col="#00608A",cex.lab=1.5,cex.axis=1.2,xaxt='n')
    
    abline(v=c(cpyear[1],cpyear[2],cpyear[3]),col="red",lwd=3,lty=2)
    if (length(cp)==0){abline(h=ints[1],col="red",lwd=1,lty=1)}else{
      if (length(cp)==1){ablineclip(h=ints[1],col="red",lwd=1,lty=1,x1=startyear-1,x2=cpyear[1])
        ablineclip(h=ints[2],col="red",lwd=1,lty=1,x1=cpyear[1],x2=finalyear)}else{
          if (length(cp)==2){ablineclip(h=ints[1],col="red",lwd=1,lty=1,x1=startyear-1,x2=cpyear[1])
            ablineclip(h=ints[2],col="red",lwd=1,lty=1,x1=cpyear[1],x2=cpyear[2])
            ablineclip(h=ints[3],col="red",lwd=1,lty=1,x1=cpyear[2],x2=finalyear)}else{
              if (length(cp)>=3){ablineclip(h=ints[1],col="red",lwd=1,lty=1,x1=startyear-1,x2=cpyear[1])
                ablineclip(h=ints[2],col="red",lwd=1,lty=1,x1=cpyear[1],x2=cpyear[2])
                ablineclip(h=ints[3],col="red",lwd=1,lty=1,x1=cpyear[2],x2=cpyear[3])
                ablineclip(h=ints[4],col="red",lwd=1,lty=1,x1=cpyear[3],x2=finalyear)}
              
            }}}
    
    axis(side=1,at=c(startyear,cpyear,finalyear),labels=c(startyear,cpyear,finalyear))
#    abline(v=cpyear,col="red",lwd=3,lty=2)
#    ablineclip(h=ints[1],col="red",lwd=1,lty=1,x1=startyear-1,x2=if(length(cp)==0){finalyear}else{cpyear})
#    ablineclip(h=ints[2],col="red",lwd=1,lty=1,x1=if(length(cp)==0){startyear-1}else{cpyear},x2=finalyear)
    legend("topright",inset=c(0.03,0.03), legend=c(datatype, "Segmented Mean","Changepoint"), col=c("#00608A", "red", "red"), lty=c(1,1,2),lwd=c(3,1,3), cex=1.0)
    dev.off()
  }
  list2env(trackyear, envir = .GlobalEnv)
  }
#####################change point analysis for Age 1 recruitment data for cod####
nm <- list.files(path =here("data/Final_Data_for_Modelers/Recruitment"), pattern = ".csv", full.names = TRUE)
nm2 <- list.files(path =here("data/Final_Data_for_Modelers/Recruitment"), pattern = ".csv", full.names =FALSE)
list2env(lapply(setNames(nm, make.names(gsub("*.csv$", "",nm2))),read.csv),envir=.GlobalEnv)
rm(nm,nm2)
#### Call function for age1 data####
#### made dataframe of year + age1 data for every stock + season####
age1df<-data.frame(WGOM_recruitment_fall[c(1:38),1],
                   WGOM_recruitment_fall[c(1:38),8],WGOM_recruitment_spring[c(1:38),8],
                   EGOM_recruitment_fall[c(1:38),8],EGOM_recruitment_spring[c(1:38),8],
                   GBK_recruitment_fall[c(1:38),8],GBK_recruitment_spring[c(1:38),8],
                   SNE_recruitment_fall[c(1:38),8],SNE_recruitment_spring[c(1:38),8])
colnames(age1df)<-c('Year',
                    'WGOM_Fall_age1','WGOM_Spring_age1',
                    'EGOM_Fall_age1','EGOM_Spring_age1',
                    'GBK_Fall_age1','GBK_Spring_age1',
                    'SNE_Fall_age1','SNE_Spring_age1')
age1names<-colnames(age1df[2:7])
changepoint_fun(age1df[-c(8:9)],age1names,foldertype="recruitment",yax="Abundance",ylabel="Recruitment",datatype="Recruitment Data")
R_cpyears<-as.data.frame(cpyear)
### PLOT ALL age1 ####
png(here("Figures/Raw_data_trends/Changepoint/recruitment/changepoint/All_age1.png"),width =898,height=1123.5, units = "px",res=100)
img1 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/changepoint/WGOM_Fall_age1.png")), interpolate = FALSE)
img2 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/changepoint/WGOM_Spring_age1.png")), interpolate = FALSE)
img3 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/changepoint/EGOM_Fall_age1.png")), interpolate = FALSE)
img4 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/changepoint/EGOM_Spring_age1.png")), interpolate = FALSE)
img5 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/changepoint/GBK_Fall_age1.png")), interpolate = FALSE)
img6 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/changepoint/GBK_Spring_age1.png")), interpolate = FALSE)
grid.arrange(img1,img2,img3,img4,img5,img6,ncol=2)
dev.off()
#### Call function for lage1 data####
#### made dataframe of year + lage1 data for every stock + season####
lage1df<-data.frame(WGOM_recruitment_fall[c(1:38),1],
                   WGOM_recruitment_fall[c(1:38),11],WGOM_recruitment_spring[c(1:38),11],
                   EGOM_recruitment_fall[c(1:38),11],EGOM_recruitment_spring[c(1:38),11],
                   GBK_recruitment_fall[c(1:38),11],GBK_recruitment_spring[c(1:38),11],
                   SNE_recruitment_fall[c(1:38),11],SNE_recruitment_spring[c(1:38),11])
colnames(lage1df)<-c('Year',
                    'WGOM_Fall_lage1','WGOM_Spring_lage1',
                    'EGOM_Fall_lage1','EGOM_Spring_lage1',
                    'GBK_Fall_lage1','GBK_Spring_lage1',
                    'SNE_Fall_lage1','SNE_Spring_lage1')
lage1names<-colnames(lage1df[c(2:7)])
changepoint_fun(lage1df[-c(8:9)],lage1names,foldertype="recruitment",yax="log(Abundance)",ylabel="Recruitment",datatype="Recruitment Data")
### PLOT ALL lage1 ####
png(here("Figures/Raw_data_trends/Changepoint/recruitment/changepoint/All_lage1.png"),width =898,height=1123.5, units = "px",res=100)
img1 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/changepoint/WGOM_Fall_lage1.png")), interpolate = FALSE)
img2 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/changepoint/WGOM_Spring_lage1.png")), interpolate = FALSE)
img3 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/changepoint/EGOM_Fall_lage1.png")), interpolate = FALSE)
img4 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/changepoint/EGOM_Spring_lage1.png")), interpolate = FALSE)
img5 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/changepoint/GBK_Fall_lage1.png")), interpolate = FALSE)
img6 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/changepoint/GBK_Spring_lage1.png")), interpolate = FALSE)
grid.arrange(img1,img2,img3,img4,img5,img6,ncol=2)
dev.off()
#### Call function for R/SSB data####
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
#### Call function for RSSB data####
rssbnames<-colnames(rssbdf[2:7])
changepoint_fun(rssbdf[-c(8:9)],rssbnames,foldertype="recruitment",yax="R/SSB",ylabel="Cod R/SSB",datatype="Recruitment Data")
RSSB_cpyears<-as.data.frame(cpyear)
### PLOT ALL R/SSB ####
png(here("Figures/Raw_data_trends/Changepoint/recruitment/changepoint/All_RSSB.png"),width =898,height=1123.5, units = "px",res=100)
img1 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/changepoint/WGOM_Fall_rssb.png")), interpolate = FALSE)
img2 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/changepoint/WGOM_Spring_rssb.png")), interpolate = FALSE)
img3 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/changepoint/EGOM_Fall_rssb.png")), interpolate = FALSE)
img4 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/changepoint/EGOM_Spring_rssb.png")), interpolate = FALSE)
img5 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/changepoint/GBK_Fall_rssb.png")), interpolate = FALSE)
img6 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/recruitment/changepoint/GBK_Spring_rssb.png")), interpolate = FALSE)
grid.arrange(img1,img2,img3,img4,img5,img6,ncol=2)
dev.off()
### make recruitment density plot ####
# Density Plot
png(here("Figures/Raw_data_trends/Changepoint/recruitment/changepoint/Recruitment_density.png"),width =449,height=374.5, units = "px",res=100)
d <- density(RSSB_cpyears$cpyear,bw=2.8)
plot(d, main="Recruitment CP Year Trends: Changepoint", xlab="Year",lwd=3,col="#00736D",xlim=c(1977,2023),ylim=c(0,0.18))
lines(density(R_cpyears$cpyear,bw=2.8),lwd=3,col="#ABB400",xlim=c(1977,2023),ylim=c(0,0.1))
legend("topleft", c(paste0("Numbers at Age 1 Data   N=",nrow(R_cpyears)),paste0("R/SSB Data   N=",nrow(RSSB_cpyears))), col=c("#ABB400","#00736D"),lty = c(1,1),lwd = c(3,3),cex = 0.8)
#legend("topleft", c(paste0("R/SSB Data   N=",nrow(RSSB_cpyears))), col=c("#00736D"),lty = c(1,1),lwd = c(3,3),cex = 0.8)

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
relknames<-colnames(relkdf[c(2:7,9)])
changepoint_fun(relkdf[c(1:7,9)],relknames,foldertype="growth",yax="Relative Condition",ylabel="Cod Rel. K",datatype="Growth Data")
Relk_cpyears<-as.data.frame(cpyear)
### PLOT ALL RelK ####
png(here("Figures/Raw_data_trends/Changepoint/growth/changepoint/All_relk.png"),width =898,height=1123.5, units = "px",res=100)
img1 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/WGOM_Fall_K.png")), interpolate = FALSE)
img2 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/WGOM_Spring_K.png")), interpolate = FALSE)
img3 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/EGOM_Fall_K.png")), interpolate = FALSE)
img4 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/EGOM_Spring_K.png")), interpolate = FALSE)
img5 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/GBK_Fall_K.png")), interpolate = FALSE)
img6 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/GBK_Spring_K.png")), interpolate = FALSE)
grid.arrange(img1,img2,img3,img4,img5,img6,ncol=2)
dev.off()
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
WAAnames<-colnames(WAAdf[2:27])
changepoint_fun(WAAdf,WAAnames,foldertype="growth",yax="",ylabel="Cod WAA",datatype="Growth Data")
WAA_cpyears<-as.data.frame(cpyear)
### PLOT ALL WGOM FALL WAA ####
png(here("Figures/Raw_data_trends/Changepoint/growth/changepoint/AllWGOMfall_WAA.png"),width =898,height=1123.5, units = "px",res=100)
img1 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/WGOM_Fall_WAA1.png")), interpolate = FALSE)
img2 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/WGOM_Fall_WAA2.png")), interpolate = FALSE)
img3 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/WGOM_Fall_WAA3.png")), interpolate = FALSE)
img4 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/WGOM_Fall_WAA4.png")), interpolate = FALSE)
img5 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/WGOM_Fall_WAA5.png")), interpolate = FALSE)
img6 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/WGOM_Fall_WAA6.png")), interpolate = FALSE)
grid.arrange(img1,img2,img3,img4,img5,img6,ncol=2)
dev.off()
### PLOT ALL WGOM Spring WAA ####
png(here("Figures/Raw_data_trends/Changepoint/growth/changepoint/AllWGOMspring_WAA.png"),width =898,height=1123.5, units = "px",res=100)
img1 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/WGOM_Spring_WAA2.png")), interpolate = FALSE)
img2 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/WGOM_Spring_WAA3.png")), interpolate = FALSE)
img3 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/WGOM_Spring_WAA4.png")), interpolate = FALSE)
img4 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/WGOM_Spring_WAA5.png")), interpolate = FALSE)
img5 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/WGOM_Spring_WAA6.png")), interpolate = FALSE)
img6 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/WGOM_Spring_WAA7.png")), interpolate = FALSE)
grid.arrange(img1,img2,img3,img4,img5,img6,ncol=2)
dev.off()
### PLOT ALL GBK+EGOM FALL WAA ####
png(here("Figures/Raw_data_trends/Changepoint/growth/changepoint/AllGBKfall_WAA.png"),width =898,height=1123.5, units = "px",res=100)
img1 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/GBK_Fall_WAA1.png")), interpolate = FALSE)
img2 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/GBK_Fall_WAA2.png")), interpolate = FALSE)
img3 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/GBK_Fall_WAA3.png")), interpolate = FALSE)
img4 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/GBK_Fall_WAA4.png")), interpolate = FALSE)
img5 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/GBK_Fall_WAA5.png")), interpolate = FALSE)
#img6 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/EGOM_Fall_WAA2.png")), interpolate = FALSE)
grid.arrange(img1,img2,img3,img4,img5,ncol=2)
dev.off()
### PLOT ALL GBK Spring WAA ####
png(here("Figures/Raw_data_trends/Changepoint/growth/changepoint/AllGBKspring_WAA.png"),width =898,height=1123.5, units = "px",res=100)
img1 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/GBK_Spring_WAA1.png")), interpolate = FALSE)
img2 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/GBK_Spring_WAA2.png")), interpolate = FALSE)
img3 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/GBK_Spring_WAA3.png")), interpolate = FALSE)
img4 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/GBK_Spring_WAA4.png")), interpolate = FALSE)
img5 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/GBK_Spring_WAA5.png")), interpolate = FALSE)
img6 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/GBK_Spring_WAA6.png")), interpolate = FALSE)
grid.arrange(img1,img2,img3,img4,img5,img6,ncol=2)
dev.off()
### PLOT ALL EGOM (Spring+Fall) WAA ####
png(here("Figures/Raw_data_trends/Changepoint/growth/changepoint/AllEGOM_WAA.png"),width =898,height=374.5, units = "px",res=100)
img1 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/EGOM_Fall_WAA2.png")), interpolate = FALSE)
img2 <-  rasterGrob(as.raster(readPNG("Figures/Raw_data_trends/Changepoint/growth/changepoint/EGOM_Spring_WAA1.png")), interpolate = FALSE)
grid.arrange(img1,img2,ncol=2)
dev.off()

### make growth density plot ####
# Density Plot
png(here("Figures/Raw_data_trends/Changepoint/growth/changepoint/growth_density.png"),width =449,height=374.5, units = "px",res=100)
d <- density(WAA_cpyears$cpyear,bw=2.8)
plot(d, main="Growth CP Year Trends: Changepoint", xlab="Year",lwd=3,col="#ABB400",xlim=c(1977,2023),ylim=c(0,0.1))
#lines(density(WAA_cpyears$cpyear,bw=2.8),lwd=3,col="#ABB400",xlim=c(1977,2023),ylim=c(0,0.1))
lines(density(c(2003,2007,2001,2011,2009,2013,1999,2012,2003,2011),bw=2.8),lwd=3,col="#003f5b",xlim=c(1977,2023),ylim=c(0,0.1)) #add line for condition cluster results
lines(density(c(1991,1995,1990,2001,1990,1993,1998,2011,1991),bw=2.8),lwd=3,col="#717700",xlim=c(1977,2023),ylim=c(0,0.1)) #add line for WAA cluster results
legend("topleft", c(paste0("Weight at Age Data   N=",nrow(WAA_cpyears)),"Cluster Analysis: Condition Data","Cluster Analysis: WAA Data"), col=c("#ABB400","#003f5b","#717700"),lty = c(1,1,1),lwd = c(3,3,3),cex = 0.8)
dev.off()
################################################### Chronological Clustering Code ####
library(tidyverse)
library(gghighlight)
library(rioja)

##### make sure the following dfs are still in environment:####
age1df_2<-reshape2::melt(age1df ,  id.vars = 'Year', variable.name = 'stockarea')
rssbdf_2<-reshape2::melt(rssbdf ,  id.vars = 'Year', variable.name = 'stockarea')
relkdf_2<-reshape2::melt(relkdf ,  id.vars = 'Year', variable.name = 'stockarea')
WAAdf_2<-reshape2::melt(WAAdf ,  id.vars = 'Year', variable.name = 'stockarea')

#### Plot####

ggplot(age1df_2, aes(Year, value, col=stockarea)) +
  geom_line(stat="identity",size=1) + ylab("Age 1 Abundance") + theme_minimal()
ggplot(rssbdf_2, aes(Year, value, col=stockarea)) +
  geom_line(stat="identity",size=1) + ylab("R/SSB") + theme_minimal()
ggplot(relkdf_2, aes(Year, value, col=stockarea)) +
  geom_line(stat="identity",size=1) + ylab("Age 1 Abundance") + theme_minimal()
ggplot(WAAdf_2[grep("Spring", WAAdf_2$stockarea), ], aes(Year, value, col=stockarea)) +
  geom_line(stat="identity",size=1) + ylab("Age 1 Abundance") + theme_minimal()
ggplot(WAAdf_2[grep("Fall", WAAdf_2$stockarea), ], aes(Year, value, col=stockarea)) +
  geom_line(stat="identity",size=1) + ylab("Age 1 Abundance") + theme_minimal()

#### Prep for cluster analysis####
age1df_WGOMF<- age1df_2[grep("WGOM_Fall", age1df_2$stockarea), ]
age1df_EGOMF<- age1df_2[grep("EGOM_Fall", age1df_2$stockarea), ]
age1df_GBKF<- age1df_2[grep("GBK_Fall", age1df_2$stockarea), ]

age1df_WGOMS<- age1df_2[grep("WGOM_Spring", age1df_2$stockarea), ]
age1df_EGOMS<- age1df_2[grep("EGOM_Spring", age1df_2$stockarea), ]
age1df_GBKS<- age1df_2[grep("GBK_Spring", age1df_2$stockarea), ]

rssbdf_WGOMF<- rssbdf_2[grep("WGOM_Fall", rssbdf_2$stockarea), ]
rssbdf_EGOMF<- rssbdf_2[grep("EGOM_Fall", rssbdf_2$stockarea), ]
rssbdf_GBKF<- rssbdf_2[grep("GBK_Fall", rssbdf_2$stockarea), ]

rssbdf_WGOMS<- rssbdf_2[grep("WGOM_Spring", rssbdf_2$stockarea), ]
rssbdf_EGOMS<- rssbdf_2[grep("EGOM_Spring", rssbdf_2$stockarea), ]
rssbdf_GBKS<- rssbdf_2[grep("GBK_Spring", rssbdf_2$stockarea), ]

relkdf_WGOMF<- relkdf_2[grep("WGOM_Fall", relkdf_2$stockarea), ]
relkdf_EGOMF<- relkdf_2[grep("EGOM_Fall", relkdf_2$stockarea), ]
relkdf_GBKF<- relkdf_2[grep("GBK_Fall", relkdf_2$stockarea), ]

relkdf_WGOMS<- relkdf_2[grep("WGOM_Spring", relkdf_2$stockarea), ]
relkdf_EGOMS<- relkdf_2[grep("EGOM_Spring", relkdf_2$stockarea), ]
relkdf_GBKS<- relkdf_2[grep("GBK_Spring", relkdf_2$stockarea), ]

WAAdf_WGOMF<- WAAdf_2[grep("WGOM_Fall", WAAdf_2$stockarea), ]
WAAdf_EGOMF<- WAAdf_2[grep("EGOM_Fall", WAAdf_2$stockarea), ]
WAAdf_GBKF<- WAAdf_2[grep("GBK_Fall", WAAdf_2$stockarea), ]

WAAdf_WGOMS<- WAAdf_2[grep("WGOM_Spring", WAAdf_2$stockarea), ]
WAAdf_EGOMS<- WAAdf_2[grep("EGOM_Spring", WAAdf_2$stockarea), ]
WAAdf_GBKS<- WAAdf_2[grep("GBK_Spring", WAAdf_2$stockarea), ]

clusterprep<- function (data_2){
  value <- data_2 %>%
    select(value, Year, stockarea) %>%
    drop_na()# must drop NAs for clustering to work

  data_2 <- value %>%
    spread(Year, value)
}
#fall
age1df_WGOMF<-clusterprep(age1df_WGOMF)
age1df_EGOMF<-clusterprep(age1df_EGOMF)
age1df_GBKF<-clusterprep(age1df_GBKF)

rssbdf_WGOMF<-clusterprep(rssbdf_WGOMF)
rssbdf_EGOMF<-clusterprep(rssbdf_EGOMF)
rssbdf_GBKF<-clusterprep(rssbdf_GBKF)

relkdf_WGOMF<-clusterprep(relkdf_WGOMF)
relkdf_EGOMF<-clusterprep(relkdf_EGOMF)
relkdf_GBKF<-clusterprep(relkdf_GBKF)

WAAdf_WGOMF<-clusterprep(WAAdf_WGOMF)
WAAdf_EGOMF<-clusterprep(WAAdf_EGOMF)
WAAdf_GBKF<-clusterprep(WAAdf_GBKF)
#spring
age1df_WGOMS<-clusterprep(age1df_WGOMS)
age1df_EGOMS<-clusterprep(age1df_EGOMS)
age1df_GBKS<-clusterprep(age1df_GBKS)

rssbdf_WGOMS<-clusterprep(rssbdf_WGOMS)
rssbdf_EGOMS<-clusterprep(rssbdf_EGOMS)
rssbdf_GBKS<-clusterprep(rssbdf_GBKS)

relkdf_WGOMS<-clusterprep(relkdf_WGOMS)
relkdf_EGOMS<-clusterprep(relkdf_EGOMS)
relkdf_GBKS<-clusterprep(relkdf_GBKS)

WAAdf_WGOMS<-clusterprep(WAAdf_WGOMS)
WAAdf_EGOMS<-clusterprep(WAAdf_EGOMS)
WAAdf_GBKS<-clusterprep(WAAdf_GBKS)

age1df_2<-clusterprep(age1df_2)
rssbdf_2<-clusterprep(rssbdf_2)
relkdf_2<-clusterprep(relkdf_2)
WAAdf_2<-clusterprep(WAAdf_2)

#### Run the chronological cluster analysis####
#chclust function####
chclustfun<-function (data, Stockarea){
  
  dat <- t(data[-1])
  dat_dist <- dist(dat,method="euclidean")  
  datfit <- chclust(dat_dist,method="coniss") 
  
  # Plot the cluster analysis
  par(mar=c(2.75,0.2,2.5,0.2),cex=1)
  plot(datfit,hang=-1,main = Stockarea, lwd = 2,cex.main=1.1,axes=FALSE,cex=0.7)
  par(lwd=3, mar=c(0,0,0,0))
  rect.hclust(datfit,k=3,border="cadetblue")
}

### PLOT ALL age1 recruitment ####
png(here("Figures/Raw_data_trends/Changepoint/cluster/All_age1.png"),width =898,height=800, units = "px",res=100)
par(mfrow = c(3, 2))
chclustfun(data=age1df_WGOMF, "WGOM Fall Recruitment")
chclustfun(data=age1df_WGOMS, "WGOM Spring Recruitment")
chclustfun(data=age1df_EGOMF, "EGOM Fall Recruitment")
chclustfun(data=age1df_EGOMS, "EGOM Spring Recruitment")
chclustfun(data=age1df_GBKF, "GBK Fall Recruitment")
chclustfun(data=age1df_GBKS, "GBK Spring Recruitment")
dev.off()
### PLOT ALL rssb recruitment ####
png(here("Figures/Raw_data_trends/Changepoint/cluster/All_rssb.png"),width =898,height=800, units = "px",res=120)
par(mfrow = c(3, 2))
chclustfun(data=rssbdf_WGOMF, "WGOM Fall R/SSB")
chclustfun(data=rssbdf_WGOMS, "WGOM Spring R/SSB")
chclustfun(data=rssbdf_EGOMF, "EGOM Fall R/SSB")
chclustfun(data=rssbdf_EGOMS, "EGOM Spring R/SSB")
chclustfun(data=rssbdf_GBKF, "GBK Fall R/SSB")
chclustfun(data=rssbdf_GBKS, "GBK Spring R/SSB")
dev.off()
### PLOT ALL relk ####
png(here("Figures/Raw_data_trends/Changepoint/cluster/All_relk.png"),width =898,height=800, units = "px",res=120)
par(mfrow = c(3, 2))
chclustfun(data=relkdf_WGOMF, "WGOM Fall Rel. Condition")
chclustfun(data=relkdf_WGOMS, "WGOM Spring Rel. Condition")
chclustfun(data=relkdf_EGOMF, "EGOM Fall Rel. Condition")
chclustfun(data=relkdf_EGOMS, "EGOM Spring Rel. Condition")
chclustfun(data=relkdf_GBKF, "GBK Fall Rel. Condition")
chclustfun(data=relkdf_GBKS, "GBK Spring Rel. Condition")
dev.off()
### PLOT ALL WGOM WAA ####
png(here("Figures/Raw_data_trends/Changepoint/cluster/All_WGOM_WAA.png"),width =1400,height=500, units = "px",res=120)
par(mfrow = c(1, 2))
chclustfun(data=WAAdf_WGOMF, "WGOM Fall WAA")
chclustfun(data=WAAdf_WGOMS, "WGOM Spring WAA")
dev.off()
### PLOT ALL EGOM WAA ####
png(here("Figures/Raw_data_trends/Changepoint/cluster/All_EGOM_WAA.png"),width =1400,height=500, units = "px",res=120)
par(mfrow = c(1, 2))
chclustfun(data=WAAdf_EGOMF, "EGOM Fall WAA")
chclustfun(data=WAAdf_EGOMS, "EGOM Spring WAA")
dev.off()
### PLOT ALL EGOM WAA ####
png(here("Figures/Raw_data_trends/Changepoint/cluster/All_GBK_WAA.png"),width =1400,height=500, units = "px",res=120)
par(mfrow = c(1, 2))
chclustfun(data=WAAdf_GBKF, "GBK Fall WAA")
chclustfun(data=WAAdf_GBKS, "GBK Spring WAA")
dev.off()
### PLOT ALL stock areas ####
png(here("Figures/Raw_data_trends/Changepoint/cluster/All_stockareas.png"),width =1200,height=1100, units = "px",res=100)
par(mfcol = c(2, 2))
chclustfun(age1df_2, "All Stock Areas: Recruitment")
chclustfun(rssbdf_2, "All Stock Areas: R/SSB")
chclustfun(relkdf_2, "All Stock Areas: Rel. Condition")
chclustfun(WAAdf_2, "All Stock Areas: WAA")
dev.off()
### Make comparison density plots####
### Recruitment ####
# Density Plot
png(here("Figures/Raw_data_trends/Changepoint/recruitment/recruitment_density.png"),width =449,height=374.5, units = "px",res=100)
d <- density(R_Envyears$cpyear,bw=2.8)
plot(d, main="Changepoint Year Trends: Recruitment", xlab="Year",lwd=3,col="#ABB400",xlim=c(1977,2023),ylim=c(0,0.13))
lines(density(R_cpyears$cpyear,bw=2.8),lwd=3,col="#003f5b",xlim=c(1977,2023),ylim=c(0,0.13))# no numbers
lines(density(c(1988,2010,1988,2000,1986,1991),bw=2.8),lwd=3,col="#717700",xlim=c(1977,2023),ylim=c(0,0.13)) #add line for condition cluster results
legend("topleft", c(paste0("EnvCpt Package  N=",nrow(R_Envyears)),paste0("Changepoint Package  N=",nrow(R_cpyears)),"Cluster Analysis  N=6"), col=c("#ABB400","#003f5b","#717700"),lty = c(1,1,1),lwd = c(3,3,3),cex = 0.8)
dev.off()
### R/SSB ####
# Density Plot
png(here("Figures/Raw_data_trends/Changepoint/recruitment/rssb_density.png"),width =449,height=374.5, units = "px",res=100)
d <- density(RSSB_Envyears$cpyear,bw=2.8)
plot(d, main="Changepoint Year Trends: R/SSB", xlab="Year",lwd=3,col="#ABB400",xlim=c(1977,2023),ylim=c(0,0.13))
lines(density(RSSB_cpyears$cpyear,bw=2.8),lwd=3,col="#003f5b",xlim=c(1977,2023),ylim=c(0,0.13))# no numbers
lines(density(c(1988,2006,1999,2000,1991),bw=2.8),lwd=3,col="#717700",xlim=c(1977,2023),ylim=c(0,0.13)) #add line for condition cluster results
legend("topleft", c(paste0("EnvCpt Package  N=",nrow(RSSB_Envyears)),paste0("Changepoint Package  N=",nrow(RSSB_cpyears)),"Cluster Analysis  N=5"), col=c("#ABB400","#003f5b","#717700"),lty = c(1,1,1),lwd = c(3,3,3),cex = 0.8)
dev.off()
### Condition ####
# Density Plot
png(here("Figures/Raw_data_trends/Changepoint/growth/condition_density.png"),width =449,height=374.5, units = "px",res=100)
d <- density(Relk_Envyears$cpyear,bw=2.8)
plot(d, main="Changepoint Year Trends: Condition", xlab="Year",lwd=3,col="#ABB400",xlim=c(1977,2023),ylim=c(0,0.13))
#lines(density(Relk_cpyears$cpyear,bw=2.8),lwd=3,col="#003f5b",xlim=c(1977,2023),ylim=c(0,0.13))# no numbers
lines(density(c(2003,2007,2001,2011,2009,2013,1999,2012,2003,2011),bw=2.8),lwd=3,col="#717700",xlim=c(1977,2023),ylim=c(0,0.13)) #add line for condition cluster results
legend("topleft", c(paste0("EnvCpt Package  N=",nrow(Relk_Envyears)),paste0("Changepoint Package  N=",nrow(Relk_cpyears)),"Cluster Analysis  N=10"), col=c("#ABB400","#003f5b","#717700"),lty = c(1,1,1),lwd = c(3,3,3),cex = 0.8)
dev.off()
### WAA ####
# Density Plot
png(here("Figures/Raw_data_trends/Changepoint/growth/WAA_density.png"),width =449,height=374.5, units = "px",res=100)
d <- density(WAA_Envyears$cpyear,bw=2.8)
plot(d, main="Changepoint Year Trends: Weight at Age", xlab="Year",lwd=3,col="#ABB400",xlim=c(1977,2023),ylim=c(0,0.13))
lines(density(WAA_cpyears$cpyear,bw=2.8),lwd=3,col="#003f5b",xlim=c(1977,2023),ylim=c(0,0.13))# no numbers
lines(density(c(1991,1995,1990,2001,1990,1993,1998,2011,1991),bw=2.8),lwd=3,col="#717700",xlim=c(1977,2023),ylim=c(0,0.13)) #add line for condition cluster results
legend("topleft", c(paste0("EnvCpt Package  N=",nrow(WAA_Envyears)),paste0("Changepoint Package  N=",nrow(WAA_cpyears)),"Cluster Analysis  N=9 (ages combined)"), col=c("#ABB400","#003f5b","#717700"),lty = c(1,1,1),lwd = c(3,3,3),cex = 0.8)
dev.off()