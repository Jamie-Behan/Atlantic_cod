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

#### view age 1 timeseries ####

plot1<-lineplot_seasonal(springdata=EGOM_growth_spring,
                  falldata=EGOM_growth_fall,
                  springY=EGOM_growth_spring$K_rel,
                  fallY=EGOM_growth_fall$K_rel,
                  fallX= EGOM_growth_fall$Year,
                  springX= EGOM_growth_spring$Year,
                  main="NEFSC Trawl Survey Relative Condition: EGOM",
                  ylab="Relative Condition (K)",
                  ylim=c(0.9,1.3))
plot1<-lineplot_seasonal(springdata=WGOM_growth_spring,
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
plot_grid( ncol = 2, nrow = 2)

