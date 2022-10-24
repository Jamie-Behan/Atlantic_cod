##### Get mean depth/lat by adult/juvenile groups#####
library(dplyr)
cod_bio<-read.csv(here("data/Cod_Bio_Data.csv"))
cod_bio<-cod_bio[(cod_bio$SURVEY == "NEFSC_BTS") & (cod_bio$YEAR > 1981)& (cod_bio$YEAR < 2020),]
cod_survey<-read.csv(here("data/Survey_data_cod.csv"))
cod_survey<-cod_survey[(cod_survey$SURVEY == "NEFSC_BTS")& (cod_survey$YEAR > 1981)& (cod_survey$YEAR < 2020),]

cod_data<-merge(cod_bio,cod_survey, by=c("HAUL_ID","YEAR","SEASON","STOCK","SURVEY"))
summary(is.na(cod_data))
cod_data<-cod_data[!(is.na(cod_data$MATURITY_STAGE)&is.na(cod_data$AGE)),]
cod_data<-cod_data[c(1:4,8,11,13,14,16,18)]

cod_data$MATURITY_STAGE_new <- ifelse((is.na(cod_data$MATURITY_STAGE) & (cod_data$AGE<4)), "IMMATURE",ifelse((is.na(cod_data$MATURITY_STAGE) & (cod_data$AGE>3)),"MATURE",cod_data$MATURITY_STAGE))

adult_cod_fall<-cod_data[(cod_data$MATURITY_STAGE_new == "MATURE")& (cod_data$SEASON =="FALL"),]
adult_cod_spring<-cod_data[(cod_data$MATURITY_STAGE_new == "MATURE")& (cod_data$SEASON =="SPRING"),]
juvenile_cod_fall<-cod_data[(cod_data$MATURITY_STAGE_new == "IMMATURE")& (cod_data$SEASON =="FALL"),]
juvenile_cod_spring<-cod_data[(cod_data$MATURITY_STAGE_new == "IMMATURE")& (cod_data$SEASON =="SPRING"),]

adult_cod_fall<-adult_cod_fall[c(1,2,4,7:10)]
adult_cod_spring<-adult_cod_spring[c(1,2,4,7:10)]
juvenile_cod_fall<-juvenile_cod_fall[c(1,2,4,7:10)]
juvenile_cod_spring<-juvenile_cod_spring[c(1,2,4,7:10)]

adult_cod_fall<-aggregate(adult_cod_fall[4:ncol(adult_cod_fall)],adult_cod_fall[1:3], FUN=mean)
adult_cod_spring<-aggregate(adult_cod_spring[4:ncol(adult_cod_spring)],adult_cod_spring[1:3], FUN=mean)
juvenile_cod_fall<-aggregate(juvenile_cod_fall[4:ncol(juvenile_cod_fall)],juvenile_cod_fall[1:3], FUN=mean)
juvenile_cod_spring<-aggregate(juvenile_cod_spring[4:ncol(juvenile_cod_spring)],juvenile_cod_spring[1:3], FUN=mean)


adult_cod_fall_EGOM<-adult_cod_fall[(adult_cod_fall$STOCK == "EGOM"),]
adult_cod_fall_WGOM<-adult_cod_fall[(adult_cod_fall$STOCK == "WGOM"),]
adult_cod_fall_GBK<-adult_cod_fall[(adult_cod_fall$STOCK == "GBK"),]
adult_cod_fall_SNE<-adult_cod_fall[(adult_cod_fall$STOCK == "SNE"),]

adult_cod_spring_EGOM<-adult_cod_spring[(adult_cod_spring$STOCK == "EGOM"),]
adult_cod_spring_WGOM<-adult_cod_spring[(adult_cod_spring$STOCK == "WGOM"),]
adult_cod_spring_GBK<-adult_cod_spring[(adult_cod_spring$STOCK == "GBK"),]
adult_cod_spring_SNE<-adult_cod_spring[(adult_cod_spring$STOCK == "SNE"),]

juvenile_cod_fall_EGOM<-juvenile_cod_fall[(juvenile_cod_fall$STOCK == "EGOM"),]
juvenile_cod_fall_WGOM<-juvenile_cod_fall[(juvenile_cod_fall$STOCK == "WGOM"),]
juvenile_cod_fall_GBK<-juvenile_cod_fall[(juvenile_cod_fall$STOCK == "GBK"),]
juvenile_cod_fall_SNE<-juvenile_cod_fall[(juvenile_cod_fall$STOCK == "SNE"),]

juvenile_cod_spring_EGOM<-juvenile_cod_spring[(juvenile_cod_spring$STOCK == "EGOM"),]
juvenile_cod_spring_WGOM<-juvenile_cod_spring[(juvenile_cod_spring$STOCK == "WGOM"),]
juvenile_cod_spring_GBK<-juvenile_cod_spring[(juvenile_cod_spring$STOCK == "GBK"),]
juvenile_cod_spring_SNE<-juvenile_cod_spring[(juvenile_cod_spring$STOCK == "SNE"),]

rm(cod_bio,cod_data,cod_survey)

weighted_meanby_year<-function(data){
  df_summary1<- 
  data %>% 
    group_by(YEAR) %>% 
    summarise(wm_depth = weighted.mean(DEPTH, COD_KG))
  df_summary2<-             
  data %>% 
    group_by(YEAR) %>% 
    summarise(wm_lat = weighted.mean(LAT, COD_KG))
              
  df_list <- list(df_summary1,df_summary2)
  df<-Reduce(function(x, y) merge(x, y, by="YEAR"), df_list)
  return(df)
}
adultFL_wm_ALL<-weighted_meanby_year(adult_cod_fall)
adultFL_wm_EGOM<-weighted_meanby_year(adult_cod_fall_EGOM)
adultFL_wm_WGOM<-weighted_meanby_year(adult_cod_fall_WGOM)
adultFL_wm_GBK<-weighted_meanby_year(adult_cod_fall_GBK)
adultFL_wm_SNE<-weighted_meanby_year(adult_cod_fall_SNE)

adultSP_wm_ALL<-weighted_meanby_year(adult_cod_spring)
adultSP_wm_EGOM<-weighted_meanby_year(adult_cod_spring_EGOM)
adultSP_wm_WGOM<-weighted_meanby_year(adult_cod_spring_WGOM)
adultSP_wm_GBK<-weighted_meanby_year(adult_cod_spring_GBK)
adultSP_wm_SNE<-weighted_meanby_year(adult_cod_spring_SNE)

juvenileFL_wm_ALL<-weighted_meanby_year(juvenile_cod_fall)
juvenileFL_wm_EGOM<-weighted_meanby_year(juvenile_cod_fall_EGOM)
juvenileFL_wm_WGOM<-weighted_meanby_year(juvenile_cod_fall_WGOM)
juvenileFL_wm_GBK<-weighted_meanby_year(juvenile_cod_fall_GBK)
juvenileFL_wm_SNE<-weighted_meanby_year(juvenile_cod_fall_SNE)

juvenileSP_wm_ALL<-weighted_meanby_year(juvenile_cod_spring)
juvenileSP_wm_EGOM<-weighted_meanby_year(juvenile_cod_spring_EGOM)
juvenileSP_wm_WGOM<-weighted_meanby_year(juvenile_cod_spring_WGOM)
juvenileSP_wm_GBK<-weighted_meanby_year(juvenile_cod_spring_GBK)
juvenileSP_wm_SNE<-weighted_meanby_year(juvenile_cod_spring_SNE)

rm(adult_cod_fall,adult_cod_fall_EGOM,adult_cod_fall_GBK,adult_cod_fall_SNE,adult_cod_fall_WGOM,adult_cod_spring,adult_cod_spring_EGOM,adult_cod_spring_GBK,adult_cod_spring_SNE,adult_cod_spring_WGOM,juvenile_cod_fall,juvenile_cod_fall_EGOM,juvenile_cod_fall_GBK,juvenile_cod_fall_SNE,juvenile_cod_fall_WGOM,juvenile_cod_spring,juvenile_cod_spring_EGOM,juvenile_cod_spring_GBK,juvenile_cod_spring_SNE,juvenile_cod_spring_WGOM)

#### combine adult/juvenile dataframes######
wmfall_ALL<- merge(adultFL_wm_ALL,juvenileFL_wm_ALL,by="YEAR",all=TRUE)
colnames(wmfall_ALL) <- c('YEAR','adult_depth','adult_lat','juv_depth','juv_lat')
wmfall_EGOM<- merge(adultFL_wm_EGOM,juvenileFL_wm_EGOM,by="YEAR",all=TRUE)
colnames(wmfall_EGOM) <- c('YEAR','adult_depth','adult_lat','juv_depth','juv_lat')
wmfall_WGOM<- merge(adultFL_wm_WGOM,juvenileFL_wm_WGOM,by="YEAR",all=TRUE)
colnames(wmfall_WGOM) <- c('YEAR','adult_depth','adult_lat','juv_depth','juv_lat')
wmfall_GBK<- merge(adultFL_wm_GBK,juvenileFL_wm_GBK,by="YEAR",all=TRUE)
colnames(wmfall_GBK) <- c('YEAR','adult_depth','adult_lat','juv_depth','juv_lat')
wmfall_SNE<- merge(adultFL_wm_SNE,juvenileFL_wm_SNE,by="YEAR",all=TRUE)
colnames(wmfall_SNE) <- c('YEAR','adult_depth','adult_lat','juv_depth','juv_lat')

wmspring_ALL<- merge(adultSP_wm_ALL,juvenileSP_wm_ALL,by="YEAR",all=TRUE)
colnames(wmspring_ALL) <- c('YEAR','adult_depth','adult_lat','juv_depth','juv_lat')
wmspring_EGOM<- merge(adultSP_wm_EGOM,juvenileSP_wm_EGOM,by="YEAR",all=TRUE)
colnames(wmspring_EGOM) <- c('YEAR','adult_depth','adult_lat','juv_depth','juv_lat')
wmspring_WGOM<- merge(adultSP_wm_WGOM,juvenileSP_wm_WGOM,by="YEAR",all=TRUE)
colnames(wmspring_WGOM) <- c('YEAR','adult_depth','adult_lat','juv_depth','juv_lat')
wmspring_GBK<- merge(adultSP_wm_GBK,juvenileSP_wm_GBK,by="YEAR",all=TRUE)
colnames(wmspring_GBK) <- c('YEAR','adult_depth','adult_lat','juv_depth','juv_lat')
wmspring_SNE<- merge(adultSP_wm_SNE,juvenileSP_wm_SNE,by="YEAR",all=TRUE)
colnames(wmspring_SNE) <- c('YEAR','adult_depth','adult_lat','juv_depth','juv_lat')

names(wmfall_ALL)[1]="Year"
names(wmspring_ALL)[1]="Year"
#write.csv(wmfall_ALL,here("data/depth_lat/wmfall_ALL.csv"), row.names = FALSE)
#write.csv(wmfall_EGOM,here("data/depth_lat/wmfall_EGOM.csv"), row.names = FALSE)
#write.csv(wmfall_WGOM,here("data/depth_lat/wmfall_WGOM.csv"), row.names = FALSE)
#write.csv(wmfall_GBK,here("data/depth_lat/wmfall_GBK.csv"), row.names = FALSE)
#write.csv(wmfall_SNE,here("data/depth_lat/wmfall_SNE.csv"), row.names = FALSE)

#write.csv(wmspring_ALL,here("data/depth_lat/wmspring_ALL.csv"), row.names = FALSE)
#write.csv(wmspring_EGOM,here("data/depth_lat/wmspring_EGOM.csv"), row.names = FALSE)
#write.csv(wmspring_WGOM,here("data/depth_lat/wmspring_WGOM.csv"), row.names = FALSE)
#write.csv(wmspring_GBK,here("data/depth_lat/wmspring_GBK.csv"), row.names = FALSE)
#write.csv(wmspring_SNE,here("data/depth_lat/wmspring_SNE.csv"), row.names = FALSE)
