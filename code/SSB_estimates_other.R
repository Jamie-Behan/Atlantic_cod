### Estimating cod SSB outside stock assessment models
### Approach is to estimate SSB based on abundance of age 4+ fish


### will use numbers at age data from trawl survey to do this
Cod_NAA<-read.csv(here("data/cod_NAA.csv"))
### WAA data from Charles
WAA_EGOM<-read.csv(here("data/WAA/EGOM_mean_weight_at_age.csv"))
WAA_WGOM<-read.csv(here("data/WAA/WGOM_mean_weight_at_age.csv"))
#WAA_GBK<-read.csv(here("data/WAA/GBK_mean_weight_at_age.csv"))
#WAA_SNEMA<-read.csv(here("data/WAA/SNEMA_mean_weight_at_age.csv"))


##### Stock specific SSB estimates#######
#NAA data wrangling
Cod_NAA<-Cod_NAA[,c(2:3,10:15,17:18)]
MENH_EGOM = Cod_NAA[Cod_NAA$SURVEY == "MENH_EGOM",]
MENH_EGOM = MENH_EGOM[!MENH_EGOM$YEAR  < 1982,]
MENH_EGOM = MENH_EGOM[!MENH_EGOM$YEAR  > 2019,]
MENH_WGOM = Cod_NAA[Cod_NAA$SURVEY == "MENH_WGOM",]
MENH_WGOM = MENH_WGOM[!MENH_WGOM$YEAR  < 1982,]
MENH_WGOM = MENH_WGOM[!MENH_WGOM$YEAR  > 2019,]

#WAA data wrnagling
WAA_EGOM<-WAA_EGOM[,c(6:8,10)]
WAA_WGOM<-WAA_WGOM[,c(6:8,10)]


###clip to years I want (1982-2019)###
WAA_EGOM = WAA_EGOM[!WAA_EGOM$YEAR  < 1982,]
WAA_EGOM = WAA_EGOM[!WAA_EGOM$AGE  < 4,]
WAA_EGOM = WAA_EGOM[!WAA_EGOM$AGE  > 9,]

WAA_WGOM = WAA_WGOM[!WAA_WGOM$YEAR  < 1982,]
WAA_WGOM = WAA_WGOM[!WAA_WGOM$AGE  < 4,]
WAA_WGOM = WAA_WGOM[!WAA_WGOM$AGE  > 9,]


## Create SSB dataframe
### GET SSB ESTIMATES FOR EACH STOCK #####
#function
get_ssb<- function(NAA_data, WAA_data,SSB_stock){
  
  SSB_stock<-NAA_data[,c(1:9)]
  names(SSB_stock)<-c("YEAR", "SEASON","Age4NAA","Age5NAA","Age6NAA","Age7NAA","Age8NAA","Age9NAA","STOCK")
  SSB_stock<-merge(SSB_stock,WAA_data,by=c("YEAR","SEASON"),all=TRUE)
  SSB_stock$SSB<-((SSB_stock$Age4NAA*SSB_stock$MEAN*(SSB_stock$AGE == 4))+
                    (SSB_stock$Age5NAA*SSB_stock$MEAN*(SSB_stock$AGE == 5))+
                    (SSB_stock$Age6NAA*SSB_stock$MEAN*(SSB_stock$AGE == 6))+
                   (SSB_stock$Age7NAA*SSB_stock$MEAN*(SSB_stock$AGE == 7))+
                   (SSB_stock$Age8NAA*SSB_stock$MEAN*(SSB_stock$AGE == 8))+
                   (SSB_stock$Age9NAA*SSB_stock$MEAN*(SSB_stock$AGE == 9)))
  SSB_stock<-SSB_stock[,c(1,2,12)]
  SSB_stock<-aggregate(SSB~YEAR+SEASON,SSB_stock,FUN=sum)
  
}
 
#SSB_EGOM<-MENH_EGOM[,c(1:9)]
#names(SSB_EGOM)<-c("YEAR", "SEASON","Age4NAA","Age5NAA","Age6NAA","Age7NAA","Age8NAA","Age9NAA","STOCK")
#SSB_EGOM<-merge(SSB_EGOM,WAA_EGOM,by=c("YEAR","SEASON"),all=TRUE)
#SSB_EGOM$SSB<-((SSB_EGOM$Age4NAA*SSB_EGOM$MEAN*(SSB_EGOM$AGE == 4))+
#                 (SSB_EGOM$Age5NAA*SSB_EGOM$MEAN*(SSB_EGOM$AGE == 5))+
#                 (SSB_EGOM$Age6NAA*SSB_EGOM$MEAN*(SSB_EGOM$AGE == 6))+
#                  (SSB_EGOM$Age7NAA*SSB_EGOM$MEAN*(SSB_EGOM$AGE == 7))+
#                  (SSB_EGOM$Age8NAA*SSB_EGOM$MEAN*(SSB_EGOM$AGE == 8))+
#                  (SSB_EGOM$Age9NAA*SSB_EGOM$MEAN*(SSB_EGOM$AGE == 9)))
#SSB_EGOM<-SSB_EGOM[,c(1,2,12)]
#SSB_EGOM<-aggregate(SSB~YEAR+SEASON,SSB_EGOM,FUN=sum)


SSB_WGOM<-get_ssb(MENH_WGOM,WAA_WGOM,SSB_WGOM)
SSB_EGOM<-get_ssb(MENH_EGOM,WAA_EGOM,SSB_EGOM)


##### GET SSB ESTIMATES FOR ALL STOCKS COMBINED #####

#SSB_allstocks<-list(SSB_EGOM,SSB_WGOM,SSB_GBK,SSB_SNE)
#SSB_allstocks<-Reduce(function(x, y) merge(x, y, all=TRUE), SSB_allstocks)
#SSB_allstocks<-aggregate(SSB~YEAR+SEASON,SSB_allstocks,FUN=sum)

##### combine seasons and EGOM+WGOM to compare to most recent stock assesment ssb plots###
SSB_GOM<-list(SSB_EGOM,SSB_WGOM)
SSB_GOM<-Reduce(function(x, y) merge(x, y, all=TRUE), SSB_GOM)
SSB_GOM<-aggregate(SSB~YEAR,SSB_GOM,FUN=sum)

#plot GOM estimates

plot(SSB_GOM$YEAR,SSB_GOM$SSB, type="l",main = "GOM SSB estimates form MENH Bottom Trawl")

#### Make Final dfs and save #####
SSB_Fall_EGOM_MENH<-SSB_EGOM[SSB_EGOM$SEASON =="FALL",]
SSB_Spring_EGOM_MENH<-SSB_EGOM[SSB_EGOM$SEASON =="SPRING",]

SSB_Fall_WGOM_MENH<-SSB_WGOM[SSB_WGOM$SEASON =="FALL",]
SSB_Spring_WGOM_MENH<-SSB_WGOM[SSB_WGOM$SEASON =="SPRING",]

#####save MENH ssb estimate data#####
write.csv(SSB_Fall_WGOM_MENH,here("data/SSB_estimates/SSB_Fall_WGOM_MENH.csv"), row.names = FALSE)
write.csv(SSB_Spring_WGOM_MENH,here("data/SSB_estimates/SSB_Spring_WGOM_MENH.csv"), row.names = FALSE)

