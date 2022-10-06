### Estimating cod SSB outside stock assessment models
### Approach is to estimate SSB basesd on abuindance of age 6+ fish


### will use numbers at age data from trawl survey to do this
Cod_NAA<-read.csv(here("data/cod_NAA.csv"))
### WAA data from Charles
WAA_EGOM<-read.csv(here("data/WAA/EGOM_mean_weight_at_age.csv"))
WAA_WGOM<-read.csv(here("data/WAA/WGOM_mean_weight_at_age.csv"))
WAA_GBK<-read.csv(here("data/WAA/GBK_mean_weight_at_age.csv"))
WAA_SNEMA<-read.csv(here("data/WAA/SNEMA_mean_weight_at_age.csv"))


##### Stock specific SSB estimates#######
#NAA data wrangling
Cod_NAA<-Cod_NAA[,c(2:3,12:15,17:18)]
Cod_NAA = Cod_NAA[Cod_NAA$SURVEY == "NEFSC_BTS",]
Cod_NAA = Cod_NAA[!Cod_NAA$YEAR  < 1982,]
Cod_NAA = Cod_NAA[!Cod_NAA$YEAR  > 2019,]
NAA_EGOM = Cod_NAA[Cod_NAA$STOCK == "EGOM",]
NAA_WGOM = Cod_NAA[Cod_NAA$STOCK == "WGOM",]
NAA_GBK = Cod_NAA[Cod_NAA$STOCK == "GBK",]
NAA_SNE = Cod_NAA[Cod_NAA$STOCK == "SNE",]

#WAA data wrnagling
WAA_EGOM<-WAA_EGOM[,c(6:8,10)]
WAA_WGOM<-WAA_WGOM[,c(6:8,10)]
WAA_GBK<-WAA_GBK[,c(6:8,10)]
WAA_SNEMA<-WAA_SNEMA[,c(6:8,10)]

###clip to years I want (1982-2019)###
WAA_EGOM = WAA_EGOM[!WAA_EGOM$YEAR  < 1982,]
WAA_EGOM = WAA_EGOM[!WAA_EGOM$AGE  < 6,]
WAA_EGOM = WAA_EGOM[!WAA_EGOM$AGE  > 9,]

WAA_WGOM = WAA_WGOM[!WAA_WGOM$YEAR  < 1982,]
WAA_WGOM = WAA_WGOM[!WAA_WGOM$AGE  < 6,]
WAA_WGOM = WAA_WGOM[!WAA_WGOM$AGE  > 9,]

WAA_GBK = WAA_GBK[!WAA_GBK$YEAR  < 1982,]
WAA_GBK = WAA_GBK[!WAA_GBK$AGE  < 6,]
WAA_GBK = WAA_GBK[!WAA_GBK$AGE  > 9,]

WAA_SNEMA = WAA_SNEMA[!WAA_SNEMA$YEAR  < 1982,]
WAA_SNEMA = WAA_SNEMA[!WAA_SNEMA$AGE  < 6,]
WAA_SNEMA = WAA_EGOM[!WAA_SNEMA$AGE  > 9,]

## Create SSB dataframe
### GET SSB ESTIMATES FOR EACH STOCK #####
#function
get_ssb<- function(NAA_data, WAA_data,SSB_stock){
  
  SSB_stock<-NAA_data[,c(1:7)]
  names(SSB_stock)<-c("YEAR", "SEASON","Age6NAA","Age7NAA","Age8NAA","Age9NAA","STOCK")
  SSB_stock<-merge(SSB_stock,WAA_data,by=c("YEAR","SEASON"),all=TRUE)
  SSB_stock$SSB<-((SSB_stock$Age6NAA*SSB_stock$MEAN*(SSB_stock$AGE == 6))+
                   (SSB_stock$Age7NAA*SSB_stock$MEAN*(SSB_stock$AGE == 7))+
                   (SSB_stock$Age8NAA*SSB_stock$MEAN*(SSB_stock$AGE == 8))+
                   (SSB_stock$Age9NAA*SSB_stock$MEAN*(SSB_stock$AGE == 9)))
  SSB_stock<-SSB_stock[,c(1,2,10)]
  SSB_stock<-aggregate(SSB~YEAR+SEASON,SSB_stock,FUN=sum)
  
}
SSB_WGOM<-get_ssb(NAA_WGOM,WAA_WGOM,SSB_WGOM)
SSB_EGOM<-get_ssb(NAA_EGOM,WAA_EGOM,SSB_EGOM)
SSB_GBK<-get_ssb(NAA_GBK,WAA_GBK,SSB_GBK)
SSB_SNE<-get_ssb(NAA_SNE,WAA_SNEMA,SSB_SNE)

##### GET SSB ESTIMATES FOR ALL STOCK COMBINED #####

SSB_allstocks<-list(SSB_EGOM,SSB_WGOM,SSB_GBK,SSB_SNE)
SSB_allstocks<-Reduce(function(x, y) merge(x, y, all=TRUE), SSB_allstocks)
SSB_allstocks<-aggregate(SSB~YEAR+SEASON,SSB_allstocks,FUN=sum)


#### plot estimates

df<-data.frame((SSB_allstocks[SSB_allstocks$SEASON =="FALL",]["YEAR"]),(SSB_allstocks[SSB_allstocks$SEASON =="FALL",]["SSB"]))
plot(df, type="l")

df<-data.frame((SSB_allstocks[SSB_allstocks$SEASON =="SPRING",]["YEAR"]),(SSB_allstocks[SSB_allstocks$SEASON =="SPRING",]["SSB"]))
plot(df, type="l")
