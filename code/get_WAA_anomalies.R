library(here)
### WAA data from Charles
WAA_EGOM<-read.csv(here("data/WAA/EGOM_mean_weight_at_age.csv"))
WAA_WGOM<-read.csv(here("data/WAA/WGOM_mean_weight_at_age.csv"))
WAA_GBK<-read.csv(here("data/WAA/GBK_mean_weight_at_age.csv"))
#WAA_SNEMA<-read.csv(here("data/WAA/SNEMA_mean_weight_at_age.csv"))

#WAA data wrnagling####
WAA_EGOM<-WAA_EGOM[,c(6:8,10)]
WAA_WGOM<-WAA_WGOM[,c(6:8,10)]
WAA_GBK<-WAA_GBK[,c(6:8,10)]
#WAA_SNEMA<-WAA_SNEMA[,c(6:8,10)]

###clip to years I want (1982-2019)####
WAA_EGOM = WAA_EGOM[!WAA_EGOM$YEAR  < 1982,]
WAA_EGOM = WAA_EGOM[!WAA_EGOM$YEAR  > 2019,]
WAA_EGOM_fall = WAA_EGOM[WAA_EGOM$SEASON== "FALL" & WAA_EGOM$AGE==2,]
names(WAA_EGOM_fall)[4]="age2WAA"
WAA_EGOM_spring =WAA_EGOM[WAA_EGOM$SEASON== "SPRING" & WAA_EGOM$AGE==1,]
names(WAA_EGOM_spring)[4]="age1WAA"

WAA_WGOM = WAA_WGOM[!WAA_WGOM$YEAR  < 1982,]
WAA_WGOM = WAA_WGOM[!WAA_WGOM$YEAR  >2019,]
WAA_WGOM_fall = WAA_WGOM[(WAA_WGOM$SEASON== "FALL" & WAA_WGOM$AGE==1)|(WAA_WGOM$SEASON== "FALL" & WAA_WGOM$AGE==2)| (WAA_WGOM$SEASON== "FALL" & WAA_WGOM$AGE==3)| (WAA_WGOM$SEASON== "FALL" & WAA_WGOM$AGE==4)| (WAA_WGOM$SEASON== "FALL" & WAA_WGOM$AGE==5)| (WAA_WGOM$SEASON== "FALL" & WAA_WGOM$AGE==6),]

WAA_WGOM_spring =WAA_WGOM[(WAA_WGOM$SEASON== "SPRING" & WAA_WGOM$AGE==1)| (WAA_WGOM$SEASON== "SPRING" & WAA_WGOM$AGE==2)| (WAA_WGOM$SEASON== "SPRING" & WAA_WGOM$AGE==3)| (WAA_WGOM$SEASON== "SPRING" & WAA_WGOM$AGE==4)| (WAA_WGOM$SEASON== "SPRING" & WAA_WGOM$AGE==5)| (WAA_WGOM$SEASON== "SPRING" & WAA_WGOM$AGE==6)| (WAA_WGOM$SEASON== "SPRING" & WAA_WGOM$AGE==7),]

WAA_GBK = WAA_GBK[!WAA_GBK$YEAR  < 1982,]
WAA_GBK = WAA_GBK[!WAA_GBK$YEAR >2019,]
WAA_GBK_fall = WAA_GBK[(WAA_GBK$SEASON== "FALL" & WAA_GBK$AGE==1)|(WAA_GBK$SEASON== "FALL" & WAA_GBK$AGE==2)| (WAA_GBK$SEASON== "FALL" & WAA_GBK$AGE==3)| (WAA_GBK$SEASON== "FALL" & WAA_GBK$AGE==4)| (WAA_GBK$SEASON== "FALL" & WAA_GBK$AGE==5),]
WAA_GBK_spring =WAA_GBK[(WAA_GBK$SEASON== "SPRING" & WAA_GBK$AGE==1)| (WAA_GBK$SEASON== "SPRING" & WAA_GBK$AGE==2)| (WAA_GBK$SEASON== "SPRING" & WAA_GBK$AGE==3)| (WAA_GBK$SEASON== "SPRING" & WAA_GBK$AGE==4)| (WAA_GBK$SEASON== "SPRING" & WAA_GBK$AGE==5)| (WAA_GBK$SEASON== "SPRING" & WAA_GBK$AGE==6)| (WAA_GBK$SEASON== "SPRING" & WAA_GBK$AGE==7),]

###get final gbk FALL WAA df#######
WAA_GBK_fall2<- data.frame (YEAR = c(1982:2019))
WAA_GBK_fall2<- merge(WAA_GBK_fall2,WAA_GBK_fall[WAA_GBK_fall$AGE==1,][c(2,4)],all=TRUE)
names(WAA_GBK_fall2)[2]<-"age1WAA"
WAA_GBK_fall2<- merge(WAA_GBK_fall2,WAA_GBK_fall[WAA_GBK_fall$AGE==2,][c(2,4)],all=TRUE)
names(WAA_GBK_fall2)[3]<-"age2WAA"
WAA_GBK_fall2<- merge(WAA_GBK_fall2,WAA_GBK_fall[WAA_GBK_fall$AGE==3,][c(2,4)],all=TRUE)
names(WAA_GBK_fall2)[4]<-"age3WAA"
WAA_GBK_fall2<- merge(WAA_GBK_fall2,WAA_GBK_fall[WAA_GBK_fall$AGE==4,][c(2,4)],all=TRUE)
names(WAA_GBK_fall2)[5]<-"age4WAA"
WAA_GBK_fall2<- merge(WAA_GBK_fall2,WAA_GBK_fall[WAA_GBK_fall$AGE==5,][c(2,4)],all=TRUE)
names(WAA_GBK_fall2)[6]<-"age5WAA"
WAA_GBK_fall<-WAA_GBK_fall2
rm(WAA_GBK_fall2)

###get final GBK SPRING WAA df#######
WAA_GBK_spring2<- data.frame (YEAR = c(1982:2019))
WAA_GBK_spring2<- merge(WAA_GBK_spring2,WAA_GBK_spring[WAA_GBK_spring$AGE==1,][c(2,4)],all=TRUE)
names(WAA_GBK_spring2)[2]<-"age1WAA"
WAA_GBK_spring2<- merge(WAA_GBK_spring2,WAA_GBK_spring[WAA_GBK_spring$AGE==2,][c(2,4)],all=TRUE)
names(WAA_GBK_spring2)[3]<-"age2WAA"
WAA_GBK_spring2<- merge(WAA_GBK_spring2,WAA_GBK_spring[WAA_GBK_spring$AGE==3,][c(2,4)],all=TRUE)
names(WAA_GBK_spring2)[4]<-"age3WAA"
WAA_GBK_spring2<- merge(WAA_GBK_spring2,WAA_GBK_spring[WAA_GBK_spring$AGE==4,][c(2,4)],all=TRUE)
names(WAA_GBK_spring2)[5]<-"age4WAA"
WAA_GBK_spring2<- merge(WAA_GBK_spring2,WAA_GBK_spring[WAA_GBK_spring$AGE==5,][c(2,4)],all=TRUE)
names(WAA_GBK_spring2)[6]<-"age5WAA"
WAA_GBK_spring2<- merge(WAA_GBK_spring2,WAA_GBK_spring[WAA_GBK_spring$AGE==6,][c(2,4)],all=TRUE)
names(WAA_GBK_spring2)[7]<-"age6WAA"
WAA_GBK_spring2<- merge(WAA_GBK_spring2,WAA_GBK_spring[WAA_GBK_spring$AGE==7,][c(2,4)],all=TRUE)
names(WAA_GBK_spring2)[8]<-"age7WAA"
WAA_GBK_spring<-WAA_GBK_spring2
rm(WAA_GBK_spring2)

###get final WGOM FALL WAA df####
WAA_WGOM_fall2<- data.frame (YEAR = c(1982:2019))
WAA_WGOM_fall2<- merge(WAA_WGOM_fall2,WAA_WGOM_fall[WAA_WGOM_fall$AGE==1,][c(2,4)],all=TRUE)
names(WAA_WGOM_fall2)[2]<-"age1WAA"
WAA_WGOM_fall2<- merge(WAA_WGOM_fall2,WAA_WGOM_fall[WAA_WGOM_fall$AGE==2,][c(2,4)],all=TRUE)
names(WAA_WGOM_fall2)[3]<-"age2WAA"
WAA_WGOM_fall2<- merge(WAA_WGOM_fall2,WAA_WGOM_fall[WAA_WGOM_fall$AGE==3,][c(2,4)],all=TRUE)
names(WAA_WGOM_fall2)[4]<-"age3WAA"
WAA_WGOM_fall2<- merge(WAA_WGOM_fall2,WAA_WGOM_fall[WAA_WGOM_fall$AGE==4,][c(2,4)],all=TRUE)
names(WAA_WGOM_fall2)[5]<-"age4WAA"
WAA_WGOM_fall2<- merge(WAA_WGOM_fall2,WAA_WGOM_fall[WAA_WGOM_fall$AGE==5,][c(2,4)],all=TRUE)
names(WAA_WGOM_fall2)[6]<-"age5WAA"
WAA_WGOM_fall2<- merge(WAA_WGOM_fall2,WAA_WGOM_fall[WAA_WGOM_fall$AGE==6,][c(2,4)],all=TRUE)
names(WAA_WGOM_fall2)[7]<-"age6WAA"
WAA_WGOM_fall<-WAA_WGOM_fall2
rm(WAA_WGOM_fall2)

###get final WGOM SPRING WAA df####
WAA_WGOM_spring2<- data.frame (YEAR = c(1982:2019))
WAA_WGOM_spring2<- merge(WAA_WGOM_spring2,WAA_WGOM_spring[WAA_WGOM_spring$AGE==1,][c(2,4)],all=TRUE)
names(WAA_WGOM_spring2)[2]<-"age1WAA"
WAA_WGOM_spring2<- merge(WAA_WGOM_spring2,WAA_WGOM_spring[WAA_WGOM_spring$AGE==2,][c(2,4)],all=TRUE)
names(WAA_WGOM_spring2)[3]<-"age2WAA"
WAA_WGOM_spring2<- merge(WAA_WGOM_spring2,WAA_WGOM_spring[WAA_WGOM_spring$AGE==3,][c(2,4)],all=TRUE)
names(WAA_WGOM_spring2)[4]<-"age3WAA"
WAA_WGOM_spring2<- merge(WAA_WGOM_spring2,WAA_WGOM_spring[WAA_WGOM_spring$AGE==4,][c(2,4)],all=TRUE)
names(WAA_WGOM_spring2)[5]<-"age4WAA"
WAA_WGOM_spring2<- merge(WAA_WGOM_spring2,WAA_WGOM_spring[WAA_WGOM_spring$AGE==5,][c(2,4)],all=TRUE)
names(WAA_WGOM_spring2)[6]<-"age5WAA"
WAA_WGOM_spring2<- merge(WAA_WGOM_spring2,WAA_WGOM_spring[WAA_WGOM_spring$AGE==6,][c(2,4)],all=TRUE)
names(WAA_WGOM_spring2)[7]<-"age6WAA"
WAA_WGOM_spring2<- merge(WAA_WGOM_spring2,WAA_WGOM_spring[WAA_WGOM_spring$AGE==7,][c(2,4)],all=TRUE)
names(WAA_WGOM_spring2)[8]<-"age7WAA"
WAA_WGOM_spring<-WAA_WGOM_spring2
rm(WAA_WGOM_spring2)

###get final EGOM WAA dfs####
WAA_EGOM_fall<-WAA_EGOM_fall[c(2,4)]
WAA_EGOM_spring<-WAA_EGOM_spring[c(2,4)]


rm(WAA_EGOM,WAA_GBK,WAA_WGOM)
#####Plow raw WAA, not anomalies###
par(mar=c(2,4,2,1), mfrow=c(2,3))
#### EGOM fall #####
plot(WAA_EGOM$YEAR[WAA_EGOM$SEASON=="FALL" & WAA_EGOM$AGE=="1"], WAA_EGOM$MEAN[WAA_EGOM$SEASON=="FALL" & WAA_EGOM$AGE=="1"], type="l",
     xlim=range(WAA_EGOM$YEAR), ylim=range(c(0:10)), xlab = "Year",ylab = "Mean Weight (Kg)",col="#535353",lwd=2,main="EGOM Fall")
lines(WAA_EGOM$YEAR[WAA_EGOM$SEASON=="FALL" & WAA_EGOM$AGE=="2"], WAA_EGOM$MEAN[WAA_EGOM$SEASON=="FALL" & WAA_EGOM$AGE=="2"], type="l",col="#00608A",lwd=2)
lines(WAA_EGOM$YEAR[WAA_EGOM$SEASON=="FALL" & WAA_EGOM$AGE=="3"], WAA_EGOM$MEAN[WAA_EGOM$SEASON=="FALL" & WAA_EGOM$AGE=="3"], type="l",col="#407331",lwd=2)
lines(WAA_EGOM$YEAR[WAA_EGOM$SEASON=="FALL" & WAA_EGOM$AGE=="4"], WAA_EGOM$MEAN[WAA_EGOM$SEASON=="FALL" & WAA_EGOM$AGE=="4"], type="l",col="#EA4F12",lwd=2)
lines(WAA_EGOM$YEAR[WAA_EGOM$SEASON=="FALL" & WAA_EGOM$AGE=="5"], WAA_EGOM$MEAN[WAA_EGOM$SEASON=="FALL" & WAA_EGOM$AGE=="5"], type="l",col="#EACA00",lwd=2)
lines(WAA_EGOM$YEAR[WAA_EGOM$SEASON=="FALL" & WAA_EGOM$AGE=="6"], WAA_EGOM$MEAN[WAA_EGOM$SEASON=="FALL" & WAA_EGOM$AGE=="6"], type="l",col="#00736D",lwd=2)
legend("topright", legend=c("Age 1 (24/38)", "Age 2 (31/38)","Age 3 (24/38)", "Age 4 (13/38)","Age 5 (10/38)", "Age 6 (4/38)"),
       col=c("#535353", "#00608A","#407331","#EA4F12","#EACA00","#00736D"), lty=1, cex=1,lwd=3)
###### WGOM #####
plot(WAA_WGOM$YEAR[WAA_WGOM$SEASON=="FALL" & WAA_WGOM$AGE=="1"], WAA_WGOM$MEAN[WAA_WGOM$SEASON=="FALL" & WAA_WGOM$AGE=="1"], type="l",
     xlim=range(WAA_WGOM$YEAR), ylim=range(c(0:11)), xlab = "Year",ylab = "Mean Weight (Kg)",col="#535353",lwd=2,main="WGOM Fall")
lines(WAA_WGOM$YEAR[WAA_WGOM$SEASON=="FALL" & WAA_WGOM$AGE=="2"], WAA_WGOM$MEAN[WAA_WGOM$SEASON=="FALL" & WAA_WGOM$AGE=="2"], type="l",col="#00608A",lwd=2)
lines(WAA_WGOM$YEAR[WAA_WGOM$SEASON=="FALL" & WAA_WGOM$AGE=="3"], WAA_WGOM$MEAN[WAA_WGOM$SEASON=="FALL" & WAA_WGOM$AGE=="3"], type="l",col="#407331",lwd=2)
lines(WAA_WGOM$YEAR[WAA_WGOM$SEASON=="FALL" & WAA_WGOM$AGE=="4"], WAA_WGOM$MEAN[WAA_WGOM$SEASON=="FALL" & WAA_WGOM$AGE=="4"], type="l",col="#EA4F12",lwd=2)
lines(WAA_WGOM$YEAR[WAA_WGOM$SEASON=="FALL" & WAA_WGOM$AGE=="5"], WAA_WGOM$MEAN[WAA_WGOM$SEASON=="FALL" & WAA_WGOM$AGE=="5"], type="l",col="#EACA00",lwd=2)
lines(WAA_WGOM$YEAR[WAA_WGOM$SEASON=="FALL" & WAA_WGOM$AGE=="6"], WAA_WGOM$MEAN[WAA_WGOM$SEASON=="FALL" & WAA_WGOM$AGE=="6"], type="l",col="#00736D",lwd=2)
legend("topright", legend=c("Age 1 (38/38)", "Age 2 (39/38)","Age 3 (39/38)", "Age 4 (39/38)","Age 5 (37/38)", "Age 6 (30/38)"),
       col=c("#535353", "#00608A","#407331","#EA4F12","#EACA00","#00736D"), lty=1, cex=1,lwd=3)

###### GBK #####
plot(WAA_GBK$YEAR[WAA_GBK$SEASON=="FALL" & WAA_GBK$AGE=="1"], WAA_GBK$MEAN[WAA_GBK$SEASON=="FALL" & WAA_GBK$AGE=="1"], type="l",
     xlim=range(WAA_GBK$YEAR), ylim=range(c(0:11)), xlab = "Year",ylab = "Mean Weight (Kg)",col="#535353",lwd=2,main="GBK Fall")
lines(WAA_GBK$YEAR[WAA_GBK$SEASON=="FALL" & WAA_GBK$AGE=="2"], WAA_GBK$MEAN[WAA_GBK$SEASON=="FALL" & WAA_GBK$AGE=="2"], type="l",col="#00608A",lwd=2)
lines(WAA_GBK$YEAR[WAA_GBK$SEASON=="FALL" & WAA_GBK$AGE=="3"], WAA_GBK$MEAN[WAA_GBK$SEASON=="FALL" & WAA_GBK$AGE=="3"], type="l",col="#407331",lwd=2)
lines(WAA_GBK$YEAR[WAA_GBK$SEASON=="FALL" & WAA_GBK$AGE=="4"], WAA_GBK$MEAN[WAA_GBK$SEASON=="FALL" & WAA_GBK$AGE=="4"], type="l",col="#EA4F12",lwd=2)
lines(WAA_GBK$YEAR[WAA_GBK$SEASON=="FALL" & WAA_GBK$AGE=="5"], WAA_GBK$MEAN[WAA_GBK$SEASON=="FALL" & WAA_GBK$AGE=="5"], type="l",col="#EACA00",lwd=2)
lines(WAA_GBK$YEAR[WAA_GBK$SEASON=="FALL" & WAA_GBK$AGE=="6"], WAA_GBK$MEAN[WAA_GBK$SEASON=="FALL" & WAA_GBK$AGE=="6"], type="l",col="#00736D",lwd=2)
legend("topright", legend=c("Age 1 (38/38)", "Age 2 (39/38)","Age 3 (39/38)", "Age 4 (37/38)","Age 5 (33/38)", "Age 6 (23/38)"),
       col=c("#535353", "#00608A","#407331","#EA4F12","#EACA00","#00736D"), lty=1, cex=1,lwd=3)

#### EGOM spring #####
plot(WAA_EGOM$YEAR[WAA_EGOM$SEASON=="SPRING" & WAA_EGOM$AGE=="1"], WAA_EGOM$MEAN[WAA_EGOM$SEASON=="SPRING" & WAA_EGOM$AGE=="1"], type="l",
     xlim=range(WAA_EGOM$YEAR), ylim=range(c(0:10)), xlab = "Year",ylab = "Mean Weight (Kg)",col="#535353",lwd=2,main="EGOM Spring")
lines(WAA_EGOM$YEAR[WAA_EGOM$SEASON=="SPRING" & WAA_EGOM$AGE=="2"], WAA_EGOM$MEAN[WAA_EGOM$SEASON=="SPRING" & WAA_EGOM$AGE=="2"], type="l",col="#00608A",lwd=2)
lines(WAA_EGOM$YEAR[WAA_EGOM$SEASON=="SPRING" & WAA_EGOM$AGE=="3"], WAA_EGOM$MEAN[WAA_EGOM$SEASON=="SPRING" & WAA_EGOM$AGE=="3"], type="l",col="#407331",lwd=2)
lines(WAA_EGOM$YEAR[WAA_EGOM$SEASON=="SPRING" & WAA_EGOM$AGE=="4"], WAA_EGOM$MEAN[WAA_EGOM$SEASON=="SPRING" & WAA_EGOM$AGE=="4"], type="l",col="#EA4F12",lwd=2)
lines(WAA_EGOM$YEAR[WAA_EGOM$SEASON=="SPRING" & WAA_EGOM$AGE=="5"], WAA_EGOM$MEAN[WAA_EGOM$SEASON=="SPRING" & WAA_EGOM$AGE=="5"], type="l",col="#EACA00",lwd=2)
lines(WAA_EGOM$YEAR[WAA_EGOM$SEASON=="SPRING" & WAA_EGOM$AGE=="6"], WAA_EGOM$MEAN[WAA_EGOM$SEASON=="SPRING" & WAA_EGOM$AGE=="6"], type="l",col="#00736D",lwd=2)
legend("topright", legend=c("Age 1 (31/38)", "Age 2 (26/38)","Age 3 (21/38)", "Age 4 (20/38)","Age 5 (8/38)", "Age 6 (5/38)"),
       col=c("#535353", "#00608A","#407331","#EA4F12","#EACA00","#00736D"), lty=1, cex=1,lwd=3)
###### WGOM #####
plot(WAA_WGOM$YEAR[WAA_WGOM$SEASON=="SPRING" & WAA_WGOM$AGE=="1"], WAA_WGOM$MEAN[WAA_WGOM$SEASON=="SPRING" & WAA_WGOM$AGE=="1"], type="l",
     xlim=range(WAA_WGOM$YEAR), ylim=range(c(0:11)), xlab = "Year",ylab = "Mean Weight (Kg)",col="#535353",lwd=2,main="WGOM Spring")
lines(WAA_WGOM$YEAR[WAA_WGOM$SEASON=="SPRING" & WAA_WGOM$AGE=="2"], WAA_WGOM$MEAN[WAA_WGOM$SEASON=="SPRING" & WAA_WGOM$AGE=="2"], type="l",col="#00608A",lwd=2)
lines(WAA_WGOM$YEAR[WAA_WGOM$SEASON=="SPRING" & WAA_WGOM$AGE=="3"], WAA_WGOM$MEAN[WAA_WGOM$SEASON=="SPRING" & WAA_WGOM$AGE=="3"], type="l",col="#407331",lwd=2)
lines(WAA_WGOM$YEAR[WAA_WGOM$SEASON=="SPRING" & WAA_WGOM$AGE=="4"], WAA_WGOM$MEAN[WAA_WGOM$SEASON=="SPRING" & WAA_WGOM$AGE=="4"], type="l",col="#EA4F12",lwd=2)
lines(WAA_WGOM$YEAR[WAA_WGOM$SEASON=="SPRING" & WAA_WGOM$AGE=="5"], WAA_WGOM$MEAN[WAA_WGOM$SEASON=="SPRING" & WAA_WGOM$AGE=="5"], type="l",col="#EACA00",lwd=2)
lines(WAA_WGOM$YEAR[WAA_WGOM$SEASON=="SPRING" & WAA_WGOM$AGE=="6"], WAA_WGOM$MEAN[WAA_WGOM$SEASON=="SPRING" & WAA_WGOM$AGE=="6"], type="l",col="#00736D",lwd=2)
legend("topright", legend=c("Age 1 (37/38)", "Age 2 (38/38)","Age 3 (38/38)", "Age 4 (38/38)","Age 5 (38/38)", "Age 6 (38/38)"),
       col=c("#535353", "#00608A","#407331","#EA4F12","#EACA00","#00736D"), lty=1, cex=1,lwd=3)

###### GBK #####
plot(WAA_GBK$YEAR[WAA_GBK$SEASON=="SPRING" & WAA_GBK$AGE=="1"], WAA_GBK$MEAN[WAA_GBK$SEASON=="SPRING" & WAA_GBK$AGE=="1"], type="l",
     xlim=range(WAA_GBK$YEAR), ylim=range(c(0:11)), xlab = "Year",ylab = "Mean Weight (Kg)",col="#535353",lwd=2,main="GBK Spring")
lines(WAA_GBK$YEAR[WAA_GBK$SEASON=="SPRING" & WAA_GBK$AGE=="2"], WAA_GBK$MEAN[WAA_GBK$SEASON=="SPRING" & WAA_GBK$AGE=="2"], type="l",col="#00608A",lwd=2)
lines(WAA_GBK$YEAR[WAA_GBK$SEASON=="SPRING" & WAA_GBK$AGE=="3"], WAA_GBK$MEAN[WAA_GBK$SEASON=="SPRING" & WAA_GBK$AGE=="3"], type="l",col="#407331",lwd=2)
lines(WAA_GBK$YEAR[WAA_GBK$SEASON=="SPRING" & WAA_GBK$AGE=="4"], WAA_GBK$MEAN[WAA_GBK$SEASON=="SPRING" & WAA_GBK$AGE=="4"], type="l",col="#EA4F12",lwd=2)
lines(WAA_GBK$YEAR[WAA_GBK$SEASON=="SPRING" & WAA_GBK$AGE=="5"], WAA_GBK$MEAN[WAA_GBK$SEASON=="SPRING" & WAA_GBK$AGE=="5"], type="l",col="#EACA00",lwd=2)
lines(WAA_GBK$YEAR[WAA_GBK$SEASON=="SPRING" & WAA_GBK$AGE=="6"], WAA_GBK$MEAN[WAA_GBK$SEASON=="SPRING" & WAA_GBK$AGE=="6"], type="l",col="#00736D",lwd=2)
legend("topright", legend=c("Age 1 (32/38)", "Age 2 (38/38)","Age 3 (38/38)", "Age 4 (38/38)","Age 5 (38/38)", "Age 6 (37/38)"),
       col=c("#535353", "#00608A","#407331","#EA4F12","#EACA00","#00736D"), lty=1, cex=1,lwd=3)

#### find length function#####
findlength<-function(WAAdata, season, age){
  length(WAAdata$YEAR[WAAdata$SEASON== season & WAAdata$AGE== age])
}
#### find how many observations at each age####
for (i in 1:15){
  result<-findlength(WAA_EGOM, season= "SPRING", age = i) 
  print(result)
}
for (i in 1:15){
  result<-findlength(WAA_WGOM, season= "SPRING", age = i) 
  print(result)
}
for (i in 1:15){
  result<-findlength(WAA_GBK, season= "SPRING", age = i) 
  print(result)
}
for (i in 1:15){
  result<-findlength(WAA_SNEMA, season= "SPRING", age = i) 
  print(result)
}

###### change YEAR column to Year######
names(WAA_EGOM_fall)[1]<-"Year"
names(WAA_EGOM_spring)[1]<-"Year"
names(WAA_WGOM_fall)[1]<-"Year"
names(WAA_WGOM_spring)[1]<-"Year"
names(WAA_GBK_fall)[1]<-"Year"
names(WAA_GBK_spring)[1]<-"Year"
###### get 30 year base period. shooting for somewhere as close to 1982-2011 as possible #####
bp_egom_fall_age2<-mean(WAA_EGOM_fall[1:30,2])
bp_egom_spring_age1<-mean(WAA_EGOM_spring[1:30,2])

bp_wgom_fall_age1<-mean(WAA_WGOM_fall[1:31,2],na.rm=TRUE)
bp_wgom_fall_age2<-mean(WAA_WGOM_fall[1:30,3],na.rm=TRUE)
bp_wgom_fall_age3<-mean(WAA_WGOM_fall[1:30,4],na.rm=TRUE)
bp_wgom_fall_age4<-mean(WAA_WGOM_fall[1:30,5],na.rm=TRUE)
bp_wgom_fall_age5<-mean(WAA_WGOM_fall[1:31,6],na.rm=TRUE)
bp_wgom_fall_age6<-mean(WAA_WGOM_fall[1:36,7],na.rm=TRUE)
bp_wgom_spring_age1<-mean(WAA_WGOM_spring[1:31,2],na.rm=TRUE)
bp_wgom_spring_age2<-mean(WAA_WGOM_spring[1:30,3],na.rm=TRUE)
bp_wgom_spring_age3<-mean(WAA_WGOM_spring[1:30,4],na.rm=TRUE)
bp_wgom_spring_age4<-mean(WAA_WGOM_spring[1:30,5],na.rm=TRUE)
bp_wgom_spring_age5<-mean(WAA_WGOM_spring[1:30,6],na.rm=TRUE)
bp_wgom_spring_age6<-mean(WAA_WGOM_spring[1:30,7],na.rm=TRUE)
bp_wgom_spring_age7<-mean(WAA_WGOM_spring[1:32,8],na.rm=TRUE)

bp_gbk_fall_age1<-mean(WAA_GBK_fall[1:31,2],na.rm=TRUE)
bp_gbk_fall_age2<-mean(WAA_GBK_fall[1:30,3],na.rm=TRUE)
bp_gbk_fall_age3<-mean(WAA_GBK_fall[1:30,4],na.rm=TRUE)
bp_gbk_fall_age4<-mean(WAA_GBK_fall[1:32,5],na.rm=TRUE)
bp_gbk_fall_age5<-mean(WAA_GBK_fall[1:35,6],na.rm=TRUE)
bp_gbk_spring_age1<-mean(WAA_GBK_spring[1:34,2],na.rm=TRUE)
bp_gbk_spring_age2<-mean(WAA_GBK_spring[1:30,3],na.rm=TRUE)
bp_gbk_spring_age3<-mean(WAA_GBK_spring[1:30,4],na.rm=TRUE)
bp_gbk_spring_age4<-mean(WAA_GBK_spring[1:30,5],na.rm=TRUE)
bp_gbk_spring_age5<-mean(WAA_GBK_spring[1:30,6],na.rm=TRUE)
bp_gbk_spring_age6<-mean(WAA_GBK_spring[1:30,7],na.rm=TRUE)
bp_gbk_spring_age7<-mean(WAA_GBK_spring[1:31,8],na.rm=TRUE)
##### get anomalies ######
####EGOM
WAA_EGOM_fall$age2_anomaly<- WAA_EGOM_fall$age2WAA  - bp_egom_fall_age2
WAA_EGOM_spring$age1_anomaly<- WAA_EGOM_spring$age1WAA  - bp_egom_spring_age1
####WGOM
WAA_WGOM_fall$age1_anomaly<- WAA_WGOM_fall$age1WAA  - bp_wgom_fall_age1
WAA_WGOM_fall$age2_anomaly<- WAA_WGOM_fall$age2WAA  - bp_wgom_fall_age2
WAA_WGOM_fall$age3_anomaly<- WAA_WGOM_fall$age3WAA  - bp_wgom_fall_age3
WAA_WGOM_fall$age4_anomaly<- WAA_WGOM_fall$age4WAA  - bp_wgom_fall_age4
WAA_WGOM_fall$age5_anomaly<- WAA_WGOM_fall$age5WAA  - bp_wgom_fall_age5
WAA_WGOM_fall$age6_anomaly<- WAA_WGOM_fall$age6WAA  - bp_wgom_fall_age6
WAA_WGOM_spring$age1_anomaly<- WAA_WGOM_spring$age1WAA  - bp_wgom_spring_age1
WAA_WGOM_spring$age2_anomaly<- WAA_WGOM_spring$age2WAA  - bp_wgom_spring_age2
WAA_WGOM_spring$age3_anomaly<- WAA_WGOM_spring$age3WAA  - bp_wgom_spring_age3
WAA_WGOM_spring$age4_anomaly<- WAA_WGOM_spring$age4WAA  - bp_wgom_spring_age4
WAA_WGOM_spring$age5_anomaly<- WAA_WGOM_spring$age5WAA  - bp_wgom_spring_age5
WAA_WGOM_spring$age6_anomaly<- WAA_WGOM_spring$age6WAA  - bp_wgom_spring_age6
WAA_WGOM_spring$age7_anomaly<- WAA_WGOM_spring$age7WAA  - bp_wgom_spring_age7
####GBK
WAA_GBK_fall$age1_anomaly<- WAA_GBK_fall$age1WAA  - bp_gbk_fall_age1
WAA_GBK_fall$age2_anomaly<- WAA_GBK_fall$age2WAA  - bp_gbk_fall_age2
WAA_GBK_fall$age3_anomaly<- WAA_GBK_fall$age3WAA  - bp_gbk_fall_age3
WAA_GBK_fall$age4_anomaly<- WAA_GBK_fall$age4WAA  - bp_gbk_fall_age4
WAA_GBK_fall$age5_anomaly<- WAA_GBK_fall$age5WAA  - bp_gbk_fall_age5
WAA_GBK_spring$age1_anomaly<- WAA_GBK_spring$age1WAA  - bp_gbk_spring_age1
WAA_GBK_spring$age2_anomaly<- WAA_GBK_spring$age2WAA  - bp_gbk_spring_age2
WAA_GBK_spring$age3_anomaly<- WAA_GBK_spring$age3WAA  - bp_gbk_spring_age3
WAA_GBK_spring$age4_anomaly<- WAA_GBK_spring$age4WAA  - bp_gbk_spring_age4
WAA_GBK_spring$age5_anomaly<- WAA_GBK_spring$age5WAA  - bp_gbk_spring_age5
WAA_GBK_spring$age6_anomaly<- WAA_GBK_spring$age6WAA  - bp_gbk_spring_age6
WAA_GBK_spring$age7_anomaly<- WAA_GBK_spring$age7WAA  - bp_gbk_spring_age7
####remove WAA columns and only keep WAA anomaly columns#####
WAA_EGOM_fall<-WAA_EGOM_fall[c(1,3)]
WAA_EGOM_spring<-WAA_EGOM_spring[c(1,3)]

WAA_WGOM_fall<-WAA_WGOM_fall[c(1,8:13)]
WAA_WGOM_spring<-WAA_WGOM_spring[c(1,9:15)]

WAA_GBK_fall<-WAA_GBK_fall[c(1,7:11)]
WAA_GBK_spring<-WAA_GBK_spring[c(1,9:15)]
##### save waa anomaly .csv files######
#write.csv(WAA_EGOM_fall,here("data/WAA/anomaly_data/WAA_EGOM_fall.csv"), row.names = FALSE)
#write.csv(WAA_EGOM_spring,here("data/WAA/anomaly_data/WAA_EGOM_spring.csv"), row.names = FALSE)
#write.csv(WAA_WGOM_fall,here("data/WAA/anomaly_data/WAA_WGOM_fall.csv"), row.names = FALSE)
#write.csv(WAA_WGOM_spring,here("data/WAA/anomaly_data/WAA_WGOM_spring.csv"), row.names = FALSE)
#write.csv(WAA_GBK_fall,here("data/WAA/anomaly_data/WAA_GBK_fall.csv"), row.names = FALSE)
#write.csv(WAA_GBK_spring,here("data/WAA/anomaly_data/WAA_GBK_spring.csv"), row.names = FALSE)




