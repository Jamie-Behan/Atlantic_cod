library(here)
### WAA data from Charles
WAA_EGOM<-read.csv(here("data/WAA/EGOM_mean_weight_at_age.csv"))
WAA_WGOM<-read.csv(here("data/WAA/WGOM_mean_weight_at_age.csv"))
WAA_GBK<-read.csv(here("data/WAA/GBK_mean_weight_at_age.csv"))
WAA_SNEMA<-read.csv(here("data/WAA/SNEMA_mean_weight_at_age.csv"))

#WAA data wrnagling
WAA_EGOM<-WAA_EGOM[,c(6:8,10)]
WAA_WGOM<-WAA_WGOM[,c(6:8,10)]
WAA_GBK<-WAA_GBK[,c(6:8,10)]
WAA_SNEMA<-WAA_SNEMA[,c(6:8,10)]

###clip to years I want (1982-2019)###
WAA_EGOM = WAA_EGOM[!WAA_EGOM$YEAR  < 1982,]
WAA_EGOM = WAA_EGOM[!WAA_EGOM$AGE  < 1,]

WAA_WGOM = WAA_WGOM[!WAA_WGOM$YEAR  < 1982,]
WAA_WGOM = WAA_WGOM[!WAA_WGOM$AGE  < 1,]

WAA_GBK = WAA_GBK[!WAA_GBK$YEAR  < 1982,]
WAA_GBK = WAA_GBK[!WAA_GBK$AGE  < 1,]

WAA_SNEMA = WAA_SNEMA[!WAA_SNEMA$YEAR  < 1982,]
WAA_SNEMA = WAA_SNEMA[!WAA_SNEMA$AGE  < 1,]

#### EGOM #####
plot(WAA_EGOM$YEAR[WAA_EGOM$SEASON=="FALL" & WAA_EGOM$AGE=="1"], WAA_EGOM$MEAN[WAA_EGOM$SEASON=="FALL" & WAA_EGOM$AGE=="1"], type="b",
     xlim=range(WAA_EGOM$YEAR), ylim=range(c(0:10)), xlab = "Year",ylab = "Mean Weight (Kg)",col="#535353",lwd=2,main="EGOM Fall")
lines(WAA_EGOM$YEAR[WAA_EGOM$SEASON=="FALL" & WAA_EGOM$AGE=="2"], WAA_EGOM$MEAN[WAA_EGOM$SEASON=="FALL" & WAA_EGOM$AGE=="2"], type="b",col="#00608A",lwd=2)
lines(WAA_EGOM$YEAR[WAA_EGOM$SEASON=="FALL" & WAA_EGOM$AGE=="3"], WAA_EGOM$MEAN[WAA_EGOM$SEASON=="FALL" & WAA_EGOM$AGE=="3"], type="b",col="#407331",lwd=2)
lines(WAA_EGOM$YEAR[WAA_EGOM$SEASON=="FALL" & WAA_EGOM$AGE=="4"], WAA_EGOM$MEAN[WAA_EGOM$SEASON=="FALL" & WAA_EGOM$AGE=="4"], type="b",col="#EA4F12",lwd=2)
lines(WAA_EGOM$YEAR[WAA_EGOM$SEASON=="FALL" & WAA_EGOM$AGE=="5"], WAA_EGOM$MEAN[WAA_EGOM$SEASON=="FALL" & WAA_EGOM$AGE=="5"], type="b",col="#EACA00",lwd=2)
lines(WAA_EGOM$YEAR[WAA_EGOM$SEASON=="FALL" & WAA_EGOM$AGE=="6"], WAA_EGOM$MEAN[WAA_EGOM$SEASON=="FALL" & WAA_EGOM$AGE=="6"], type="b",col="#00736D",lwd=2)
legend(2011, 10, legend=c("Age 1 (24/38)", "Age 2 (31/38)","Age 3 (24/38)", "Age 4 (13/38)","Age 5 (10/38)", "Age 6 (4/38)"),
       col=c("#535353", "#00608A","#407331","#EA4F12","#EACA00","#00736D"), lty=1, cex=1,lwd=3)
###### WGOM #####
plot(WAA_WGOM$YEAR[WAA_WGOM$SEASON=="FALL" & WAA_WGOM$AGE=="1"], WAA_WGOM$MEAN[WAA_WGOM$SEASON=="FALL" & WAA_WGOM$AGE=="1"], type="b",
     xlim=range(WAA_WGOM$YEAR), ylim=range(c(0:11)), xlab = "Year",ylab = "Mean Weight (Kg)",col="#535353",lwd=2,main="WGOM Fall")
lines(WAA_WGOM$YEAR[WAA_WGOM$SEASON=="FALL" & WAA_WGOM$AGE=="2"], WAA_WGOM$MEAN[WAA_WGOM$SEASON=="FALL" & WAA_WGOM$AGE=="2"], type="b",col="#00608A",lwd=2)
lines(WAA_WGOM$YEAR[WAA_WGOM$SEASON=="FALL" & WAA_WGOM$AGE=="3"], WAA_WGOM$MEAN[WAA_WGOM$SEASON=="FALL" & WAA_WGOM$AGE=="3"], type="b",col="#407331",lwd=2)
lines(WAA_WGOM$YEAR[WAA_WGOM$SEASON=="FALL" & WAA_WGOM$AGE=="4"], WAA_WGOM$MEAN[WAA_WGOM$SEASON=="FALL" & WAA_WGOM$AGE=="4"], type="b",col="#EA4F12",lwd=2)
lines(WAA_WGOM$YEAR[WAA_WGOM$SEASON=="FALL" & WAA_WGOM$AGE=="5"], WAA_WGOM$MEAN[WAA_WGOM$SEASON=="FALL" & WAA_WGOM$AGE=="5"], type="b",col="#EACA00",lwd=2)
lines(WAA_WGOM$YEAR[WAA_WGOM$SEASON=="FALL" & WAA_WGOM$AGE=="6"], WAA_WGOM$MEAN[WAA_WGOM$SEASON=="FALL" & WAA_WGOM$AGE=="6"], type="b",col="#00736D",lwd=2)
legend(2014, 11.25, legend=c("Age 1 (38/38)", "Age 2 (39/38)","Age 3 (39/38)", "Age 4 (39/38)","Age 5 (37/38)", "Age 6 (30/38)"),
       col=c("#535353", "#00608A","#407331","#EA4F12","#EACA00","#00736D"), lty=1, cex=1,lwd=3)

###### GBK #####
plot(WAA_GBK$YEAR[WAA_GBK$SEASON=="FALL" & WAA_GBK$AGE=="1"], WAA_GBK$MEAN[WAA_GBK$SEASON=="FALL" & WAA_GBK$AGE=="1"], type="b",
     xlim=range(WAA_GBK$YEAR), ylim=range(c(0:11)), xlab = "Year",ylab = "Mean Weight (Kg)",col="#535353",lwd=2,main="GBK Fall")
lines(WAA_GBK$YEAR[WAA_GBK$SEASON=="FALL" & WAA_GBK$AGE=="2"], WAA_GBK$MEAN[WAA_GBK$SEASON=="FALL" & WAA_GBK$AGE=="2"], type="b",col="#00608A",lwd=2)
lines(WAA_GBK$YEAR[WAA_GBK$SEASON=="FALL" & WAA_GBK$AGE=="3"], WAA_GBK$MEAN[WAA_GBK$SEASON=="FALL" & WAA_GBK$AGE=="3"], type="b",col="#407331",lwd=2)
lines(WAA_GBK$YEAR[WAA_GBK$SEASON=="FALL" & WAA_GBK$AGE=="4"], WAA_GBK$MEAN[WAA_GBK$SEASON=="FALL" & WAA_GBK$AGE=="4"], type="b",col="#EA4F12",lwd=2)
lines(WAA_GBK$YEAR[WAA_GBK$SEASON=="FALL" & WAA_GBK$AGE=="5"], WAA_GBK$MEAN[WAA_GBK$SEASON=="FALL" & WAA_GBK$AGE=="5"], type="b",col="#EACA00",lwd=2)
lines(WAA_GBK$YEAR[WAA_GBK$SEASON=="FALL" & WAA_GBK$AGE=="6"], WAA_GBK$MEAN[WAA_GBK$SEASON=="FALL" & WAA_GBK$AGE=="6"], type="b",col="#00736D",lwd=2)
legend(2014, 11.25, legend=c("Age 1 (38/38)", "Age 2 (39/38)","Age 3 (39/38)", "Age 4 (37/38)","Age 5 (33/38)", "Age 6 (23/38)"),
       col=c("#535353", "#00608A","#407331","#EA4F12","#EACA00","#00736D"), lty=1, cex=1,lwd=3)
###### SNE #####
plot(WAA_SNEMA$YEAR[WAA_SNEMA$SEASON=="FALL" & WAA_SNEMA$AGE=="1"], WAA_SNEMA$MEAN[WAA_SNEMA$SEASON=="FALL" & WAA_SNEMA$AGE=="1"], type="b",
     xlim=range(WAA_SNEMA$YEAR), ylim=range(c(0:11)), xlab = "Year",ylab = "Mean Weight (Kg)",col="#535353",lwd=2,main="SNEMA Fall")
lines(WAA_SNEMA$YEAR[WAA_SNEMA$SEASON=="FALL" & WAA_SNEMA$AGE=="2"], WAA_SNEMA$MEAN[WAA_SNEMA$SEASON=="FALL" & WAA_SNEMA$AGE=="2"], type="b",col="#00608A",lwd=2)
lines(WAA_SNEMA$YEAR[WAA_SNEMA$SEASON=="FALL" & WAA_SNEMA$AGE=="3"], WAA_SNEMA$MEAN[WAA_SNEMA$SEASON=="FALL" & WAA_SNEMA$AGE=="3"], type="b",col="#407331",lwd=2)
lines(WAA_SNEMA$YEAR[WAA_SNEMA$SEASON=="FALL" & WAA_SNEMA$AGE=="4"], WAA_SNEMA$MEAN[WAA_SNEMA$SEASON=="FALL" & WAA_SNEMA$AGE=="4"], type="b",col="#EA4F12",lwd=2)
lines(WAA_SNEMA$YEAR[WAA_SNEMA$SEASON=="FALL" & WAA_SNEMA$AGE=="5"], WAA_SNEMA$MEAN[WAA_SNEMA$SEASON=="FALL" & WAA_SNEMA$AGE=="5"], type="b",col="#EACA00",lwd=2)
lines(WAA_SNEMA$YEAR[WAA_SNEMA$SEASON=="FALL" & WAA_SNEMA$AGE=="6"], WAA_SNEMA$MEAN[WAA_SNEMA$SEASON=="FALL" & WAA_SNEMA$AGE=="6"], type="b",col="#00736D",lwd=2)
legend(2014, 11.25, legend=c("Age 1 (1/38)", "Age 2 (6/38)","Age 3 (6/38)", "Age 4 (0/38)","Age 5 (0/38)", "Age 6 (0/38)"),
       col=c("#535353", "#00608A","#407331","#EA4F12","#EACA00","#00736D"), lty=1, cex=1,lwd=3)





library(data.table)
setDT(WAA_EGOM)[, Mean := mean(MEAN), by = .(Year, AGE, SEASON)][]