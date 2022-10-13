####### Check Outliars######
dotchart_fun_8<-function(data){
  par(mar=c(2,2,2,0), mfrow=c(2,4))
  apply(data,MARGIN=2,dotchart)
}
#####Line plot spring/fal together on same plot#####
lineplot_seasonal<- function (springdata, falldata, springY, fallY,springX,fallX,main,ylab,ylim){
layout(matrix(1:1, ncol=1, byrow=FALSE))
par(mar=c(4.1,4.5,1.5,1), oma=c(1.0,0,1.0,0.1))

plot(fallY ~fallX, data=falldata, main=main, xlab="Year",ylab=ylab, type="l",lwd=3,col="#00608A",ylim= ylim, cex.lab=1.4,cex.axis=1.1)
abline(lm(fallY~fallX),col="#00608A",lwd=2.5,lty=2)

lines(springY ~springX, data=springdata, xlab="Year", type="l",col="#EA4F12",lwd=3)
abline(lm(springY~springX),col="#EA4F12",lwd=2.5,lty=2)
legend("topleft",inset=c(0.03,0.03), legend=c("Spring", "Fall"), col=c("#EA4F12", "#00608A"), lty=1,lwd=3, cex=1.0)
}

#lineplot_seasonal(springdata=EGOM_recruitment_spring,falldata=EGOM_recruitment_fall,springY=EGOM_recruitment_spring$bt_anomaly,fallY=EGOM_recruitment_fall$bt_anomaly,fallX= EGOM_recruitment_fall$Year,springX= EGOM_recruitment_spring$Year,main="main title",ylab="bt anomaly",ylim=c(-2,3))
######Boxplots######
view_boxplot_fun8<- function (data){
  par(mar=c(2,2,2,0), mfrow=c(2,4))
  apply(data, MARGIN=2,boxplot)
}

##### check distribtuion ####
hist_fun8<-function(data){
par(mar=c(2,2,2,0), mfrow=c(2,4))
apply(data,MARGIN=2,hist)
}

#####shapiro test for normality#####
#if p >0.05, we can assume normality
#SPRING
shapiro_fun6<-function(data){
apply(data,MARGIN=2, FUN=shapiro.test)
}
#shapiro_fun6(EGOM_recruitment_fall[2:7]) #example

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

#Mypairs(distribution_fall[c(4:8)]) #example


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