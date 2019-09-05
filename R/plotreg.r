#' Returns scatter plot with regression results
#' 
#' @param x string with name of variable on x axis
#' @param y string with name of variable on y axis
#' @param dat data frame with data
#' @param titl string with tittle
#' @param xlab string with lable x axis, x by default
#' @param ylab string with lable y axisa, y by default
#' @param scx scale x variable. E.g 10s,100s, 1000s,... scx=1 by default
#' @param scy scale y variable. E.g 10s,100s, 1000s,... scy=1 by default
#' @param w string with name of regression weights variable. Equal weight by default
#' @param names string with name of data points labels. E.g. City, State, Country
#' @param sepa parameter used to calibrate the frequency tick marks,  large values imply lower frequency. sepa=0 produces one label per data point
#' @param ini number beyond which the frequency of names is reduced
#' @param font plot font, serif by default
#' @param logs use of logs in the regression, logs=0 y~x, logs=1: log(y)~log(x), logs=2: log(y)~x, logs=3: y~log(x)
#' @param cv name of covariates
#' @param nx horizontal adjustment for data points labels
#' @param ny vertical adjustment for data points labels
#' @return weighted scater plot with weighted linear regression
#' @examples  
#' exmp<-plotreg(x='Distance_from_Largest_City_to_NYC_miles',y='Total_Trade_Amount_2015_Thousand_USD',dat=trade,titl='trade',xlab = 'distance',ylab='trade',w='GDP_2015_USD',names='Name_country',nx=100,ny=0)
#' exmp
plotreg<-function(x,y,dat,titl,xlab,ylab,scx,scy,w,names,sepa,ini,font,logs,cv,nx,ny,stepx,stepy){
  if(!require(ggplot2, quietly = TRUE)){install.packages('ggplot2',quietly = TRUE)}
  if(missing(titl)){titl<-' '}
  if(missing(xlab)){xlab<-'x'}
  if(missing(ylab)){ylab<-'y'}
  if(missing(font)){font<-'serif'}
  if(missing(w)){w<-rep(1,nrow(dat))}
  if(missing(ini)){ini<-1}
  if(missing(sepa)){sepa<-1}
  if(missing(scx)){scx<-1}
  if(missing(scy)){scy<-1}
  if(missing(names)){dat$names<-' '
  names<-'names'}
  if(missing(logs)){logs<-0}
  if(missing(nx)){nx<-0}
  if(missing(ny)){ny<-0}
  names(dat)[names(dat)==x]<-'x'
  names(dat)[names(dat)==y]<-'y'
  names(dat)[names(dat)==w]<-'w'
  x<-dat$x
  y<-dat$y
  w<-dat$w
  if(missing(cv)){
    if(logs==0){fm<-as.formula("y ~ x")}
    if(logs==1){if(min(y,na.rm = TRUE)<0|min(x,na.rm = TRUE)<0){stop("y or x is <0, and using log")}
      fm<-as.formula("log(y) ~ log(x)")}
    if(logs==2){if(min(y,na.rm = TRUE)<0){stop("y is <0, and using log")}
      fm<-as.formula("log(y) ~ x")}
    if(logs==3){if(min(x,na.rm = TRUE)<0){stop("x is <0, and using log")}
      fm<-as.formula("y ~ log(x)")}}else{
        if(logs==0){fm<-as.formula(paste("y ~ x+", paste(cv, collapse= "+")))
        fmcv<-as.formula(paste("y ~", paste(cv, collapse= "+")))}
        if(logs==1){if(min(y,na.rm = TRUE)<0|min(x,na.rm = TRUE)<0){stop("y or x is <0, and using log")}
          fm<-as.formula(paste("log(y) ~ log(x)+", paste(cv, collapse= "+")))
          fmcv<-as.formula(paste("log(y) ~", paste(cv, collapse= "+")))}
        if(logs==2){if(min(y,na.rm = TRUE)<0){stop("y is <0, and using log")}
          fm<-as.formula(paste("log(y) ~ x+", paste(cv, collapse= "+")))
          fmcv<-as.formula(paste("log(y) ~", paste(cv, collapse= "+")))}
        if(logs==3){if(min(x,na.rm = TRUE)<0){stop("x is <0, and using log")}
          fm<-as.formula(paste("y ~ log(x)+", paste(cv, collapse= "+")))
          fmcv<-as.formula(paste("y ~", paste(cv, collapse= "+")))}}
  if(!missing(cv)){seqcv<-summary(lm(fmcv,weights=w,data=dat))}
  seq<-summary(lm(fm,weights=w,data=dat))
  yhat<-dat$y-seq$residuals
  if(seq$coefficients[2]<0){sign<-'-'}else{sign<-'+'}
  if(abs(round(seq$coefficients[2],1))<0.1){coef2<-'0.0'}else{coef2<-toString(abs(round(seq$coefficients[2],1)))}
  if(abs(round(seq$coefficients[1],1))<0.1){coef1<-'0.0'}else{coef1<-toString(abs(round(seq$coefficients[1],1)))}
  eql<-paste0(ylab,'=',coef1,sign,coef2,' (',xlab,')',' R2=',round(seq$r.squared,2))
  names<-as.character(dat[,names(dat)==names])
  xma<-max(x,na.rm=TRUE)
  yma<-max(y,na.rm=TRUE)
  xf<-x/xma
  yf<-y/yma
  tpos<-xf+yf
  df_names<-data.frame(cbind(names,tpos),stringsAsFactors=FALSE)
  rtpos<--sort(-tpos)
  diftpos<-NULL
  for(i in 1:(length(rtpos)-1)){diftpos<-c(diftpos,(rtpos[i]-rtpos[i+1]))}
  diftpos_m<-mean(diftpos)*sepa
  df_names$short_name<-' '
  for (i in 1:ini){df_names$short_name[df_names$tpos==as.character(rtpos[i])]<-df_names$names[df_names$tpos==as.character(rtpos[i])&!is.na(df_names$tpos)]}
  ini_tpos<-rtpos[ini]
  while(ini<=(length(rtpos)-1)){
    if((ini_tpos-rtpos[ini+1])>=diftpos_m){
      df_names$short_name[df_names$tpos==as.character(rtpos[ini+1])]<-df_names$names[df_names$tpos==as.character(rtpos[ini+1])&!is.na(df_names$tpos)]
      ini_tpos<-rtpos[ini+1]}
    ini=ini+1}
  names_sh<-df_names$short_name
  if(logs==0){
    maxx<-(ceiling(xma/(scx*10))*10)
    minx<-(floor(min(x,na.rm=TRUE)/(scx*10))*10)
    maxy<-(ceiling(yma/(scy*10))*10)
    miny<-(floor(min(y,na.rm=TRUE)/(scy*10))*10)}else if(logs==1){
      maxx<-log((ceiling(xma)))
      minx<-log(((min(x,na.rm=TRUE))))
      maxy<-log((ceiling(yma)))
      miny<-log(((min(y,na.rm=TRUE))))}else if(logs==2){
        maxx<-(ceiling(xma/(scx*10))*10)
        minx<-(floor(min(x,na.rm=TRUE)/(scx*10))*10)
        maxy<-log((ceiling(yma)))
        miny<-log(((min(y,na.rm=TRUE))))}else if(logs==3){
          maxx<-log((ceiling(xma)))
          minx<-log(((min(x,na.rm=TRUE))))
          maxy<-(ceiling(yma/(scy*10))*10)
          miny<-(floor(min(y,na.rm=TRUE)/(scy*10))*10)}
  if(missing(stepx)){
    stepx<-seq(from =minx, to = maxx, by =((maxx-minx)/5))}
  if(missing(stepy)){
    stepy<-seq(from =miny, to = maxy, by =((maxy-miny)/5))}
  stepx_ex<-round(exp(stepx),0)
  stepy_ex<-round(exp(stepy),0)
  for (i in 1:length(stepx)){stepx[i]<-round(stepx[i],digits = 0)
  stepy[i]<-round(stepy[i],digits = 0)}
  dataf<-data.frame(cbind(x,y,w))
  if(missing(cv)){
    if(logs==0){
      plot <- ggplot(dataf, aes(x=x/scx,y=y/scy))+geom_point(aes(size = w),show.legend = FALSE,alpha = 1/2)+
        ggtitle(titl)+
        xlab(xlab)+ 
        ylab(ylab)+
        geom_smooth(method='lm', se = FALSE,aes(weight=w,colour=eql))+
        theme_classic(base_family = font)+
        theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5))+
        scale_colour_manual(name=' ',values="grey50")+
        geom_text(aes(label=names_sh),size = 3,family = font,nudge_x=nx,nudge_y=ny)
  }else if(logs==1){
    plot <- ggplot(dataf, aes(x=x/scx,y=y/scy))+geom_point(aes(size = w),show.legend = FALSE,alpha = 1/2)+
      ggtitle(titl)+
      xlab(xlab)+ 
      ylab(ylab)+
      scale_y_continuous(trans = 'log2',breaks=stepy_ex)+
      scale_x_continuous(trans = 'log2',breaks=stepx_ex)+
      geom_smooth(method='lm', se = FALSE,aes(weight=w,colour=eql))+
      theme_classic(base_family = font)+
      theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5))+
      scale_colour_manual(name=' ',values="grey50")+
      geom_text(aes(label=names_sh),size = 3,family = font,nudge_x=nx,nudge_y=ny)
  }else if(logs==2){
    plot <- ggplot(dataf, aes(x=x/scx,y=y/scy))+geom_point(aes(size = w),show.legend = FALSE,alpha = 1/2)+
      ggtitle(titl)+
      xlab(xlab)+ 
      ylab(ylab)+
      scale_y_continuous(trans = 'log2',breaks=stepy_ex)+
      geom_smooth(method='lm', se = FALSE,aes(weight=w,colour=eql))+
      theme_classic(base_family = font)+
      theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5))+
      scale_colour_manual(name=' ',values="grey50")+
      geom_text(aes(label=names_sh),size = 3,family = font,nudge_x=nx,nudge_y=ny)
  }else if(logs==3){
    plot <- ggplot(dataf, aes(x=x/scx,y=y/scy))+geom_point(aes(size = w),show.legend = FALSE,alpha = 1/2)+
      ggtitle(titl)+
      xlab(xlab)+ 
      ylab(ylab)+
      scale_x_continuous(trans = 'log2',breaks=stepx_ex)+
      geom_smooth(method='lm', se = FALSE,aes(weight=w,colour=eql))+
      theme_classic(base_family = font)+
      theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5))+
      scale_colour_manual(name=' ',values="grey50")+
      geom_text(aes(label=names_sh),size = 3,family = font,nudge_x=nx,nudge_y=ny)
  }}else{
    if(logs==0){
      plot <- ggplot(dataf, aes(x=x/scx,y=y/scy))+geom_point(aes(size = w),show.legend = FALSE,alpha = 1/2)+
        geom_point(aes(y=yhat, size = w), color="grey50",show.legend = FALSE,alpha = 1/2)+
        ggtitle(titl)+
        xlab(xlab)+ 
        ylab(ylab)+
        geom_smooth(method='lm', se = FALSE,aes(y=yhat,weight=w,colour=eql))+
        theme_classic(base_family = font)+
        theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5))+
        scale_colour_manual(name=' ',values="grey50")+
        geom_text(aes(label=names_sh),size = 3,family = font,nudge_x=nx,nudge_y=ny)
    }else if(logs==1){
      plot <- ggplot(dataf, aes(x=x/scx,y=y/scy))+geom_point(aes(size = w),show.legend = FALSE,alpha = 1/2)+
        geom_point(aes(y=yhat, size = w), color="grey50",show.legend = FALSE,alpha = 1/2)+
        ggtitle(titl)+
        xlab(xlab)+ 
        ylab(ylab)+
        scale_y_continuous(trans = 'log2',breaks=stepy_ex)+
        scale_x_continuous(trans = 'log2',breaks=stepx_ex)+
        geom_smooth(method='lm', se = FALSE,aes(y=yhat,weight=w,colour=eql))+
        theme_classic(base_family = font)+
        theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5))+
        scale_colour_manual(name=' ',values="grey50")+
        geom_text(aes(label=names_sh),size = 3,family = font,nudge_x=nx,nudge_y=ny)
    }else if(logs==2){
      plot <- ggplot(dataf, aes(x=x/scx,y=y/scy))+geom_point(aes(size = w),show.legend = FALSE,alpha = 1/2)+
        geom_point(aes(y=yhat, size = w), color="grey50",show.legend = FALSE,alpha = 1/2)+
        ggtitle(titl)+
        xlab(xlab)+ 
        ylab(ylab)+
        scale_y_continuous(trans = 'log2',breaks=stepy_ex)+
        geom_smooth(method='lm', se = FALSE,aes(y=yhat,weight=w,colour=eql))+
        theme_classic(base_family = font)+
        theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5))+
        scale_colour_manual(name=' ',values="grey50")+
        geom_text(aes(label=names_sh),size = 3,family = font,nudge_x=nx,nudge_y=ny)
    }else if(logs==3){
      plot <- ggplot(dataf, aes(x=x/scx,y=y/scy))+geom_point(aes(size = w),show.legend = FALSE,alpha = 1/2)+
        geom_point(aes(y=yhat, size = w), color="grey50",show.legend = FALSE,alpha = 1/2)+
        ggtitle(titl)+
        xlab(xlab)+ 
        ylab(ylab)+
        scale_x_continuous(trans = 'log2',breaks=stepx_ex)+
        geom_smooth(method='lm', se = FALSE,aes(y=yhat,weight=w,colour=eql))+
        theme_classic(base_family = font)+
        theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5))+
        scale_colour_manual(name=' ',values="grey50")+
        geom_text(aes(label=names_sh),size = 3,family = font,nudge_x=nx,nudge_y=ny)
    }}}
