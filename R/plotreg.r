plotreg<-function(x,y,dat,titl,xlab,ylab,scx,scy,w,names,sepa,ini,font,logs,cv){
  if(!require(ggplot2, quietly = TRUE)){install.packages('ggplot',dep = TRUE,quietly = TRUE)}
  if(missing(titl)){titl<-' '}
  if(missing(xlab)){xlab<-'x'}
  if(missing(ylab)){ylab<-'y'}
  if(missing(font)){font<-'serif'}
  if(missing(w)){w<-rep(1,nrow(dat))}
  if(missing(ini)){ini<-1}
  if(missing(sepa)){sepa<-1}
  if(missing(scx)){scx<-1}
  if(missing(scy)){scy<-1}
  if(missing(names)){names<-rep(' ',length(x))}
  if(missing(font)){font<-"serif"}
  if(missing(logs)){logs<-0}
  if(missing(cv)){
    if(logs==0){fm<-as.formula("y ~ x")}
    if(logs==1){fm<-as.formula("log(y) ~ log(x)")}
    if(logs==2){fm<-as.formula("log(y) ~ x")}
    if(logs==3){fm<-as.formula("y ~ log(x)")}}else{
      if(logs==0){fm<-as.formula(paste("y ~ x+", paste(cv, collapse= "+")))
        fmcv<-as.formula(paste("y ~", paste(cv, collapse= "+")))}
      if(logs==1){fm<-as.formula(paste("log(y) ~ log(x)+", paste(cv, collapse= "+")))
        fmcv<-as.formula(paste("log(y) ~", paste(cv, collapse= "+")))}
      if(logs==2){fm<-as.formula(paste("log(y) ~ x+", paste(cv, collapse= "+")))
        fmcv<-as.formula(paste("log(y) ~", paste(cv, collapse= "+")))}
      if(logs==3){fm<-as.formula(paste("y ~ log(x)+", paste(cv, collapse= "+")))
        fmcv<-as.formula(paste("y ~", paste(cv, collapse= "+")))}}
  names(dat)[names(dat)==x]<-'x'
  names(dat)[names(dat)==y]<-'y'
  names(dat)[names(dat)==w]<-'w'
  x<-dat$x
  y<-dat$y
  w<-dat$w
  if(!missing(cv)){seqcv<-summary(lm(fmcv,weights=w,data=dat))}
  seq<-summary(lm(fm,weights=w,data=dat))
  yhat<-dat$y-seq$residuals
  if(seq$coefficients[2]<0){sign<-'-'}else{sign<-'+'}
  if(abs(round(seq$coefficients[2],1))<0.1){coef2<-'0.0'}else{coef2<-toString(abs(round(seq$coefficients[2],1)))}
  if(abs(round(seq$coefficients[1],1))<0.1){coef1<-'0.0'}else{coef1<-toString(abs(round(seq$coefficients[1],1)))}
  eql<-paste0(ylab,'=',coef1,sign,coef2,'(',xlab,') R2=',round(seq$r.squared,2))
  names<-as.character(names)
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
  stepx<-seq(from =minx, to = maxx, by =((maxx-minx)/5))
  stepy<-seq(from =miny, to = maxy, by =((maxy-miny)/5))
  stepx<-exp(stepx)
  stepy<-exp(stepy)
  for (i in 1:length(stepx)){stepx[i]<-round(stepx[i],digits = 0)
  stepy[i]<-round(stepy[i],digits = 0)}
  dataf<-data.frame(cbind(x,y,w))
  if(missing(cv)){if(logs==0){
    plot <- ggplot(dataf, aes(x=x/scx,y=y/scy))+geom_point(aes(size = w),show.legend = FALSE,alpha = 1/2)+
      ggtitle(titl)+
      xlab(xlab)+ 
      ylab(ylab)+
      geom_smooth(method='lm', se = FALSE,aes(weight=w,colour=eql))+
      theme_classic(base_family = font)+
      theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5))+
      scale_colour_manual(name=' ',values="grey50")+
      geom_text(aes(label=names_sh),size = 3,family = font,hjust=minx*0.1, vjust=-miny*0.5)
  }else if(logs==1){
    plot <- ggplot(dataf, aes(x=x/scx,y=y/scy))+geom_point(aes(size = w),show.legend = FALSE,alpha = 1/2)+
      ggtitle(titl)+
      xlab(xlab)+ 
      ylab(ylab)+
      scale_y_continuous(trans = 'log2',breaks=stepy)+
      scale_x_continuous(trans = 'log2',breaks=stepx)+
      geom_smooth(method='lm', se = FALSE,aes(weight=w,colour=eql))+
      theme_classic(base_family = font)+
      theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5))+
      scale_colour_manual(name=' ',values="grey50")+
      geom_text(aes(label=names_sh),size = 3,family = font,hjust=minx*0.1, vjust=-miny*0.5)
  }else if(logs==2){
    plot <- ggplot(dataf, aes(x=x/scx,y=y/scy))+geom_point(aes(size = w),show.legend = FALSE,alpha = 1/2)+
      ggtitle(titl)+
      xlab(xlab)+ 
      ylab(ylab)+
      scale_y_continuous(trans = 'log2',breaks=stepy)+
      geom_smooth(method='lm', se = FALSE,aes(weight=w,colour=eql))+
      theme_classic(base_family = font)+
      theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5))+
      scale_colour_manual(name=' ',values="grey50")+
      geom_text(aes(label=names_sh),size = 3,family = font,hjust=minx*0.1, vjust=-miny*0.5)
  }else if(logs==3){
    plot <- ggplot(dataf, aes(x=x/scx,y=y/scy))+geom_point(aes(size = w),show.legend = FALSE,alpha = 1/2)+
      ggtitle(titl)+
      xlab(xlab)+ 
      ylab(ylab)+
      scale_x_continuous(trans = 'log2',breaks=stepx)+
      geom_smooth(method='lm', se = FALSE,aes(weight=w,colour=eql))+
      theme_classic(base_family = font)+
      theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5))+
      scale_colour_manual(name=' ',values="grey50")+
      geom_text(aes(label=names_sh),size = 3,family = font,hjust=minx*0.1, vjust=-miny*0.5)
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
        geom_text(aes(label=names_sh),size = 3,family = font,hjust=minx*0.1, vjust=-miny*0.5)
    }else if(logs==1){
      plot <- ggplot(dataf, aes(x=x/scx,y=y/scy))+geom_point(aes(size = w),show.legend = FALSE,alpha = 1/2)+
        geom_point(aes(y=yhat, size = w), color="grey50",show.legend = FALSE,alpha = 1/2)+
        ggtitle(titl)+
        xlab(xlab)+ 
        ylab(ylab)+
        scale_y_continuous(trans = 'log2',breaks=stepy)+
        scale_x_continuous(trans = 'log2',breaks=stepx)+
        geom_smooth(method='lm', se = FALSE,aes(y=yhat,weight=w,colour=eql))+
        theme_classic(base_family = font)+
        theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5))+
        scale_colour_manual(name=' ',values="grey50")+
        geom_text(aes(label=names_sh),size = 3,family = font,hjust=minx*0.1, vjust=-miny*0.5)
    }else if(logs==2){
      plot <- ggplot(dataf, aes(x=x/scx,y=y/scy))+geom_point(aes(size = w),show.legend = FALSE,alpha = 1/2)+
        geom_point(aes(y=yhat, size = w), color="grey50",show.legend = FALSE,alpha = 1/2)+
        ggtitle(titl)+
        xlab(xlab)+ 
        ylab(ylab)+
        scale_y_continuous(trans = 'log2',breaks=stepy)+
        geom_smooth(method='lm', se = FALSE,aes(y=yhat,weight=w,colour=eql))+
        theme_classic(base_family = font)+
        theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5))+
        scale_colour_manual(name=' ',values="grey50")+
        geom_text(aes(label=names_sh),size = 3,family = font,hjust=minx*0.1, vjust=-miny*0.5)
    }else if(logs==3){
      plot <- ggplot(dataf, aes(x=x/scx,y=y/scy))+geom_point(aes(size = w),show.legend = FALSE,alpha = 1/2)+
        #geom_smooth(method='lm', se = FALSE,aes(x=x/scx,y=y/scy,weight=w,colour=smpl))+
        geom_point(aes(y=yhat, size = w), color="grey50",show.legend = FALSE,alpha = 1/2)+
        ggtitle(titl)+
        xlab(xlab)+ 
        ylab(ylab)+
        scale_x_continuous(trans = 'log2',breaks=stepx)+
        geom_smooth(method='lm', se = FALSE,aes(y=yhat,weight=w,colour=eql))+
        theme_classic(base_family = font)+
        theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5))+
        scale_colour_manual(name=' ',values="grey50")+
        geom_text(aes(label=names_sh),size = 3,family = font,hjust=minx*0.1, vjust=-miny*0.5)
    }}}
