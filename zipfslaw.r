#' Returns plot of Zip's law regression
#'
#' @param data data set
#' @param var variable name string
#' @param name lable observation
#' @param namevar name string to equation
#' @param sepa higher numbers reduce the frequency at which observation names apper
#' @param color text color
#' @param width figure width
#' @param pointsize size data points
#' @param height height plot
#' @param path filepath where the output is stored (e.g. path='C:/Users/jj/R'). If missing is stored at the current loaction (getwd()). filed is saved as Zips_+namevar
#' @return png file stored in path
#' @examples  
#' zipfslaw(data=trade,var='GDP_2015_USD',name='Name_country',namevar = 'GDP',sepa=3,path='C:/Users/jj/file')
zipfslaw<-function(data,var,name, namevar,sepa,color,width,pointsize,height,path){
  if(missing(sepa)){sepa=1}
  if(missing(color)){color<-'gray'}
  if(missing(width)){width=1200}
  if(missing(pointsize)){pointsize=25}
  if(missing(height)){height=1200}
  if(missing(path)){path=getwd()}
  namv<-c(1:9,seq(from = 10, to = nrow(data), by =10))
  data$name_full<-data[,names(data)==name]
  data$var<-data[,names(data)==var]
  rtpos<--sort(-data$var)
  diftpos<-NULL
  for(i in 1:(length(rtpos)-1)){diftpos<-c(diftpos,(rtpos[i]-rtpos[i+1]))}
  diftpos_m<-abs(mean(diftpos)*sepa)
  data$name_short<-' '
  data$rank<-rank(-data$var)
  data$name_short[1]<-as.character(data$name_full[1])
  for (i in 2:nrow(data)){
    if(abs(data$var[i]-data$var[i-1])>=diftpos_m){data$name_short[i]<-as.character(data$name_full[i])}}
  abl<-lm(log(data$rank)~log(data$var))
  abls<-summary(abl)
  png(filename=paste0(path,"/Zips_",namevar,".png"),
      width=width,
      pointsize=pointsize,
      height=height)
  plot(log(data$var[data$rank>0]),log(data$rank[data$rank>0]),pch=16,main=paste0("Zipf Regression: ",namevar),xlab=paste0("Log(",namevar,")"),ylab=paste0("Log(Rank ",namevar,")"),xlim=c(min(log(data$var[data$rank>0]),na.rm=TRUE)-1,max(log(data$var[data$rank>0]),na.rm=TRUE)+1),family="serif")
  text(log(data$var),log(data$rank),data$name_short,pos=4,col=color,family="serif")
  abline(abl)
  #text(min(log(data$var[data$rank>0]),na.rm=TRUE)*1,1,paste0('Log(Rank ',namevar,')= ',round(summary(abl)$coefficients[1],2),round(summary(abl)$coefficients[2],2),"(Log(",namevar,"))","  R2=",round(abls$r.squared,2)),family="serif")
  mtext(text=paste0('Log(Rank ',namevar,')= ',round(summary(abl)$coefficients[1],2),round(summary(abl)$coefficients[2],2),"(Log(",namevar,"))","  R2=",round(abls$r.squared,2))
        ,side=1,line=2,family="serif")
  dev.off()}