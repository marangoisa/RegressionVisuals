#' Returns a matrix and LaTex table with results from a set of linear regressions (using lm())
#' 
#' @param form list with formulas
#' @param datav data for regressions
#' @param namescol tittles of regression
#' @param namesrow regressors name
#' @param stars TRUE for stars in the table
#' @param wgh name of variable used to weight regression
#' @param rbst TRUE for robust errors 
#' @param clst name of variable used to cluster errors
#' @param dec number of decimals in the table
#' @return $matrix returns a matrix with all the results from the regression, $latex returns a latex table with all the results from the regression
#' @examples 
#' form<-list()
#' form[[1]]<-log(Total_Trade_Amount_2015_Thousand_USD)~log(GDP_2015_USD)
#' form[[2]]<-log(Total_Trade_Amount_2015_Thousand_USD)~log(GDP_2015_USD)+log(Distance_from_Largest_City_to_NYC_miles)
#' namescol<-c('GDP 2015', 'GDP & Distance')
#' namesrow<-c('GDP 2015', 'Distance')
#' lmtable_res<-regtable(form,datav=trade,namescol,namesrow)
#' mtrx<-lmtable_res$matrix
#' lmtable_res$latex
regtable<-function(form,datav,namescol,namesrow,stars,wgh,rbst,clst,dec){
  results<-list()
  if(!require(sandwich, quietly = TRUE)){install.packages('sandwich',dep = TRUE,quietly = TRUE)}
  if(!require(lmtest, quietly = TRUE)){install.packages('lmtes',dep = TRUE,quietly = TRUE)}
  if(!require(xtable, quietly = TRUE)){install.packages('xtable',dep = TRUE,quietly = TRUE)}
  if(!missing(clst)){rbst<-TRUE}
  if(missing(dec)){dec<-2}
  if(missing(stars)){stars=FALSE}
  if(!missing(wgh)){datav$w<-datav[,names(datav)==wgh]}
  nc=length(form)
  reg=(length(namesrow))*2+4
  if(stars){nncol=(nc*2)}else{nncol=nc}
  mat <- matrix(" ", nrow = (reg), ncol=nncol)
  colnm<-c(1:nncol)
  for(i in 1:length(colnm)){colnm[i]<-paste0('(',colnm[i],')')}
  i=1
  j=1
  while(j<=length(form)){
    if(!missing(wgh)){regi<-lm(form[[j]],data=datav,weights=w)}else{regi<-lm(form[[j]],data=datav)}
    if(!missing(rbst)){if(!missing(clst)){coefsa<-coeftest(regi, vcov = vcovCL(regi, cluster =clst))}else{coefsa<-coeftest(regi, vcov = vcovCL(regi))}}else{coefsa<-summary(regi)$coefficients}
    nr<-nrow(coefsa)
    mat[(seq(1,(nr*2),2)),i]<-format(round(coefsa[1:nr],dec),nsmall=2)
    aaa<-format(round(coefsa[(nr*3+1):(nr*4)],2),nsmall=2)
    aab<-aaa
    for(ii in 1:length(aaa)){if(aaa[ii]<0.001){aab[ii]<-"***"}else{if(0.001<aaa[ii]&aaa[ii]<0.01){aab[ii]<-"**"}else{if(0.01<aaa[ii]&aaa[ii]<0.05){aab[ii]<-"*"}else{if(0.05<aaa[ii]&aaa[ii]<0.1){aab[ii]<-"."}else{aab[ii]<-" "}}}}}
    mat[(seq(2,(nr*2),2)),i]<-paste0('(',format(round(coefsa[(nr+1):(nr*2)],dec),nsmall=2),')')
    mat[(reg-1),i]<-format(round(summary(regi)$adj.r.squared,dec),nsmall=2)
    mat[(reg),i]<-summary(regi)$df[[2]]+summary(regi)$df[[1]]
    if(stars){mat[(seq(1,(nr*2),2)),i+1]<-aab
    i=i+2}else{i=i+1}
    j=j+1}
  mat<- rbind(colnm,mat)  
  namesrowv<-c(' ','Constant')
  spacer<-c('  ')
  for(ii in 1:length(namesrow)){
    namesrowv<-c(namesrowv,spacer,namesrow[ii])
    spacer<-paste0(spacer,' ')}
  namesrowv<-c(namesrowv,spacer,'Adjusted R-squared','Number observations')
  namescolv<-c()
  if(stars){for(iii in 1:length(namescol)){namescolv<-c(namescolv,namescol[iii],' ')}}else{namescolv<-namescol}
  rownames(mat)<-namesrowv
  colnames(mat)<-namescolv
  datav$w<-NULL
  results$matrix<-mat
  results$latex<-xtable(mat)
  return(results)}
