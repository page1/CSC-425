library(parallel)
library(dplyr)

backtest <- function(m1,rt,orig,h,fixed=NULL,inc.mean=TRUE, inc.drift=FALSE){
  # m1: is a time-series model object
  # orig: is the starting forecast origin
  # rt: the time series
  # xre: the independent variables
  # h: forecast horizon
  # fixed: parameter constraint
  # inc.mean: flag for constant term of the model.
  # inc.drift: flag for drift term of the model (only for differenced TS).
  #
  xre = m1$xreg
  regor=c(m1$arma[1],m1$arma[6],m1$arma[2])
  seaor=list(order=c(m1$arma[3],m1$arma[7],m1$arma[4]),period=m1$arma[5])
  length_rt=length(rt)
  if(!is.null(xre) && !is.matrix(xre)) xre=as.matrix(xre)
  if(!is.na(m1$coef["drift"])) inc.drift=TRUE 	
  ncx=ncol(xre)
  if(orig > length_rt)orig=length_rt
  if(h < 1) h=1
  rmse=rep(0,h)
  mabso=rep(0,h)
  mape=rep(0,h)
  smape=rep(0,h)
  #nori=length_rt-orig
  #err=matrix(0,nori,h)
  #fcast=matrix(0, nori,h)
  #obs=matrix(0, nori,h)
  #sumerr=matrix(0,nori,h)
  jlast=length_rt-h
  results <- mclapply(orig:jlast, function(n){
    jcnt=n-orig+1
    x=rt[jcnt:n]
    if (!is.null(xre)){
      pretor=xre[jcnt:n,]
      mm=Arima(x,order=regor,seasonal=seaor,xreg=pretor,method="ML",fixed=fixed,include.mean=inc.mean, include.drift=inc.drift, lambda=m1$lambda)
      nx=xre[(n+1):(n+h),]
      if(h==1) nx=matrix(nx,1,ncx)
      fore=forecast.Arima(mm,xreg=nx)
    }
    else {
      mm=Arima(x,order=regor,seasonal=seaor,xreg=NULL,method="ML",fixed=fixed,include.mean=inc.mean,include.drift=inc.drift, lambda=m1$lambda)
      fore=forecast.Arima(mm,h)
    }
    kk=min(length_rt,(n+h))
    # nof is the effective number of forecats at the forecast origin n.
    nof=kk-n
    pred=fore$mean[1:nof]
    obsd=rt[(n+1):kk]
    time = time(rt[(n+1):kk])
    
    
    data.frame(ahead_n = 1:length(obsd),
               time = time,
               obs = obsd,
               fcast = pred,
               err = obsd-pred,
               sumerr = abs(obsd)+abs(pred))
  }, mc.cores = 8)
  
  results <- do.call("rbind", results)
  
  summary_of_results <- results %>%
    group_by(ahead_n) %>%
    summarize(mabso = mean(abs(err), na.rm = T),
              rmse = sqrt(mean(err^2, na.rm = T)),
              mape = mean(abs(err/obs), na.rm = T),
              smape = mean(abs(err)/(sumerr/2), na.rm = T))
  
  
  print(summary_of_results)
  return(results)
}