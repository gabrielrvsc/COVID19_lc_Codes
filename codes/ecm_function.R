
####### Arguments ##########
#
# 1) modelpanel: A panel in the same format as is the folder date. The full country must have subregion = FULL in the panel
# 2) M: Forecasting horizon
# 3) ssize: Size of the sample to be used. The model will use the ssize most recent observations
# 4) inflate: How may observations will be inflated. The most recent observation will be repeated "inflate" times, the one before "inflate"-1 and so on.
# 5) is.cv: FALSE if the model is to compute confidence intervals.
# 6) alpha_s: 1 for lasso, 0 for ridge. 

####### Returns #########
#
# A list where the first object inside contains the forecasts for cases and deaths and the 95% confidence intervals

run_lasso = function(modelpanel,M,target = "Brazil",ssize = 30, inflate = 4, is.cv = FALSE, alpha_s = 1){
  cases = dcast(modelpanel,day~country, value.var = "cases")
  y_end = which(is.na(cases[,target]))[1]
  remove = which(is.na(cases[y_end+1,]))
  cases_filt = cbind(cases[,-remove], target = cases[,target])%>% select(-day)
  
  
  df = cases_filt[1:(y_end+M-1),]
  
  insample = df[(1:y_end+1-2),]
  oos = df[-c(1:y_end+1-2),]
  
  N = min(nrow(insample),ssize)
  
  idx = c()
  if(inflate>0){
    for(l in 1:inflate){
      idx = c(idx,rep(N+1-l,inflate+1-l))
    }
  }
  
  idx = c(idx,1:N)
  #idx = c(1:N)
  
  
  y = tail(log(insample$target),N)
  x = tail(log(as.matrix(insample%>%select(-target))),N)
  xout = log(as.matrix(oos%>%select(-target)))
  y = y[idx]
  x = x[idx,]
  
  remove = which(is.na(xout),arr.ind = TRUE)
  if(nrow(remove)>0){
    remove = unique(remove[,2])
    x = x[,-remove]
    xout = xout[,-remove]
  }
  
  
  model = ic.glmnet(x,y, alpha = alpha_s)
  
  nvar = model$nvar
  r2 = exp(var(fitted(model)[1:N]))/exp(var(y[1:N]))
  pval = pp.test(model$residuals[1:N])$p.value 
  variables = names(which(coef(model)!=0))[-1]
  
  ehat = head(tail(model$residuals,N),N-1)
  
  xd = tail(diff(x),N-1)
  xd[,which(coef(model)[-1]==0)] = 0
  dy = tail(diff(y),N-1)
  
  didx = idx-1
  didx = didx[didx>0]
  xd = cbind(xd,ehat)
  
  dy = dy[didx]
  xd = xd[didx,]
  
  dmodel = lm(dy ~ xd)
  c = coef(dmodel)
  
  prlog = c()
  for(h in 1:M){
    
    if(h==1){
      deltaxt = xout[h,]-tail(x,1)
      p3 = -c[length(c)]*sum(coef(model)[-1]*tail(x,1),na.rm=TRUE)
      p4 = (1+c[length(c)])*tail(y,1)
    }else{
      deltaxt = xout[h,]-xout[h-1,]
      p3 = -c[length(c)]*sum(coef(model)[-1]*xout[h-1,],na.rm=TRUE)
      p4 = (1+c[length(c)])*prlog[h-1]
    }
    p1 = (c[1] - coef(model)[1]*c[length(c)])
    p2 = sum(c[2:(length(c)-1)]*deltaxt,na.rm = TRUE)
    prlog[h] = p1+p2+p3+p4
  }
  
  fitted = c(tail(fitted(dmodel),N-1))
  fitted[1] = tail(y,N)[1] + fitted[1]
  for(i in 2:length(fitted)){
    fitted[i] = fitted[i-1]+fitted[i]
  }
  residuals = tail(y,N-1)-fitted
  
  alpha0 = mean(exp(model$residuals))
  pr = alpha0*exp(prlog)
  
  pr_mat = data.frame(forecast = pr)
  
  modelstat = c(r2 = r2, pval = pval, nvar = nvar, sd = sd(residuals),dsd = sd(dmodel$residuals),
                alpha0 = alpha0)
  
  ############  
  
  max_date = max((modelpanel%>%filter(country==target))$date)
  max_day = max((modelpanel%>%filter(country==target))$day)
  
  pr_mat$date = seq(max_date+1, length.out = nrow(pr_mat), by = "day")
  pr_mat$day = seq(max_day+1, length.out = nrow(pr_mat), by = 1)
  
  pr_mat = pr_mat[,c(2,3,1)]
  
  tgt_mat = matrix(0,M,1000)
  
  if(is.cv==FALSE){
    for(m in 1:1000){
      tgt = c()
      for(i in 1:M){
        if(i==1){
          aux = log((1/modelstat["alpha0"])*pr_mat[i,"forecast"]) + rnorm(1,0,modelstat["dsd"])
          tgt[i] = max(aux,tail(y,1))
        }else{
          aux = log(pr_mat[i,"forecast"])-log(pr_mat[i-1,"forecast"])
          tgt[i] = max(tgt[i-1] + rnorm(1,aux,modelstat["dsd"]),tgt[i-1])
        }
      }
      tgt = modelstat["alpha0"]*exp(tgt)
      tgt_mat[,m] = tgt
    }
  }
  tgt_matq = apply(tgt_mat,1,function(x)quantile(x,probs = c(0.025,0.975)))
  pr_mat[,"lb"] = tgt_matq[1,]
  pr_mat[,"ub"] = tgt_matq[2,]
  
  
  aux = modelpanel %>% filter(country==target,subregion=="FULL")
  
  dforecast = diff(c(max(aux$cases),pr_mat$forecast))
  dtgt_mat = rbind(tgt_mat[1,]-exp(tail(y,1)),diff(tgt_mat))
  dtgt_matq = t(apply(dtgt_mat,1,function(x)quantile(x,probs = c(0.025,0.975))))
  
  pr_mat$delta_forecast = dforecast
  pr_mat$delta_lb = dtgt_matq[,1]
  pr_mat$delta_ub = dtgt_matq[,2]
  
  dforecastpc = TTR::ROC(c(max(aux$cases),pr_mat$forecast), type = "discrete")[-1]
  dlbpc = ((pr_mat$delta_lb)*(dforecastpc))/(pr_mat$delta_forecast)
  dubpc = ((pr_mat$delta_ub)*(dforecastpc))/(pr_mat$delta_forecast)
  
  pr_mat$rate_forecast = dforecastpc
  pr_mat$rate_lb = dlbpc
  pr_mat$rate_ub = dubpc
  
  #########
  death_ratio = modelpanel%>%filter(country==target) %>% arrange(date)
  #death_ratio = mean(tail(death_ratio$deaths,1)/tail(death_ratio$cases,1))
  
  mdeaths = lm(tail(log(death_ratio$deaths),5)~tail(log(death_ratio$cases),5))
  prd = exp(cbind(1,log(pr_mat$forecast))%*%coef(mdeaths))
  prdlb = exp(cbind(1,log(pr_mat$lb))%*%coef(mdeaths))
  prdub = exp(cbind(1,log(pr_mat$ub))%*%coef(mdeaths))
  
  pr_mat$forecast_deaths =  prd#pr_mat$forecast*death_ratio
  pr_mat$lb_deaths =  prdlb#pr_mat$lb*death_ratio
  pr_mat$ub_deaths =  prdub#pr_mat$ub*death_ratio
  
  
  ddeaths = diff(c(max(aux$deaths),pr_mat$forecast_deaths))
  tgt_mat_deaths = matrix(exp(cbind(1,log(as.vector(tgt_mat)))%*%coef(mdeaths)),ncol = 1000)
  dtgt_mat_deaths = rbind(tgt_mat_deaths[1,]-max(aux$deaths),diff(tgt_mat_deaths))
  dtgt_matq_deaths = t(apply(dtgt_mat_deaths,1,function(x)quantile(x,probs = c(0.025,0.975))))
  
  pr_mat$delta_forecast_deaths = ddeaths
  pr_mat$delta_lb_deaths = dtgt_matq_deaths[,1]
  pr_mat$delta_ub_deaths = dtgt_matq_deaths[,2]
  
  ddeathspc = TTR::ROC(c(max(aux$deaths),pr_mat$forecast_deaths), type = "discrete")[-1]
  dlbpc_deaths = ((pr_mat$delta_lb_deaths)*(ddeathspc))/(pr_mat$delta_forecast_deaths)
  dubpc_deaths = ((pr_mat$delta_ub_deaths)*(ddeathspc))/(pr_mat$delta_forecast_deaths)
  
  pr_mat$rate_forecast_deaths = ddeathspc
  pr_mat$rate_lb_deaths = dlbpc_deaths
  pr_mat$rate_ub_deaths = dubpc_deaths
  pr_mat[which(pr_mat<0,arr.ind = TRUE)] = 0
  res = list(result = pr_mat, countries = variables, betas = coef(model), betasecm = coef(dmodel), modelstat = modelstat)
}

