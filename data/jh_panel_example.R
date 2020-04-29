
##### Arguments #######
# ds_conf: dataset of confirmed cases from the John Hopkins github
# ds_deaths: dataset of deaths from the John Hopkins github
# ncases_start: number of cases to start as day 1.



panel_jh = function(ds_conf,ds_death, ncases_start){
  
  ds_conf_melt = melt(ds_conf%>%select(-Lat,-Long),id.vars = c("Country.Region","Province.State"))
  ds_death_melt = melt(ds_death%>%select(-Lat,-Long),id.vars = c("Country.Region","Province.State"))
  
  
  colnames(ds_conf_melt) = c("country","subregion","date","cases")
  colnames(ds_death_melt) = c("country","subregion","date","deaths")
  
  ds_conf_melt$date = as.Date(str_sub(ds_conf_melt$date,2),format = "%m.%d.%y")
  ds_death_melt$date = as.Date(str_sub(ds_death_melt$date,2),format = "%m.%d.%y")
  
  ds_melt = ds_conf_melt%>%inner_join(ds_death_melt,by = c("date","country","subregion"))
  
  ds_melt_sub = ds_melt %>%
    filter(subregion!="") %>%
    filter(country %in% (ds_melt%>%filter(subregion==""))$country ==FALSE)
  
  ds_melt_country = ds_melt_sub %>%
    group_by(date,country) %>%
    summarise(cases = sum(cases), deaths = sum(deaths))
  ds_melt_country$subregion = "FULL"
  
  ds_melt = rbind(as.data.frame(ds_melt),as.data.frame(ds_melt_country))
  ds_melt$subregion[ds_melt$subregion==""] = "FULL"
  
  ds_melt = ds_melt %>% filter(cases>=ncases_start)
  
  ds_melt = ds_melt %>% arrange(country, subregion, date)
  
  z = rep(NA,nrow(ds_melt))
  for(i in 1:nrow(ds_melt)){
    if(i==1){
      z[i]=1
      next}
    if(ds_melt$country[i]==ds_melt$country[i-1]&ds_melt$subregion[i]==ds_melt$subregion[i-1]){
      z[i] = z[i-1]+1
    }else{z[i] = 1}
  }
  ds_melt$day = z
  
  return(ds_melt)
}

###############################
################# Example #####
###############################

library(tidyverse)
library(reshape2)
confirmed_jh = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
deaths_jh = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

panel = panel_jh(confirmed_jh, deaths_jh, 100)
