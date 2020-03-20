

library(dslabs)
library(tidyverse)    # includes readr
library(readxl)
library(lubridate)

# FICHIER 1 

path <- "~/Projects/cov19/COVID-19/who_covid_19_situation_reports/who_covid_19_sit_rep_time_series"
list.files()
fullpath<-file.path(path,"who_covid_19_sit_rep_time_series.csv")
file.exists(fullpath)

setwd("~/Projects/cov19/COVID-19/")
fullpath<-file.path("who_covid_19_situation_reports","who_covid_19_sit_rep_time_series","who_covid_19_sit_rep_time_series.csv")
list.files()
file.exists(fullpath)

dat <- read_csv(fullpath)
head(dat)
dat%>%filter(`Country/Region`=="China")
str(dat)
levels(factor(dat$`Country/Region`))
levels(factor(dat$`Province/States`))
levels(factor(dat$`WHO region`))

dat[str_detect(colnames(dat),"\\d")]
datecols<-colnames(dat[str_detect(colnames(dat),"\\d")])
cov<-dat %>% gather(date,count,datecols) %>% filter(!is.na(count)) %>% rename(country=`Country/Region`, region=`WHO region`, state=`Province/States`)

# cov %>% count(country, region,state)%>%view()
# cov%>%group_by(country,region,state)%>%filter(date==max(date) & state %in% c("Deaths","Confirmed"))%>%summarize(max(date),sum(count))%>%view()
# cov%>%filter(region=="European Region")

cov2<-cov%>%filter(is.na(state)) %>% mutate(region=factor(region))

#cov2%>%group_by(date,region)%>%summarize(sum(count))

# cov2%>% group_by(date,region)%>%
#   summarize(total=sum(count))%>%arrange(region,date)%>%view()
# str(cov2)
# class(cov2)

cov2<-cov2%>%mutate(date=mdy(date))

cov2%>% group_by(date,region)%>%
  summarize(total=sum(count))%>%
  ggplot(aes(x=date,y=total,colour=region))+
  geom_line(na.rm=TRUE)+theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_log10()

# FILE 2
# ---------------
setwd("~/Projects/cov19/COVID-19/")
fullpath<-file.path("csse_covid_19_data/csse_covid_19_time_series")
list.files(fullpath)

timeseriesfile <- function(fullpath, suffix) {
  filename<-paste("time_series_19-covid-", str_trim(suffix),".csv", sep = "")
  path<-file.path(fullpath,filename)
  dat <- read_csv(path)
  dat[str_detect(colnames(dat),"\\d")]
  datecols<-colnames(dat[str_detect(colnames(dat),"\\d")])
  dat %>% gather(date,count,datecols) %>% 
    filter(!is.na(count)) %>% 
    rename(country=`Country/Region`,  state=`Province/State`) %>% 
    mutate(type=suffix)%>%
    mutate(date=mdy(date), country=factor(country), state=factor(state))
}
deaths <-timeseriesfile (fullpath,"Deaths")
# deaths
confirmed <-timeseriesfile (fullpath,"Confirmed")
# confirmed
recov <-timeseriesfile (fullpath,"Recovered")
# recov
# str(recov)
# levels(recov$state)
# levels(recov$country)

time_series <- rbind(confirmed, deaths, recov)

# evol temporelle Chine France echelle LOG10
time_series %>% group_by(date,country, type)%>%
  summarize(total=sum(count))%>%
  filter(country %in% c("France","China")) %>%
  ggplot(aes(x=date,y=total,colour=type))+
  geom_line(na.rm=TRUE)+theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_log10() +
  facet_wrap(~country)

 # evol temporelle  6 pays echelle LOG2
time_series %>% group_by(date,country, type)%>%
  summarize(total=sum(count))%>%
  filter(country %in% c("France","China","Spain","Italy","US","Germany")) %>% 
  ggplot(aes(x=date,y=total,colour=type))+
  geom_line(na.rm=TRUE)+theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2")+
  facet_wrap(~country, ncol = 3)

# evol temporelle 20 fev. pays europe + US 
time_series %>% group_by(date,country, type)%>%
  summarize(total=sum(count))%>%
  filter(country %in% c("France","United Kingdom","Spain","Italy","US","Germany") &
           date>="2020-02-20") %>% 
  ggplot(aes(x=date,y=total,colour=type))+
  geom_line(na.rm=TRUE)+theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~country, ncol = 3)

# comparaison totaux actuels 6 pays 
time_series %>% group_by(country, type)%>%
  filter(country %in% c("France","China","Spain","Italy","US","Germany")) %>% 
  filter(date==max(date)) %>% 
  summarize(total=sum(count),date=max(date))%>% 
  # spread(type,total) %>%
  ggplot(aes(x=country,total, fill=type))+geom_bar(stat="identity") 
  
# comparaison ratios actuels 6 pays
time_series %>% spread(type,count) %>% 
  filter(country %in% c("France","China","Spain","Italy","US","Germany")) %>%
  #mutate(DeathsPer=ifelse(Confirmed!=0,Deaths/Confirmed*100,0), RecovPer= ifelse(Confirmed!=0,Recovered/Confirmed*100,0)) %>%
  group_by(country)%>%
  filter(date==max(date)) %>% 
  summarize(deathratio=100*sum(Deaths)/sum(Confirmed),
            recovratio=100*sum(Recovered)/sum(Confirmed), 
            illratio=100-deathratio-recovratio, date=max(date))  %>%
  gather(type,ratio,deathratio, recovratio, illratio) %>%
  ggplot(aes(x=country,ratio, fill=type))+geom_bar(stat="identity") 

# todo: top 10
# evolution temporelle recente
