################################# clean workspace           #################################################

rm(list=ls())

############################### set option to do wls/ols ####################################################

wls=1

################################# loading required packages #################################################

# install.packages("readxl")
# install.packages('car')
# install.packages('multiwayvcov')
# install.packages("lmtest")
# install.packages("broom")
# install.packages('stargazer')
# install.packages('vars')

library(readxl)
library(car)
library(multiwayvcov)
library(lmtest)
library(broom)
library(sandwich)
library(stargazer)
library(ggplot2)
library(vars)

################################# define helping routines   #################################################

log.diff<-function(vec){
  vec=log(t(vec))
  return(vec[-1]-vec[-length(vec)])
}

geom<-function(x){
  n=length(x)
  return(prod((1+x))^(1/n))-1
}

to.num<-function(x){
  x=data.frame(x)
  h<-matrix(nrow = nrow(x),ncol = ncol(x))
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      h[i,j]=as.numeric(x[i,j])
    }
  }
  return(h)
}

year<-function(vec,start=1,sq=1){
  vec=vec+sq-1
  return(vec/4-(vec%%4)/4+start)
}

################################# load raw data            ##################################################

################################## Oxford Covid-19 Government Response Tracker ##############################

mc_raw<-read_xlsx('C:/Users/BongBong/Desktop/nat_ac/OxCGRT_data/OxCGRTUS_timeseries_all.xlsx',sheet = 1) # restrictions
ps_raw<-read_xlsx('C:/Users/BongBong/Desktop/nat_ac/OxCGRT_data/OxCGRTUS_timeseries_all.xlsx',sheet = 4) # policy support
ic_raw<-read_xlsx('C:/Users/BongBong/Desktop/nat_ac/OxCGRT_data/OxCGRTUS_timeseries_all.xlsx',sheet = 'confirmed_cases') # confirmed cases
mr_raw<-read_xlsx('C:/Users/BongBong/Desktop/nat_ac/OxCGRT_data/OxCGRTUS_timeseries_all.xlsx',sheet = 'c7_movementrestrictions') # c7_movementrestrictions
sh_raw<-read_xlsx('C:/Users/BongBong/Desktop/nat_ac/OxCGRT_data/OxCGRTUS_timeseries_all.xlsx',sheet = 'c6_stay_at_home_requirements') # c6_stay_at_home_requirements

################################## state codes and names ####################################################

# excluding DC since cooks D suggests overproportional influence on coefficients

us_codes<-mc_raw$region_code[-8]
us_names<-mc_raw$region_name[-8]

################################## get quarterly means   ####################################################

cutoffs<-c('region_name','31Mar2020','30Jun2020','30Sep2020','31Dec2020')
# drop 2021 observations
mc_raw<-mc_raw[,-grep('2021',names(mc_raw))]
ps_raw<-ps_raw[,-grep('2021',names(ps_raw))]
mr_raw<-mr_raw[,-grep('2021',names(mr_raw))]
sh_raw<-sh_raw[,-grep('2021',names(sh_raw))]
ic_raw<-ic_raw[,names(ic_raw) %in% cutoffs]
ic_raw[,-(1:2)]<-ic_raw[,-(1:2)]-ic_raw[,-c(1,5)]

Q1=c('Jan','Feb','Mar')
Q2=c('Apr','May','Jun')
Q3=c('Jul','Aug','Sep')
Q4=c('Oct','Nov','Dec')


for(t in 1:4){
       assign(paste('mc_Q',t,sep = ''),cbind(us_names,rowMeans(mc_raw[,c(grep(get(paste('Q',t,sep = ''))[1],names(mc_raw)),grep(get(paste('Q',t,sep = ''))[2],names(mc_raw)),grep(get(paste('Q',t,sep = ''))[3],names(mc_raw)))])))
       assign(paste('ps_Q',t,sep = ''),cbind(us_names,rowMeans(ps_raw[,c(grep(get(paste('Q',t,sep = ''))[1],names(ps_raw)),grep(get(paste('Q',t,sep = ''))[2],names(ps_raw)),grep(get(paste('Q',t,sep = ''))[3],names(ps_raw)))])))
       assign(paste('mr_Q',t,sep = ''),cbind(us_names,rowMeans(mr_raw[,c(grep(get(paste('Q',t,sep = ''))[1],names(mr_raw)),grep(get(paste('Q',t,sep = ''))[2],names(mr_raw)),grep(get(paste('Q',t,sep = ''))[3],names(mr_raw)))]==2)))
       assign(paste('sh_Q',t,sep = ''),cbind(us_names,rowMeans(sh_raw[,c(grep(get(paste('Q',t,sep = ''))[1],names(sh_raw)),grep(get(paste('Q',t,sep = ''))[2],names(sh_raw)),grep(get(paste('Q',t,sep = ''))[3],names(sh_raw)))]>1)))
}

################################## get population weights                   ##################################

pop_raw<-read.csv("C:/Users/BongBong/Desktop/nat_ac/bea_data/pop_bea2020.csv",header=FALSE,stringsAsFactors = FALSE)
pop_raw<-pop_raw[pop_raw$V2 %in% us_names,c(2,6)]

################################## get production and income for all states ##################################

gsp_raw<-read.csv("C:/Users/BongBong/Desktop/nat_ac/bea_data/SQGDP9.csv",header=FALSE,stringsAsFactors = FALSE)
inc_raw<-read.csv("C:/Users/BongBong/Desktop/nat_ac/bea_data/SQINC1.csv",header=FALSE,stringsAsFactors = FALSE)[,-3]

################################## get 'tilde' values: difference to 'global' growth rate ####################

gsp_tilde=gsp_raw[gsp_raw$V2 %in% c('United States',us_names),-1]
inc_tilde=inc_raw[inc_raw$V2 %in% c('United States',us_names),-1]

gsp_tilde[-1,-1]=to.num(gsp_tilde[-1,-1])-matrix(rep(to.num(gsp_tilde[1,-1]),length(gsp_tilde$V2)-1),nrow = length(gsp_tilde$V2)-1,byrow = TRUE)
inc_tilde[-1,-1]=to.num(inc_tilde[-1,-1])-matrix(rep(to.num(inc_tilde[1,-1]),length(inc_tilde$V2)-1),nrow = length(inc_tilde$V2)-1,byrow = TRUE)

################################# prepare a data frame #######################################################

dta.us<-data.frame('state'=rep(us_names,each=63),'pit'=rep(1:63,length(us_names)))
dta.us$year=year(dta.us$pit,start = 2005,sq=1)
dta.us$gsp=0
dta.us$inc=0
dta.us$mc=0
dta.us$ps=0
dta.us$pop=0
dta.us$ic=0
dta.us$mr=0
dta.us$sh=0

################################# fill data frame     ########################################################

for (r in 1:nrow(dta.us)) {
  state=dta.us$state[r]
  pit=dta.us$pit[r]
  
  dta.us$gsp[r]=gsp_tilde[gsp_tilde[,1]==state,pit+1]
  dta.us$inc[r]=inc_tilde[inc_tilde[,1]==state,pit+1]
  dta.us$pop[r]=pop_raw[pop_raw[,1]==state,2]
  
  if(pit>=60){dta.us$mc[r]=get(paste('mc_Q',pit-59,sep = ''))[get(paste('mc_Q',pit-59,sep = ''))[,1]==state,2]
              dta.us$ps[r]=get(paste('ps_Q',pit-59,sep = ''))[get(paste('ps_Q',pit-59,sep = ''))[,1]==state,2]
              dta.us$mr[r]=get(paste('mr_Q',pit-59,sep = ''))[get(paste('mr_Q',pit-59,sep = ''))[,1]==state,2]
              dta.us$sh[r]=get(paste('sh_Q',pit-59,sep = ''))[get(paste('sh_Q',pit-59,sep = ''))[,1]==state,2]
              dta.us$ic[r]=ic_raw[c(ic_raw[,1])$region_name==toString(state),pit-59+1]/as.numeric(dta.us$pop[r])
    }
}

############################### make sure type of vars ins as desired ########################################

dta.us$gsp=as.numeric(dta.us$gsp)/100
dta.us$inc=as.numeric(dta.us$inc)/100
dta.us$mc=as.numeric(dta.us$mc)
dta.us$ps=as.numeric(dta.us$ps)
dta.us$pop=as.numeric(dta.us$pop)
dta.us$ic=as.numeric(dta.us$ic)
dta.us$mr=as.numeric(dta.us$mr)
dta.us$sh=as.numeric(dta.us$sh)

############################### option to do wls/ols #########################################################

if(wls==0){
  dta.us$pop=1
}

############################### first regression #############################################################

output='text'  # latex of text output???

first.reg=glm(inc~factor(state)+ic+gsp*ps*mr,data = dta.us,weights=dta.us$pop)
stargazer(coeftest(first.reg,vcovCL,cluster=dta.us[,c('state','year')]),type=output,omit = 'factor')

second.reg=glm(inc~factor(state)+ic+gsp*ps*sh,data = dta.us,weights=dta.us$pop)
stargazer(coeftest(second.reg,vcovCL,cluster=dta.us[,c('state','year')]),type=output,omit = 'factor')


dta.us$gmi=dta.us$gsp-dta.us$inc

chan.reg=glm(gmi~factor(state)+ic+gsp*ps*mr,data = dta.us,weights=dta.us$pop)
stargazer(coeftest(first.reg,vcovCL,cluster=dta.us[,c('state','year')]),coeftest(chan.reg,vcovCL,cluster=dta.us[,c('state','year')]),type=output,omit = 'factor')
