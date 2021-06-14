################################# clean workspace           #################################################

rm(list=ls())

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
  return(diff(vec,lag = 4))
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

################################# select sample countries   #################################################

sel=1:22 # c(3,9,10,14,16) # -c(6,16,21) # tmp.sel
codes=c('as','au','be','br','cd','cz','dn','fn','fr','ge','gr','it','jp','nl','nw','po','ru','sp','sw','tk','uk','us')[sel]
names=c('Australia','Austria','Belgium','Brazil','Canada','Czech Republic','Denmark','Finland','France','Germany','Greece','Italy','Japan','Netherlands','Norway','Portugal','Russian Federation','Spain','Sweden','Turkey','United Kingdom','United States')[sel]
obs=length(names)

################################# un-data B1GQ_S1, P3_S1    #################################################

z=0
for(k in codes){
  z=z+1
  assign(k,read_xlsx(paste('C:/Users/BongBong/Desktop/nat_ac/un_data/',names[z],'.xlsx',sep = '')))
}

################################## eurostat raw income data #################################################

disp_raw<-read_xlsx("C:/Users/BongBong/Desktop/nat_ac/eurostat_data/NASQ_10_NF_TR__custom_9241181620470056331.xlsx",sheet = 3)

################################## eurostat data to get deflator ############################################

raw_defl<-read_xlsx("C:/Users/BongBong/Desktop/nat_ac/eurostat_data/NAMQ_10_GDP__custom_9282471620593160149.xlsx",sheet = 3)

################################## eurostat nat income data     #############################################

natinc_raw<-read_xlsx("C:/Users/BongBong/Desktop/nat_ac/eurostat_data/NASQ_10_NF_TR__custom_9524881621008698098.xlsx",sheet = 3)

################################## Oxford Covid-19 Government Response Tracker ##############################

mc_raw<-read_xlsx('C:/Users/BongBong/Desktop/nat_ac/OxCGRT_data/OxCGRT_timeseries_all.xlsx',sheet = 1) # restrictions
ps_raw<-read_xlsx('C:/Users/BongBong/Desktop/nat_ac/OxCGRT_data/OxCGRT_timeseries_all.xlsx',sheet = 4) # policy support

################################## incidence data    ########################################################

ic_raw<-read_xlsx('C:/Users/BongBong/Desktop/nat_ac/OxCGRT_data/OxCGRT_timeseries_all.xlsx',sheet = 'confirmed_cases') # confirmed cases
pop_raw<-read_xlsx('C:/Users/BongBong/Desktop/nat_ac/eurostat_data/DEMO_PJAN$DEFAULTVIEW1623014373650.xlsx',sheet = 3) # population at 01/01/2019 pre covid such that deaths do not influece incidence 

################################## select only where data is available  #####################################

sel=names %in% raw_defl$`Data extracted on 09/05/2021 23:13:09 from [ESTAT]` # subset where eurostat data is available
names=names[sel]
codes=codes[sel]
obs=length(names)

################################## get y,c and i 'tilde' ####################################################

row_gdp='Gross domestic product at market prices [B1GQ_S1]'
row_con='Final consumption expenditure [P3_S1]'

gdp_tot=rep(0,60)
con_tot=rep(0,60)

for (i in codes) { # extract relevant variables
  assign(paste("gdp",i,sep="_"),get(i)[get(i)$Indicator==row_gdp & is.na(get(i)$`Reference year`)==0,][1,][7:66])
  assign(paste("con",i,sep="_"),get(i)[get(i)$Indicator==row_con & is.na(get(i)$`Reference year`)==0,][1,][7:66])
  gdp_tot=gdp_tot+get(paste("gdp",i,sep="_"))
  con_tot=con_tot+get(paste("con",i,sep="_")) 
}

for (i in c(codes,'tot')){ # get log growth rates
  assign(paste("gdp",i,sep="_"),log.diff(get(paste("gdp",i,sep="_"))))
  assign(paste("con",i,sep="_"),log.diff(get(paste("con",i,sep="_"))))
}

for (i in codes){ # get tilde
  assign(paste("gdp",i,sep="_"),get(paste("gdp",i,sep="_"))-gdp_tot)
  assign(paste("con",i,sep="_"),get(paste("con",i,sep="_"))-con_tot)
}

################################## get disp inc 'tilde'  ####################################################

disp<-disp_raw[disp_raw$`Data extracted on 08/05/2021 12:34:19 from [ESTAT]` %in% c(names,'TIME'),]
cols=c(1)
for(t in 2006:2020){
  cols=c(cols,grep(toString(t), disp[1,]))
}
disp=disp[,cols]
inc_tot=rep(0,60)
z=0
bad=c() # verctor for index of countries with unsufficient data
for(k in names){
  z=z+1
  c=codes[z]
  tmp=disp[disp[,1]==k,][,-1]
  assign(paste("inc",c,sep="_"),c(to.num(tmp[2,])-to.num(tmp[1,])))
  if(is.na(sum(get(paste("inc",c,sep="_"))))==0){
  inc_tot=inc_tot+get(paste("inc",c,sep="_"))
  }
  else{bad=c(bad,z)}
}

names=names[-bad]
codes=codes[-bad]
obs=length(codes)

for (i in c(codes,'tot')){ # get log growth rates
  assign(paste("inc",i,sep="_"),log.diff(t(get(paste("inc",i,sep="_")))))
}

#begin deflate income
defl<-raw_defl[raw_defl$`Data extracted on 09/05/2021 23:13:09 from [ESTAT]` %in% c('TIME','UNIT (Labels)',names),]
col=c(1,grep('20',defl[1,]))
defl<-to.num(defl[,col][-(1:2),-1])
tot.defl<-colSums(defl)
defl=defl[,seq(1,length(defl[1,]),2)]/defl[,seq(2,length(defl[1,]),2)]
tot.defl=tot.defl[seq(1,length(tot.defl),2)]/tot.defl[seq(2,length(tot.defl),2)]
tot.defl=log.diff(t(tot.defl))
z=0
for(i in codes){
  z=z+1
  assign(paste("defl",i,sep="_"),log.diff(t(defl[z,]))-tot.defl)
}
#end deflate income
for (i in codes){ # get tilde
  assign(paste("inc",i,sep="_"),get(paste("inc",i,sep="_"))-inc_tot-get(paste("defl",i,sep="_")))
}

################################# get net and gross nat income ###############################################

natinc<-natinc_raw[natinc_raw$`Data extracted on 14/05/2021 18:11:41 from [ESTAT]` %in% c(names,'TIME'),]
cols=c(1,2)
for(t in 2006:2020){
  cols=c(cols,grep(toString(t), natinc[1,]))
}
natinc=natinc[,cols]

gni_tot=log.diff(t(colSums(to.num(natinc[natinc$`Data extracted on 14/05/2021 18:11:41 from [ESTAT]`!='TIME' & natinc$...2=='Gross national income at market prices',-(1:2)]))))
nni_tot=log.diff(t(colSums(to.num(natinc[natinc$`Data extracted on 14/05/2021 18:11:41 from [ESTAT]`!='TIME' & natinc$...2=='Net national income at market prices',-(1:2)]))))
z=0

for(k in names){
  z=z+1
  c=codes[z]
  assign(paste("nni",c,sep="_"),log.diff(t(c(to.num(natinc[natinc$`Data extracted on 14/05/2021 18:11:41 from [ESTAT]`==k & natinc$...2=='Net national income at market prices',-(1:2)]))))-nni_tot-tot.defl)
  assign(paste("gni",c,sep="_"),log.diff(t(c(to.num(natinc[natinc$`Data extracted on 14/05/2021 18:11:41 from [ESTAT]`==k & natinc$...2=='Gross national income at market prices',-(1:2)]))))-gni_tot-tot.defl)
}

################################# prepare measure data #######################################################

mc<-mc_raw[mc_raw$country_name %in% names,]   # here alphabetical ordering becomes importand
mc.2020<-mc[, -grep("2021", colnames(mc))]

mc.q1<-rowMeans(mc.2020[,c(grep("Jan", colnames(mc.2020)),grep("Feb", colnames(mc.2020)),grep("Mar", colnames(mc.2020)))])
mc.q2<-rowMeans(mc.2020[,c(grep("Apr", colnames(mc.2020)),grep("May", colnames(mc.2020)),grep("Jun", colnames(mc.2020)))])
mc.q3<-rowMeans(mc.2020[,c(grep("Jul", colnames(mc.2020)),grep("Aug", colnames(mc.2020)),grep("Sep", colnames(mc.2020)))])
mc.q4<-rowMeans(mc.2020[,c(grep("Oct", colnames(mc.2020)),grep("Nov", colnames(mc.2020)),grep("Dec", colnames(mc.2020)))])

ps<-ps_raw[ps_raw$country_name %in% names,]
ps.2020<-ps[, -grep("2021", colnames(ps))]

ps.q1<-rowMeans(ps.2020[,c(grep("Jan", colnames(ps.2020)),grep("Feb", colnames(ps.2020)),grep("Mar", colnames(ps.2020)))])
ps.q2<-rowMeans(ps.2020[,c(grep("Apr", colnames(ps.2020)),grep("May", colnames(ps.2020)),grep("Jun", colnames(ps.2020)))])
ps.q3<-rowMeans(ps.2020[,c(grep("Jul", colnames(ps.2020)),grep("Aug", colnames(ps.2020)),grep("Sep", colnames(ps.2020)))])
ps.q4<-rowMeans(ps.2020[,c(grep("Oct", colnames(ps.2020)),grep("Nov", colnames(ps.2020)),grep("Dec", colnames(ps.2020)))])

################################ prepare incidence data #######################################################

cutoffs<-c('country_name','31Mar2020','30Jun2020','30Sep2020','31Dec2020')
ic_raw<-ic_raw[,names(ic_raw) %in% cutoffs]
ic_raw[,-(1:2)]<-ic_raw[,-(1:2)]-ic_raw[,-c(1,5)]

ic_raw=ic_raw[c(t(ic_raw[,1])) %in% names,]
pop_raw=pop_raw[c(t(pop_raw[,1])) %in% names,1:2]
ic_meet=ic_raw

pop=c()
k=0
for (i in names) {
  k=k+1
  l=codes[k]
  assign(paste("ic",l,sep="_"),c(rep(0,52),to.num(ic_meet[ic_meet[,1]==i,-1])/to.num(pop_raw[pop_raw[,1]==i,-1])[1,1]))
  pop[k]=to.num(pop_raw[pop_raw[,1]==i,-1])[1,1]
}

############################## put everything into a df #######################################################

t=length(get(paste('gdp',codes[1],sep='_')))
dta<-data.frame(pit=1:t,'nat'=rep(codes[1],t),'con'=get(paste('con',codes[1],sep='_')),'inc'=get(paste('inc',codes[1],sep='_')),'gdp'=get(paste('gdp',codes[1],sep='_')),'ic'=get(paste('ic',codes[1],sep='_')),'mc'=rep(0,t),'ps'=rep(0,t),'nni'=get(paste('nni',codes[1],sep = '_')),'gni'=get(paste('gni',codes[1],sep = '_')))

for (i in codes[2:obs]) {
  tmp=data.frame(pit=1:t,'nat'=rep(i,t),'con'=get(paste('con',i,sep='_')),'inc'=get(paste('inc',i,sep='_')),'gdp'=get(paste('gdp',i,sep='_')),'ic'=get(paste('ic',i,sep='_')),'mc'=rep(0,t),'ps'=rep(0,t),'nni'=get(paste('nni',i,sep = '_')),'gni'=get(paste('gni',i,sep = '_')))
  dta=rbind(dta,tmp)
}

k=0
for (i in codes[1:obs]) {
  k=k+1
  dta[dta$nat==i & dta$pit==53,7]=mc.q1[k]
  dta[dta$nat==i & dta$pit==54,7]=mc.q2[k]
  dta[dta$nat==i & dta$pit==55,7]=mc.q3[k]
  dta[dta$nat==i & dta$pit==56,7]=mc.q4[k]
  dta[dta$nat==i & dta$pit==53,8]=ps.q1[k]
  dta[dta$nat==i & dta$pit==54,8]=ps.q2[k]
  dta[dta$nat==i & dta$pit==55,8]=ps.q3[k]
  dta[dta$nat==i & dta$pit==56,8]=ps.q4[k]

}

#################################################################################################################
################################### finish of data preperation ##################################################
#################################################################################################################


################################### looking at correlations    ##################################################

cor(dta$con,dta$gdp)  # full sample
cor(dta$inc,dta$gdp)

cor(dta$con[-(53:56)],dta$gdp[-(53:56)])  # pre 2020
cor(dta$inc[-(53:56)],dta$gdp[-(53:56)])

cor(dta$con[53:56],dta$gdp[53:56])  # 2020
cor(dta$inc[53:56],dta$gdp[53:56])

################################### regressions with all pits  ##################################################

r.s.c.1<-lm(con~factor(nat)+gdp,data = dta)
summary(r.s.c.1) # simple regression to get beta_u

r.s.i.1<-lm(inc~factor(nat)+gdp,data = dta)
summary(r.s.i.1) # simple regression to get beta_u


summary(lm(con~factor(nat)+gdp+ic+mc,data = dta)) # simple regression to get beta_u controlling for incidence and restrictions

summary(lm(inc~factor(nat)+gdp+ic+mc,data = dta)) # simple regression to get beta_u controlling for incidence and restrictions


summary(lm(con~factor(nat)+gdp+ic+mc+ps,data = dta)) # simple regression to get beta_u controlling for incidence and measures

summary(lm(inc~factor(nat)+gdp+ic+mc+ps,data = dta)) # simple regression to get beta_u controlling for incidence and measures


r.i.c.2<-lm(con~factor(nat)+gdp+ic+gdp*mc+ps,data = dta)
summary(lm(con~factor(nat)+gdp+ic+gdp*mc+ps,data = dta)) # simple regression to get beta_u interaction of gdp and restrictions

r.i.i.2<-lm(inc~factor(nat)+gdp+ic+gdp*mc+ps,data = dta)
summary(lm(inc~factor(nat)+gdp+ic+gdp*mc+ps,data = dta)) # simple regression to get beta_u interaction of gdp and restrictions


r.i.c.3<-lm(con~factor(nat)+gdp+ic+mc+gdp*ps,data = dta)
summary(r.i.c.3) # simple regression to get beta_u interaction of gdp and support

r.i.i.3<-lm(inc~factor(nat)+gdp+ic+mc+gdp*ps,data = dta)
summary(r.i.i.3) # simple regression to get beta_u interaction of gdp and support


r.i.c.1<-lm(con~factor(nat)+gdp+ic+mc*gdp*ps,data = dta)
summary(r.i.c.1) # simple regression to get beta_u triple interaction

r.i.i.1<-lm(inc~factor(nat)+gdp+ic+mc*gdp*ps,data = dta)
summary(r.i.i.1) # simple regression to get beta_u triple interaction

################################### results with clustered se ######################################################

full.inc=lm(inc~factor(nat)+gdp+ic+mc*gdp*ps,data = dta)
full.con=lm(con~factor(nat)+gdp+ic+mc*gdp*ps,data = dta)
cov.fi=cluster.vcov(full.inc,dta[,c(1)])
cov.fc=cluster.vcov(full.con,dta[,c(1)])
coeftest(full.inc,cov.fi)
coeftest(full.con,cov.fc)

################################### dynamic estimations ############################################################

# simple regression to get beta_u

b.con.1=c()
for(i in 4:56){
  reg.con.1<-lm(con~factor(nat)+gdp,data = dta[dta$pit %in% (i-3):i,])
  b.con.1[i-3]=reg.con.1$coefficients['gdp']
  assign(paste('r.ds.c',i-3,sep = '.'),reg.con.1)
}

b.inc.1=c()
for(i in 4:56){
  reg.inc.1<-lm(inc~factor(nat)+gdp,data = dta[dta$pit %in% (i-3):i,])
  b.inc.1[i-3]=reg.inc.1$coefficients['gdp']
  assign(paste('r.ds.i',i-3,sep = '.'),reg.inc.1)
}

plot(seq(as.Date("2007/12/31"), as.Date("2020/12/31"), by = "quarter"),b.con.1,type = 'b',ylab = 'beta_u',xlab = 'Year',ylim = c(0,1),col = 'blue')
lines(seq(as.Date("2007/12/31"), as.Date("2020/12/31"), by = "quarter"),b.inc.1,type = 'b',ylab = 'beta_u',xlab = 'Year',ylim = c(0,1),col = 'red')
title('Simple regression estimation of beta_u: income=red consum=blue')

# simple regression to get beta_u controlling for incidence and restrictions

b.con.2=c()
for(i in 4:56){
  reg.con.2<-lm(con~factor(nat)+gdp+ic+mc,data = dta[dta$pit %in% (i-3):i,])
  b.con.2[i-3]=reg.con.2$coefficients['gdp']
}

b.inc.2=c()
for(i in 4:56){
  reg.inc.2<-lm(inc~factor(nat)+gdp+ic+mc,data = dta[dta$pit %in% (i-3):i,])
  b.inc.2[i-3]=reg.inc.2$coefficients['gdp']
}

plot(seq(as.Date("2007/12/31"), as.Date("2020/12/31"), by = "quarter"),b.con.2,type = 'b',ylab = 'beta_u',xlab = 'Year',ylim = c(0,1),col = 'blue')
lines(seq(as.Date("2007/12/31"), as.Date("2020/12/31"), by = "quarter"),b.inc.2,type = 'b',ylab = 'beta_u',xlab = 'Year',ylim = c(0,1),col = 'red')
title('Regression estimation of beta_u controlling for incidence and restrictions: income=red consum=blue')

# simple regression to get beta_u controlling for incidence and measures

b.con.3=c()
for(i in 4:56){
  reg.con.3<-lm(con~factor(nat)+gdp+ic+mc+ps,data = dta[dta$pit %in% (i-3):i,])
  b.con.3[i-3]=reg.con.3$coefficients['gdp']
}

b.inc.3=c()
for(i in 4:56){
  reg.inc.3<-lm(inc~factor(nat)+gdp+ic+mc+ps,data = dta[dta$pit %in% (i-3):i,])
  b.inc.3[i-3]=reg.inc.3$coefficients['gdp']
}

plot(seq(as.Date("2007/12/31"), as.Date("2020/12/31"), by = "quarter"),b.con.3,type = 'b',ylab = 'beta_u',xlab = 'Year',ylim = c(0,1),col = 'blue')
lines(seq(as.Date("2007/12/31"), as.Date("2020/12/31"), by = "quarter"),b.inc.3,type = 'b',ylab = 'beta_u',xlab = 'Year',ylim = c(0,1),col = 'red')
title('Regression estimation of beta_u controlling for incidence and measures: income=red consum=blue')

# simple regression to get beta_u interaction of gdp and restrictions

b.con.4=c()
for(i in 4:56){
  reg.con.4<-lm(con~factor(nat)+gdp*mc+ic+ps,data = dta[dta$pit %in% (i-3):i,])
  b.con.4[i-3]=reg.con.4$coefficients['gdp']
}

b.inc.4=c()
for(i in 4:56){
  reg.inc.4<-lm(inc~factor(nat)+gdp*mc+ic+ps,data = dta[dta$pit %in% (i-3):i,])
  b.inc.4[i-3]=reg.inc.4$coefficients['gdp']
}

plot(seq(as.Date("2007/12/31"), as.Date("2020/12/31"), by = "quarter"),b.con.4,type = 'b',ylab = 'beta_u',xlab = 'Year',ylim = c(0,1),col = 'blue')
lines(seq(as.Date("2007/12/31"), as.Date("2020/12/31"), by = "quarter"),b.inc.4,type = 'b',ylab = 'beta_u',xlab = 'Year',ylim = c(0,1),col = 'red')
title('Regression estimation of beta_u controlling for interaction gdp and restrictions (b_u shown at mc=0) : income=red consum=blue')

# simple regression to get beta_u interaction of gdp and support

b.con.5=c()
for(i in 4:56){
  reg.con.5<-lm(con~factor(nat)+gdp*ps+ic+mc,data = dta[dta$pit %in% (i-3):i,])
  b.con.5[i-3]=reg.con.5$coefficients['gdp']
}

b.inc.5=c()
for(i in 4:56){
  reg.inc.5<-lm(inc~factor(nat)+gdp*ps+ic+mc,data = dta[dta$pit %in% (i-3):i,])
  b.inc.5[i-3]=reg.inc.5$coefficients['gdp']
}

plot(seq(as.Date("2007/12/31"), as.Date("2020/12/31"), by = "quarter"),b.con.5,type = 'b',ylab = 'beta_u',xlab = 'Year',ylim = c(0,1),col = 'blue')
lines(seq(as.Date("2007/12/31"), as.Date("2020/12/31"), by = "quarter"),b.inc.5,type = 'b',ylab = 'beta_u',xlab = 'Year',ylim = c(0,1),col = 'red')
title('Regression estimation of beta_u controlling for interaction gdp and support (b_u shown at ps=0) : income=red consum=blue')

# simple regression to get beta_u triple interaction

b.con.6=c()
for(i in 4:56){
  reg.con.6<-lm(con~factor(nat)+gdp*ps*mc+ic,data = dta[dta$pit %in% (i-3):i,])
  b.con.6[i-3]=reg.con.6$coefficients['gdp']
  assign(paste('r.di.c',i-3,sep = '.'),reg.con.6)
}

b.inc.6=c()
for(i in 4:56){
  reg.inc.6<-lm(inc~factor(nat)+gdp*ps*mc+ic,data = dta[dta$pit %in% (i-3):i,])
  b.inc.6[i-3]=reg.inc.6$coefficients['gdp']
  assign(paste('r.di.i',i-3,sep = '.'),reg.inc.6)
}

plot(seq(as.Date("2007/12/31"), as.Date("2020/12/31"), by = "quarter"),b.con.6,type = 'b',ylab = 'beta_u',xlab = 'Year',ylim = c(0,1),col = 'blue')
lines(seq(as.Date("2007/12/31"), as.Date("2020/12/31"), by = "quarter"),b.inc.6,type = 'b',ylab = 'beta_u',xlab = 'Year',ylim = c(0,1),col = 'red')
title('Regression estimation of beta_u controlling for interaction gdp and measures (b_u shown at measures=0) : income=red consum=blue')

# res.g+c(rep(0,length(res.g)-4),mean(cov.q1)/4,mean(c(cov.q1,cov.q2))/2,mean(c(cov.q1,cov.q2,cov.q3))*0.75,mean(c(cov.q1,cov.q2,cov.q3,cov.q4)))*res.gm # use to get b_u for measures at their mean

############################## estimators for 2020 ################################################################

summary(reg.con.6)
summary(reg.inc.6)

summary(reg.con.5)
summary(reg.inc.5)

summary(reg.con.4)
summary(reg.inc.4)

############################## impact of single countries on estimates    #########################################

inf.con<-lm.influence(reg.con.6)$coefficients
inf.inc<-lm.influence(reg.inc.6)$coefficients

sort(abs(inf.con[,'gdp']),decreasing = TRUE) 
sort(abs(inf.inc[,'gdp']),decreasing = TRUE)

############################### look on savings channel ###########################################################

# dynamic point of view
B=matrix(nrow = 52,ncol = 5)
B.s=B
for (t in 1:52) {
  
  s=t-1 # s+1 = starting point in time
  f=t+4 # l-1 = end point in time
  # use pit.to.q to translate point in time to actual dates
  # pit.to.q=data.frame('pit'=1:56,'qtr'=seq(as.Date("2006/4/1"), as.Date("2020/12/31"), by = "quarter"))
  
  b=c()
  b_f<-lm((gdp-gni)~factor(nat)+ic+gdp*mc*ps,data = dta[dta$pit>s & dta$pit<f,])
  b[1]=b_f$coefficients['gdp']
  b_d<-lm((gni-nni)~factor(nat)+ic+gdp*mc*ps,data = dta[dta$pit>s & dta$pit<f,])
  b[2]=b_d$coefficients['gdp']
  b_t<-lm((nni-inc)~factor(nat)+ic+gdp*mc*ps,data = dta[dta$pit>s & dta$pit<f,])
  b[3]=b_t$coefficients['gdp']
  b_s<-lm((inc-con)~factor(nat)+ic+gdp*mc*ps,data = dta[dta$pit>s & dta$pit<f,])
  b[4]=b_s$coefficients['gdp']
  b_u<-lm(con~factor(nat)+ic+gdp*mc*ps,data = dta[dta$pit>s & dta$pit<f,])
  b[5]=b_u$coefficients['gdp']

B[t,]=b
    
  b.s=c()
  b.s_f<-lm((gdp-gni)~factor(nat)+gdp,data = dta[dta$pit>s & dta$pit<f,])
  b.s[1]=b.s_f$coefficients['gdp']
  b.s_d<-lm((gni-nni)~factor(nat)+gdp,data = dta[dta$pit>s & dta$pit<f,])
  b.s[2]=b.s_d$coefficients['gdp']
  b.s_t<-lm((nni-inc)~factor(nat)+gdp,data = dta[dta$pit>s & dta$pit<f,])
  b.s[3]=b.s_t$coefficients['gdp']
  b.s_s<-lm((inc-con)~factor(nat)+gdp,data = dta[dta$pit>s & dta$pit<f,])
  b.s[4]=b.s_s$coefficients['gdp']
  b.s_u<-lm(con~factor(nat)+gdp,data = dta[dta$pit>s & dta$pit<f,])
  b.s[5]=b.s_u$coefficients['gdp']
  
B.s[t,]=b.s
  
}

b.labs=c('b_f','b_d','b_t','b_s','b_u')
B=data.frame(B)
colnames(B)=b.labs
plot(B) # interesting???

b.labs=c('b_f','b_d','b_t','b_s','b_u')
B.s=data.frame(B.s)
colnames(B.s)=b.labs
plot(B.s) # interesting???

plot(seq(as.Date("2008/1/1"), as.Date("2020/12/31"), by = "quarter"),B$b_f,type = 'l',ylab = 'beta',xlab = 'Year',ylim = c(-0.5,1),col = 'blue')
lines(seq(as.Date("2008/1/1"), as.Date("2020/12/31"), by = "quarter"),B$b_d,type = 'l',ylab = 'beta',xlab = 'Year',ylim = c(-0.5,1),col = 'black')
lines(seq(as.Date("2008/1/1"), as.Date("2020/12/31"), by = "quarter"),B$b_t,type = 'l',ylab = 'beta',xlab = 'Year',ylim = c(-0.5,1),col = 'orange')
lines(seq(as.Date("2008/1/1"), as.Date("2020/12/31"), by = "quarter"),B$b_s,type = 'l',ylab = 'beta',xlab = 'Year',ylim = c(-0.5,1),col = 'green')
lines(seq(as.Date("2008/1/1"), as.Date("2020/12/31"), by = "quarter"),B$b_u,type = 'l',ylab = 'beta',xlab = 'Year',ylim = c(-0.5,1),col = 'red')
title('Channel contributions over time (triple interaction estimation, measures at zero)')
legend(x = c(as.Date("2008/1/1"), as.Date("2009/1/1")), y = c(0.725, 1),legend = c('b_f','b_d','b_t','b_s', 'b_u'),col = c('blue','black','orange','green','red'),lty = c(1,1))
abline(v=as.Date("2009/10/01"))
abline(v=as.Date("2020/04/01"))

plot(seq(as.Date("2008/1/1"), as.Date("2020/12/31"), by = "quarter"),B.s$b_f,type = 'l',ylab = 'beta',xlab = 'Year',ylim = c(-0.5,1),col = 'blue')
lines(seq(as.Date("2008/1/1"), as.Date("2020/12/31"), by = "quarter"),B.s$b_d,type = 'l',ylab = 'beta',xlab = 'Year',ylim = c(-0.5,1),col = 'black')
lines(seq(as.Date("2008/1/1"), as.Date("2020/12/31"), by = "quarter"),B.s$b_t,type = 'l',ylab = 'beta',xlab = 'Year',ylim = c(-0.5,1),col = 'orange')
lines(seq(as.Date("2008/1/1"), as.Date("2020/12/31"), by = "quarter"),B.s$b_s,type = 'l',ylab = 'beta',xlab = 'Year',ylim = c(-0.5,1),col = 'green')
lines(seq(as.Date("2008/1/1"), as.Date("2020/12/31"), by = "quarter"),B.s$b_u,type = 'l',ylab = 'beta',xlab = 'Year',ylim = c(-0.5,1),col = 'red')
title('Channel contributions over time (simple estimation, no covid controll vars)')
legend(x = c(as.Date("2008/1/1"), as.Date("2009/1/1")), y = c(0.725, 1),legend = c('b_f','b_d','b_t','b_s', 'b_u'),col = c('blue','black','orange','green','red'),lty = c(1,1))
abline(v=as.Date("2009/10/01"))
abline(v=as.Date("2020/04/01"))

# static point of view

s=0 # s+1 = starting point in time
f=57 # l-1 = end point in time
# use pit.to.q to translate point in time to actual dates
# pit.to.q=data.frame('pit'=1:52,'qtr'=seq(as.Date("2006/4/1"), as.Date("2020/12/31"), by = "quarter"))

b=c()
b_f<-lm((gdp-gni)~factor(nat)+ic+gdp*mc*ps,data = dta[dta$pit>s & dta$pit<f,])
summary(b_f)
b[1]=b_f$coefficients['gdp']
b_d<-lm((gni-nni)~factor(nat)+ic+gdp*mc*ps,data = dta[dta$pit>s & dta$pit<f,])
summary(b_d)
b[2]=b_d$coefficients['gdp']
b_t<-lm((nni-inc)~factor(nat)+ic+gdp*mc*ps,data = dta[dta$pit>s & dta$pit<f,])
summary(b_t)
b[3]=b_t$coefficients['gdp']
b_s<-lm((inc-con)~factor(nat)+ic+gdp*mc*ps,data = dta[dta$pit>s & dta$pit<f,])
summary(b_s)
b[4]=b_s$coefficients['gdp']
b_u<-lm(con~factor(nat)+ic+gdp*mc*ps,data = dta[dta$pit>s & dta$pit<f,])
summary(b_u)
b[5]=b_u$coefficients['gdp']

b
sum(b)

###################################################################################################################
############################## end of estimations #################################################################
###################################################################################################################

############################## sample selection vector to make sure tilde is right ################################

tmp.sel=1:22*(c('as','au','be','br','cd','cz','dn','fn','fr','ge','gr','it','jp','nl','nw','po','ru','sp','sw','tk','uk','us') %in% codes)
tmp.sel=tmp.sel[tmp.sel>0]