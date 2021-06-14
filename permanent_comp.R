source('C:/Users/BongBong/Desktop/nat_ac/pre_reg_wls_newic.R')

##########################################################################################
########################## get permanent component of gdp ################################
##########################################################################################

l.gdp_tot=rep(0,60)

for (i in codes) { # log gdp levels seasonal and calendar adjusted
  assign(paste("l.gdp",i,sep="_"),get(i)[get(i)$Indicator==row_gdp & is.na(get(i)$`Reference year`)==0,][2,][7:66])
  l.gdp_tot=l.gdp_tot+get(paste("l.gdp",i,sep="_"))
  assign(paste("l.gdp",i,sep="_"),as.ts(log(t(get(paste("l.gdp",i,sep="_")))),frequency=4,start=c(2006,1),end=c(2020,4)))
  assign(paste('ar',i,sep = '_'),arima(get(paste("l.gdp",i,sep="_")),order= c(1,1,0),include.mean = FALSE))
}

l.gdp_tot=as.ts(log(t(l.gdp_tot)),start=c(2006,1),end=c(2020,4))
ar_tot=arima(l.gdp_tot,order = c(1,1,0),include.mean = FALSE  )


l.con_tot=rep(0,60)

for (i in codes) { # log con levels seasonal and calendar adjusted
  assign(paste("l.con",i,sep="_"),get(i)[get(i)$Indicator==row_con & is.na(get(i)$`Reference year`)==0,][2,][7:66])
  l.con_tot=l.con_tot+get(paste("l.con",i,sep="_"))
  assign(paste("l.con",i,sep="_"),as.ts(log(t(get(paste("l.con",i,sep="_")))),frequency=4,start=c(2006,1),end=c(2020,4)))
}

l.con_tot=as.ts(log(t(l.con_tot)),start=c(2006,1),end=c(2020,4))


for (i in c(codes,'tot')){
  gdp=diff(get(paste("l.gdp",i,sep="_")))
  GDP=diff(l.gdp_tot)
  con=diff(get(paste("l.con",i,sep="_")))
  CON=diff(get(paste("l.con",i,sep="_"))-l.con_tot)
  
  assign(paste('X',i,sep = '_'),ts.intersect(gdp,GDP,CON))
}

for (i in c(codes,'tot')){
  assign(paste('A',i,sep = '_'),Bcoef(VAR(get(paste('X',i,sep = '_')),type = 'none')))
  assign(paste('a',i,sep = '_'),cbind(c(get(paste('ar',i,sep = '_'))$coef,0),c(0,ar_tot$coef)))
}


################## helper function to get E[...] #############################

E<-function(A,dx,r){
  A=A/(1+r)
  return(c(1,-1,0)%*%A%*%solve(diag(3)-A)%*%dx)
}

# E(A_au,X_au[5,],0.02) #try

################# actual calculation of perm gdp comp ########################

r=0.02

for (i in codes) {
  helper=NaN*(1:59)
  for (z in 1:59) {
    helper[z]=get(paste('l.gdp',i,sep='_'))[z+1]-l.gdp_tot[z+1]+E(get(paste('A',i,sep = '_')),get(paste('X',i,sep = '_'))[z,],r)
  }
assign(paste('gdp.p',i,sep='_'),diff(helper))
}
 
################ get data together in a data frame ###########################

t=length(get(paste('gdp.p',codes[1],sep='_')))
dta.p<-data.frame(pit=1:t,'nat'=rep(codes[1],t),'con'=diff(get(paste('l.con',codes[1],sep='_')))[-1]-diff(l.con_tot)[-1],'gdp.p'=get(paste('gdp.p',codes[1],sep='_')),'gdp'=diff(get(paste('l.gdp',codes[1],sep='_')))[-1]-diff(l.gdp_tot)[-1])

for (i in 2:obs) {
  tmp=data.frame(pit=1:t,'nat'=rep(codes[i],t),'con'=diff(get(paste('l.con',codes[i],sep='_')))[-1]-diff(l.con_tot)[-1],'gdp.p'=get(paste('gdp.p',codes[i],sep='_')),'gdp'=diff(get(paste('l.gdp',codes[i],sep='_')))[-1]-diff(l.gdp_tot)[-1])
  dta.p=rbind(dta.p,tmp)
}

dta.p$gdp.t=dta.p$gdp-dta.p$gdp.p

############### first regression #############################################

dta.p.s=dta.p[dta.p$pit>0,] # >0 full sample -- >54 covid -- <55 non-covid

perm<-lm(con~factor(nat)+gdp.p,data = dta.p.s)
tran<-lm(con~factor(nat)+gdp.t,data = dta.p.s)
norm<-lm(con~factor(nat)+gdp,data = dta.p.s)
stargazer(norm,perm,tran,type = 'text')
stargazer(coeftest(norm,vcovCL,cluster=dta.p.s$nat),coeftest(perm,vcovCL,cluster=dta.p.s$nat),coeftest(tran,vcovCL,cluster=dta.p.s$nat),type = 'text',omit = 'factor',omit.labels = 'Country fixed effects?')

############# developments over time #########################################

b.perm=c()
b.tran=c()
b.norm=c()
for(i in 4:58){
  reg.norm<-lm(con~factor(nat)+gdp,data = dta.p[dta.p$pit %in% (i-3):i,])
  b.norm[i-3]=reg.norm$coefficients['gdp']
  reg.perm<-lm(con~factor(nat)+gdp.p,data = dta.p[dta.p$pit %in% (i-3):i,])
  b.perm[i-3]=reg.perm$coefficients['gdp.p']
  reg.tran<-lm(con~factor(nat)+gdp.t,data = dta.p[dta.p$pit %in% (i-3):i,])
  b.tran[i-3]=reg.tran$coefficients['gdp.t']
}

plot(seq(as.Date("2007/4/1"), as.Date("2020/12/31"), by = "quarter"),b.norm,type='b',col='red',ylim=c(-1,1),xlab='Time',ylab='beta')
lines(seq(as.Date("2007/4/1"), as.Date("2020/12/31"), by = "quarter"),b.perm,type = 'b',col = 'blue')
lines(seq(as.Date("2007/4/1"), as.Date("2020/12/31"), by = "quarter"),b.tran,type = 'b',col='yellow')
title('beta_u (red), beta_perm (blue) and beta_tran (yellow)')

###############
# adding the control variables
###############

c.dta=dta[dta$pit!=1,]
c.dta$nat==dta.p$nat
dta.p.c=data.frame(dta.p,'mc'=c.dta$mc,'ps'=c.dta$ps,'ic'=c.dta$ic)

############## rerunning regressions with intersections #######################

# consumption

perm.c<-lm(con~factor(nat)+gdp.p*mc*ps+ic,data = dta.p.c)
tran.c<-lm(con~factor(nat)+gdp.t*mc*ps+ic,data = dta.p.c)
norm.c<-lm(con~factor(nat)+gdp*mc*ps+ic,data = dta.p.c)
stargazer(norm.c,perm.c,tran.c,type = 'text')
stargazer(coeftest(norm.c,vcovCL,cluster=dta.p.c$nat),coeftest(perm.c,vcovCL,cluster=dta.p.c$nat),coeftest(tran.c,vcovCL,cluster=dta.p.c$nat),type = 'text',omit = 'factor',omit.labels = 'Country fixed effects?')

# income

dta.p.c=data.frame(dta.p.c,'inc'=c.dta$inc)

perm.i<-lm(inc~factor(nat)+gdp.p*mc*ps+ic,data = dta.p.c)
tran.i<-lm(inc~factor(nat)+gdp.t*mc*ps+ic,data = dta.p.c)
norm.i<-lm(inc~factor(nat)+gdp*mc*ps+ic,data = dta.p.c)
stargazer(norm.i,perm.i,tran.i,type = 'text')
stargazer(coeftest(norm.i,vcovCL,cluster=dta.p.c$nat),coeftest(perm.i,vcovCL,cluster=dta.p.c$nat),coeftest(tran.i,vcovCL,cluster=dta.p.c$nat),type = 'text',omit = 'factor',omit.labels = 'Country fixed effects?')

# cluster additionally on year level

year<-function(vec,start=1,sq=1){
  vec=vec+sq-1
  return(vec/4-(vec%%4)/4+start)
}

# year(dta$pit,start=2006)

dta.p.c=data.frame(dta.p.c,'year'=year(dta.p.c$pit,start = 2006,sq=2))

stargazer(coeftest(norm.c,vcovCL,cluster=dta.p.c[,c('nat','year')]),coeftest(perm.c,vcovCL,cluster=dta.p.c[,c('nat','year')]),coeftest(tran.c,vcovCL,cluster=dta.p.c[,c('nat','year')]),type = 'text',omit = 'factor',omit.labels = 'Country fixed effects?')
stargazer(coeftest(norm.i,vcovCL,cluster=dta.p.c[,c('nat','year')]),coeftest(perm.i,vcovCL,cluster=dta.p.c[,c('nat','year')]),coeftest(tran.i,vcovCL,cluster=dta.p.c[,c('nat','year')]),type = 'text',omit = 'factor',omit.labels = 'Country fixed effects?')
