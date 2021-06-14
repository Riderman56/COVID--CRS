source('C:/Users/BongBong/Desktop/nat_ac/pre_reg_wls_newic.R')

########################## define helping routine    #####################################

to.year<-function(vec){
  n=length(vec)
  stopifnot((n/4)%%1==0) # all quarters for each year have to be in the data
  res=c()
  for (t in 1:(n/4)) {
    res[t]=sum(vec[(4*t-3):(4*t)])
  }
  return(res)
}

##########################################################################################
########################## get permanent component of gdp ################################
##########################################################################################

l.gdp_tot=rep(0,15)

for (i in codes) { # log gdp levels seasonal and calendar adjusted
  assign(paste("l.gdp",i,sep="_"),to.year(get(i)[get(i)$Indicator==row_gdp & is.na(get(i)$`Reference year`)==0,][2,][7:66]))
  l.gdp_tot=l.gdp_tot+get(paste("l.gdp",i,sep="_"))
  assign(paste("l.gdp",i,sep="_"),as.ts(log(get(paste("l.gdp",i,sep="_")))))
  assign(paste('ar',i,sep = '_'),arima(get(paste("l.gdp",i,sep="_")),order= c(1,1,0),include.mean = FALSE))
}

l.gdp_tot=as.ts(log(l.gdp_tot))
ar_tot=arima(l.gdp_tot,order = c(1,1,0),include.mean = FALSE  )


l.con_tot=rep(0,15)

for (i in codes) { # log con levels seasonal and calendar adjusted
  assign(paste("l.con",i,sep="_"),to.year(get(i)[get(i)$Indicator==row_con & is.na(get(i)$`Reference year`)==0,][2,][7:66]))
  l.con_tot=l.con_tot+get(paste("l.con",i,sep="_"))
  assign(paste("l.con",i,sep="_"),as.ts(log(get(paste("l.con",i,sep="_")))))
}

l.con_tot=as.ts(log(l.con_tot))


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
  helper=NaN*(1:14)
  for (z in 1:14) {
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
for(i in 2:13){
  reg.norm<-lm(con~factor(nat)+gdp,data = dta.p[dta.p$pit %in% (i-1):i,])
  b.norm[i-1]=reg.norm$coefficients['gdp']
  reg.perm<-lm(con~factor(nat)+gdp.p,data = dta.p[dta.p$pit %in% (i-1):i,])
  b.perm[i-1]=reg.perm$coefficients['gdp.p']
  reg.tran<-lm(con~factor(nat)+gdp.t,data = dta.p[dta.p$pit %in% (i-1):i,])
  b.tran[i-1]=reg.tran$coefficients['gdp.t']
}

plot(2009:2020,b.norm,type='b',col='red',ylim=c(-1,1),xlab='Year',ylab='beta')
lines(2009:2020,b.perm,type = 'b',col = 'blue')
lines(2009:2020,b.tran,type = 'b',col='yellow')
title('beta_u (red), beta_perm (blue) and beta_tran (yellow)')
