source('C:/Users/BongBong/Desktop/nat_ac/vgl_usa.R')

##########################################################################################
########################## get permanent component of gsp ################################
##########################################################################################

inc_ste=inc_raw[-c(1:5,66:69),-1]
gsp_ste=gsp_raw[-c(1:5,66:68),-1]

for (i in us_names){
  gsp=as.numeric(gsp_ste[gsp_ste[,1]==i,-1])
  GSP=as.numeric(gsp_ste[gsp_ste[,1]=='United States',-1])
  INC=as.numeric(inc_ste[inc_ste[,1]==i,-1])-as.numeric(inc_ste[inc_ste[,1]=='United States',-1])
  assign(paste('X',i,sep = '_'),ts(cbind(gsp,GSP,INC)/100))
}

for (i in c(us_names)){
  assign(paste('A',i,sep = '_'),Bcoef(VAR(get(paste('X',i,sep = '_')),type = 'none')))
}

################## helper function to get E[...] #############################

E<-function(A,dx,r){
  A=A/(1+r)
  return(c(1,-1,0)%*%A%*%solve(diag(3)-A)%*%dx)
}

# E(A_au,X_au[5,],0.02) #try

################# actual calculation of perm gsp comp ########################

r=0.02

for (i in us_names) {
  helper=NaN*(1:63)
  for (z in 1:63) {
    helper[z]=E(get(paste('A',i,sep = '_')),get(paste('X',i,sep = '_'))[z,],r)
  }
assign(paste('dif.p',i,sep='_'),diff(helper))
}
 
################ add perm gsp component in the data frame #####################

dta.us.p=dta.us[dta.us$pit!=1,]

dta.us.p$gsp.perm=NA

for (i in us_names) {
  dta.us.p$gsp.perm[dta.us.p$state==i]=dta.us.p$gsp[dta.us.p$state==i]+get(paste('dif.p',i,sep='_'))
}

# have a look - do results look like expected???  gsp.perm should be smoother then gsp... a bit like a trend

plot(1:62,dta.us.p$gsp[dta.us.p$state==i],type='l')
lines(1:62,dta.us.p$gsp.perm[dta.us.p$state==i])

################################################################################
############### redo regressions with permanent component ######################
################################################################################

dta.us.p$gsp.tran<-dta.us.p$gsp-dta.us.p$gsp.perm

normreg=glm(inc~factor(state)+ic+gsp*ps*mc,data = dta.us.p,weights=dta.us.p$pop)
permreg=glm(inc~factor(state)+ic+gsp.perm*ps*mc,data = dta.us.p,weights=dta.us.p$pop)
tranreg=glm(inc~factor(state)+ic+gsp.tran*ps*mc,data = dta.us.p,weights=dta.us.p$pop)
stargazer(coeftest(normreg,vcovCL,cluster=dta.us.p[,c('state','year')]),coeftest(permreg,vcovCL,cluster=dta.us.p[,c('state','year')]),coeftest(tranreg,vcovCL,cluster=dta.us.p[,c('state','year')]),type=output,omit = 'factor')

normreg.sh=glm(inc~factor(state)+ic+gsp*ps*sh,data = dta.us.p,weights=dta.us.p$pop)
permreg.sh=glm(inc~factor(state)+ic+gsp.perm*ps*sh,data = dta.us.p,weights=dta.us.p$pop)
tranreg.sh=glm(inc~factor(state)+ic+gsp.tran*ps*sh,data = dta.us.p,weights=dta.us.p$pop)
stargazer(coeftest(normreg.sh,vcovCL,cluster=dta.us.p[,c('state','year')]),coeftest(permreg.sh,vcovCL,cluster=dta.us.p[,c('state','year')]),coeftest(tranreg.sh,vcovCL,cluster=dta.us.p[,c('state','year')]),type=output,omit = 'factor')


# further investigation

dta.us.p$up=dta.us.p$gsp>0

permreg=glm(inc~factor(state)+ic+gsp.perm*ps*mc*up,data = dta.us.p,weights=dta.us.p$pop)
stargazer(coeftest(permreg,vcovCL,cluster=dta.us.p[,c('state','year')]),type=output,omit = 'factor')

permreg=glm(inc~factor(state)+gsp*up,data = dta.us.p,weights=dta.us.p$pop)
stargazer(coeftest(permreg,vcovCL,cluster=dta.us.p[,c('state','year')]),type=output,omit = 'factor')


# look at subperiods (after 2015)

# permreg=glm(inc~factor(state)+ic+gsp.perm*ps*mc,data = dta.us.p[dta.us.p$year>=2015,],weights=dta.us.p[dta.us.p$year>=2015,]$pop)
# stargazer(coeftest(permreg,vcovCL,cluster=dta.us.p[dta.us.p$year>=2015,c('state','year')]),type=output,omit = 'factor')
