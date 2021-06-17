source('C:/Users/BongBong/Desktop/nat_ac/pre_reg.R')

##################################################################################
################# regression results with SEs clustered at country level #########
##################################################################################

################# simple regression (2006-2020) ##################################

stargazer(r.s.c.1,coeftest(r.s.c.1,vcovCL,cluster=dta[,c('nat','year')]),type = 'text',title = 'Simple regression: Consumption',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects')

stargazer(r.s.i.1,coeftest(r.s.i.1,vcovCL,cluster=dta[,c('nat','year')]),type = 'text',title = 'Simple regression: Net disp. Income',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects')

################# regressions with interactions ##################################

stargazer(r.i.c.1,coeftest(r.i.c.1,vcovCL,cluster=dta[,c('nat','year')]),type = 'text',title = 'Triple interaction regression: Consumption',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects')

stargazer(r.i.i.1,coeftest(r.i.i.1,vcovCL,cluster=dta[,c('nat','year')]),type = 'text',title = 'Triple interaction regression: Net disp. Income',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects')


stargazer(r.i.c.2,coeftest(r.i.c.2,vcovCL,cluster=dta[,c('nat','year')]),type = 'text',title = 'Simple interaction regression (restrictive measures): Consumption',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects')

stargazer(r.i.i.2,coeftest(r.i.i.2,vcovCL,cluster=dta[,c('nat','year')]),type = 'text',title = 'Simple interaction regression (restrictive measures): Net disp. Income',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects')


stargazer(r.i.c.3,coeftest(r.i.c.3,vcovCL,cluster=dta[,c('nat','year')]),type = 'text',title = 'Simple interaction regression (policy support): Consumption',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects')

stargazer(r.i.i.3,coeftest(r.i.i.3,vcovCL,cluster=dta[,c('nat','year')]),type = 'text',title = 'Simple interaction regression (policy support): Net disp. Income',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects')

################ regressions using last 4 quarters ###############################

pit=56 # element 1:56, where 56 = q4 2020

if(pit%%4!=0){gr=c('nat','year')} else{gr=c('nat')}

#simple 

stargazer(get(paste('r.ds.c',pit,sep = '.')),coeftest(get(paste('r.ds.c',pit,sep = '.')),vcovCL,cluster=dta[dta$pit %in% pit:(pit+3),][,gr]),type = 'text',title = 'Simple regression: Consumption',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects')

stargazer(get(paste('r.ds.i',pit,sep = '.')),coeftest(get(paste('r.ds.i',pit,sep = '.')),vcovCL,cluster=dta[dta$pit %in% pit:(pit+3),][,gr]),type = 'text',title = 'Simple regression: Net disp. Income',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects')

#interaction

stargazer(get(paste('r.di.c',pit,sep = '.')),coeftest(get(paste('r.di.c',pit,sep = '.')),vcovCL,cluster=dta[dta$pit %in% pit:(pit+3),][,gr]),type = 'text',title = 'Triple interaction regression: Consumption',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects')

stargazer(get(paste('r.di.i',pit,sep = '.')),coeftest(get(paste('r.di.i',pit,sep = '.')),vcovCL,cluster=dta[dta$pit %in% pit:(pit+3),][,gr]),type = 'text',title = 'Triple interaction regression: Net disp. Income',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects')

################ decomposition of risk sharing channels ##########################

b=data.frame(b)
rownames(b)=b.labs
stargazer(b,type = 'text',summary = FALSE, title = 'contribution of different channels')

stargazer(b_f,coeftest(b_f,vcovCL,cluster=dta[dta$pit>s & dta$pit<f,][,c('nat','year')]),type = 'text',omit = 'factor',omit.labels = 'Country  Fixed Effects',title = 'Factor income channel')
stargazer(b_d,coeftest(b_d,vcovCL,cluster=dta[dta$pit>s & dta$pit<f,][,c('nat','year')]),type = 'text',omit = 'factor',omit.labels = 'Country  Fixed Effects',title = 'Depreciation channel')
stargazer(b_t,coeftest(b_t,vcovCL,cluster=dta[dta$pit>s & dta$pit<f,][,c('nat','year')]),type = 'text',omit = 'factor',omit.labels = 'Country  Fixed Effects',title = 'Transfer channel')
stargazer(b_s,coeftest(b_s,vcovCL,cluster=dta[dta$pit>s & dta$pit<f,][,c('nat','year')]),type = 'text',omit = 'factor',omit.labels = 'Country  Fixed Effects',title = 'Saving channel')
stargazer(b_u,coeftest(b_u,vcovCL,cluster=dta[dta$pit>s & dta$pit<f,][,c('nat','year')]),type = 'text',omit = 'factor',omit.labels = 'Country  Fixed Effects',title = 'Uninsured residual')

################ look for assymetrics #############################################

dta$up=dta$gdp>0

asym<-lm(con~factor(nat)+ic+gdp*up,data = dta)
stargazer(asym,coeftest(asym,vcovCL,cluster=dta[,c('nat','year')]),type = 'text',title = 'Triple interaction regression: Consumption - asym?',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects')

############### use stay at home instead of mc index ##############################

sh.reg<-lm(inc~factor(nat)+ic+gdp*ps*sh,data = dta)
stargazer(sh.reg,coeftest(sh.reg,vcovCL,cluster=dta[,c('nat','year')]),type = 'text',title = 'Triple interaction regression: Consumption - asym?',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects')
