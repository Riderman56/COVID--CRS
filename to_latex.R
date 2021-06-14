output='text'

dta$year=year(dta$pit,start = 2006)

################# simple regression (2006-2020) ##################################

stargazer(r.s.c.1,coeftest(r.s.c.1,vcovCL,cluster=dta[,c('nat','year')]),type = output,title = 'Simple regression: Consumption',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects')

stargazer(r.s.i.1,coeftest(r.s.i.1,vcovCL,cluster=dta[,c('nat','year')]),type = output,title = 'Simple regression: Net disp. Income',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects')

################# regressions with interactions ##################################

stargazer(r.i.c.1,coeftest(r.i.c.1,vcovCL,cluster=dta[,c('nat','year')]),type = output,title = 'Triple interaction regression: Consumption',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects')

stargazer(r.i.i.1,coeftest(r.i.i.1,vcovCL,cluster=dta[,c('nat','year')]),type = output,title = 'Triple interaction regression: Net disp. Income',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects')

stargazer(b,type = output,summary = FALSE, title = 'contribution of diferent channels')

stargazer(coeftest(norm,vcovCL,cluster=dta.p.s$nat),coeftest(perm,vcovCL,cluster=dta.p.s$nat),coeftest(tran,vcovCL,cluster=dta.p.s$nat),type = output,omit = 'factor',omit.labels = 'Country fixed effects?')

############## lower level interactions alone ####################################

stargazer(coeftest(r.i.c.2,vcovCL,cluster=dta[,c('nat','year')]),coeftest(r.i.c.3,vcovCL,cluster=dta[,c('nat','year')]),type = output,title = 'Simple interaction regressions: Consumption',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects')

stargazer(coeftest(r.i.i.2,vcovCL,cluster=dta[,c('nat','year')]),coeftest(r.i.i.3,vcovCL,cluster=dta[,c('nat','year')]),type = output,title = 'Simple interaction regressions: Net disp. Income',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects')
 

############## experimenting with data ###########################################

sub.dta=dta[dta$pit>55,c('pit','nat','mc','ps')]

corrs<-data.frame(sub.dta[1:4,3:4])
for(i in 2:13){
  corrs<-data.frame(corrs,sub.dta[(1+4*(i-1)):(4+4*(i-1)),3:4])
}

summary(corrs)

ratio<-rbind(names,colMeans(corrs)[seq(1,26,2)]/colMeans(corrs)[seq(2,26,2)])

subsam<-ratio[2,]>1
othsam<-ratio[2,]<=1

# sample split

stargazer(coeftest(lm(con~factor(nat)+ic+gdp*mc*ps,data = dta[dta$nat %in% codes[subsam],]),vcovCL,cluster=dta[dta$nat %in% codes[subsam],][,c('nat','year')]),coeftest(lm(con~factor(nat)+ic+gdp*mc*ps,data = dta[dta$nat %in% codes[othsam],]),vcovCL,cluster=dta[dta$nat %in% codes[othsam],][,c('nat','year')]),coeftest(lm(con~factor(nat)+ic+gdp*mc*ps,data = dta[dta$nat %in% codes,]),vcovCL,cluster=dta[dta$nat %in% codes,][,c('nat','year')]),type=output)

stargazer(coeftest(lm(con~factor(nat)+ic+gdp,data = dta[dta$nat %in% codes[subsam],]),vcovCL,cluster=dta[dta$nat %in% codes[subsam],][,c('nat','year')]),coeftest(lm(con~factor(nat)+ic+gdp,data = dta[dta$nat %in% codes[othsam],]),vcovCL,cluster=dta[dta$nat %in% codes[othsam],][,c('nat','year')]),coeftest(lm(con~factor(nat)+ic+gdp,data = dta[dta$nat %in% codes,]),vcovCL,cluster=dta[dta$nat %in% codes,][,c('nat','year')]),type=output)

############## investigating cross border incomes and covid ###########

stargazer(coeftest(lm((gdp-gni)~factor(nat)+ic+gdp*mc*ps,data=dta),vcovCL,cluster=dta[,c('nat','year')]),type = output)

# other channels

stargazer(coeftest(lm((gni-nni)~factor(nat)+ic+gdp*mc*ps,data=dta),vcovCL,cluster=dta[,c('nat','year')]),type = output)

stargazer(coeftest(lm((gni-inc)~factor(nat)+ic+gdp*mc*ps,data=dta),vcovCL,cluster=dta[,c('nat','year')]),type = output)

stargazer(coeftest(lm((inc-con)~factor(nat)+ic+gdp*mc*ps,data=dta),vcovCL,cluster=dta[,c('nat','year')]),type = output)

stargazer(coeftest(lm((con)~factor(nat)+ic+gdp*mc*ps,data=dta),vcovCL,cluster=dta[,c('nat','year')]),type = output)

# furter exploration b_f

stargazer(coeftest(lm((gdp-gni)~factor(nat):gdp*mc+gdp*ps+ic,data = dta),vcovCL,cluster=dta[,c('nat','year')]),type='text')

stargazer(coeftest(lm((gdp-gni)~gdp*ps+factor(nat):gdp:mc+ic,data = dta),vcovCL,cluster=dta[,c('nat','year')]),type='text')

stargazer(coeftest(lm((gdp-gni)~factor(nat)+pop*gdp+ic,data = dta),vcovCL,cluster=dta[,c('nat','year')]),type='text')
