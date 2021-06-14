cb_raw<-read_xlsx("C:/Users/BongBong/Desktop/nat_ac/eurostat_data/LFSQ_EGAN__custom_10069781622282993343.xlsx",sheet = 3)[-c(1:10,37:41),]

cb=cb_raw[,is.na(c(cb_raw[1,]))==0&c(cb_raw[1,])!='b']

cb_f=cb[cb$...2=='Foreign country',-(1:2)]
cb_t=cb[cb$...2=='Total',-(1:2)]

sfe=to.num(cb_f)/to.num(cb_t)

means_cb<-cbind(names,rowMeans(sfe))
cut_off<-mean(rowMeans(sfe[-3,]))
sel_cb<-rowMeans(sfe)>cut_off
so<-(sel_cb*1:13)[sel_cb]
so<-c(na.omit(so))
sel_cb<-rowMeans(sfe)<cut_off
st<-(sel_cb*1:13)[sel_cb]
st<-c(na.omit(st))

plot(colMeans(sfe[-3,])) # with out denmark due to missing values
matplot(sfe[-3,],type = 'l')

stargazer(coeftest(lm((gdp-gni)~factor(nat)+ic+gdp*mc*ps,data=dta[dta$nat %in% codes[so],]),vcovCL,cluster=dta[dta$nat %in% codes[so],c('nat','year')]),coeftest(lm((gdp-gni)~factor(nat)+ic+gdp*mc*ps,data=dta[dta$nat %in% codes[st],]),vcovCL,cluster=dta[dta$nat %in% codes[st],c('nat','year')]),coeftest(lm((gdp-gni)~factor(nat)+ic+gdp*mc*ps,data=dta[dta$nat %in% codes[-3],]),vcovCL,cluster=dta[dta$nat %in% codes[-3],c('nat','year')]),type = output)
 # corssboarder working and investment flows might correlated

######################## use goverment response index #########################

dcb_raw<-read_xlsx('C:/Users/BongBong/Desktop/nat_ac/OxCGRT_data/OxCGRT_timeseries_all.xlsx',sheet = 'c8_internationaltravel') # international movement restrictions

dcb<-dcb_raw[dcb_raw$country_name %in% names,]   # here alphabetical ordering becomes importand
dcb.2020<-dcb[, -grep("2021", colnames(dcb))]

dcb.q1<-rowMeans(dcb.2020[,c(grep("Jan", colnames(dcb.2020)),grep("Feb", colnames(dcb.2020)),grep("Mar", colnames(dcb.2020)))]==4)
dcb.q2<-rowMeans(dcb.2020[,c(grep("Apr", colnames(dcb.2020)),grep("May", colnames(dcb.2020)),grep("Jun", colnames(dcb.2020)))]==4)
dcb.q3<-rowMeans(dcb.2020[,c(grep("Jul", colnames(dcb.2020)),grep("Aug", colnames(dcb.2020)),grep("Sep", colnames(dcb.2020)))]==4)
dcb.q4<-rowMeans(dcb.2020[,c(grep("Oct", colnames(dcb.2020)),grep("Nov", colnames(dcb.2020)),grep("Dec", colnames(dcb.2020)))]==4)

dta$dcb=0

k=0
for (i in codes[1:obs]) {
  k=k+1
  dta[dta$nat==i & dta$pit==56,'dcb']=dcb.q1[k]
  dta[dta$nat==i & dta$pit==57,'dcb']=dcb.q2[k]
  dta[dta$nat==i & dta$pit==58,'dcb']=dcb.q3[k]
  dta[dta$nat==i & dta$pit==59,'dcb']=dcb.q4[k]
}

cor(dta[dta$pit>55,-(1:2)])
cor(dta[dta$pit>55 & dta$nat %in% codes[so],-(1:2)])

dcb.reg<-lm((gdp-gni)~factor(nat)+ic+gdp*dcb*ps,data=dta) # year fe do not change anything
stargazer(coeftest(dcb.reg,vcovCL,cluster=dta[,c('nat','year')]),type = output)
dcb.means=colMeans(dta[dta$dcb>=1/2,-(1:2)],na.rm = TRUE)
ndcb.means=colMeans(dta[dta$dcb<1/2,-(1:2)],na.rm = TRUE)
dcb.bf<-dcb.reg$coefficients['gdp']+dcb.reg$coefficients['gdp:ps']*dcb.means['ps']+dcb.reg$coefficients['gdp:dcb']*dcb.means['dcb']+dcb.reg$coefficients['gdp:dcb:ps']*dcb.means['ps']*dcb.means['dcb']
sprintf("in our sample this implies a mean beta_f of %s for those observations where borders have been closed", dcb.bf)
ndcb.bf<-dcb.reg$coefficients['gdp']+dcb.reg$coefficients['gdp:ps']*ndcb.means['ps']+dcb.reg$coefficients['gdp:dcb']*ndcb.means['dcb']+dcb.reg$coefficients['gdp:dcb:ps']*ndcb.means['ps']*ndcb.means['dcb']
sprintf("in our sample this implies a mean beta_f of %s for those observations where borders have been open", ndcb.bf)
# alternative explaination, countries with less benefit from cross border traffic are more likely to close borders

stargazer(coeftest(lm((gdp-gni)~factor(nat)+ic+gdp*dcb*ps,data=dta[dta$nat %in% codes[so],]),vcovCL,cluster=dta[dta$nat %in% codes[so],c('nat','year')]),coeftest(lm((gdp-gni)~factor(nat)+ic+gdp*dcb*ps,data=dta[dta$nat %in% codes[st],]),vcovCL,cluster=dta[dta$nat %in% codes[st],c('nat','year')]),coeftest(lm((gdp-gni)~factor(nat)+ic+gdp*dcb*ps,data=dta),vcovCL,cluster=dta[,c('nat','year')]),type=output)
stargazer(coeftest(lm((gdp-gni)~factor(nat)+ic+ps+gdp*dcb,data=dta[dta$nat %in% codes[so],]),vcovCL,cluster=dta[dta$nat %in% codes[so],c('nat','year')]),coeftest(lm((gdp-gni)~factor(nat)+ic+ps+gdp*dcb,data=dta[dta$nat %in% codes[st],]),vcovCL,cluster=dta[dta$nat %in% codes[st],c('nat','year')]),coeftest(lm((gdp-gni)~factor(nat)+ic+ps+gdp*dcb,data=dta),vcovCL,cluster=dta[,c('nat','year')]),type=output)
stargazer(coeftest(lm(con~factor(nat)+ic+gdp*dcb*ps,data=dta),vcovCL,cluster=dta[,c('nat','year')]),type=output)


# sort(lm.influence(dcb.reg)$coefficients[,'gdp:dcb'],decreasing = TRUE)


###### redo exercise for internal movement restrictions ###############################################################

######################## use goverment response index #########################

mr_raw<-read_xlsx('C:/Users/BongBong/Desktop/nat_ac/OxCGRT_timeseries_all.xlsx',sheet = 'c7_movementrestrictions') # movement restrictions

mr<-mr_raw[mr_raw$country_name %in% names,]   # here alphabetical ordering becomes importand
mr.2020<-mr[, -grep("2021", colnames(mr))]

mr.q1<-rowMeans(mr.2020[,c(grep("Jan", colnames(mr.2020)),grep("Feb", colnames(mr.2020)),grep("Mar", colnames(mr.2020)))]==2)
mr.q2<-rowMeans(mr.2020[,c(grep("Apr", colnames(mr.2020)),grep("May", colnames(mr.2020)),grep("Jun", colnames(mr.2020)))]==2)
mr.q3<-rowMeans(mr.2020[,c(grep("Jul", colnames(mr.2020)),grep("Aug", colnames(mr.2020)),grep("Sep", colnames(mr.2020)))]==2)
mr.q4<-rowMeans(mr.2020[,c(grep("Oct", colnames(mr.2020)),grep("Nov", colnames(mr.2020)),grep("Dec", colnames(mr.2020)))]==2)

dta$mr=0

k=0
for (i in codes[1:obs]) {
  k=k+1
  dta[dta$nat==i & dta$pit==56,'mr']=mr.q1[k]
  dta[dta$nat==i & dta$pit==57,'mr']=mr.q2[k]
  dta[dta$nat==i & dta$pit==58,'mr']=mr.q3[k]
  dta[dta$nat==i & dta$pit==59,'mr']=mr.q4[k]
}

cor(dta[dta$pit>55,-(1:2)])
cor(dta[dta$pit>55 & dta$nat %in% codes[so],-(1:2)])

mr.reg<-lm((gdp-gni)~factor(nat)+ic+gdp*mr*ps,data=dta) # year fe do not change anything
stargazer(coeftest(mr.reg,vcovCL,cluster=dta[,c('nat','year')]),type = output)
mr.means=colMeans(dta[dta$mr>=1/2,-(1:2)],na.rm = TRUE)
nmr.means=colMeans(dta[dta$mr<1/2,-(1:2)],na.rm = TRUE)
mr.bf<-mr.reg$coefficients['gdp']+mr.reg$coefficients['gdp:ps']*mr.means['ps']+mr.reg$coefficients['gdp:mr']*mr.means['mr']+mr.reg$coefficients['gdp:mr:ps']*mr.means['ps']*mr.means['mr']
sprintf("in our sample this implies a mean beta_f of %s for those observations where borders have been closed", mr.bf)
nmr.bf<-mr.reg$coefficients['gdp']+mr.reg$coefficients['gdp:ps']*nmr.means['ps']+mr.reg$coefficients['gdp:mr']*nmr.means['mr']+mr.reg$coefficients['gdp:mr:ps']*nmr.means['ps']*nmr.means['mr']
sprintf("in our sample this implies a mean beta_f of %s for those observations where borders have been open", nmr.bf)
# alternative explaination, countries with less benefit from cross border traffic are more likely to close borders

stargazer(coeftest(lm((gdp-gni)~factor(nat)+ic+gdp*mr*ps,data=dta[dta$nat %in% codes[so],]),vcovCL,cluster=dta[dta$nat %in% codes[so],c('nat','year')]),coeftest(lm((gdp-gni)~factor(nat)+ic+gdp*mr*ps,data=dta[dta$nat %in% codes[st],]),vcovCL,cluster=dta[dta$nat %in% codes[st],c('nat','year')]),coeftest(lm((gdp-gni)~factor(nat)+ic+gdp*mr*ps,data=dta),vcovCL,cluster=dta[,c('nat','year')]),type=output)
stargazer(coeftest(lm((gdp-gni)~factor(nat)+ic+ps+gdp*mr,data=dta[dta$nat %in% codes[so],]),vcovCL,cluster=dta[dta$nat %in% codes[so],c('nat','year')]),coeftest(lm((gdp-gni)~factor(nat)+ic+ps+gdp*mr,data=dta[dta$nat %in% codes[st],]),vcovCL,cluster=dta[dta$nat %in% codes[st],c('nat','year')]),coeftest(lm((gdp-gni)~factor(nat)+ic+ps+gdp*mr,data=dta),vcovCL,cluster=dta[,c('nat','year')]),type=output)
stargazer(coeftest(lm(con~factor(nat)+ic+gdp*mr*ps,data=dta),vcovCL,cluster=dta[,c('nat','year')]),type=output)

# for fgl_us

mr.reg<-lm(inc~factor(nat)+ic+gdp*mr*ps,data=dta) # year fe do not change anything
stargazer(coeftest(mr.reg,vcovCL,cluster=dta[,c('nat','year')]),type = output)
