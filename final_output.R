#######################################################################################
########### get all regressions as latex output #######################################
#######################################################################################

# 1 plot of correlations of main variables

source('C:/Users/BongBong/Desktop/nat_ac/pre_reg.R')

output='latex' # latex or text in console

# 2 simple regression

Consumption<-coeftest(r.s.c.1,vcovCL,cluster=dta[,c('nat','year')])
Disposable_Income<-coeftest(r.s.i.1,vcovCL,cluster=dta[,c('nat','year')])
stargazer(Consumption,Disposable_Income,type = output,title = 'OLS: Y = country fixed effects + GDP',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects',column.labels = c('Y=Consumption','Y=Disposable Income'))

# 3 plot of dynamics of simple regressions

# 4 regression with covid controlls

Consumption<-coeftest(r.i.c.1,vcovCL,cluster=dta[,c('nat','year')])
Disposable_Income<-coeftest(r.i.i.1,vcovCL,cluster=dta[,c('nat','year')])
stargazer(Consumption,Disposable_Income,type = output,title = 'OLS: Y = country fixed effects + incidence + GDP * covid measures * policy support',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects',column.labels = c('Y=Consumption','Y=Disposable Income'))


# 5 plot of dynamics of interaction regressions

# 6 sample split based on measure/support balance

sub.dta=dta[dta$pit>55,c('pit','nat','mc','ps')]

corrs<-data.frame(sub.dta[1:4,3:4])
for(i in 2:13){
  corrs<-data.frame(corrs,sub.dta[(1+4*(i-1)):(4+4*(i-1)),3:4])
}

# summary(corrs)

ratio<-rbind(names,colMeans(corrs)[seq(1,26,2)]/colMeans(corrs)[seq(2,26,2)])

subsam<-ratio[2,]>1
othsam<-ratio[2,]<=1

# sample split

stargazer(coeftest(lm(con~factor(nat)+ic+gdp*mc*ps,data = dta[dta$nat %in% codes[subsam],]),vcovCL,cluster=dta[dta$nat %in% codes[subsam],][,c('nat','year')]),coeftest(lm(con~factor(nat)+ic+gdp*mc*ps,data = dta[dta$nat %in% codes[othsam],]),vcovCL,cluster=dta[dta$nat %in% codes[othsam],][,c('nat','year')]),coeftest(lm(con~factor(nat)+ic+gdp*mc*ps,data = dta[dta$nat %in% codes,]),vcovCL,cluster=dta[dta$nat %in% codes,][,c('nat','year')]),type=output,title = 'OLS: Consumption = country fixed effects + incidence + GDP * covid measures * policy support',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects',column.labels = c('high measures to support','low measures to support','all'))

stargazer(coeftest(lm(inc~factor(nat)+ic+gdp*mc*ps,data = dta[dta$nat %in% codes[subsam],]),vcovCL,cluster=dta[dta$nat %in% codes[subsam],][,c('nat','year')]),coeftest(lm(inc~factor(nat)+ic+gdp*mc*ps,data = dta[dta$nat %in% codes[othsam],]),vcovCL,cluster=dta[dta$nat %in% codes[othsam],][,c('nat','year')]),coeftest(lm(inc~factor(nat)+ic+gdp*mc*ps,data = dta[dta$nat %in% codes,]),vcovCL,cluster=dta[dta$nat %in% codes,][,c('nat','year')]),type=output,title = 'OLS: Disposable Income = country fixed effects + incidence + GDP * covid measures * policy support',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects',column.labels = c('high measures to support','low measures to support','all'))

stargazer(coeftest(lm((inc-con)~factor(nat)+ic+gdp*mc*ps,data = dta[dta$nat %in% codes[subsam],]),vcovCL,cluster=dta[dta$nat %in% codes[subsam],][,c('nat','year')]),coeftest(lm((inc-con)~factor(nat)+ic+gdp*mc*ps,data = dta[dta$nat %in% codes[othsam],]),vcovCL,cluster=dta[dta$nat %in% codes[othsam],][,c('nat','year')]),coeftest(lm((inc-con)~factor(nat)+ic+gdp*mc*ps,data = dta[dta$nat %in% codes,]),vcovCL,cluster=dta[dta$nat %in% codes,][,c('nat','year')]),type=output,title = 'OLS: Savings = country fixed effects + incidence + GDP * covid measures * policy support',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects',column.labels = c('high measures to support','low measures to support','all'))

stargazer(coeftest(lm((gdp-gni)~factor(nat)+ic+gdp*mc*ps,data = dta[dta$nat %in% codes[subsam],]),vcovCL,cluster=dta[dta$nat %in% codes[subsam],][,c('nat','year')]),coeftest(lm((inc-con)~factor(nat)+ic+gdp*mc*ps,data = dta[dta$nat %in% codes[othsam],]),vcovCL,cluster=dta[dta$nat %in% codes[othsam],][,c('nat','year')]),coeftest(lm((inc-con)~factor(nat)+ic+gdp*mc*ps,data = dta[dta$nat %in% codes,]),vcovCL,cluster=dta[dta$nat %in% codes,][,c('nat','year')]),type=output,title = 'OLS: Net Factor Income = country fixed effects + incidence + GDP * covid measures * policy support',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects',column.labels = c('high measures to support','low measures to support','all'))


# 7 channels contributing to our results

stargazer(coeftest(b_f,vcovCL,cluster=dta[,c('nat','year')]),coeftest(b_d,vcovCL,cluster=dta[,c('nat','year')]),coeftest(b_t,vcovCL,cluster=dta[,c('nat','year')]),coeftest(b_s,vcovCL,cluster=dta[,c('nat','year')]),coeftest(b_u,vcovCL,cluster=dta[,c('nat','year')]),type = output,title = 'OLS: Channel decomposition',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects',column.labels = c('b_f','b_d','b_t','b_s','b_u'))

# 8 plot dynamics of channels w/o covid controlls

# 9 plot dynamics of channels with measures at zero

# 10 show correlations of risk sharing channels

# looking closer at b_f

source('C:/Users/BongBong/Desktop/nat_ac/cross_border.R')

output='latex'

# 11 sample split in b_f estimation based on share of foreign workers above or below mean

stargazer(coeftest(lm((gdp-gni)~factor(nat)+ic+gdp*mc*ps,data=dta[dta$nat %in% codes[so],]),vcovCL,cluster=dta[dta$nat %in% codes[so],c('nat','year')]),coeftest(lm((gdp-gni)~factor(nat)+ic+gdp*mc*ps,data=dta[dta$nat %in% codes[st],]),vcovCL,cluster=dta[dta$nat %in% codes[st],c('nat','year')]),coeftest(lm((gdp-gni)~factor(nat)+ic+gdp*mc*ps,data=dta[dta$nat %in% codes[-3],]),vcovCL,cluster=dta[dta$nat %in% codes[-3],c('nat','year')]),type=output,title = 'OLS: (GDP - GNI) = country fixed effects + incidence + GDP * covid measures * policy support',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects',column.labels = c('many foreign workers','few foreign workers','all'))

# 12 adding international border closings to our regression, leave out mc because dcb is included in mc

stargazer(coeftest(lm((gdp-gni)~factor(nat)+ic+gdp*dcb*ps,data=dta[dta$nat %in% codes[so],]),vcovCL,cluster=dta[dta$nat %in% codes[so],c('nat','year')]),coeftest(lm((gdp-gni)~factor(nat)+ic+gdp*dcb*ps,data=dta[dta$nat %in% codes[st],]),vcovCL,cluster=dta[dta$nat %in% codes[st],c('nat','year')]),coeftest(lm((gdp-gni)~factor(nat)+ic+gdp*dcb*ps,data=dta),vcovCL,cluster=dta[,c('nat','year')]),type=output,title = 'OLS: (GDP - GNI) = country fixed effects + incidence + GDP * share of days with closed borders * policy support',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects',column.labels = c('many foreign workers','few foreign workers','all'))

# are we really seeing the effect of the foreign employment??? yes, net international investment is not significantly different for the two groups / remark: not only net position matters in this context

# intinv_raw<-read_xlsx("C:/Users/BongBong/Desktop/nat_ac/eurostat_data/BOP_IIP6_Q__custom_10998231624869611973.xlsx",sheet = 3)
# intinv<-intinv_raw[intinv_raw$`Data extracted on 28/06/2021 10:40:16 from [ESTAT]` %in% c(names,'TIME'),]
# cols=c(1,2)
# for(t in 2006:2020){
#   cols=c(cols,grep(toString(t), intinv[1,]))
# }
# intinv=intinv[,cols]
# dta$ii=NA
# for(r in 1:13){
#   assign(paste('ii',intinv$`Data extracted on 28/06/2021 10:40:16 from [ESTAT]`[r+1],sep = '_'),intinv[r+1,-c(1:3)])
# }
# 
# for(r in 1:13){
#   n<-names[r]
#   c<-codes[r]
#   dta$ii[dta$nat==c]=get(paste('ii',n,sep = '_'))
# }
# 
# dta$ii=as.numeric(dta$ii)/as.numeric(dta$Gdp)
# 
# mean(dta$ii[dta$nat %in% codes[so]],na.rm = TRUE) # big share of foreigners in employment
# sd(dta$ii[dta$nat %in% codes[so]],na.rm = TRUE)
# mean(dta$ii[dta$nat %in% codes[st]],na.rm = TRUE) # small share of foreigners in employment
# sd(dta$ii[dta$nat %in% codes[st]],na.rm = TRUE)
# dta$gd=dta$nat %in% codes[so]
# summary(lm(ii~factor(gd),data=dta)) # in terms of a simple regression SE neither HC robust nor clustered implies here a more conservative approach

# permanent component 

source('C:/Users/BongBong/Desktop/nat_ac/permanent_comp.R')

output='latex'

# 13 permanent component consumption

tot=coeftest(norm.c,vcovCL,cluster=dta.p.c[,c('nat','year')])
per=coeftest(perm.c,vcovCL,cluster=dta.p.c[,c('nat','year')])
tra=coeftest(tran.c,vcovCL,cluster=dta.p.c[,c('nat','year')])
rownames(per)=rownames(tot)
rownames(tra)=rownames(tot)
stargazer(tot,per,tra,type = output,title = 'OLS: Consumption = country fixed effects + incidence + GDP component * covid measures * policy support',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects',column.labels = c('total','permanent','transitory'))

# 14 permanent component disposable income

toti=coeftest(norm.i,vcovCL,cluster=dta.p.c[,c('nat','year')])
peri=coeftest(perm.i,vcovCL,cluster=dta.p.c[,c('nat','year')])
trai=coeftest(tran.i,vcovCL,cluster=dta.p.c[,c('nat','year')])
rownames(peri)=rownames(toti)
rownames(trai)=rownames(toti)
stargazer(toti,peri,trai,type = output,title = 'OLS: Disposable Income = country fixed effects + incidence + GDP component * covid measures * policy support',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects',column.labels = c('total','permanent','transitory'))

# 15 dynamics over time

# 16 dynamics over time with break (second break in appendix as well as yearly data results)

# us comparison <-cross border and permanent component

source('C:/Users/BongBong/Desktop/nat_ac/permanent_comp_usa.R')

output='latex'

# 17 travel restrictions within states

travel.reg=glm(inc~factor(state)+ic+gsp*ps*mr,data = dta.us,weights=dta.us$pop)
stargazer(coeftest(travel.reg,vcovCL,cluster=dta.us[,c('state','year')]),type = output,title = 'WLS: Personal Income = state fixed effects + incidence + GSP * travel restrictions * policy support',style = 'qje',omit = 'factor',omit.labels = ' State fixed effects')

# 18 obligatory stay at home requirements

home.reg=glm(inc~factor(state)+ic+gsp*ps*sh,data = dta.us,weights=dta.us$pop)
stargazer(coeftest(home.reg,vcovCL,cluster=dta.us[,c('state','year')]),type = output,title = 'WLS: Personal Income = state fixed effects + incidence + GSP * stay at home requirements * policy support',style = 'qje',omit = 'factor',omit.labels = ' State fixed effects')

# 19 same for permanent component 

totus=coeftest(normreg.sh,vcovCL,cluster=dta.us.p[,c('state','year')])
perus=coeftest(permreg.sh,vcovCL,cluster=dta.us.p[,c('state','year')])
traus=coeftest(tranreg.sh,vcovCL,cluster=dta.us.p[,c('state','year')])
rownames(perus)=rownames(totus)
rownames(traus)=rownames(totus)
stargazer(totus,perus,traus,type=output,title = 'WLS: Personal Income = state fixed effects + incidence + GDP component * stay at home requirements * policy support',style = 'qje',omit = 'factor',omit.labels = ' State fixed effects',column.labels = c('total','permanent','transitory'))

#######################################################################################################
########################## begin appendix #############################################################
#######################################################################################################

# get europe comparison

source('C:/Users/BongBong/Desktop/nat_ac/permanent_comp.R')

output='latex'

# 20 stay home in perm component for europe # attenton difference between personal and disposable income

totsh=coeftest(norm.sh,vcovCL,cluster=dta.p.c[,c('nat','year')])
persh=coeftest(perm.sh,vcovCL,cluster=dta.p.c[,c('nat','year')])
trash=coeftest(tran.sh,vcovCL,cluster=dta.p.c[,c('nat','year')])
rownames(persh)=rownames(totsh)
rownames(trash)=rownames(totsh)
stargazer(totsh,persh,trash,type = output,title = 'OLS: Disposable Income = country fixed effects + incidence + GDP component * stay home requirements * policy support',style = 'qje',omit = 'factor',omit.labels = ' Country fixed effects',column.labels = c('total','permanent','transitory'))
