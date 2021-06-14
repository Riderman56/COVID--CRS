dta.l=dta
dta.l$lgdp=NA
dta.l$lcon=NA
for (i in codes) {
    dta.l$lgdp[dta.l$nat==i][-(1:4)]=dta.l$gdp[dta.l$nat==i][-(56:59)]
    dta.l$lcon[dta.l$nat==i][-(1:4)]=dta.l$con[dta.l$nat==i][-(56:59)]
}
dta.l=dta.l[dta.l$pit %in% 5:59,]

summary(lm(con~factor(nat)+gdp+lgdp,data = dta.l))
summary(lm(inc~factor(nat)+gdp+lgdp,data = dta.l))


################ test stationarity of quarterly data #################

for (i in codes) {
  print(i)
print(Box.test(get(paste('con',i,sep = '_'))-mean(get(paste('con',i,sep = '_'))), 4, type="Box-Pierce"))
print(Box.test(get(paste('gdp',i,sep = '_'))-mean(get(paste('gdp',i,sep = '_'))), 4, type="Box-Pierce"))
print('')
print('')
}

############## test stationarity of yearly data ######################

source('C:/Users/BongBong/Desktop/nat_ac/permanent_comp_yearlevel.R')

for (i in codes) {
  print(i)
  print(Box.test(diff(get(paste('l.con',i,sep = '_')))-mean(diff(get(paste('l.con',i,sep = '_')))), 4, type="Box-Pierce"))
  print(Box.test(diff(get(paste('l.gdp',i,sep = '_')))-mean(diff(get(paste('l.gdp',i,sep = '_')))), 4, type="Box-Pierce"))
  print('')
  print('')
}