###########################################################################################################################################
###################### discrete choice problem for risk sharing in presence of consumption cap and transfer ###############################
###########################################################################################################################################

# utility function

util<-function(c,mc=999){
  log(min(c,mc))
}

scenario<-function(mc=999,ps=0,p=0.5)
{
# set seed and # simulated incomes

set.seed(123)
g=10000

# set exogeneous variables

mc=mc#40            # consumption restrictive measures - lower = stricter 
ps=ps#15            # transfer compensation for covid related losses

ep=35               # mean / expected production
sd=8                # standard deviation of production

cf=2                # standard deviation of common factor

p=p/min(mc,50)*50   # price of full risk sharing compared to autarky (linear)

# simulate income for A and B (two ex ante identical countries)

com<-rnorm(g,0,cf)

y_a<-rnorm(g,ep,sd)+com
sd(y_a)

y_b<-rnorm(g,ep,sd)+com
sd(y_b)

y_s<-(y_a+y_b)/2
sd(y_s)

# grid for possible degrees of risk sharing (0 no risk sharing at all 1 full risk sharing)

k<-seq(0,1,0.01)

# simulation of resulting utility for different degrees of risk sharing - small economies (-> price takers) 
# assuming that a priori countries take each realisation of income as equally likely we can simulate expectations by taking means across realizations

UA=matrix(nrow = g,ncol = 101)
for (i in 1:g) {
 ua<-sapply(k*y_s[i]+(1-k)*y_a[i]+ps-k*p, util,mc=mc) 
 UA[i,]=ua
}
res_a=colMeans(UA)
plot(res_a)

UB=matrix(nrow = g,ncol = 101)
for (i in 1:g) {
  ub<-sapply(k*y_s[i]+(1-k)*y_b[i]+ps-k*p, util,mc=mc) 
  UB[i,]=ub
}
res_b=colMeans(UB)
plot(res_b)
}

# basecase
scenario()

# restrictive measures
scenario(mc=40)

# policy support
scenario(ps=10)

# interaction of policy support and restrictive measures
scenario(ps=10,mc=40)
