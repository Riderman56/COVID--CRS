###########################################################################################################################################
###################### discrete choice problem for risk sharing in presence of consumption cap and transfer ###############################
###########################################################################################################################################

# utility function

util<-function(c,mc=999){
  log(min(c,mc))
}

# set seed and # simulated incomes

set.seed(123)
g=10000

# set exogeneous variables

mc=50#40            # consumption restrictive measures - lower = stricter 
ps=10#15            # transfer compensation for covid related losses

ep=30               # mean / expected production
sd=5                # standard deviation of production

cf=8                # standard deviation of common factor

p=1                 # price of full risk sharing compared to autarky (linear)

# simulate income for A and B (two ex ante identical countries)

y_a<-rnorm(g,ep,sd)+rnorm(g,0,cf)

y_b<-rnorm(g,ep,sd)+rnorm(g,0,cf)

y_s<-(y_a+y_b)/2

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

