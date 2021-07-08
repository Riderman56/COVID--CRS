util<-function(x){
  return(log(x))
}

c<-seq(1,400,0.1)
u<-sapply(c,util)
plot(c,u,type = 'l',xlim = c(35,75),ylim = c(3.8,4.2))
abline(v=55,col='orange')
abline(v=65,col='blue')
abline(h=util(55),col='orange',lty='dashed')
abline(h=util(65),col='blue',lty='dashed')
lines(c(50,60),util(c(50,60)),type = 'p',col='red',lwd=6)
lines(c(40,60),c(util(50),util(50)))
lines(c(50,70),c(util(60),util(60)))
lines(c(70,70),c(util(60)-0.02,util(60)+0.02))
lines(c(50,50),c(util(60)-0.02,util(60)+0.02))
lines(c(40,40),c(util(50)-0.02,util(50)+0.02))
lines(c(60,60),c(util(50)-0.02,util(50)+0.02))

lines(c(48,58),util(c(48,58)),type = 'p',col='green',lwd=6)
lines(c(42,54),c(util(48),util(48)))
lines(c(52,64),c(util(58),util(58)))
lines(c(64,64),c(util(58)-0.02,util(58)+0.02))
lines(c(52,52),c(util(58)-0.02,util(58)+0.02))
lines(c(42,42),c(util(48)-0.02,util(48)+0.02))
lines(c(54,54),c(util(48)-0.02,util(48)+0.02))

