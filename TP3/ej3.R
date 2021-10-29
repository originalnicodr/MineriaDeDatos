#3)

library(stats)
library(cluster)

#-----Gaussianas--------
tot.puntos<-100
gap=2
x<-rnorm(tot.puntos,mean=-gap)
y<-rnorm(tot.puntos,mean=-gap)
gausianas<-cbind(x,y,rep(1,length(x)))
x<-rnorm(tot.puntos,mean=2*gap)
y<-rnorm(tot.puntos,mean=0)
gausianas<-rbind(gausianas,cbind(x,y,rep(2,length(x))))
x<-rnorm(tot.puntos,mean=0.7*gap,sd=0.5)
y<-rnorm(tot.puntos,mean=2.5*gap,sd=0.5)
gausianas<-rbind(gausianas,cbind(x,y,rep(3,length(x))))
x<-rnorm(tot.puntos,mean=-gap,sd=0.5)
y<-rnorm(tot.puntos,mean=gap,sd=0.5)
gausianas<-rbind(gausianas,cbind(x,y,rep(4,length(x))))
plot(gausianas[,1:2],col=gausianas[,3])

print("GAP gaussianas")
print(GAP(gausianas[,-3],10,30))
print("Stability gaussianas")
print(stability_new(gausianas[,-3],10,30))
#-----------------------

#-----Iris--------
print("GAP iris")
print(GAP(prcomp(scale(log(iris[-5])))$x,10,30))
print("Stability iris")
print(stability_new(prcomp(scale(log(iris[-5])))$x,10,30))
#-----------------

#----Lampone------
scaled_lampone<-prcomp(lampone[,-c(dim(lampone)[2],143, 1)],retx=T)$x

print("GAP lampone")
print(GAP(scaled_lampone,10,30))
print("Stability lampone")
print(stability_new(scaled_lampone,10,30))
#-----------------
