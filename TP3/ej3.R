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
print(stability(gausianas[,-3],10,30))
#-----------------------

#-----Iris--------
print("GAP iris")
print(GAP(prcomp(scale(log(iris[-5])))$x,10,30))
print("Stability iris")
print(stability(prcomp(scale(log(iris[-5])))$x,10,30))
#-----------------


#----Lampone------

#prcomp(lampone[,-dim(lampone)[2]][,-143][,-1])

scaled_lampone<-prcomp(scale(log(iris[-5])))$x#saco las clases y una columna que decia el numero de la muestra nomas, sino me rompia todo

print("GAP lampone")
print(GAP(scaled_lampone,10,30))
print("Stability lampone")
print(stability(scaled_lampone,10,30))

#-----------------