
#ej2




#datosA
d<-10
n<-1000
datos<-crea.ruido.unif(n=n,d=d)

#tomar 50% de los datos al azar, y hacer que la clase sea el signo de la 8 variable
shuffle<-sample(1:dim(datos)[1])
sub<-shuffle[1:dim(datos)[1]*0.5]
datos[sub,d+1]<-sign(datos[sub,8])
#tomar 20% de los datos al azar (fuera de los anteriores), y hacer que la clase sea el signo de la 6 variable
sub<-shuffle[(dim(datos)[1]*0.5):(dim(datos)[1]*0.7)]
datos[sub,d+1]<-sign(datos[sub,6])
#tomar 10% de los datos al azar, y hacer que la clase sea el signo de la 4 variable
sub<-shuffle[(dim(datos)[1]*0.7):(dim(datos)[1]*0.8)]
datos[sub,d+1]<-sign(datos[sub,4])
#tomar 5% de los datos al azar, y hacer que la clase sea el signo de la 2 variable
sub<-shuffle[(dim(datos)[1]*0.8):(dim(datos)[1]*0.85)]
datos[sub,d+1]<-sign(datos[sub,2])
datos[,d+1]<-factor(datos[,d+1])

datosA<-datos

claseApos<-dim(datosA)[2]

#datosB
#generar n=100,d=8
d<-8
n<-1000
datos<-crea.ruido.unif(n=n,d=d)
#hacer que la clase sea el xor de las 2 primeras variables (es usando el signo)
datos[,d+1]<-sign(datos[,1]*datos[,2])
#hacer que las variables 3 y 4 tengan un 50% de correlacion con la clase
shuffle<-sample(1:dim(datos)[1])
sub<-shuffle[1:dim(datos)[1]*0.5]
datos[sub,3]<-abs(datos[sub,3])*datos[sub,d+1]
shuffle<-sample(1:dim(datos)[1])
sub<-shuffle[1:dim(datos)[1]*0.5]
datos[sub,4]<-abs(datos[sub,4])*datos[sub,d+1]
datos[,d+1]<-factor(datos[,d+1])

datosB<-datos

claseBpos<-dim(datosB)[2]



print("FORW.rf.dA")
FORW.rf.dA <-forward.ranking(datosA[,-claseApos],datosA[,claseApos],method="rf.est" ,tot.trees=100,equalize.classes=F, verbosity=0)
print("FORW.lda.dA")
FORW.lda.dA <-forward.ranking(datosA[,-claseApos],datosA[,claseApos],method="lda.est" , verbosity=0)
#print("FORW.svm.dA")
#FORW.svm.dA <-forward.ranking(datosA[,-claseApos],datosA[,claseApos],method="svm.est" , verbosity=0)

print("FORW.rf.dB")
FORW.rf.dB <-forward.ranking(datosB[,-claseBpos],datosB[,claseBpos],method="rf.est" ,tot.trees=100,equalize.classes=F, verbosity=0)
print("FORW.lda.dB")
FORW.lda.dB <-forward.ranking(datosB[,-claseBpos],datosB[,claseBpos],method="lda.est" , verbosity=0)
#print("FORW.svm.dB")
#FORW.svm.dB <-forward.ranking(datosB[,-claseBpos],datosB[,claseBpos],method="svm.est" , verbosity=0)

print("BACKW.rf.dA")
BACKW.rf.dA <- backward.ranking(datosA[,-claseApos],datosA[,claseApos],method="rf.est" ,tot.trees=100,equalize.classes=F, verbosity=0)
print("BACKW.lda.dA")
BACKW.lda.dA <- backward.ranking(datosA[,-claseApos],datosA[,claseApos],method="lda.est" , verbosity=0)
#print("BACKW.svm.dA")
#BACKW.svm.dA<-backward.ranking(datosA[,-claseApos],datosA[,claseApos],method="svm.est" , verbosity=0)

print("BACKW.rf.dB")
BACKW.rf.dB <- backward.ranking(datosB[,-claseBpos],datosB[,claseBpos],method="rf.est" ,tot.trees=100,equalize.classes=F, verbosity=0)
print("BACKW.lda.dB")
BACKW.lda.dB <- backward.ranking(datosB[,-claseBpos],datosB[,claseBpos],method="lda.est" , verbosity=0)
#print("BACKW.svm.dB")
#BACKW.svm.dB<-backward.ranking(datosB[,-claseBpos],datosB[,claseBpos],method="svm.est" , verbosity=0)

print("KWF.dA")
KWF.dA <- kwfilter(datosA[,-claseApos],datosA[,claseApos])
print("KWF.dB")
KWF.dB <- kwfilter(datosB[,-claseBpos],datosB[,claseBpos])

print("RFE.rf.dA")
RFE.rf.dA <- rfe(datosA,imp.rf)
print("RFE.rf.dB")
RFE.rf.dB <- rfe(datosB,imp.rf)

print("RFE.linsvm.dA")
RFE.linsvm.dA <- rfe(datosA,imp.linsvm)
print("RFE.linsvm.dB")
RFE.linsvm.dB <- rfe(datosB,imp.linsvm)




	features<-forward.ranking(dataset,clases,method="rf.est" ,tot.trees=100,equalize.classes=F, verbosity=0)$ordered.features.list[1:10]
	sum(features %in% 1:10)/10