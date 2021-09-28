#-------------------------------------------------------------------------------------
# Crea la lista de los features usados en cada paso a partir de una secuencia de features a sacar
# y una lista con todos los features
#-------------------------------------------------------------------------------------
makefeatlist <- function(list.feat,total.feat){
	result<-list(total.feat)
	i<-1
	for(x in list.feat){
		test<-unlist(result[i])
		#print(test[test!=x])
		result<-append(result,list(test[test!=x]))
		#print(result)
		i<-i+1
	}
	#test<-unlist(result[1])
	#print(append(result,list(test[test!=list.feat[1]])))
	return(result)
}

backward.ranking <- function(x,y,method,verbosity=0,... )
{

	max.feat<-dim(x)[2]
	num.feat<-max.feat-1 #por que ya hice el primer paso
	list.feat<-0:max.feat #0 representa que no saque ninguna feature

	list.feat[1]<- 0#no hace falta

	class.error<-double(max.feat+1)

	#print("Se rompe despues de esta linea")

	class.error[1]<- do.call(method, c(list(x, y), list(...)) ) #usar todas las variables, pero que hago con esto?

	
	

	#ranking inicial: elijo la combinacion de variables con menor error de prediccion (primer nivel)
    x.train<-matrix(0,dim(x)[1],1)
	
	#para cada i, creo el dataset con esa variable sola, entreno un modelo y le mido el error, que lo guardo en class.error[i]
	for(i in 1:max.feat){
		#x.train[,1]<-x[,i]#de mas
		class.error[i] <- do.call(method, c(list(x[,-i], y), list(...)) )
	}

	
	#guardo la variable con minimo error como primera. Guardo una lista keep.feat con las que me quedan para seguir eligiendo.
	list.feat[2]<-which.min(class.error)-1#-1 por que cuenta el 0
	keep.feat<-sort(class.error,decreasing=FALSE,index=T)$ix[-1]

	keep.feat<-keep.feat[-list.feat[2]] #saco la que acabo de guardar, esta bien?

	#print(list.feat)
	#print(keep.feat)

	#print("class.error")
	#print(class.error)
	print("keep.feat")
	print(keep.feat)
	
	#armo un dataset con las variables que ya elegi, para ir agregando en cada paso.
	
	x.prev<-x[,-list.feat[2]]#x.train[,1]<-x[,-list.feat[2]]

	if(verbosity>1){
		cat("\nFirst feature: ",list.feat[1],"\n")
		cat("\nSecond feature: ",list.feat[2],"\n")
	}

    #loop principal. A cada paso agrego todas las variables disponibles, de a una, le mido el error y me quedo con la de minimo error. Hasta llegar a meter todas.
	while(num.feat>1){
        #class.error guarda el error de cada modelo. Son max.feat-num.feat modelos.
		class.error<-double(num.feat)

		#print("tamalño class.error")
		#print(max.feat-num.feat)
		#para cada variable que me queda, la agrego al dataset del paso anterior, entreno el modelo y le mido el error.
		#print("num.feat")
		#print(num.feat)
		#print("x.prev")
		#print(x.prev)

		
		for(i in 1:(num.feat)){
			#print("i")
			#print(i)

			#los numeros obtenidos del keep no los puedo aplicar a prev.

			#print("keep.feat[i]")
			#print(keep.feat[i])
			#print("list.feat")
			#print(list.feat)
			#print("2:(1+max.feat-num.feat)")
			#print(2:(1+max.feat-num.feat))
			#print("max.feat")
			#print(max.feat)

			#print("num.feat")
			#print(num.feat)

			#print("list.feat")
			#print(list.feat)
			


			#x.train<-x.prev[,-keep.feat[i]]

			print(list.feat)
			#print("2+max.feat-num.feat")
			#print(2+max.feat-num.feat)


			x.train<-as.matrix(x[,-c(keep.feat[i],list.feat[2:(1+max.feat-num.feat)])]) #las columnas que vengo sacando hasta ahora (sin el 0) mas la nueva

			
			#print(dim(x.train))

			#class.error[i] <- do.call(method, c(list(x.train, y), list(...)) )
			class.error[i] <- do.call(method, c(list(x.train, y), list(...)) )
		}
		if(verbosity>2) cat("\nFeatures:\n",keep.feat,"\nErrors:\n",class.error)
		#me quedo con el modelo de minimo error, guardo ese feature en la lista de las elegidas, lo saco de la lista de las que quedan, y actualizo el dataset de partida de la iteracion.
		best.index<-which.min(class.error)
		list.feat[max.feat-num.feat+2]<-keep.feat[best.index]
		if(verbosity>1) cat("\n---------\nStep ",max.feat-num.feat+1,"\nFeature ",best.index)

		#x.prev<-x.prev[,-keep.feat[best.index]]

		print("class.error")
		print(class.error)
		print("best.index")
		print(best.index)
		print("keep.feat")
		print(keep.feat)

		keep.feat<-keep.feat[-best.index]
		if(verbosity>2) cat("\nNew search list: ",keep.feat)
		num.feat<-num.feat-1
	}

	list.feat<-c(list.feat[-length(list.feat)],keep.feat)

	print(list.feat)


	search.names<-colnames(x)[list.feat]#c("AllVariables",colnames(x)[list.feat])

	#print("search.names")
	#print(search.names)
	#print("max.feat:1")
	#print(max.feat:1)

	#le asigno a cada feature una importacia proporcional al orden en que lo seleccionamos
	imp<-(max.feat:1)/max.feat
	names(imp)<-search.names

	combined.feat.list<-makefeatlist(list.feat[-1],1:max.feat) #para sacar el 0 que no saca nada

	if(verbosity>1){
		cat("\n---------\nFinal ranking ",num.feat," features.")
		cat("\nFeatures: ",search.names,"\n")
	}

 	return( list(ordered.names.list=search.names,ordered.features.list=combined.feat.list,importance=imp, combined.feat.error=class.error) )

}




#-------------------------------------------------------------------------------------
# AVISO: este codigo esta adaptado de un paquete mayor. 
# No es optimo y tiene cosas inutiles para nosotros. Es un ejemplo nada mas
##-------------------------------------------------------------------------------------
# Crea la lista de los features usados en cada paso a partir de una secuencia de features a sacar
# y una lista con todos los features
#-------------------------------------------------------------------------------------
makefeatlist <- function(list.feat,total.feat){
	result<-list(total.feat)
	i<-1
	for(x in list.feat){
		test<-unlist(result[i])
		#print(test[test!=x])
		result<-append(result,list(test[test!=x]))
		#print(result)
		i<-i+1
	}
	#test<-unlist(result[1])
	#print(append(result,list(test[test!=list.feat[1]])))
	return(result)
}
#general forward greedy selection function
#input:
# x,y inputs and targets
# method is an external function that estimates classification error with a given model
# ... parameters for method
#output:
#ordered.names.list <- nombre de las variables ordenadas de la mas importante a la menos
#ordered.features.list <-numero de orden inicial de las variables, con el mismo orden
#importance <- importancia de cada variables en el mismo orden

#-------------------------------------------------------------------------------------
forward.ranking <- function(x,y,method,verbosity=0,... )
{

	max.feat<-dim(x)[2]
	num.feat<-1
	list.feat<-1:max.feat

	#ranking inicial: elijo la variable con menor error de prediccion
    x.train<-matrix(0,dim(x)[1],1)
	class.error<-double(max.feat)
	#para cada i, creo el dataset con esa variable sola, entreno un modelo y le mido el error, que lo guardo en class.error[i]
	for(i in 1:max.feat){
		x.train[,1]<-x[,i]
		class.error[i] <- do.call(method, c(list(x.train, y), list(...)) )
	}
	#guardo la variable con minimo error como primera. Guardo una lista keep.feat con las que me quedan para seguir eligiendo.
	list.feat[1]<-which.min(class.error)
	keep.feat<-sort(class.error,decreasing=FALSE,index=T)$ix[-1]
	#armo un dataset con las variables que ya elegi, para ir agregando en cada paso.
	print(list.feat)
	x.prev<-x.train[,1]<-x[,list.feat[1]]

	if(verbosity>1) cat("\nFirst feature: ",list.feat[1],"\n")

    #loop principal. A cada paso agrego todas las variables disponibles, de a una, le mido el error y me quedo con la de minimo error. Hasta llegar a meter todas.
	while(num.feat<max.feat){
        #class.error guarda el error de cada modelo. Son max.feat-num.feat modelos.
		class.error<-double(max.feat-num.feat)
		#para cada variable que me queda, la agrego al dataset del paso anterior, entreno el modelo y le mido el error.
		for(i in 1:(max.feat-num.feat)){
			x.train<-cbind(x.prev,x[,keep.feat[i]])
			class.error[i] <- do.call(method, c(list(x.train, y), list(...)) )
		}
		if(verbosity>2) cat("\nFeatures:\n",keep.feat,"\nErrors:\n",class.error)
		#me quedo con el modelo de minimo error, guardo ese feature en la lista de las elegidas, lo saco de la lista de las que quedan, y actualizo el dataset de partida de la iteracion.
		best.index<-which.min(class.error)
		list.feat[num.feat+1]<-keep.feat[best.index]
		if(verbosity>1) cat("\n---------\nStep ",1+num.feat,"\nFeature ",best.index)

		keep.feat<-keep.feat[-best.index]
		if(verbosity>2) cat("\nNew search list: ",keep.feat)
		num.feat<-num.feat+1
		x.prev<-x[,list.feat[1:num.feat]]
	}


	search.names<-colnames(x)[list.feat]
	#le asigno a cada feature una importacia proporcional al orden en que lo seleccionamos
	imp<-(max.feat:1)/max.feat
	names(imp)<-search.names

	if(verbosity>1){
		cat("\n---------\nFinal ranking ",num.feat," features.")
		cat("\nFeatures: ",search.names,"\n")
	}


 	return( list(ordered.names.list=search.names,ordered.features.list=list.feat,importance=imp) )

}


#---------------------------------------------------------------------------
#random forest error estimation (OOB) for greedy search
#---------------------------------------------------------------------------
rf.est <- function(x.train,y,equalize.classes=TRUE,tot.trees=500,mtry=0)
{
	#print("typeof(x)")
	#print(typeof(x.train))
	#print(x.train)
	#print("typeof(y)")
	#print(typeof(y))
	#print("y")
	#print(y)

	if(mtry<1) mtry<-floor(sqrt(dim(x.train)[2]))
	prop.samples<-table(y)
	if(equalize.classes) prop.samples<-rep(min(prop.samples),length(prop.samples))
	return( randomForest(x.train,y,mtry=mtry,ntree=tot.trees,sampsize=prop.samples)$err.rate[tot.trees] )
}

#---------------------------------------------------------------------------
#LDA error estimation (LOO) for greedy search
#---------------------------------------------------------------------------
lda.est <- function(x.train,y)
{
	m.lda <- lda(x.train,y,CV=TRUE)
	return(error.rate( y , m.lda$class))
}
error.rate <- function(dataA, dataB) sum( dataA != dataB ) / length(dataB)

#---------------------------------------------------------------------------
#SVM error estimation (internal CV) for greedy search
#---------------------------------------------------------------------------
svm.est <- function(x.train,y,type="C-svc",kernel="vanilladot",C=1,cross = 4)
{
	#print(dim(x.train))
	#print(dim(y))
	return ( ksvm(x.train, y, type=type,kernel=kernel,C=C,cross = cross)@cross )
}


#---------------------------------------------------------------------------
#random forest ranking method for rfe.
#---------------------------------------------------------------------------
imp.rf <- function(x.train,y,equalize.classes=TRUE,tot.trees=500,mtry=0)
{
	#print("x")
	#print(dim(x.train))
	#print("y")
	#print(dim(y))

	if(mtry<1) mtry<-floor(sqrt(dim(x.train)[2]))
	prop.samples<-table(y)
	if(equalize.classes) prop.samples<-rep(min(prop.samples),length(prop.samples))
	
	m.rf<-randomForest(x.train,y,ntree=tot.trees,mtry=mtry,sampsize=prop.samples,importance=TRUE)
	imp.mat<-importance(m.rf)
	imp.col<-dim(imp.mat)[2]-1
	rank.list<-sort(imp.mat[,imp.col],decreasing=FALSE,index=T)
	return(list(feats=rank.list$ix,imp=rank.list$x))
}


#---------------------------------------------------------------------------
#linear svm ranking method for rfe. Using kernlab. Multiclass
#---------------------------------------------------------------------------
imp.linsvm <- function(x.train,y,C=100)
{
	num.feat<-dim(x.train)[2]
	tot.problems<-nlevels(y)*(nlevels(y)-1)/2

	m.svm <- ksvm(as.matrix(x.train), y, type="C-svc",kernel="vanilladot",C=C)

	w<-rep(0.0,num.feat)
	for(i in 1:tot.problems) for(feat in 1:num.feat)
		w[feat]<-w[feat]+abs(m.svm@coef[[i]] %*% m.svm@xmatrix[[i]][,feat])
	rank.list<-sort(w,decreasing=FALSE,index=T)
	return(list(feats=rank.list$ix,imp=rank.list$x))
}



#---------------------------------------------------------------------------
# Filtro no parametrico Kruskal-Wallis
#---------------------------------------------------------------------------
kwfilter <- function(x,y){
	stats<-c()
	for (i in 1:(dim(x)[2])){
		c<-x[,i]
		stats<-c(stats,kruskal.test(c,y)[1]$statistic)
	}
	#print(sort(stats))
	return(sort(stats,decreasing = TRUE,index.return = TRUE)$ix)
}

#---------------------------------------------------------------------------
# Filtro no parametrico Kruskal-Wallis
#---------------------------------------------------------------------------
rfe<-function(T,M){
	F<-1:(dim(T)[2]-1)
	maxft<-length(F)
	
	R<-double(maxft)
	C<-unlist(T[maxft+1])
	print(C)
	for (i in 1:maxft){
		#print(F)
		#print(C)
		#y.pos<-length(F)
		#list.feat<-do.call(M, T[F],T[maxft+1]$Species)#M(T[F],T[maxft+1])
		#print(C)
		#print(T[maxft+1]$Species)
		test<-(M(T[F],C)$feats)
		#print(test)
		list.feat<-F[test]
		#print(list.feat$feats)
		f<-list.feat[length(F)]#$feats[length(F)]
		#print(length(F))
		#print(f)
		R[maxft-i+1]<-f
		#print("list.feat")
		#print(list.feat)
		#print("maxft-i+1")
		#print(maxft-i+1)
		#print("f")
		#print(f)
		#print("F")
		#print(F)
		F<-F[F!=f]
		#print("F<-F[F!=f]")
		#print(F)
	}
	return(R)
}



library(randomForest)
library(kernlab)
library(MASS)

#demo: aplicar el wrapper a los datos de iris
#data(iris)
#FORW.rf <-forward.ranking(iris[,-5],iris[,5],method="rf.est" ,tot.trees=100,equalize.classes=F, verbosity=0)
#print("test")
#FORW.lda<-forward.ranking(iris[,-5],iris[,5],method="lda.est")

#BACKW.rf <-backward.ranking(iris[,-5],iris[,5],method="rf.est" ,tot.trees=100,equalize.classes=F, verbosity=3)
#BACKW.lda<-backward.ranking(iris[,-5],iris[,5],method="lda.est")


#kwfilter(iris[,-5],iris[,5])
#print(iris[,-5])
#print(iris[,5])
#r<-rfe(iris,imp.linsvm)

#a<-imp.rf(iris[,-5],iris[,5])



#ej2


crea.ruido.unif<-function(n=100,d=2){
x<-runif(2*n*d,min=-1)	#genero los datos
dim(x)<-c(2*n,d)
return(cbind(as.data.frame(x),y=factor(rep(c(-1,1),each=n))))	#le agrego la clase
}

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
BACKW.rf.dA<-backward.ranking(datosA[,-claseApos],datosA[,claseApos],method="rf.est" ,tot.trees=100,equalize.classes=F, verbosity=0)
print("BACKW.lda.dA")
BACKW.lda.dA<-backward.ranking(datosA[,-claseApos],datosA[,claseApos],method="lda.est" , verbosity=0)
#print("BACKW.svm.dA")
#BACKW.svm.dA<-backward.ranking(datosA[,-claseApos],datosA[,claseApos],method="svm.est" , verbosity=0)

print("BACKW.rf.dB")
BACKW.rf.dB<-backward.ranking(datosB[,-claseBpos],datosB[,claseBpos],method="rf.est" ,tot.trees=100,equalize.classes=F, verbosity=0)
print("BACKW.lda.dB")
BACKW.lda.dB<-backward.ranking(datosB[,-claseBpos],datosB[,claseBpos],method="lda.est" , verbosity=0)
#print("BACKW.svm.dB")
#BACKW.svm.dB<-backward.ranking(datosB[,-claseBpos],datosB[,claseBpos],method="svm.est" , verbosity=0)

print("KWF.dA")
KWF.dA<-kwfilter(datosA[,-claseApos],datosA[,claseApos])
print("KWF.dB")
KWF.dB<-kwfilter(datosB[,-claseBpos],datosB[,claseBpos])

print("RFE.rf.dA")
RFE.rf.dA<-rfe(datosA,imp.rf)
print("RFE.rf.dB")
RFE.rf.dB<-rfe(datosB,imp.rf)

print("RFE.linsvm.dA")
RFE.linsvm.dA<-rfe(datosA,imp.linsvm)
print("RFE.linsvm.dB")
RFE.linsvm.dB<-rfe(datosB,imp.linsvm)
