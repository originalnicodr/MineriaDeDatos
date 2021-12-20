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

	error.temp<-double(max.feat)
	step.error<-double(max.feat)

	#print("Se rompe despues de esta linea")

	error.temp[1]<- do.call(method, c(list(x, y), list(...)) ) #usar todas las variables, pero que hago con esto?
	step.error[1]<-error.temp[1]
	
	

	#ranking inicial: elijo la combinacion de variables con menor error de prediccion (primer nivel)
    #x.train<-matrix(0,dim(x)[1],1)
	
	#para cada i, creo el dataset con esa variable sola, entreno un modelo y le mido el error, que lo guardo en class.error[i]
	for(i in 1:max.feat){
		#x.train[,1]<-x[,i]#de mas
		error.temp[i] <- do.call(method, c(list(x[,-i], y), list(...)) )
	}

	#print(error.temp)

	#Guardo una lista keep.feat con las que me quedan para seguir eligiendo.
	list.feat[2]<-which.min(error.temp)#-1 por que cuenta el 0
	step.error[2]<-error.temp[which.min(error.temp)]
	keep.feat<-sort(error.temp,decreasing=FALSE,index=T)$ix[-1]

	#print("keep.feat before")
	#print(keep.feat)

	keep.feat<-keep.feat[keep.feat!=list.feat[2]] #saco la que acabo de guardar, esta bien?

	#print(list.feat)
	#print(error.temp)
	#print(keep.feat)

	#print("keep.feat after")
	#print(keep.feat)

	#print(list.feat)
	#print(keep.feat)

	#print("class.error")
	#print(class.error)

	
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

			#print(list.feat)
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
		#print("saco la feature:")
		#print(keep.feat[best.index])

		#print("best.index")
		#print(best.index)
		#print("class.error")
		#print(class.error)

		list.feat[max.feat-num.feat+2]<-keep.feat[best.index]
		if(verbosity>1) cat("\n---------\nStep ",max.feat-num.feat+1,"\nFeature ",best.index)

		#x.prev<-x.prev[,-keep.feat[best.index]]

		#print(max.feat-num.feat+2)

		#print(max.feat-num.feat+2)
		#print(class.error[best.index])

		step.error[max.feat-num.feat+2]<-class.error[best.index]


		keep.feat<-keep.feat[-best.index]
		if(verbosity>2) cat("\nNew search list: ",keep.feat)
		num.feat<-num.feat-1


		#print("list.feat")
		#print(list.feat)
		#print("class.error")
		#print(class.error)
		#print("best.index")
		#print(best.index)
		#print("keep.feat")
		#print(keep.feat)


	}

	

	list.feat<-c(list.feat[-length(list.feat)],keep.feat)
	#print(list.feat)
	#lo doy vuelta por que las ultimas features en sacar son las mas importantes
	#print(list.feat)

	combined.feat.list<-makefeatlist(list.feat,1:max.feat) 

	
	list.feat<-rev(list.feat[-1])#para sacar el 0 que no saca nada y ordenar por importancia las features
	#print(list.feat)
	#print(list.feat)

	#print(list.feat)



	search.names<-colnames(x)[list.feat]#c("AllVariables",colnames(x)[list.feat])

	#print("search.names")
	#print(search.names)
	#print("max.feat:1")
	#print(max.feat:1)

	#le asigno a cada feature una importacia proporcional al orden en que lo seleccionamos
	imp<-(max.feat:1)/max.feat
	names(imp)<-search.names

	#print("list.feat")
	#print(list.feat)

	

	if(verbosity>1){
		cat("\n---------\nFinal ranking ",num.feat," features.")
		cat("\nFeatures: ",search.names,"\n")
	}

 	return( list(ordered.names.list=search.names,ordered.features.list=combined.feat.list,importance=imp, step.error=step.error) )

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
	step.error<-double(max.feat)
	#para cada i, creo el dataset con esa variable sola, entreno un modelo y le mido el error, que lo guardo en class.error[i]
	for(i in 1:max.feat){
		x.train[,1]<-x[,i]
		class.error[i] <- do.call(method, c(list(x.train, y), list(...)) )
	}
	#guardo la variable con minimo error como primera. Guardo una lista keep.feat con las que me quedan para seguir eligiendo.
	list.feat[1]<-which.min(class.error)

	step.error[1]<- class.error[which.min(class.error)]

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

		step.error[num.feat]<-class.error[best.index]
		#print(class.error[best.index])
		
	}


	search.names<-colnames(x)[list.feat]
	#le asigno a cada feature una importacia proporcional al orden en que lo seleccionamos
	imp<-(max.feat:1)/max.feat
	names(imp)<-search.names

	if(verbosity>1){
		cat("\n---------\nFinal ranking ",num.feat," features.")
		cat("\nFeatures: ",search.names,"\n")
	}


 	return( list(ordered.names.list=search.names,ordered.features.list=list.feat,importance=imp, step.error=step.error) )

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

	print(maxft)
	
	R<-double(maxft)
	C<-unlist(T[maxft+1])
	for (i in 1:maxft){
		print(T[F])
		print(C)
		test<-(M(T[F],C)$feats)
		list.feat<-F[test]
		f<-list.feat[1]#$feats[length(F)]
		R[maxft-i+1]<-f
		F<-F[F!=f]
	}
	return(R)
}

#la original me estaba dando problemas con los tipos
rfe_new <- function(x, y, method = "imp.rf")
{
    var_names <- colnames(x)
    nvars <- ncol(x)
    vars <- 1:nvars
    ranking <- c()

    for (i in 1:(nvars -1))
    {
        partial_ranking <- do.call(method, list(x[, vars], y))$feats
        worst_ranked <- partial_ranking[1]

        ranking <- c(ranking, var_names[worst_ranked])

        vars <- vars[-worst_ranked]
        var_names <- var_names[-worst_ranked]
    }

    ranking <- c(ranking, var_names)

    return(ranking)
}


library(randomForest)
library(kernlab)
library(MASS)


crea.ruido.unif<-function(n=100,d=2){
x<-runif(2*n*d,min=-1)	#genero los datos
dim(x)<-c(2*n,d)
return(cbind(as.data.frame(x),y=factor(rep(c(-1,1),each=n))))	#le agrego la clase
}