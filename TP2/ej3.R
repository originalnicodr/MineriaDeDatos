#ej3
#source('/tp1-functions.R')
#-------------------Forward Wrapper rf------------------------------------------
print("Forward Wrapper rf")
FORW.rf.est.aciertos<-c()
for (i in 1:30){
	print(paste0("Iteracion n°", i))
	#----Dataset generation-----
	dataset<-diagonal(100,10,2)
	#print(as.matrix(dataset))
	clases<-as.factor(unlist(dataset[dim(dataset)[2]]))
	#print(clases)
	dataset<-dataset[-dim(dataset)[2]]
	#print(dataset)
	ruido<-replicate(90,runif(100, -5, 5)) #ver bien el rango
	dataset<-cbind(dataset,ruido)

	#print(dim(dataset))
	#print(dim(clases))
	#---------------------------

	features<-forward.ranking(dataset,clases,method="rf.est" ,tot.trees=100,equalize.classes=F, verbosity=0)$imp[1:10]
	FORW.rf.est.aciertos <- c(FORW.rf.est.aciertos, sum(features %in% 1:10)/10)
}
#print(FORW.rf.est.aciertos)
#print(unlist(FORW.rf.est.aciertos))

FORW.rf.est.resultado<-sum(FORW.rf.est.aciertos)/30
print(FORW.rf.est.resultado)
#---------------------------------------------------------------------------------



#-------------------Forward Wrapper lda------------------------------------------
print("Forward Wrapper lda")
FORW.lda.aciertos<-c()
for (i in 1:30){
	print(paste0("Iteracion n°", i))
	#----Dataset generation-----
	dataset<-diagonal(100,10,2)
	#print(as.matrix(dataset))
	clases<-as.factor(unlist(dataset[dim(dataset)[2]]))
	#print(clases)
	dataset<-dataset[-dim(dataset)[2]]
	#print(dataset)
	ruido<-replicate(90,runif(100, -5, 5)) #ver bien el rango
	dataset<-cbind(dataset,ruido)

	#print(dim(dataset))
	#print(dim(clases))
	#---------------------------

	features<-forward.ranking(dataset,clases,method="lda.est" , verbosity=0)#$imp[1:10]
	FORW.lda.aciertos <- c(FORW.lda.aciertos, sum(features %in% 1:10)/10)
}

FORW.lda.resultado<-sum(FORW.lda.aciertos)/30
print(FORW.lda.resultado)
#---------------------------------------------------------------------------------


#-------------------Backward Wrapper rf------------------------------------------
print("Backward Wrapper rf")
BACKW.rf.est.aciertos<-c()
for (i in 1:30){
	print(paste0("Iteracion n°", i))
	#----Dataset generation-----
	dataset<-diagonal(100,10,2)
	#print(as.matrix(dataset))
	clases<-as.factor(unlist(dataset[dim(dataset)[2]]))
	#print(clases)
	dataset<-dataset[-dim(dataset)[2]]
	#print(dataset)
	ruido<-replicate(90,runif(100, -5, 5)) #ver bien el rango
	dataset<-cbind(dataset,ruido)

	#print(dim(dataset))
	#print(dim(clases))
	#---------------------------

	features<-backward.ranking(dataset,clases,method="rf.est" ,tot.trees=100,equalize.classes=F, verbosity=0)$ordered.features.list[1:10]
	BACKW.rf.est.aciertos <- c(BACKW.rf.est.aciertos, sum(features %in% 1:10)/10)
}
#print(BACKW.rf.est.aciertos)
#print(unlist(BACKW.rf.est.aciertos))

BACKW.rf.est.resultado<-sum(BACKW.rf.est.aciertos)/30
print(BACKW.rf.est.resultado)
#---------------------------------------------------------------------------------

#-------------------Backward Wrapper lda------------------------------------------
print("Backward Wrapper lda")
BACKW.lda.aciertos<-c()
for (i in 1:30){
	print(paste0("Iteracion n°", i))
	#----Dataset generation-----
	dataset<-diagonal(100,10,2)
	#print(as.matrix(dataset))
	clases<-as.factor(unlist(dataset[dim(dataset)[2]]))
	#print(clases)
	dataset<-dataset[-dim(dataset)[2]]
	#print(dataset)
	ruido<-replicate(90,runif(100, -5, 5)) #ver bien el rango
	dataset<-cbind(dataset,ruido)

	#print(dim(dataset))
	#print(dim(clases))
	#---------------------------

	features<-backward.ranking(dataset,clases,method="lda.est" , verbosity=0)$imp[1:10]
	BACKW.lda.aciertos <- c(BACKW.lda.aciertos, sum(features %in% 1:10)/10)
}

BACKW.lda.resultado<-sum(BACKW.lda.aciertos)/30
print(BACKW.lda.resultado)
#---------------------------------------------------------------------------------

#-------------------Filter con test no-paramétrico (Kruskal-Wallis)------------------------------------------
print("Filter con test no-paramétrico (Kruskal-Wallis)")
KWF.aciertos<-c()
for (i in 1:30){
	print(paste0("Iteracion n°", i))
	#----Dataset generation-----
	dataset<-diagonal(100,10,2)
	#print(as.matrix(dataset))
	clases<-as.factor(unlist(dataset[dim(dataset)[2]]))
	#print(clases)
	dataset<-dataset[-dim(dataset)[2]]
	#print(dataset)
	ruido<-replicate(90,runif(100, -5, 5)) #ver bien el rango
	dataset<-cbind(dataset,ruido)

	#print(dim(dataset))
	#print(dim(clases))
	#---------------------------

	features <- kwfilter(dataset,clases)[1:10]
	KWF.aciertos <- c(KWF.aciertos, sum(features %in% 1:10)/10)
}

KWF.resultado<-sum(KWF.aciertos)/30
print(KWF.resultado)
#---------------------------------------------------------------------------------

#-------------------RFE rf------------------------------------------
print("RFE rf")
RFE.rf.aciertos<-c()
for (i in 1:30){
	print(paste0("Iteracion n°", i))
	#----Dataset generation-----
	dataset<-diagonal(100,10,2)
	#print(as.matrix(dataset))
	clases<-as.factor(unlist(dataset[dim(dataset)[2]]))
	#print(clases)
	dataset<-dataset[-dim(dataset)[2]]
	#print(dataset)
	ruido<-replicate(90,runif(100, -5, 5)) #ver bien el rango
	dataset<-cbind(dataset,ruido)

	#print(dim(dataset))
	#print(dim(clases))
	#---------------------------

	features<-rfe(cbind(dataset,clases),imp.rf)[1:10]
	RFE.rf.aciertos <- c(RFE.rf.aciertos, sum(features %in% 1:10)/10)
}


RFE.rf.resultado<-sum(RFE.rf.aciertos)/30
print(RFE.rf.resultado)
#---------------------------------------------------------------------------------


#-------------------RFE linsvm------------------------------------------
print("RFE linsvm")
RFE.linsvm.aciertos<-c()
for (i in 1:30){
	print(paste0("Iteracion n°", i))
	#----Dataset generation-----
	dataset<-diagonal(100,10,2)
	#print(as.matrix(dataset))
	clases<-as.factor(unlist(dataset[dim(dataset)[2]]))
	#print(clases)
	dataset<-dataset[-dim(dataset)[2]]
	#print(dataset)
	ruido<-replicate(90,runif(100, -5, 5)) #ver bien el rango
	dataset<-cbind(dataset,ruido)

	#print(dim(dataset))
	#print(dim(clases))
	#---------------------------

	features<-rfe(cbind(dataset,clases),imp.linsvm)[1:10]
	RFE.linsvm.aciertos <- c(RFE.linsvm.aciertos, sum(features %in% 1:10)/10)
}


RFE.linsvm.resultado<-sum(RFE.linsvm.aciertos)/30
print(RFE.linsvm.resultado)
#---------------------------------------------------------------------------------