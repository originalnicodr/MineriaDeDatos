#ej3
#source('/tp1-functions.R')
#-------------------Wrapper forward------------------------------------------
print("Wrapper forward")
FORW.rf.est.aciertos<-list()
for (i in 1:30){
	print(paste0("Iteracion n°", i))
	#----Dataset generation-----
	dataset<-diagonal(100,10,2)
	#print(as.matrix(dataset))
	clases<-as.factor(unlist(dataset[dim(dataset)[2]]))
	#print(clases)
	dataset<-dataset[-dim(dataset)[2]]
	#print(dataset)
	ruido<-replicate(90,runif(100, -1, 1)) #ver bien el rango
	dataset<-cbind(dataset,ruido)

	#print(dim(dataset))
	#print(dim(clases))
	#---------------------------

	features<-forward.ranking(dataset,clases,method="rf.est" ,tot.trees=100,equalize.classes=F, verbosity=0)$imp[1:10]
	FORW.rf.est.aciertos <- append(FORW.rf.est.aciertos, sum(features[features %in% 1:10])/10)
}
#print(FORW.rf.est.aciertos)
#print(unlist(FORW.rf.est.aciertos))

FORW.rf.est.resultado<-sum(unlist(FORW.rf.est.aciertos))/2
print(FORW.rf.est.resultado)
#-----------------------------------------------------------------------------