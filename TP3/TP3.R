#1)a)

#buscar 2 clusters y ver si detecta primero la especie o el sexo, dependiendo del metodo me puede dar uno u otro
#de que depende eso?

library(MASS)
data(crabs)
summary(crabs)
plot(scale(crabs[,4:8]),col=as.numeric(crabs[,1]),pch=as.numeric(crabs[,2]))

#En este dataset se sugiere usar una transformación logaritmica de los datos en primer lugar, y a partir de allí usar los
#datos con distintos escalados (por ejemplo, usar scale() o usar PCA -prcomp()- escalando los datos
#previamente o usar primero PCA y después escalar los datos, una vez girados).

#aplico k-means
cckmeans<-kmeans(scale(crabs[,4:8]),cent=2)
#plot(crabs[,4:8],pch=as.numeric(crabs[,2]),col=cc$cluster)
plot(scale(crabs[,4]),pch=as.numeric(crabs[,2]),col=cckmeans$cluster)
abline(v=0)


cchlust<-hclust(dist(crabs[,4:8]),method="average") #"single" "average" "complete"
#controlar el dendograma
plot(cchlust)
#ver el clustering
plot(crabs[,8],pch=as.numeric(crabs[,2]),col=cutree(cchlust,k=2))

#hago una tabla de confusion para comparar
cont.table <- table(cckmeans$cluster, cutree(cchlust,k=2))
print(cont.table)

library(e1071)
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])


#La uniforme siempre es el peor dataset para buscar clusters
#b)