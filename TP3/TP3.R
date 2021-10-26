#-------------------------1)a)---------------------------

#buscar 2 clusters y ver si detecta primero la especie o el sexo, dependiendo del metodo me puede dar uno u otro
#de que depende eso?
library(e1071)

library(MASS)
data(crabs)
summary(crabs)
#scaled_crabs<-prcomp(scale(log(crabs[,4:8])),retx=T)
scaled_crabs<-prcomp(scale(log(crabs[,4:8])),retx=T)

plot(scaled_crabs$x,col=as.numeric(crabs[,1]),pch=as.numeric(crabs[,2]),main = "Crabs dataset")

#En este dataset se sugiere usar una transformación logaritmica de los datos en primer lugar, y a partir de allí usar los
#datos con distintos escalados (por ejemplo, usar scale() o usar PCA -prcomp()- escalando los datos
#previamente o usar primero PCA y después escalar los datos, una vez girados).

#---------------------k-means----------------------------
cckmeans<-kmeans(scaled_crabs$x,cent=2)

    #---------------Specie-----------------------------
plot(scaled_crabs$x,pch=as.numeric(crabs[,1]),col=cckmeans$cluster,main = "Crabs dataset specie - kmeans")
#abline(v=0)
cont.table <- table(cckmeans$cluster,as.numeric(crabs[,1]))
print(cont.table)
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #----------------------------------------------------

    #---------------Sex-----------------------------
plot(scaled_crabs$x,pch=as.numeric(crabs[,2]),col=cckmeans$cluster,main = "Crabs dataset sex - kmeans")
#abline(v=0)
cont.table <- table(cckmeans$cluster,as.numeric(crabs[,2]))
print(cont.table)
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #----------------------------------------------------
#---------------------------------------------------------


#------------------h-clust-single-------------------------
cchlust<-hclust(dist(scaled_crabs$x),method="single") #"single" "average" "complete"
#plot(cchlust)

    #----------------Specie-------------------------------
plot(scaled_crabs$x,pch=as.numeric(crabs[,1]),col=cutree(cchlust,k=2),main = "Crabs dataset specie - hclust single")
#abline(v=0)
cont.table <- table(cutree(cchlust,k=2),as.numeric(crabs[,1]))
print(cont.table)
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------

    #----------------Sex-------------------------------
plot(scaled_crabs$x,pch=as.numeric(crabs[,2]),col=cutree(cchlust,k=2),main = "Crabs dataset sex - hclust single")
#abline(v=0)
cont.table <- table(cutree(cchlust,k=2),as.numeric(crabs[,2]))
print(cont.table)
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------

#------------------h-clust-average-------------------------
cchlust<-hclust(dist(scaled_crabs$x),method="average") #"single" "average" "complete"
#plot(cchlust)
    #----------------Specie-------------------------------
plot(scaled_crabs$x,pch=as.numeric(crabs[,1]),col=cutree(cchlust,k=2),main = "Crabs dataset specie - hclust average")
#abline(v=0)
cont.table <- table(cutree(cchlust,k=2),as.numeric(crabs[,1]))
print(cont.table)
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------

    #----------------Sex-------------------------------
plot(scaled_crabs$x,pch=as.numeric(crabs[,2]),col=cutree(cchlust,k=2),main = "Crabs dataset sex - hclust average")
#abline(v=0)
cont.table <- table(cutree(cchlust,k=2),as.numeric(crabs[,2]))
print(cont.table)
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------
#---------------------------------------------------------


#------------------h-clust-complete-------------------------
cchlust<-hclust(dist(scaled_crabs$x),method="complete") #"single" "average" "complete"
#plot(cchlust)


    #----------------Specie-------------------------------
plot(scaled_crabs$x,pch=as.numeric(crabs[,1]),col=cutree(cchlust,k=2),main = "Crabs dataset specie - hclust complete")
#abline(v=0)
cont.table <- table(cutree(cchlust,k=2),as.numeric(crabs[,1]))
print(cont.table)
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------

    #----------------Sex-------------------------------
plot(scaled_crabs$x,pch=as.numeric(crabs[,2]),col=cutree(cchlust,k=2),main = "Crabs dataset sex - hclust complete")
#abline(v=0)
cont.table <- table(cutree(cchlust,k=2),as.numeric(crabs[,2]))
print(cont.table)
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------
#---------------------------------------------------------



#La uniforme siempre es el peor dataset para buscar clusters
#-----------------------------b)-------------------------------

#load("lampone.Rdata")
print("test")

#prcomp(lampone[,-dim(lampone)[2]][,-143][,-1])


lampone_prc<-prcomp(lampone[,-c(dim(lampone)[2],143, 1)],retx=T)#saco las clases y una columna que decia el numero de la muestra nomas, sino me rompia todo

plot(lampone_prc$x,type = "p",col=as.numeric(lampone[,1]),pch=as.numeric(lampone[,143]),main = "Lampone dataset") 

#En este dataset se sugiere usar una transformación logaritmica de los datos en primer lugar, y a partir de allí usar los
#datos con distintos escalados (por ejemplo, usar scale() o usar PCA -prcomp()- escalando los datos
#previamente o usar primero PCA y después escalar los datos, una vez girados).



#---------------------k-means----------------------------
cckmeans<-kmeans(lampone_prc$x,cent=2)
#plot(crabs[,4:8],pch=as.numeric(crabs[,2]),col=cc$cluster)

    #----------------Year-------------------------------
plot(lampone_prc$x,pch=as.numeric(lampone[,1]),col=cckmeans$cluster,main = "Lampone dataset year - kmeans")
#abline(v=0)
cont.table <- table(cckmeans$cluster,as.numeric(lampone[,1]))
print(cont.table)
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------

    #----------------Specie-------------------------------
plot(lampone_prc$x,pch=as.numeric(lampone[,143]),col=cckmeans$cluster,main = "Lampone dataset specie - kmeans")
#abline(v=0)
cont.table <- table(cckmeans$cluster,as.numeric(lampone[,143]))
print(cont.table)
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------

#---------------------------------------------------------



#------------------h-clust-single-------------------------
cchlust<-hclust(dist(lampone_prc$x),method="single") #"single" "average" "complete"
#plot(cchlust)

    #----------------Year-------------------------------
plot(lampone_prc$x,pch=as.numeric(lampone[,1]),col=cutree(cchlust,k=2),main = "Lampone dataset year - hclust single")
abline(v=0)
cont.table <- table(cutree(cchlust,k=2),as.numeric(lampone[,1]))
print(cont.table)
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------

    #----------------Specie-------------------------------
plot(lampone_prc$x,pch=as.numeric(lampone[,143]),col=cutree(cchlust,k=2),main = "Lampone dataset specie - hclust single")
abline(v=0)
cont.table <- table(cutree(cchlust,k=2),as.numeric(lampone[,143]))
print(cont.table)
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------


#---------------------------------------------------------


#------------------h-clust-average-------------------------
cchlust<-hclust(dist(lampone_prc$x),method="average") #"single" "average" "complete"
#plot(cchlust)
    #----------------Year-------------------------------
plot(lampone_prc$x,pch=as.numeric(lampone[,1]),col=cutree(cchlust,k=2),main = "Lampone dataset year - hclust average")
abline(v=0)
cont.table <- table(cutree(cchlust,k=2),as.numeric(lampone[,1]))
print(cont.table)
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------

    #----------------Specie-------------------------------
plot(lampone_prc$x,pch=as.numeric(lampone[,143]),col=cutree(cchlust,k=2),main = "Lampone dataset specie - hclust average")
abline(v=0)
cont.table <- table(cutree(cchlust,k=2),as.numeric(lampone[,143]))
print(cont.table)
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------
#---------------------------------------------------------


#------------------h-clust-complete-------------------------
cchlust<-hclust(dist(lampone_prc$x),method="complete") #"single" "average" "complete"
#plot(cchlust)
    #----------------Year-------------------------------
plot(lampone_prc$x,pch=as.numeric(lampone[,1]),col=cutree(cchlust,k=2),main = "Lampone dataset year - hclust complete")
abline(v=0)
cont.table <- table(cutree(cchlust,k=2),as.numeric(lampone[,1]))
print(cont.table)
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------

    #----------------Specie-------------------------------
plot(lampone_prc$x,pch=as.numeric(lampone[,143]),col=cutree(cchlust,k=2),main = "Lampone dataset specie - hclust complete")
abline(v=0)
cont.table <- table(cutree(cchlust,k=2),as.numeric(lampone[,143]))
print(cont.table)
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------
#---------------------------------------------------------



#Fausto

plotKmeans <- function(data, k = 2) {
    cc <- kmeans(data, cent = k)
    plot(data, col=cc$cluster, pch=as.numeric(crabs[,2]),main = "test")#cambiar por [,1] para compara con genero

    cont.table <- table(cc$cluster,as.numeric(crabs[,2]))
    print(cont.table)

    class.match <- matchClasses(as.matrix(cont.table),method="exact")
    # Print the confusion table, with rows permuted to maximize the diagonal
    print(cont.table[,class.match])
}

#plotKmeans(scaled_crabs$x,2)