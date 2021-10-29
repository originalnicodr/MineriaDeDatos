#-------------------------1)a)---------------------------

#buscar 2 clusters y ver si detecta primero la especie o el sexo, dependiendo del metodo me puede dar uno u otro
#de que depende eso?
library(e1071)

library(MASS)
data(crabs)
summary(crabs)
#scaled_crabs<-prcomp(scale(log(crabs[,4:8])),retx=T)
scaled_crabs<-scale(prcomp(log(crabs[,4:8]),retx=T)$x)

plot(scaled_crabs,col=as.numeric(crabs[,1]),pch=as.numeric(crabs[,2]),main = "Crabs dataset")

#En este dataset se sugiere usar una transformación logaritmica de los datos en primer lugar, y a partir de allí usar los
#datos con distintos escalados (por ejemplo, usar scale() o usar PCA -prcomp()- escalando los datos
#previamente o usar primero PCA y después escalar los datos, una vez girados).



#---------------------k-means----------------------------
cckmeans<-kmeans(scaled_crabs,cent=2)
plot(scaled_crabs, col=cckmeans$cluster, pch=as.numeric(crabs[,1]))

    #---------------Specie-----------------------------
plot(scaled_crabs,pch=as.numeric(crabs[,1]),col=cckmeans$cluster,main = "Crabs dataset specie - kmeans")
#abline(v=0)
cont.table <- table(cckmeans$cluster,as.numeric(crabs[,1]))
print("Crabs k-means Species")
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #----------------------------------------------------

    #---------------Sex-----------------------------
plot(scaled_crabs,pch=as.numeric(crabs[,2]),col=cckmeans$cluster,main = "Crabs dataset sex - kmeans")
#abline(v=0)
cont.table <- table(cckmeans$cluster,as.numeric(crabs[,2]))
print("Crabs k-means Sex")
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #----------------------------------------------------
#---------------------------------------------------------


#------------------h-clust-single-------------------------
cchlust<-hclust(dist(scaled_crabs),method="single") #"single" "average" "complete"
#plot(cchlust)

    #----------------Specie-------------------------------
plot(scaled_crabs,pch=as.numeric(crabs[,1]),col=cutree(cchlust,k=2),main = "Crabs dataset specie - hclust single")
#abline(v=0)
cont.table <- table(cutree(cchlust,k=2),as.numeric(crabs[,1]))
print("Crabs h-clust single Species")
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------

    #----------------Sex-------------------------------
plot(scaled_crabs,pch=as.numeric(crabs[,2]),col=cutree(cchlust,k=2),main = "Crabs dataset sex - hclust single")
#abline(v=0)
cont.table <- table(cutree(cchlust,k=2),as.numeric(crabs[,2]))
print("Crabs h-clust single Sex")
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------

#------------------h-clust-average-------------------------
cchlust<-hclust(dist(scaled_crabs),method="average") #"single" "average" "complete"
#plot(cchlust)
    #----------------Specie-------------------------------
plot(scaled_crabs,pch=as.numeric(crabs[,1]),col=cutree(cchlust,k=2),main = "Crabs dataset specie - hclust average")
#abline(v=0)
cont.table <- table(cutree(cchlust,k=2),as.numeric(crabs[,1]))
print("Crabs h-clust average Species")
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------

    #----------------Sex-------------------------------
plot(scaled_crabs,pch=as.numeric(crabs[,2]),col=cutree(cchlust,k=2),main = "Crabs dataset sex - hclust average")
#abline(v=0)
cont.table <- table(cutree(cchlust,k=2),as.numeric(crabs[,2]))
print("Crabs h-clust average Sex")
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------
#---------------------------------------------------------


#------------------h-clust-complete-------------------------
cchlust<-hclust(dist(scaled_crabs),method="complete") #"single" "average" "complete"
#plot(cchlust)


    #----------------Specie-------------------------------
plot(scaled_crabs,pch=as.numeric(crabs[,1]),col=cutree(cchlust,k=2),main = "Crabs dataset specie - hclust complete")
#abline(v=0)
cont.table <- table(cutree(cchlust,k=2),as.numeric(crabs[,1]))
print("Crabs h-clust complete Specie")
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------

    #----------------Sex-------------------------------
plot(scaled_crabs,pch=as.numeric(crabs[,2]),col=cutree(cchlust,k=2),main = "Crabs dataset sex - hclust complete")
#abline(v=0)
cont.table <- table(cutree(cchlust,k=2),as.numeric(crabs[,2]))
print("Crabs h-clust complete Sex")
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


#lampone_prc<-prcomp(lampone[,-c(dim(lampone)[2],143, 1)],retx=T)#saco las clases y una columna que decia el numero de la muestra nomas, sino me rompia todo

lampone_prc<-scale(prcomp(lampone[,-c(dim(lampone)[2],143, 1)],retx=T)$x)#saco las clases y una columna que decia el numero de la muestra nomas, sino me rompia todo


plot(lampone_prc,type = "p",col=as.numeric(lampone[,1]),pch=as.numeric(lampone[,143]),main = "Lampone dataset") 

#En este dataset se sugiere usar una transformación logaritmica de los datos en primer lugar, y a partir de allí usar los
#datos con distintos escalados (por ejemplo, usar scale() o usar PCA -prcomp()- escalando los datos
#previamente o usar primero PCA y después escalar los datos, una vez girados).



#---------------------k-means----------------------------
cckmeans<-kmeans(lampone_prc,cent=2)
#plot(crabs[,4:8],pch=as.numeric(crabs[,2]),col=cc$cluster)

    #----------------Year-------------------------------
plot(lampone_prc,pch=as.numeric(lampone[,1]),col=cckmeans$cluster,main = "Lampone dataset year - kmeans")
#abline(v=0)
cont.table <- table(cckmeans$cluster,as.numeric(lampone[,1]))
print("Lampone k-means Year")
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------

    #----------------Specie-------------------------------
plot(lampone_prc,pch=as.numeric(lampone[,143]),col=cckmeans$cluster,main = "Lampone dataset specie - kmeans")
#abline(v=0)
cont.table <- table(cckmeans$cluster,as.numeric(lampone[,143]))
print("Lampone k-means Specie")
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------

#---------------------------------------------------------



#------------------h-clust-single-------------------------
cchlust<-hclust(dist(lampone_prc),method="single") #"single" "average" "complete"
#plot(cchlust)

    #----------------Year-------------------------------
plot(lampone_prc,pch=as.numeric(lampone[,1]),col=cutree(cchlust,k=2),main = "Lampone dataset year - hclust single")
cont.table <- table(cutree(cchlust,k=2),as.numeric(lampone[,1]))
print("Lampone h-clust single Year")
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------

    #----------------Specie-------------------------------
plot(lampone_prc,pch=as.numeric(lampone[,143]),col=cutree(cchlust,k=2),main = "Lampone dataset specie - hclust single")
cont.table <- table(cutree(cchlust,k=2),as.numeric(lampone[,143]))
print("Lampone h-clust single Specie")
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------


#---------------------------------------------------------


#------------------h-clust-average-------------------------
cchlust<-hclust(dist(lampone_prc),method="average") #"single" "average" "complete"
#plot(cchlust)
    #----------------Year-------------------------------
plot(lampone_prc,pch=as.numeric(lampone[,1]),col=cutree(cchlust,k=2),main = "Lampone dataset year - hclust average")
cont.table <- table(cutree(cchlust,k=2),as.numeric(lampone[,1]))
print("Lampone h-clust average Year")
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------

    #----------------Specie-------------------------------
plot(lampone_prc,pch=as.numeric(lampone[,143]),col=cutree(cchlust,k=2),main = "Lampone dataset specie - hclust average")
cont.table <- table(cutree(cchlust,k=2),as.numeric(lampone[,143]))
print("Lampone h-clust average Specie")
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------
#---------------------------------------------------------


#------------------h-clust-complete-------------------------
cchlust<-hclust(dist(lampone_prc),method="complete") #"single" "average" "complete"
#plot(cchlust)
    #----------------Year-------------------------------
plot(lampone_prc,pch=as.numeric(lampone[,1]),col=cutree(cchlust,k=2),main = "Lampone dataset year - hclust complete")
cont.table <- table(cutree(cchlust,k=2),as.numeric(lampone[,1]))
print("Lampone h-clust complete Year")
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------

    #----------------Specie-------------------------------
plot(lampone_prc,pch=as.numeric(lampone[,143]),col=cutree(cchlust,k=2),main = "Lampone dataset specie - hclust complete")
cont.table <- table(cutree(cchlust,k=2),as.numeric(lampone[,143]))
print("Lampone h-clust complete Specie")
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------
#---------------------------------------------------------