#2)

reference_dataset_a <- function(dataset){
    returnset<-matrix(, nrow = dim(dataset)[1], ncol = 0)
    for(i in 1:dim(dataset)[2]){
        min<-min(dataset[,i])
        max<-max(dataset[,i])

        ruido<-runif(dim(dataset)[1], min, max) #cuantos puntos tengo que generar?
        returnset<-cbind(returnset,ruido)
    }
    return(returnset)
}

#Ver, por que deberia estar volviendo del PCA a los ejes originales. Ademas deberia saber de antemano que el dataset no se le aplico PCA?
reference_dataset_b <- function(dataset){
    r<-reference_dataset_a(prcomp(dataset)$x)
    return(returnset)
}

#tengo que aplicar scale antes de hacer esto?
#sumo la distancia de cada fila con el vector promedio
weight_sum <- function(datos,k){
    m<-c()
    for(i in 1:dim(datos)[2]){
        m<-c(m,sum(datos[,i])/dim(datos)[1])
    }
    r<-0
    for(i in 1:dim(datos)[1]){
        r<-r+dist(rbind(datos[i,],m))
    }
    return(r)
}

GAP <- function(data, K, B){
    uniform_weights<-matrix(, nrow = B, ncol = K)
    gap_results<-c()
    s_list<-c()
    for(k in 1:K){

        weights<-weight_sum(data,k)

        for(b in 1:B){
            uniform_weights[b,k]<- weight_sum(reference_dataset_a(data),k)#tengo que guardar los B conjuntos de datos para otras corridas de k o los puedo generar devuelta?
        }


        #r<-0#donde guardo el resultado de la sumatoria del punto 2
        #for(b in 1:B){
        #    r<-r+log(uniform_weights[b][k])-log(weights[k])
        #}
        r<-sum(log(uniform_weights[,k]))/B-log(weights)

        gap_results<-c(gap_results,r)

        l<-sum(log(uniform_weights[,k]))/B
        sd<-sqrt(sum((log(uniform_weights[,k])-l)^2)/B)
        s_list<-c(s_list,sd*sqrt(1+1/B))
    }
    #print(s_list)
    #print(gap_results)
    #print(s_list)
    print(gap_results)
    print(s_list)
    for (k in 1:(K-1)){
        if(gap_results[k]>=gap_results[k+1]-s_list[k+1]){
            return(k)
        }
    }
    print("No deberia llegar aca")
    return(0)
}

#revisar que este dando bien
#GAP(iris[-5],2,30)


#Entre mas alto el numero, mas similares son los dos conjuntos de datos.
jaccard <- function(clusters_dataset1, clusters_dataset2) {
    intersection = length(intersect(clusters_dataset1, clusters_dataset2))
    union = length(clusters_dataset1) + length(clusters_dataset2) - intersection
    return (intersection/union)
}


#Revisar lo que dice el pdf del tp sobre la estabildiad
stability <- function(dataset,K,B){
    #VEEEEEEEEEEEEEEEEEEER ESTO
    max_noise<-var(dataset[,1])*0.02 #esta bien? Si esta bien.
    
    k_results<-c()
    for(k in 2:K){#si arrancara con 1 siempre daria como resultado 1

        r<-0
        for(b in 1:B){

            dataset_perturbado<-jitter(as.matrix(dataset), amount = max_noise) #amount es +-el valor

            #----k-means------
            #clusters_dataset<-kmeans(dataset,cent=k)$cluster
            #clusters_dataset_perturbado<-kmeans(dataset_perturbado,cent=k)$cluster
            #-----------------

            #----hclust-single------
            #clusters_dataset<-cutree(hclust(dist(dataset),method="single"),k=k) #"single" "average" "complete"
            #clusters_dataset_perturbado<-cutree(hclust(dist(dataset_perturbado),method="single"),k=k)
            #-----------------

            #----hclust-avarage------
            #clusters_dataset<-cutree(hclust(dist(dataset),method="average"),k=k) #"single" "average" "complete"
            #clusters_dataset_perturbado<-cutree(hclust(dist(dataset_perturbado),method="average"),k=k)
            #------------------------

            #----hclust-complete------
            clusters_dataset<-cutree(hclust(dist(dataset),method="complete"),k=k) #"single" "average" "complete"
            clusters_dataset_perturbado<-cutree(hclust(dist(dataset_perturbado),method="complete"),k=k)
            #-----------------

            v1<-as.matrix(clusters_dataset)
            v2<-as.matrix(clusters_dataset_perturbado)
            #v1[ind1]<-cc1
            #v2[ind2]<-cc2
            #creo una matriz m con 1 donde los dos puntos estan en el mismo cluster, -1 en distinto cluster y 0 si alguno no esta, para cada clustering
            a<-sqrt(v1%*%t(v1))
            m1<-a / -a + 2*(a==round(a))
            m1[is.nan(m1)]<-0
            a<-sqrt(v2%*%t(v2))
            m2<-a / -a + 2*(a==round(a))
            m2[is.nan(m2)]<-0
            #calculo el score, los pares de puntos que estan en la misma situacion en los dos clustering dividido el total de pares validos.
            validos<-sum(v1*v2>0)
            score<-sum((m1*m2)[upper.tri(m1)]>0)/(validos*(validos-1)/2)
            #print(score)


            r<-r+score#jaccard(clusters_dataset,clusters_dataset_perturbado)

        }
        k_results<-c(k_results,r/B)
    }
    print(k_results)
    return(which.max(k_results)+1)#sumo 1 por que la lista de k_results arranca desde k=2
}





