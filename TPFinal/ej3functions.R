#2)

reference_dataset_a <- function(dataset){
    returnset<-matrix(, nrow = dim(dataset)[1], ncol = 0)
    for(i in 1:dim(dataset)[2]){
        min<-min(dataset[,i])
        max<-max(dataset[,i])

        ruido<-runif(dim(dataset)[1], min, max)
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
    #m<-c()
    #for(i in 1:dim(datos)[2]){
    #    m<-c(m,sum(datos[,i])/dim(datos)[1])
    #}
    #r<-0
    #for(i in 1:dim(datos)[1]){
    #    r<-r+dist(rbind(datos[i,],m))
    #}
    #return(r)
    #return(sum(kmeans(data,nclusters,nsta=10)$withinss))
    #print(datos)
    return(sum(kmeans(datos,k)$withinss))
}

GAP <- function(data, K, B){
    uniform_weights<-matrix(, nrow = B, ncol = K)
    gap_results<-c()
    s_list<-c()
    for(k in 1:K){
        weights<-weight_sum(data,k)
        
        for(b in 1:B){
            uniform_weights[b,k]<- log(weight_sum(reference_dataset_a(data),k))
        }


        #r<-0#donde guardo el resultado de la sumatoria del punto 2
        #for(b in 1:B){
        #    r<-r+log(uniform_weights[b][k])-log(weights[k])
        #}
        l<-mean(uniform_weights[,k])#sum(log(uniform_weights[,k]))/B

        r<-l-log(weights)

        gap_results<-c(gap_results,r)

        
        sd<-sd(uniform_weights[,k])#sqrt(sum((log(uniform_weights[,k])-l)^2)/B)
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

#stability sin graficar
stability <- function(dataset,K,B){
    max_noise<-var(dataset[,1])*0.02
    
    k_results<-c()
    for(k in 2:K){#si arrancara con 1 siempre daria como resultado 1

        r<-0
        for(b in 1:B){

            dataset_perturbado1<-jitter(as.matrix(dataset), amount = max_noise) #amount es +-el valor
            dataset_perturbado2<-jitter(as.matrix(dataset), amount = max_noise)

            #----k-means------
            #clusters_dataset_perturbado1<-kmeans(dataset_perturbado1,cent=k)$cluster
            #clusters_dataset_perturbado2<-kmeans(dataset_perturbado2,cent=k)$cluster
            #-----------------

            #----hclust-single------
            #clusters_dataset_perturbado1<-cutree(hclust(dist(dataset_perturbado1),method="single"),k=k) #"single" "average" "complete"
            #clusters_dataset_perturbado2<-cutree(hclust(dist(dataset_perturbado2),method="single"),k=k)
            #-----------------

            #----hclust-average------
            #clusters_dataset_perturbado1<-cutree(hclust(dist(dataset_perturbado1),method="average"),k=k) #"single" "average" "complete"
            #clusters_dataset_perturbado2<-cutree(hclust(dist(dataset_perturbado2),method="average"),k=k)
            #------------------------

            #----hclust-complete------
            clusters_dataset_perturbado1<-cutree(hclust(dist(dataset_perturbado1),method="complete"),k=k) #"single" "average" "complete"
            clusters_dataset_perturbado2<-cutree(hclust(dist(dataset_perturbado2),method="complete"),k=k)
            #-----------------

            v1<-as.matrix(clusters_dataset_perturbado1)
            v2<-as.matrix(clusters_dataset_perturbado2)
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


            r<-r+score

        }
        k_results<-c(k_results,r/B)
    }
    print(k_results)
    return(which.max(k_results)+1)#sumo 1 por que la lista de k_results arranca desde k=2
}


stability_new <- function(dataset,K,B){
    max_noise<-var(dataset[,1])*0.02
    
    k_results<-c()
    for(k in 2:K){#si arrancara con 1 siempre daria como resultado 1

        r<-c()
        for(b in 1:B){

            dataset_perturbado1<-jitter(as.matrix(dataset), amount = max_noise) #amount es +-el valor
            dataset_perturbado2<-jitter(as.matrix(dataset), amount = max_noise)

            #----k-means------
            clusters_dataset_perturbado1<-kmeans(dataset_perturbado1,cent=k)$cluster
            clusters_dataset_perturbado2<-kmeans(dataset_perturbado2,cent=k)$cluster
            #-----------------

            #----hclust-single------
            #clusters_dataset_perturbado1<-cutree(hclust(dist(dataset_perturbado1),method="single"),k=k) #"single" "average" "complete"
            #clusters_dataset_perturbado2<-cutree(hclust(dist(dataset_perturbado2),method="single"),k=k)
            #-----------------

            #----hclust-average------
            #clusters_dataset_perturbado1<-cutree(hclust(dist(dataset_perturbado1),method="average"),k=k) #"single" "average" "complete"
            #clusters_dataset_perturbado2<-cutree(hclust(dist(dataset_perturbado2),method="average"),k=k)
            #------------------------

            #----hclust-complete------
            #clusters_dataset_perturbado1<-cutree(hclust(dist(dataset_perturbado1),method="complete"),k=k) #"single" "average" "complete"
            #clusters_dataset_perturbado2<-cutree(hclust(dist(dataset_perturbado2),method="complete"),k=k)
            #-----------------

            v1<-as.matrix(clusters_dataset_perturbado1)
            v2<-as.matrix(clusters_dataset_perturbado2)
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

            #r<-r+score
            r<-c(r,score)

        }
        k_results<-cbind(k_results,r)
    }
    #print(k_results)


    #print(k_results)
    #x11()
    plot(NULL, xlim = c(.75, 1), ylim = c(0, 1), xlab = 'similarity', ylab = 'cummulative')

    n_candidates<-dim(k_results)[2]

    legend('topleft', legend = paste('k =', 2:n_candidates), lty = 1, cex = .8, col = 2:n_candidates)
    for (i in 2:n_candidates) {
        x <- k_results[,i]
        lines( sort(x), (1:length(x))/length(x), type="l", col=i)
    }
    #Ademas de graficar la similitud, devolvemos un resultado
    return(which.max(colMeans(k_results)+1))#sumo 1 por que la lista de k_results arranca desde k=2
}



