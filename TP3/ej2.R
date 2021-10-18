#2)

reference_dataset_a <- function(dataset){
    returnset<-
    for(i in 1:dim(dataset)[2]){
        min<-min(dataset[,i])
        max<-max(dataset[,i])

        ruido<-runif(dim(dataset)[1], min, max) #cuantos puntos tengo que generar?
        returnset<-cbind(returnset,ruido)
    }
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
    weights<-c()
    uniform_weights<-matrix(, nrow = B, ncol = K)
    gap_results<-c()
    s_list<-c()
    for(k in 1:K){

        weights<-c(weights,weight_sum(datos,k)) #agrego el calculo de Wk y lo guardo en weights

        for(b in 1:B){
            uniform_weights[b,k]<- weight_sum(reference_dataset_a(data),k)#tengo que guardar los B conjuntos de datos para otras corridas de k o los puedo generar devuelta?
        }


        #r<-0#donde guardo el resultado de la sumatoria del punto 2
        #for(b in 1:B){
        #    r<-r+log(uniform_weights[b][k])-log(weights[k])
        #}
        r<-sum(log(uniform_weights[,k])-log(weights[k]))/B

        gap_results<-c(gap_results,r)

        l<-sum(log(uniform_weights[,k]))/B
        sd<-sqrt(sum((log(uniform_weights[,k])-l)^2)/B)
        s_list<-c(s_list,sd*sqrt(1+1/B))
    }
    #print(s_list)
    #print(gap_results)
    #print(s_list)
    for (k in 1:(K-1)){
        if(gap_results[k]>=gap_results[k+1]-s_list[k+1]){
            return(k)
        }
    }
    print("No deberia llegar aca")
    return(0)
}


#Entre mas alto el numero, mas similares son los dos conjuntos de datos.
jaccard <- function(clusters_dataset1, clusters_dataset2) {
    intersection = length(intersect(clusters_dataset1, clusters_dataset2))
    union = length(clusters_dataset1) + length(clusters_dataset2) - intersection
    return (intersection/union)
}


#Stability

stability <- function(dataset,K,B){
    scaled_dataset<-dataset
    max_noise<-var(scaled_dataset[,1])*0.02 #esta bien?
    
    k_results<-c()
    for(k in 1:K){

        r<-0
        for(b in 1:B){

        dataset_perturbado<-jitter(scaled_dataset, amount = max_noise) #amount es +-el valor
        
        #obtener lista de clusters de cada punto y aplicar
        clusters_dataset<-#aplicar un metodo al scaled_dataset
        clusters_dataset_perturbado<-#aplicar un metodo al dataset_perturbado

        r<-r+jaccard(clusters_dataset,clusters_dataset_perturbado)

        }
        k_results<-c(k_results,r<-r/B)
    }
    print(k_results)
    return(which.max(k_results))
}