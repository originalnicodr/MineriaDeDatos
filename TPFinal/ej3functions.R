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


run.kmeans <- function(data, k) {
  cc <- kmeans(data,cent=k,nsta=10)
  return (cc$cluster)
}

run.hclust <- function(data, kdado, meth) {
  return (cutree(hclust(dist(data),method=meth), k = kdado))
}

estabilidad <- function(kmax, N, data, method, ...) {
  scores <- matrix(0.0,kmax,N)
  
  for (k in 2:kmax) {
    prev <- method(data,k, ...)
    #print(prev)
    for (i in 1:N) {
      noise.data <- apply(data,2,jitter)
      act <- method(noise.data,k, ...)
      scores[k,i] <- similitud(prev,act)
      prev <- act
    }
  }
  
  # plot cummulative scores
  plot(NULL, main='Scores acumulativos', xlim = c(0.6, 1), ylim = c(0, 1), xlab = 'similitudes', ylab = 'acumulativo')
  legend('topleft', legend = paste('k =', 2:kmax), lty = 1, cex = .8, col = rainbow(kmax-1))
  for (i in 2:kmax) {
    x <- scores[i,]
    lines( sort(x), (1:length(x))/length(x), type="b", pch=20, col=rainbow(9)[i])
  }
  
  return(scores)
  
}

similitud <- function(cc1, cc2) {
  v1<-as.matrix(cc1)
  v2<-as.matrix(cc2)
  #creo una matriz m con 1 donde los dos puntos estan en el mismo cluster, -1 en distinto cluster y 0 si alguno no esta, para cada clustering
  a<-sqrt(v1%*%t(v1))
  m1<-a / -a + 2*(a==round(a))
  a<-sqrt(v2%*%t(v2))
  m2<-a / -a + 2*(a==round(a))
  #calculo el score, los pares de puntos que estan en la misma situacion en los dos clustering dividido el total de pares validos.
  validos<-sum(v1*v2>0)
  score<-sum((m1*m2)[upper.tri(m1)]>0)/(validos*(validos-1)/2)
  return(score)  
}