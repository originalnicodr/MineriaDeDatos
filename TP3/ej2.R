#2)

reference_dataset_a <- function(dataset){
    returnset<-
    for(i in 1:dim(dataset)[2]){
        min<-min(dataset[,i])
        max<-max(dataset[,i])

        ruido<-runif(dataset[1], min, max) #cuantos puntos tengo que generar?
        returnset<-cbind(returnset,ruido)
    }
}

weight_sum <- function(datos,k){
    return(5)#ver
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