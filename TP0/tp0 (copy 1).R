diagonal <- function(n,d,c){
    a1<-t(replicate(n/2,rnorm(d,1,c*sqrt(d))))
    a0<-t(replicate(n/2,rnorm(d,-1,c*sqrt(d))))
    a1<-cbind(a1,t(t(rep(1,n/2))))
    a0<-cbind(a0,t(t(rep(-1,n/2))))
    return(as.data.frame(rbind(a1,a0)))
}

l1 <- function(theta)return((theta+pi)/(4*pi))
l2 <- function(theta)return((theta)/(4*pi))

identificar_espiral <- function(t,r){
    if (t > 0) {
        theta <- t%%(2*pi)
    } else {
        theta <- (2*pi + t)
    }
    #print(theta)

    if ((l2(theta)<= r) && (r <=l1(theta))){
        return(0)
    }
    else if ((l1(theta) <= r) && (r <= l2(theta + 2*pi))){
        return(1)
    }
    else if ((l2(theta + 2*pi) <= r) && (r <= l1(theta + 2*pi))){
        return(0)
    }
    else if (l1(theta + 2*pi) <= r){
        return(1)
    }
    else if (pi<=theta && r <= l1(theta - 2*pi)){
        return(0)
    }
    else if (r <= l2(theta)){
        return(1)
    }
    else return(2)
}

espirales <- function(n){
    cord<-t(replicate(n,runif(2,-1,1)))
    r <- t(t(sqrt(cord[,1]^2+cord[,2]^2)))

    theta <-t(t(atan2(cord[,2],cord[,1])))
    clase <- t(t(mapply(identificar_espiral,theta,r)))

    #print(cord)
    #print(r)
    #print(theta)
    #print(clase) #print(t_matrix[,5])

    t_matrix <- cbind(cbind(cbind(cord,r),theta),clase)
    t_matrix <- t_matrix[order(t_matrix[,5]),]

    #print('t_matrix pre filtrado')
    #print(t_matrix)

    #t_matrix=[x,y,r,theta,clase]

    while(length(t_matrix[t_matrix>1]) != 0 || sum(t_matrix[,5]==1)!=n/2) {

        if (dim(t_matrix)[1] > n) break
        
        #print('t_matrix pre filtrado')
        #print(t_matrix)

        #----------Saco puntos fuera del circulo y agrego nuevos----------
        print('t_matrix pre filtrado 1')
        print(t_matrix)

        print(t_matrix[t_matrix[,3]>1,])

        m<-dim(t_matrix[t_matrix[,3]>1,])[1]
        
        t_matrix<-t_matrix[t_matrix[,3]<=1,]

        cord<-t(replicate(m,runif(2,-1,1)))
        r <- t(t(sqrt(cord[,1]^2+cord[,2]^2)))

        theta <-t(t(atan2(cord[,2],cord[,1])))
        clase <- t(t(mapply(identificar_espiral,theta,r)))

        nueva_t_matrix <- cbind(cbind(cbind(cord,r),theta),clase)

        t_matrix <- rbind(t_matrix,nueva_t_matrix)
        t_matrix <- t_matrix[order(t_matrix[,5]),]
        #------------------------------------------------
        print('t_matrix post filtrado 1')
        print(t_matrix)



        #----------Saco puntos de clases ya completas y agrego nuevos----------
        m<-0
        if (sum(t_matrix==0)>n/2){
            print('muchos ceros')
            m <-sum(t_matrix==0) - n/2  #por que el primer indice es 1
            t_matrix<-t_matrix[(m+1):dim(t_matrix)[1],]
        } else if (sum(t_matrix==1)>n/2){
            print('muchos unos')
            m <-sum(t_matrix==1) - n/2 + 1 #por que el primer indice es 1
            t_matrix<-t_matrix[1:(dim(t_matrix)[1]-m),]
        }


        cord<-t(replicate(m,runif(2,-1,1)))
        r <- t(t(sqrt(cord[,1]^2+cord[,2]^2)))

        theta <-t(t(atan2(cord[,2],cord[,1])))
        clase <- t(t(mapply(identificar_espiral,theta,r)))

        nueva_t_matrix <- cbind(cbind(cbind(cord,r),theta),clase)

        t_matrix <- rbind(t_matrix,nueva_t_matrix)
        t_matrix <- t_matrix[order(t_matrix[,5]),]
        #------------------------------------------------

        print(t_matrix)
    }


    #theta <-atan2(cord[,2],cord[,1])
    #clase <- mapply(identificar_espiral,theta,r)
    #return(cbind(theta,r))
    return(as.data.frame(t_matrix))
}

#plot(test$V1, test$V2, main="Espirales", xlab="X", ylab="Y", col=ifelse(test$clase==1, "blue","red"))