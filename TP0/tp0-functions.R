diagonal <- function(n,d,c){
    a1<-t(replicate(n/2,rnorm(d,1,c*sqrt(d))))
    a0<-t(replicate(n/2,rnorm(d,-1,c*sqrt(d))))
    a1<-cbind(a1,t(t(rep(1,n/2))))
    a0<-cbind(a0,t(t(rep(0,n/2))))
    return(as.data.frame(rbind(a0,a1)))
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

how_many_rows <- function(M){
    r <- dim(M)[1]
    if (is.null(r)) r <- 0
    return(r)
}

espirales <- function(n){
    cord<-t(replicate(n,runif(2,-1,1)))
    r <- t(t(sqrt(cord[,1]^2+cord[,2]^2)))

    theta <-t(t(atan2(cord[,2],cord[,1])))
    clase <- t(t(mapply(identificar_espiral,theta,r)))

    t_matrix <- cbind(cbind(cbind(cord,r),theta),clase)
    t_matrix <- t_matrix[order(t_matrix[,5]),]

    while(sum(t_matrix[,3]>1) != 0 || sum(t_matrix[,5]==1)!=n/2) {

        #----------Saco puntos fuera del circulo y agrego nuevos----------
        m<-sum(t_matrix[,3]>1)


        if (m!=0){

            t_matrix<-t_matrix[t_matrix[,3]<=1,]

            cord<-t(replicate(m,runif(2,-1,1)))
            r <- t(t(sqrt(cord[,1]^2+cord[,2]^2)))

            theta <-t(t(atan2(cord[,2],cord[,1])))
            clase <- t(t(mapply(identificar_espiral,theta,r)))

            nueva_t_matrix <- cbind(cbind(cbind(cord,r),theta),clase)

            t_matrix <- rbind(t_matrix,nueva_t_matrix)
            t_matrix <- t_matrix[order(t_matrix[,5]),]
            #------------------------------------------------
        }


        #----------Saco puntos de clases ya completas y agrego nuevos----------
        m<-0
        if (sum(t_matrix[,5]==0)>n/2){
            #print('muchos ceros')
            m <-sum(t_matrix[,5]==0) - n/2  #por que el primer indice es 1
            t_matrix<-t_matrix[(m+1):how_many_rows(t_matrix),]
        } else if (sum(t_matrix[,5]==1)>n/2){
            #print('muchos unos')
            m <-sum(t_matrix[,5]==1) - n/2 + 1 #por que el primer indice es 1
            t_matrix<-t_matrix[1:(how_many_rows(t_matrix)-m),]
        }

        if (m!=0){
            cord<-t(replicate(m,runif(2,-1,1)))
            r <- t(t(sqrt(cord[,1]^2+cord[,2]^2)))

            theta <-t(t(atan2(cord[,2],cord[,1])))
            clase <- t(t(mapply(identificar_espiral,theta,r)))

            nueva_t_matrix <- cbind(cbind(cbind(cord,r),theta),clase)

            t_matrix <- rbind(t_matrix,nueva_t_matrix)
            t_matrix <- t_matrix[order(t_matrix[,5]),]
        }
        #------------------------------------------------

        #Debug
        #print('Cantidad puntos fuera del circulo')
        #print(dim(c(t_matrix[t_matrix[,3]>1,]))[1])
        #print('Porcentaje clase 0')
        #print(sum(t_matrix[,5]==0))
        #print('Porcentaje clase 1')
        #print(sum(t_matrix[,5]==1))
    }
    #print(t_matrix)

    #t_matrix = subset(as.data.frame(t_matrix,col.names = c(format(1:4),"Clases")), select = -c(V3,V4)) #saco columnas que no me importan
    t_matrix = subset(as.data.frame(t_matrix), select = -c(V3,V4)) #saco columnas que no me importan
    return(t_matrix)
}

library(rpart)

cross_validation <- function(m,k){
    class_index<-dim(m)[2]
    clases<-unique(m[,3])

    folders_errors<-{}
    for (n in 1:k){

        train_folder<-{}
        test_folder<-{}
        for (c in clases){ 
            c_p<- sum(m[,3]==c)/length(m[,3])
            c_cant<-c_p*length(m[,3])/k

            t_train_m<-m[m[,3]==c,]
            #t_test_m<-m[m[,3]!=c,]
            #print("index")
            #print((c_cant*(n-1)+1):(c_cant*n))


            indexes_train<- (c_cant*(n-1)+1):(c_cant*n)

            indexes_test<-setdiff(1:length(t_train_m[,3]),indexes_train)

            t_test_m<-t_train_m[indexes_test,]

            

            t_train_m<-t_train_m[indexes_train,]
            #print("t_m")
            #print(t_m)
            train_folder<-rbind(train_folder,t_train_m)
            test_folder<-rbind(test_folder,t_test_m)
        }

        #trees
        train_folder<-as.data.frame(train_folder)
        test_folder<-as.data.frame(test_folder)

        #print("train_folder")
        #print(train_folder)
        print("test_folder")
        print(test_folder)

        
        class_column_name<-names(train_folder)[ncol(train_folder)]
        #print(class_column_name)

        mod.tree <- rpart("V3~.", data=train_folder, method="class")
        #print("subset(test_folder, select = V5)")
        #print(subset(test_folder, select = V5))
        #print("predict")
        #print(as.numeric(as.vector(predict(mod.tree,subset(test_folder, select = -V3),type="class"))))
        print("aciertos")
        print(sum(as.numeric(as.vector(predict(mod.tree,subset(test_folder, select = -V3),type="class")))==subset(test_folder, select = V3)))
        #print(predict(mod.tree,subset(test_folder, select = -V3),type="class"))
        error <- 1-sum(as.numeric(as.vector(predict(mod.tree,subset(test_folder, select = -V3),type="class")))==subset(test_folder, select = V3))/dim(m)[1]
        folders_errors<-c(folders_errors,error)

    }
    print(folders_errors)
    return(sum(folders_errors)/k)
}