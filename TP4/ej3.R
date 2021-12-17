library(e1071)
library(groupdata2)
library("adabag")
library(randomForest)


lampone2<-lampone[,-c(dim(lampone)[2],143, 1)]
lampone_no_zero<-lampone2[, colSums(lampone2 != 0) > 0]
lampone_prc<-cbind(prcomp(scale(lampone_no_zero),retx=T)$x,as.integer(lampone[,'N_tipo']))#saco las clases y una columna que decia el numero de la muestra nomas, sino me rompia todo
lampone_prc<-as.data.frame(lampone_prc,col.names=colnames(lampone_prc))
colnames(lampone_prc)[dim(lampone_prc)[2]] <- 'N_tipo'
lampone_prc[, 'N_tipo'] <- sapply(lampone_prc[, 'N_tipo'], as.factor)


crossvalidate_svmradial <- function(data, k = 10, classname= 'N_tipo'){
    folded_data <- fold(data, k, cat_col = classname)#creates a column called .folds used in the crossvalidate function
    
    results<-c()
    #for (c in c(10^(-5),10^(-4),10^(-3),10^(-2),10^(-1),1,10,10^2,10^3,10^4,10^5)){
    for (c in c(10^(-5),10^(-4),10^(-3),10^(-2),10^(-1),1,10,10^2,10^3,10^4,10^5)){
        for(g in 10*(1:40)){
            performances <- c()
            for (fold in 1:k){
                training_set <- data[folded_data$.folds != fold,]
                testing_set <- data[folded_data$.folds == fold,]        
                modelo <-svm(formula = N_tipo~., data = training_set, kernel = "radial", cost = c, gauss=g, scale = FALSE)   
                prediction<-predict(modelo, newdata = testing_set)
                #print(prediction)
                #print(training_set[,"N_tipo"])
                error<-1-sum(prediction==testing_set[,"N_tipo"])/dim(testing_set)[1]        
                performances<-c(performances,error)
            }
            #print(performances)
            results<-c(results,mean(performances))
        }
    }
    index<-which.min(results)
    print(results)
    print(index)
    return(results[index])
}
crossvalidate_svmradial(lampone_prc)#resultado: 0.1533333



crossvalidate_svmpoly <- function(data, k = 10,classname='N_tipo'){
    folded_data <- fold(data, k, cat_col = classname)#creates a column called .folds used in the crossvalidate function
    
    results<-c()
    #for (c in c(10^(-5),10^(-4),10^(-3),10^(-2),10^(-1),1,10,10^2,10^3,10^4,10^5)){
    for (c in 1:10){
        for(d in 1:10){
            performances <- c()
            for (fold in 1:k){
                training_set <- data[folded_data$.folds != fold,]
                testing_set <- data[folded_data$.folds == fold,]        
                modelo <-svm(formula = N_tipo ~ ., data = training_set, kernel = "radial", cost = c, scale = FALSE, degree=d)       
                #print(modelo)
                prediction<-predict(modelo, newdata = testing_set)
                error<-1-sum(prediction==testing_set[,"N_tipo"])/dim(testing_set)[1]        
                performances<-c(performances,error)
            }
            print(mean(performances))
            results<-c(results,mean(performances))
        }
    }
    index<-which.min(results)
    print(results)
    print(index)
    return(results[index])
}
crossvalidate_svmpoly(lampone_prc)#0.1616667



crossvalidate_boosting <- function(data, k = 10, classname="N_tipo"){
    folded_data <- fold(data, k, cat_col = classname)#creates a column called .folds used in the crossvalidate function
    errors<-c()
    for (i in 1:20){
        performances <- c()
        for (fold in 1:k){
            print(i)
            print(fold)
            training_set <- data[folded_data$.folds != fold,]
            testing_set <- data[folded_data$.folds == fold,]        


            adaboost <- boosting(N_tipo~., data=training_set, mfinal=200,
                             coef="Freund",
                             control=rpart.control(maxdepth=i))#esto va a devolver error si el dataset dado no tiene columna "class"
            performances<-c(performances,predict.boosting(adaboost, newdata=testing_set)$error)
        }
        errors<-c(errors,sum(performances)/k)
    }
    min_index<-which.min(errors)
    print(errors)
    print(min_index)
    return(errors[min_index])
}
crossvalidate_boosting(lampone_prc)


crossvalidate_randomforest <- function(data, k = 10, classname='N_tipo'){
    #folded_data <- fold(data, k, cat_col = classname)#creates a column called .folds used in the crossvalidate function
    errors<-c()

    class_index<-grep(classname, colnames(data))

    for (i in 1:5){
        performances <- c()
        for (fold in 1:k){
            print(i)
            print(fold)

            test_confussion_temp<-randomForest(x=data[,-class_index], y=data[,class_index],  ntree=200, mtry=i)$confusion
            performances<-c(performances,(test_confussion_temp[1,2]+test_confussion_temp[2,1])/sum(test_confussion_temp[,-3]))#necesito mas cosas ademas del error?
        }
        errors<-c(errors,mean(performances))
    }
    min_index<-which.min(errors)
    print(min_index)
    return(errors[min_index])
}
crossvalidate_randomforest(lampone_prc)
