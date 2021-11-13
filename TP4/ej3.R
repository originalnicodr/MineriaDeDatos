library(e1071)
library(groupdata2)
library("adabag")
library(randomForest)

#como uso lampone con boosting si no tengo set de test?
# tengo que usar los valores para boosting y random forest que estuve usando en el ej 1 y 2?
#iterar entre varios valores hasta obtener uno bueno (lo mismo con c en svm)


#trabajar el dataset antes de usarlo?:
lampone2<-lampone[,-c(dim(lampone)[2],143, 1)]
lampone_no_zero<-lampone2[, colSums(lampone2 != 0) > 0]
lampone_prc<-cbind(prcomp(scale(lampone_no_zero),retx=T)$x,as.integer(lampone[,'N_tipo']))#saco las clases y una columna que decia el numero de la muestra nomas, sino me rompia todo
lampone_prc<-as.data.frame(lampone_prc,col.names=colnames(lampone_prc))
#colnames(lampone_prc)<-["PCA"+str(i) for i in range(1, dim(lampone_prc)[2])]
colnames(lampone_prc)[dim(lampone_prc)[2]] <- 'N_tipo'
lampone_prc[, 'N_tipo'] <- sapply(lampone_prc[, 'N_tipo'], as.factor)
#names(lampone_prc)[length(names(lampone_prc))]<-"N_tipo"

crossvalidate_svmradial <- function(data, k = 10, classname= 'N_tipo'){
    folded_data <- fold(data, k, cat_col = classname)#creates a column called .folds used in the crossvalidate function
    
    results<-c()
    #for (c in c(10^(-5),10^(-4),10^(-3),10^(-2),10^(-1),1,10,10^2,10^3,10^4,10^5)){
    for (c in c(10^(-5),10^(-4.9),10^(-4.8),10^(-4.7),10^(-4.6),10^(-4.5),10^(-4.4),10^(-4.3),10^(-4.2),10^(-4.1))){
        for(g in c(10^(-6),10^(-5),10^(-4),10^(-3),10^(-2),10^(-1))){
            # Initialize empty list for recording performances
            performances <- c()
            for (fold in 1:k){
                # Create training set for this iteration
                # Subset all the datapoints where .folds does not match the current fold
                training_set <- data[folded_data$.folds != fold,]
                # Create test set for this iteration
                # Subset all the datapoints where .folds matches the current fold
                testing_set <- data[folded_data$.folds == fold,]        
                #modelo <-svm(formula = N_tipo ~ ., data = training_set, kernel = "polynomial", cost = 10, scale = FALSE)
                modelo <-svm(formula = N_tipo~., data = training_set, kernel = "radial", cost = c, gauss=g, scale = FALSE)   
                #print(modelo)
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
crossvalidate_svmradial(lampone_prc)
crossvalidate_svmradial(lampone)

crossvalidate_svmpoly <- function(data, k = 10,classname='N_tipo'){
    folded_data <- fold(data, k, cat_col = classname)#creates a column called .folds used in the crossvalidate function
    
    results<-c()
    #for (c in c(10^(-5),10^(-4),10^(-3),10^(-2),10^(-1),1,10,10^2,10^3,10^4,10^5)){
    for (c in c(10^(-5),10^(-4.9),10^(-4.8),10^(-4.7),10^(-4.6),10^(-4.5),10^(-4.4),10^(-4.3),10^(-4.2),10^(-4.1))){
        for(g in c(10^(-6),10^(-5),10^(-4),10^(-3),10^(-2),10^(-1))){
            for(d in 1:10){
                for(coeficiente in c(0.1, 1, 10)){
                    # Initialize empty list for recording performances
                    performances <- c()
                    for (fold in 1:k){
                        # Create training set for this iteration
                        # Subset all the datapoints where .folds does not match the current fold
                        training_set <- data[folded_data$.folds != fold,]
                        # Create test set for this iteration
                        # Subset all the datapoints where .folds matches the current fold
                        testing_set <- data[folded_data$.folds == fold,]        
                        modelo <-svm(formula = N_tipo ~ ., data = training_set, kernel = "polynomial", cost = c, scale = FALSE, degree=d, coef0=coeficiente, gamma = g)
                        #modelo <-svm(formula = N_tipo ~ ., data = training_set, kernel = "radial", cost = c, scale = FALSE)       
                        #print(modelo)
                        print(modelo)
                        prediction<-predict(modelo, newdata = testing_set)
                        error<-1-sum(prediction==training_set[,"N_tipo"])/dim(training_set)[1]        
                        performances<-c(performances,error)
                    }
                    #return(sum(performances)/k)
                    results<-c(results,mean(performances))
                }
            }
        }
    }
    index<-which.min(results)
    #print(results)
    print(index)
    return(results[index])
}


#crossvalidate_svmpoly <- function(data, k = 10,classname='N_tipo'){
#    folded_data <- fold(data, k, cat_col = classname)#creates a column called .folds used in the crossvalidate function
#    modelo <- tune.svm(N_tipo~.,kernel = "polynomial" ,data = data, cost=10^(-5:0), gamma = 10^(-6:-1), degree = 1:10, coef0 = 10^(-2:2))$best.parameters
#    print(modelo)
#}
crossvalidate_svmpoly(lampone)



crossvalidate_boosting <- function(data, k = 10, classname="N_tipo"){
    folded_data <- fold(data, k, cat_col = classname)#creates a column called .folds used in the crossvalidate function
    errors<-c()
    for (i in 1:20){
        # Initialize empty list for recording performances
        performances <- c()
        # One iteration per fold
        for (fold in 1:k){
            print(i)
            print(fold)
            # Create training set for this iteration
            # Subset all the datapoints where .folds does not match the current fold
            training_set <- data[folded_data$.folds != fold,]
            # Create test set for this iteration
            # Subset all the datapoints where .folds matches the current fold
            testing_set <- data[folded_data$.folds == fold,]        


            adaboost <- boosting(N_tipo~., data=training_set, mfinal=200,
                             coef="Freund",
                             control=rpart.control(maxdepth=i))#esto va a devolver error si el dataset dado no tiene columna "class"
            performances<-c(performances,predict.boosting(adaboost, newdata=testing_set)$error)#necesito mas cosas ademas del error?
        }
        errors<-c(errors,sum(performances)/k)
    }
    min_index<-which.min(errors)
    print(min_index)
    return(errors[min_index])
}
crossvalidate_boosting(lampone_prc)


crossvalidate_randomforest <- function(data, k = 10, classname='N_tipo'){
    folded_data <- fold(data, k, cat_col = classname)#creates a column called .folds used in the crossvalidate function
    errors<-c()

    class_index<-grep(classname, colnames(data))

    for (i in 1:5){
        # Initialize empty list for recording performances
        performances <- c()
        # One iteration per fold
        for (fold in 1:k){
            print(i)
            print(fold)

            test_confussion_temp<-randomForest(x=data[,-class_index], y=data[,class_index],  ntree=200, mtry=i)$confusion
            test_error_temp<-c(test_error_temp,(test_confussion_temp[1,2]+test_confussion_temp[2,1])/sum(test_confussion_temp[,-3]))#necesito mas cosas ademas del error?
            


            adaboost <- boosting(N_tipo~., data=train_set, mfinal=200,
                             coef="Freund",
                             control=rpart.control(maxdepth=i))#esto va a devolver error si el dataset dado no tiene columna "class"
            performances<-c(performances,predict.boosting(adaboost, newdata=test_set)$error)#necesito mas cosas ademas del error?
        }
        errors<-c(errors,sum(performances)/k)
    }
    min_index<-which.min(errors)
    print(min_index)
    return(errors[min_index])
}
crossvalidate_randomforest(lampone)



parts<-partition(lampone, p = 0.2, cat_col = 'N_tipo')

test_set <- parts[[1]]
train_set <- parts[[2]]

