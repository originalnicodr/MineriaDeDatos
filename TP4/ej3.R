library(e1071)


#como uso lampone con boosting si no tengo set de test?
# tengo que usar los valores para boosting y random forest que estuve usando en el ej 1 y 2?
#iterar entre varios valores hasta obtener uno bueno (lo mismo con c en svm)

install.packages("boot",dep=TRUE)
library(rsample)
computos_boot <- bootstraps(lampone, times = 100)

fc <- function(data, i){
 d2 <- data[i,] 
 return(d2)
}
bootcorr <- boot(lampone, fc, R=500)


#trabajar el dataset antes de usarlo?:
lampone2<-lampone[,-c(dim(lampone)[2],143, 1)]
lampone_no_zero<-lampone2[, colSums(lampone2 != 0) > 0]
lampone_prc<-prcomp(scale(lampone_no_zero),retx=T)$x#saco las clases y una columna que decia el numero de la muestra nomas, sino me rompia todo

#sacar ceros, scale, PCA, log?

lampone_no_zero<-lampone2[, colSums(lampone2 != 0) > 0]#tengo que volver a agregar N_tipo, capaz con cbind?

#para estimar el error uso 

#cost=c
svm.poly<-svm(formula = N_tipo ~ ., data = lampone, kernel = "polynomial",
                  cost = 10, scale = FALSE)

svm.radial<-svm(formula = N_tipo ~ ., data = lampone, kernel = "radial",
                  cost = 10, scale = FALSE)


for (i in )
predict(svm.poly, newdata = lampone)


library(caret)
train_control<- trainControl(method="cv", number=10, savePredictions = TRUE)
model<- train(N_tipo ~ ., data=lampone, trControl=train_control, method="rpart")

model$pred



length(filter(lampone[,"N_tipo"],2))


crossvalidate_svmradial <- function(data, k = 10,classname='N_tipo'){
    folded_data <- fold(data, k, cat_col = classname)#creates a column called .folds used in the crossvalidate function
    
    for (i in 1:20){
        # Initialize empty list for recording performances
        performances <- c()
        # One iteration per fold
        for (fold in 1:k){
            # Create training set for this iteration
            # Subset all the datapoints where .folds does not match the current fold
            training_set <- data[folded_data$.folds != fold,]
            # Create test set for this iteration
            # Subset all the datapoints where .folds matches the current fold


            testing_set <- data[folded_data$.folds == fold,]        
            #modelo <-svm(formula = N_tipo ~ ., data = training_set, kernel = "polynomial", cost = 10, scale = FALSE)
            modelo <-svm(formula = N_tipo ~ ., data = training_set, kernel = "radial", cost = 10, scale = FALSE)       
            
            #print(modelo)
            prediction<-predict(modelo, newdata = testing_set)
            error<-1-sum(prediction==training_set[,"N_tipo"])/dim(training_set)[1]        
            performances<-c(performances,error)
        }
        return(sum(performances)/k)
    }
}





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

crossvalidate_boosting(lampone)


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




parts<-partition(lampone, p = 0.2, cat_col = 'N_tipo')

test_set <- parts[[1]]
train_set <- parts[[2]]

