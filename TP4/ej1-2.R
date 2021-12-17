library("adabag")
library(e1071)

require(ggplot2)

exec_boosting <- function(train_dataset,test_dataset,max_trees=200){
    test_error<-c()
    for (i in 1:20){
        adaboost <- boosting(class~., data=train_dataset, mfinal=max_trees,
                             coef="Freund",
                             control=rpart.control(maxdepth=i))#esto va a devolver error si el dataset dado no tiene columna "class"
        test_error<-c(test_error,predict.boosting(adaboost, newdata=test_dataset)$error)
    }
    print(test_error)

    plot(1:20, test_error, type = "b", frame = FALSE, pch = 19, col = "red", xlab = "Max Tree Depth", ylab = "Error", main="Espirales boosting classification")
    #plot(1:20, test_error, type = "b", frame = FALSE, pch = 19, col = "red", xlab = "Max Tree Depth", ylab = "Error", main="Diagonal boosting classification")
}
exec_boosting(diag_train,diag_test)
exec_boosting(esp_train,esp_test)


library(randomForest)
exec_random_forest <- function(train_dataset,class="class",max_trees=1000){
    class_index<-grep(class, colnames(train_dataset))
    test_error<-c()
    train_error<-c()
    i<-dim(train_dataset)[2]-1#cantidad de features
    i<-69

    test_error<-c()
    xdata<-c()
    while (i!=0){#siempre se va a cumplir, solo necesito algo que loopeee hasta que termine en 1
        print(i)
        test_error_temp<-c()
        for(j in 1:5){#tengo que correrlo 5 veces con cada mtry

            test_confussion_temp<-randomForest(x=train_dataset[,-class_index], y=train_dataset[,class_index],  ntree=200, mtry=i)$confusion
            test_error_temp<-c(test_error_temp,(test_confussion_temp[1,2]+test_confussion_temp[2,1])/sum(test_confussion_temp[,-3]))#necesito mas cosas ademas del error?

        }
        test_error<-c(test_error,mean(test_error_temp))

        xdata<-c(xdata,i)
        if(i==1) break;
        i<-i%/%2#ver que la division sea entera
        
    }
    print(test_error)

    plot(xdata, test_error, type = "b", frame = FALSE, pch = 19, col = "red", xlab = "Numero de variables por cada split", ylab = "Error", main="RRL randomForest classification")
    
}
exec_random_forest(RRL,"Tipo")