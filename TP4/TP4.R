library("adabag")
library(e1071)

require(ggplot2)

exec_boosting <- function(train_dataset,test_dataset,max_trees=200){
    test_error<-c()
    #train_error<-c()
    for (i in 1:20){
        adaboost <- boosting(class~., data=train_dataset, mfinal=max_trees,
                             coef="Freund",
                             control=rpart.control(maxdepth=i))#esto va a devolver error si el dataset dado no tiene columna "class"
        test_error<-c(test_error,predict.boosting(adaboost, newdata=test_dataset)$error)#necesito mas cosas ademas del error?
        #train_error<-c(train_error,adaboost$error)
        #train_error<-c(train_error,predict.boosting(adaboost, newdata=train_dataset)$error)
    }

    #ggplot() + geom_freqpoly()

    #print(train_error)
    print(test_error)

    # Create a first line
    plot(1:20, test_error, type = "b", frame = FALSE, pch = 19, col = "red", xlab = "Max Tree Depth", ylab = "Error", main="Diagonal boosting classification")
    # Add a second line
    #lines(1:2, test_error, pch = 18, col = "blue", type = "b", lty = 2)
    # Add a legend to the plot
    #legend("topleft", legend=c("Test Error", "Training Error"), col=c("red", "blue"), lty = 1:2, cex=0.8)

    #ggplot(as.data.frame(test_error), aes(1:length(test_error)))
    #+ geom_line(aes(y = as.data.frame(train_error))) #+ theme(legend.position="bottom")
}
exec_boosting(diag_train,diag_test)

    ggplot(as.data.frame(cbind(cbind(1:5,5:1),1:5)), aes(x=V1)) + 
    geom_line(aes(y = V2), color = "darkred") + 
    geom_line(aes(y = V3), color="steelblue", linetype="twodash") 


    plot(1:2, 1:2, type = "b", frame = FALSE, pch = 19, 
         col = "red", xlab = "Max Tree Depth", ylab = "Error")
    # Add a second line
    lines(1:2, 2:1, pch = 18, col = "blue", type = "b", lty = 2)
    # Add a legend to the plot
    legend("topleft", legend=c("Test Error", "Training Error"),
           col=c("red", "blue"), lty = 1:2, cex=0.8)


library(randomForest)
exec_random_forest <- function(train_dataset,class="class",max_trees=1000){
    class_index<-grep(class, colnames(train_dataset))
    test_error<-c()
    train_error<-c()
    i<-dim(train_dataset)[2]-1#cantidad de features
    i<-69

    test_error<-c()
    #train_error<-c()
    xdata<-c()
    while (i!=0){#siempre se va a cumplir, solo necesito algo que loopeee hasta que termine en 1
        print(i)
        test_error_temp<-c()
        #train_error_temp<-c()
        for(j in 1:5){#tengo que correrlo 5 veces con cada mtry

            #test_confussion_temp<-randomForest(x=train_dataset[,-class_index], y=train_dataset[,class_index],  xtest=test_dataset[,-class_index], ytest=test_dataset[,class_index], ntree=200, mtry=i)$test$confusion
            test_confussion_temp<-randomForest(x=train_dataset[,-class_index], y=train_dataset[,class_index],  ntree=200, mtry=i)$confusion
            #train_confussion_temp<-randomForest(x=train_dataset[,-class_index], y=train_dataset[,class_index],  xtest=train_dataset[,-class_index], ytest=train_dataset[,class_index], ntree=200, mtry=i)$test$confusion
            test_error_temp<-c(test_error_temp,(test_confussion_temp[1,2]+test_confussion_temp[2,1])/sum(test_confussion_temp[,-3]))#necesito mas cosas ademas del error?
            #train_error_temp<-c(train_error_temp,(train_confussion_temp[1,2]+train_confussion_temp[2,1])/sum(train_confussion_temp[,-3]))#fijar si lo puedo hacer de otra manera que no requiera volver a entrenar

            #test_error<-c(randomForest(x=train_dataset[,-class_index], y=train_dataset[,class_index],  xtest=test_dataset[,-class_index], ytest=test_dataset[,class_index], ntree=200, mtry=i))#necesito mas cosas ademas del error?
            #train_error<-c(randomForest(x=train_dataset[,-class_index], y=train_dataset[,class_index],  xtest=train_dataset[,-class_index], ytest=train_dataset[,class_index], ntree=200, mtry=i))#fijar si lo puedo hacer de otra manera que no requiera volver a entrenar

            #rf=randomForest(class ~ . , data = train_dataset , mtry=i,ntree=200) 

            #rf = RandomForestRegressor(n_estimators=max_trees)
            #rf.fit(X_train, y_train)
    
            #pred<-predict(rf,train_dataset) #Predictions on Test Set for each Tree
            #print(pred)


        }
        test_error<-c(test_error,mean(test_error_temp))
        #train_error<-c(train_error,mean(train_error_temp))

        xdata<-c(xdata,i)
        if(i==1) break;
        i<-i%/%2#ver que la division sea entera
        
    }
    #print(train_error)
    print(test_error)

    plot(xdata, test_error, type = "b", frame = FALSE, pch = 19, col = "red", xlab = "Numero de variables por cada split", ylab = "Error", main="RRL randomForest classification")
    
}
exec_random_forest(RRL,"Tipo")






# Add Third curve to the same plot by calling points() and lines()
# Use symbol '+' for points.
 points(xdata, y3, col="dark red",pch="+")
 lines(xdata, y3, col="dark red", lty=3)





randomForest(x=diag_train[,-3], y=diag_train[,3],  xtest=diag_test[,-3], ytest=diag_test[,3], ntree=200, mtry=2)$test$confusion

    rf = RandomForestRegressor(n_estimators=nb_trees)
    rf.fit(X_train, y_train)

    mean_squared_error(y_train, rf.predict(X_train)