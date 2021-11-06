library("adabag")
data("iris")
train <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
iris.adaboost <- boosting(Species ~ ., data = iris[train, ], mfinal = 10, coef="Freund", control = rpart.control(maxdepth =
3))
iris.adaboost

library(e1071)
cont.table <- table(iris.adaboost$class,as.numeric(iris[train, "Species"]))
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])

train <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
iris.adaboost <- boosting(Species ~ ., data = iris[train, ], mfinal = 200, coef="Freund", control = rpart.control(maxdepth =
3))
iris.adaboost$class# para ver el resultado y scar error de test?

require(ggplot2)

exec_boosting <- function(train_dataset,test_dataset,max_trees=200){
    test_error<-c()
    train_error<-c()
    for (i in 1:1){
        adaboost <- boosting(class~., data=train_dataset, mfinal=max_trees,
                             coef="Freund",
                             control=rpart.control(maxdepth=i))#esto va a devolver error si el dataset dado no tiene columna "class"
        test_error<-c(test_error,predict.boosting(adaboost, newdata=test_dataset)$error)#necesito mas cosas ademas del error?
        train_error<-c(train_error,adaboost$error)
    }

    #ggplot() + geom_freqpoly()

    ggplot(as.data.frame(test_error), aes(1:length(test_error)))
    #+ geom_line(aes(y = as.data.frame(train_error))) #+ theme(legend.position="bottom")
}
exec_boosting(diag_train,diag_test)


library(randomForest)
exec_random_forest <- function(train_dataset,test_dataset,max_trees=200){
    class_index<-grep("class", colnames(train_dataset))
    test_error<-c()
    train_error<-c()
    for (i in 1:1){
        #test_error<-c(randomForest(x=train_dataset[,-class_index], y=train_dataset[,class_index],  xtest=test_dataset[,-class_index], ytest=test_dataset[,class_index], ntree=200, mtry=i))#necesito mas cosas ademas del error?
        #train_error<-c(randomForest(x=train_dataset[,-class_index], y=train_dataset[,class_index],  xtest=train_dataset[,-class_index], ytest=train_dataset[,class_index], ntree=200, mtry=i))#fijar si lo puedo hacer de otra manera que no requiera volver a entrenar
    
        #rf=randomForest(class ~ . , data = train_dataset , mtry=i,ntree=200) 

        #rf = RandomForestRegressor(n_estimators=max_trees)
        rf.fit(X_train, y_train)
  
        pred<-predict(rf,train_dataset) #Predictions on Test Set for each Tree
        print(pred)
    }
    #print(train_error)
}
exec_random_forest(diag_train,diag_test)


    rf = RandomForestRegressor(n_estimators=nb_trees)
    rf.fit(X_train, y_train)

    mean_squared_error(y_train, rf.predict(X_train)