library(e1071)


#como uso lampone con boosting si no tengo set de test?
# tengo que usar los valores para boosting y random forest que estuve usando en el ej 1 y 2?

#trabajar el dataset antes de usarlo?:
lampone2<-lampone[,-c(dim(lampone)[2],143, 1)]
lampone_no_zero<-lampone2[, colSums(lampone2 != 0) > 0]
lampone_prc<-prcomp(scale(lampone_no_zero),retx=T)$x#saco las clases y una columna que decia el numero de la muestra nomas, sino me rompia todo

#sacar ceros, scale, PCA, log?

lampone_no_zero<-lampone2[, colSums(lampone2 != 0) > 0]#tengo que volver a agregar N_tipo, capaz con cbind?


#como hago mi set de test con mis datos de lampone? fede recomendo cross validation, puedo hacer oob aca o nada que ver?
#para estimar el error uso 

#cost=c
svm.poly<-svm(formula = N_tipo ~ ., data = lampone, kernel = "polynomial",
                  cost = 10, scale = FALSE)

svm.radial<-svm(formula = N_tipo ~ ., data = lampone, kernel = "radial",
                  cost = 10, scale = FALSE)



predict(svm.poly, newdata = test_set[-3])