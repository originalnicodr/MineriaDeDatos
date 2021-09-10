#d<-diagonal(1000,2,0.5)
#plot(d$V1, d$V2, main="Diagonal: n=1000, d=2, c=0.5", xlab="X", ylab="Y", col=ifelse(d$V3==1, "blue","red"))
#e <- espirales(3000)
#plot(e$V1, e$V2, main="Espirales: n=3000", xlab="X", ylab="Y", col=ifelse(e$V5==1, "blue","red"))

#arboles de desicion

library(rpart)

diag_train <- diagonal(200,2,0.75)
diag_test <- diagonal(2000,2,0.75)
mod.tree <- rpart("V3~.", data=diag_train, method="class")
print("Error arboles en datos Diagonal")
1-sum(as.numeric(as.vector(predict(mod.tree,subset(diag_test, select = -V3),type="class")))==subset(diag_test, select = V3))/2000

esp_train <- espirales(200)
esp_test <- espirales(2000)

mod.tree <- rpart("V5~.", data=esp_train, method="class")

print("Error arboles en datos Espirales")
1-sum(as.numeric(as.vector(predict(mod.tree,subset(esp_test, select = -V5),type="class")))==subset(esp_test, select = V5))/2000

#knn

library(class)

diag_train_M <- data.matrix(diag_train)
diag_test_M <- data.matrix(diag_test)

mod.knn <- knn(train=diag_train_M[,-3],test=diag_test_M[,-3],cl=diag_train_M[,3],,k=3)
print("Error knn en datos Diagonal")
1-sum(mod.knn==diag_test_M[,3])/2000


esp_train_M <- data.matrix(esp_train)
esp_test_M <- data.matrix(esp_test)

mod.knn <- knn(train=esp_train_M[,-3],test=esp_test_M[,-3],cl=esp_train_M[,3],,k=3)
print("Error knn en datos Espirales")
1-sum(mod.knn==esp_test_M[,3])/2000

#cross-fold-validation arboles

library(tidyverse)
library(caret)

# R program to implement
# K-fold cross-validation
 
# setting seed to generate a
# reproducible random sampling
set.seed(125)
 
# defining training control
# as cross-validation
train_control <- trainControl(method = "cv", number = 5)
 
# training the model by assigning sales column
# as target variable and rest other column
# as independent variable
model <- train(V3~., data = diag_train, method = "lm", trControl = train_control)
 
# printing model performance metrics
# along with other details
print(model)


cv_diagonal <- diagonal(2000,2,0.75)
cross_validation(as.matrix(cv_diagonal),10)

#train_fold= 200, test_fold=1600