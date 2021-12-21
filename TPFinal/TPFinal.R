#library('purrr')

library(lubridate)
library(MASS)
library(kernlab)

library(ggplot2)

library(e1071)
library(caret)

#1) 
# Pre-Tratamiento de datos
#--------------------------------------------------
ToKSP<-function(date){
    d<-as.integer(format(date, format = "%y")) #por alguna razon el dia me lo devuelve en %y y el año en %d
    m<-as.integer(format(date, format = "%m"))
    #y<-as.integer(format(date, format = "%d")) #siempre 2019 en este dataset

    #formatted_date<-as.Date(paste(paste("2019",as.character(m),sep = "-"),as.character(d),sep = "-"))

    days_starting_jan_1<- as.integer(date - as.Date('2019-1-01'))

    r<-2019+(days_starting_jan_1-0.5)/365
    return(r)
}

clean_dataset<-Traffic.accidents_2019_Leeds


#Balanceo de clase

temp_dataset<-clean_dataset[clean_dataset[, 16]==3,]
clean_dataset<-clean_dataset[clean_dataset[, 16]!=3,]
temp_dataset<-temp_dataset[sample(nrow(temp_dataset), 500),]#pongo mas o menos la misma cantidad de datos
clean_dataset<-rbind(clean_dataset,temp_dataset)



clean_dataset<-clean_dataset[,-1] #saco columna de reference number

clean_dataset[,"Weekday"]<-wday(as.Date(unlist(clean_dataset[,"Accident.Date"]),format="%d/%m/%Y"))

nuevas_fechas<-ToKSP(as.Date(unlist(clean_dataset["Accident.Date"]),tryFormats = c("%d/%m/%y")))
nuevas_fechas<-nuevas_fechas-2020
clean_dataset[,"Accident.Date"]<-nuevas_fechas

clean_dataset$"X1st.Road.Class...No" <- NULL #elimino variables que no voy a usar
clean_dataset$"Local.Authority" <- NULL 

#Ver, muy relacionada con la gravedad del accidente? Para sacar
clean_dataset$"Number.of.Vehicles" <- NULL
clean_dataset$"Vehicle.Number" <- NULL 


#clean_dataset[,"Sex.of.Casualty"][clean_dataset[,"Sex.of.Casualty"]==2]<- clean_dataset[,"Sex.of.Casualty"][clean_dataset[,"Sex.of.Casualty"]==2] -3 # para mantener el rango entre -1 y 1, no creo que cambie en nada

clean_dataset<-clean_dataset[clean_dataset[,"Weather.Conditions"]!=9,] #elimino entradas que tienen clima no clasificado




#clean_dataset<-clean_dataset[clean_dataset[,"X1st.Road.Class"]!=6,] #elimino entradas con calles sin clasificar
clean_dataset["X1st.Road.Class"] <- NULL #elimino la variable en si
#Si eliminara en su lugar las entradas con calles sin clasificar me quedo con muy pocos datos




clean_dataset[,"Vehicle.Pedal.Cycle"]<-as.numeric(clean_dataset["Type.of.Vehicle"]==1)
#Junto valores de la variable "Type.of.Vehicle" asociadas a motos en una sola
clean_dataset[,"Vehicle.Motorcycle"]<-as.numeric(clean_dataset["Type.of.Vehicle"]==2) + 2*as.numeric(clean_dataset["Type.of.Vehicle"]==3) + 3*as.numeric(clean_dataset["Type.of.Vehicle"]==4) + 4*as.numeric(clean_dataset["Type.of.Vehicle"]==5)
clean_dataset[,"Vehicle.TaxiPrivateHireCar"]<-as.numeric(clean_dataset["Type.of.Vehicle"]==8)
clean_dataset[,"Vehicle.Car"]<-as.numeric(clean_dataset["Type.of.Vehicle"]==9)

clean_dataset[,"Vehicle.BusOrCoach"]<-as.numeric(clean_dataset["Type.of.Vehicle"]==11)

#Junto valores de la variable "Type.of.Vehicle" asociadas a goods vehicle en una sola
clean_dataset[,"Vehicle.GoodsVehicle"]<-as.numeric(clean_dataset["Type.of.Vehicle"]==19) +2*as.numeric(clean_dataset["Type.of.Vehicle"]==20) + 3*as.numeric(clean_dataset["Type.of.Vehicle"]==21)


#En lugar de crear una columna para cada valor de la variable elimino los valores que no tienen casi entradas.

#clean_dataset[,"Vehicle.Minibus"]<-as.numeric(clean_dataset["Type.of.Vehicle"]==10) #4
#clean_dataset[,"Vehicle.RiddenHorse"]<-as.numeric(clean_dataset["Type.of.Vehicle"]==16) #2
#clean_dataset[,"Vehicle.TramLightRial"]<-as.numeric(clean_dataset["Type.of.Vehicle"]==17) #1
#clean_dataset[,"Vehicle.MobilityScooter"]<-as.numeric(clean_dataset["Type.of.Vehicle"]==22) #3
#clean_dataset[,"Vehicle.OtherVehicle"]<-as.numeric(clean_dataset["Type.of.Vehicle"]==90) #2
#clean_dataset[,"Vehicle.MotorcycleUnknownCC"]<-as.numeric(clean_dataset["Type.of.Vehicle"]==97) #3

clean_dataset<-clean_dataset[clean_dataset[,"Type.of.Vehicle"]!=10,]
clean_dataset<-clean_dataset[clean_dataset[,"Type.of.Vehicle"]!=16,]
clean_dataset<-clean_dataset[clean_dataset[,"Type.of.Vehicle"]!=17,]
clean_dataset<-clean_dataset[clean_dataset[,"Type.of.Vehicle"]!=22,]
clean_dataset<-clean_dataset[clean_dataset[,"Type.of.Vehicle"]!=90,]
clean_dataset<-clean_dataset[clean_dataset[,"Type.of.Vehicle"]!=97,]

#Elimino la columna original
clean_dataset$"Type.of.Vehicle" <- NULL

#
#clean_dataset["Casualty.Severity"]<- as.factor(unlist(clean_dataset["Casualty.Severity"]))

clean_dataset$Casualty.Severity <- as.factor(clean_dataset$Casualty.Severity)#as.numeric(as.factor(clean_dataset$Casualty.Severity))
#clean_dataset$Casualty.Severity <- as.numeric(as.factor(clean_dataset$Casualty.Severity))

#typeof(unlist(clean_dataset[,1]))


clean_dataset[,"Grid.Ref..Easting"]<-as.numeric(unlist(clean_dataset[,"Grid.Ref..Easting"]))
clean_dataset[,"Grid.Ref..Northing"]<-as.numeric(unlist(clean_dataset[,"Grid.Ref..Northing"]))
clean_dataset[,"Time..24hr."]<-as.numeric(unlist(clean_dataset[,"Time..24hr."]))
clean_dataset[,"Road.Surface"]<-as.numeric(unlist(clean_dataset[,"Road.Surface"]))
clean_dataset[,"Lighting.Conditions"]<-as.numeric(unlist(clean_dataset[,"Lighting.Conditions"]))
clean_dataset[,"Weather.Conditions"]<-as.numeric(unlist(clean_dataset[,"Weather.Conditions"]))
clean_dataset[,"Casualty.Class"]<-as.numeric(unlist(clean_dataset[,"Casualty.Class"]))
clean_dataset[,"Age.of.Casualty"]<-as.numeric(unlist(clean_dataset[,"Age.of.Casualty"]))


#clean_dataset[,"X1st.Road.Class"]<-as.numeric(unlist(clean_dataset[,"X1st.Road.Class"]))

#typeof(unlist(clean_dataset[,"Vehicle.BusOrCoach"  ]))
#typeof(unlist(clean_dataset[,1]))

#---------------------------------------------------------------

#Visualizacion de datos
#---------------------------------------------------------------

summary(clean_dataset)


#plot(x=unlist(clean_dataset["Accident.Date"]),y=unlist(clean_dataset["Time..24hr."]),col=unlist(clean_dataset["Casualty.Severity"]),
#xlab="Accident Date (from beggenning of 2019 to end of 2019)", ylab="Accident Time (24hr)", main="Date and time of Accidents")

#plot(y=unlist(clean_dataset["Weekday"]),x=unlist(clean_dataset["Time..24hr."]),col=unlist(clean_dataset["Casualty.Severity"]),
#xlab="Accident Time (24hr)", ylab="Weekday (From Sunday to Saturday)", main="Weekday and time of Accidents")


plot(x=unlist(clean_dataset["Grid.Ref..Easting"]),y=unlist(clean_dataset["Grid.Ref..Northing"]),col=unlist(clean_dataset["Casualty.Severity"]),
xlab="Easting", ylab="Northing", main="Accidents in Cartesian coordinates")


#Accidentes dependiendo de la hora del dia
#rbPal <- colorRampPalette(c('blue','red'))
#dcolor <- rbPal(10)[as.numeric(cut(clean_dataset$"Time..24hr.",breaks = 10))]
#plot(clean_dataset$"Grid.Ref..Easting",clean_dataset$"Grid.Ref..Northing",col = dcolor,
#xlab="Easting", ylab="Northing", main="Accidents in Cartesian coordinates")

barplot(prop.table(table(clean_dataset$Sex.of.Casualty)),col=c("blue","yellow"),
        legend.text=c("Hombre","Mujer"),main="Sexo de la victima",
        ylab ="Frecuencias Relativas",las=1,font.axis=4)

barplot(prop.table(table(clean_dataset$Casualty.Severity)),col=c("red","orange","yellow"),
        legend.text=c("Fatal","Seria","Leve"),main="Gravedad de la victima",
        ylab ="Frecuencias Relativas",las=1,font.axis=4)

barplot(prop.table(table(clean_dataset$Casualty.Class)),col=c("red","purple","blue"),
        legend.text=c("Conductor","Pasajero acompañante","Peaton"),main="Tipo de victima",
        ylab ="Frecuencias Relativas",las=1,font.axis=4)

boxplot(clean_dataset$Age.of.Casualty ~ clean_dataset$Casualty.Severity,
        xlab = "Gravedad de la victima",
        ylab = "Edad",  
        main = "Edad de las victimas")


#sum(Traffic.accidents_2019_Leeds["Casualty.Severity"]==3)
#sum(Traffic.accidents_2019_Leeds["Type.of.Vehicle"]==2)
#sum(Traffic.accidents_2019_Leeds[Traffic.accidents_2019_Leeds[,"X1st.Road.Class"]==6,]["Casualty.Severity"]==3)#elimino los que no tienen clasificacion de camino?

#2)
#---------------------------------------------------------------

#class_index<-11
class_index<-grep("Casualty.Severity", colnames(clean_dataset))


FORW.rf <-forward.ranking(clean_dataset[,-class_index],as.factor(clean_dataset$Casualty.Severity),method="rf.est" ,tot.trees=100,equalize.classes=F, verbosity=0)
FORW.lda<-forward.ranking(clean_dataset[,-class_index],as.factor(clean_dataset$Casualty.Severity),method="lda.est")

BACKW.rf <-backward.ranking(clean_dataset[,-class_index],as.factor(clean_dataset$Casualty.Severity),method="rf.est" ,tot.trees=100,equalize.classes=F, verbosity=3) #ver este
BACKW.lda<-backward.ranking(clean_dataset[,-class_index],as.factor(clean_dataset$Casualty.Severity),method="lda.est") #ver este

res_kwfilter<-kwfilter(clean_dataset[,-class_index],as.factor(clean_dataset$Casualty.Severity)) #ver este

res_linsvm<-rfe_new(clean_dataset[,-class_index],clean_dataset[,class_index],imp.linsvm)
res_rf<-rfe_new(clean_dataset[,-class_index],clean_dataset[,class_index],imp.rf)

cat(" ", FORW.rf$ordered.names.list,"\n",sep=", ")
cat(" ", FORW.lda$ordered.names.list,"\n",sep=", ")
cat(" ", BACKW.rf$ordered.names.list,"\n",sep=", ")
cat(" ", BACKW.lda$ordered.names.list,"\n",sep=", ")
cat(" ", colnames(clean_dataset[,-class_index])[res_kwfilter],"\n",sep=", ")
cat(" ", res_linsvm,"\n",sep=", ")
cat(" ", res_rf,"\n",sep=", ")



#3)
#---------------------------------------------------------------

scaled_dataset<-scale(prcomp(clean_dataset[,-class_index],retx=T)$x) #no tiene sentido aplicar log no? lo saque por que tengo muchos valores iguales a 0. A menos que solo aplique log a los valores de fecha y hora?


#---------------------k-means----------------------------
cckmeans<-kmeans(scaled_dataset,cent=3)
plot(scaled_dataset, col=cckmeans$cluster, pch=as.numeric(clean_dataset[,class_index]))

    #---------------Casualty.Severity-----------------------------
plot(scaled_dataset,pch=as.numeric(clean_dataset[,class_index]),col=cckmeans$cluster,main = "Casualty.Severity - kmeans")
cont.table <- table(cckmeans$cluster,as.numeric(clean_dataset[,class_index]))
print("k-means Casualty.Severity")
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #----------------------------------------------------

#---------------------------------------------------------

#------------------h-clust-single-------------------------
cchlust<-hclust(dist(scaled_dataset),method="single") #"single" "average" "complete"
plot(cchlust)

    #----------------Casualty.Severity-------------------------------
plot(scaled_dataset,pch=as.numeric(clean_dataset[,class_index]),col=cutree(cchlust,k=3),main = "Casualty.Severity - hclust single")
cont.table <- table(cutree(cchlust,k=3),as.numeric(clean_dataset[,class_index]))
print("h-clust single Casualty.Severity")
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------


#------------------h-clust-average-------------------------
cchlust<-hclust(dist(scaled_dataset),method="average") #"single" "average" "complete"
plot(cchlust)

    #----------------Casualty.Severity-------------------------------
plot(scaled_dataset,pch=as.numeric(clean_dataset[,class_index]),col=cutree(cchlust,k=3),main = "Casualty.Severity - hclust average")
cont.table <- table(cutree(cchlust,k=3),as.numeric(clean_dataset[,class_index]))
print("h-clust average Casualty.Severity")
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------

#---------------------------------------------------------


#------------------h-clust-complete-------------------------
cchlust<-hclust(dist(clean_dataset[,-class_index]),method="complete") #"single" "average" "complete"
plot(cchlust)


    #----------------Casualty.Severity-------------------------------
plot(clean_dataset[,-class_index],pch=as.numeric(clean_dataset[,class_index]),col=cutree(cchlust,k=3),main = "Casualty.Severity - hclust complete")
cont.table <- table(cutree(cchlust,k=3),as.numeric(clean_dataset[,class_index]))
print("h-clust complete Casualty.Severity")
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------
#---------------------------------------------------------


#Opcional
#---------------------------------------------------------------
result_gap<-(GAP(scaled_dataset,10,30))
#da como resultado uno, sacarlo? alto quilombo arreglar las funciones de estabilidad

#---------------------------------------------------------------

#4
#---------------------------------------------------------------
#class_model_dataset<-as.data.frame(prcomp(scale(clean_dataset),retx=T)$x)#incluye las variables de las clases

data1<-clean_dataset[,-class_index]
class_model_dataset<-cbind(prcomp(scale(data1),retx=T)$x,as.integer(clean_dataset[,class_index]))#saco las clases y una columna que decia el numero de la muestra nomas, sino me rompia todo
class_model_dataset<-as.data.frame(class_model_dataset,col.names=colnames(class_model_dataset))
colnames(class_model_dataset)[dim(class_model_dataset)[2]] <- "Casualty.Severity"
#class_model_dataset[, "Casualty.Severity"] <- sapply(class_model_dataset[, "Casualty.Severity"], as.factor)

#Intente usar este dataset pasado por PCA y scale pero por alguna razon las funciones de clasificadores se rompian, por lo cual use el dataset limpiado original.


#svmradial<-crossvalidate_svmradial(class_model_dataset,classname= "Casualty.Severity")
svmradial<-crossvalidate_svmradial(clean_dataset,classname= "Casualty.Severity")
#svmpoly<-crossvalidate_svmpoly(class_model_dataset,classname= "Casualty.Severity")
svmpoly<-crossvalidate_svmpoly(clean_dataset,classname= "Casualty.Severity")


#result_boosting<-crossvalidate_boosting(class_model_dataset,classname= "Casualty.Severity")
result_boosting<-crossvalidate_boosting(clean_dataset,classname= "Casualty.Severity")
#result_randomforest<-crossvalidate_randomforest(class_model_dataset,classname= "Casualty.Severity")
result_randomforest<-crossvalidate_randomforest(clean_dataset,classname= "Casualty.Severity")