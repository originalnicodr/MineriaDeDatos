#library('purrr')

library(lubridate)
library(MASS)
library(kernlab)

library(ggplot2)

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
clean_dataset<-clean_dataset[,-1] #saco columna de reference number

clean_dataset[,"Weekday"]<-wday(as.Date(unlist(Traffic.accidents_2019_Leeds[,"Accident.Date"]),format="%d/%m/%Y")) #shiftear los numeros para que doming este en 7 (o capaz lunes por la madrugada) para ver como incrementan los accidentes los fines de semana?

nuevas_fechas<-ToKSP(as.Date(unlist(Traffic.accidents_2019_Leeds["Accident.Date"]),tryFormats = c("%d/%m/%y")))
nuevas_fechas<-nuevas_fechas-2020
clean_dataset[,"Accident.Date"]<-nuevas_fechas

clean_dataset$"X1st.Road.Class...No" <- NULL #elimino una variable que no voy a usar
clean_dataset$"Local.Authority" <- NULL 

#Ver, muy relacionada con la gravedad del accidente? Para sacar
clean_dataset$"Vehicle.Number" <- NULL 


clean_dataset[,"Sex.of.Casualty"][clean_dataset[,"Sex.of.Casualty"]==2]<- clean_dataset[,"Sex.of.Casualty"][clean_dataset[,"Sex.of.Casualty"]==2] -3 # para mantener el rango entre -1 y 1, no se si ayudara en algo

clean_dataset<-clean_dataset[clean_dataset[,"Weather.Conditions"]!=9,] #elimino entradas que tienen clima no clasificado








#decidir que hacer
clean_dataset<-clean_dataset[clean_dataset[,"X1st.Road.Class"]!=6,] #elimino entradas con calles sin clasificar
#clean_dataset["X1st.Road.Class"] <- NULL #elimino entrada


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
clean_dataset["Casualty.Severity"]<- as.factor(unlist(clean_dataset["Casualty.Severity"]))
#---------------------------------------------------------------

#Visualizacion de datos
#---------------------------------------------------------------

summary(clean_dataset)


plot(x=unlist(clean_dataset["Accident.Date"]),y=unlist(clean_dataset["Time..24hr."]),col=unlist(clean_dataset["Casualty.Severity"]),
xlab="Accident Date (from beggenning of 2019 to end of 2019)", ylab="Accident Time (24hr)", main="Date and time of Accidents")

plot(y=unlist(clean_dataset["Weekday"]),x=unlist(clean_dataset["Time..24hr."]),col=unlist(clean_dataset["Casualty.Severity"]),
xlab="Accident Time (24hr)", ylab="Weekday (From Sunday to Saturday)", main="Weekday and time of Accidents")


plot(x=unlist(clean_dataset["Grid.Ref..Easting"]),y=unlist(clean_dataset["Grid.Ref..Northing"]),col=unlist(clean_dataset["Casualty.Severity"]),
xlab="Easting", ylab="Northing", main="Accidents in Cartesian coordinates")


#Create a function to generate a continuous color palette
rbPal <- colorRampPalette(c('blue','red'))
#This adds a column of color values
# based on the y values
dcolor <- rbPal(10)[as.numeric(cut(clean_dataset$"Time..24hr.",breaks = 10))]
plot(clean_dataset$"Grid.Ref..Easting",clean_dataset$"Grid.Ref..Northing",col = dcolor,
xlab="Easting", ylab="Northing", main="Accidents in Cartesian coordinates")





sum(Traffic.accidents_2019_Leeds["Casualty.Severity"]==3)

sum(Traffic.accidents_2019_Leeds["Type.of.Vehicle"]==2)

sum(Traffic.accidents_2019_Leeds[Traffic.accidents_2019_Leeds[,"X1st.Road.Class"]==6,]["Casualty.Severity"]==3)#elimino los que no tienen clasificacion de camino?



#se puede sacar los dias de la semana con $weekday al vector de fecha



#2)
#---------------------------------------------------------------

class_index<-11

FORW.rf <-forward.ranking(clean_dataset[,-class_index],clean_dataset[,class_index],method="rf.est" ,tot.trees=100,equalize.classes=F, verbosity=0)
FORW.lda<-forward.ranking(clean_dataset[,-class_index],clean_dataset[,class_index],method="lda.est")

BACKW.rf <-backward.ranking(clean_dataset[,-class_index],clean_dataset[,class_index],method="rf.est" ,tot.trees=100,equalize.classes=F, verbosity=3) #ver este
BACKW.lda<-backward.ranking(clean_dataset[,-class_index],clean_dataset[,class_index],method="lda.est") #ver este


res_kwfilter<-kwfilter(clean_dataset[,-class_index],clean_dataset[,class_index]) #ver este



#Necesito datos numericos en lugar de factor para correr las siguientes funciones
#clean_dataset2 <-clean_dataset
#clean_dataset2["Casualty.Severity"]<- as.numeric(unlist(clean_dataset["Casualty.Severity"]))


r1<-rfe(clean_dataset,imp.linsvm) #no funca
r2<-rfe(clean_dataset,imp.rf) #no funca

a<-imp.rf(clean_dataset[,-class_index],clean_dataset[,class_index])
colnames(clean_dataset)[a$feats]

#3)
#---------------------------------------------------------------
library(e1071)

#tengo que sacar cada variable que creo que los datos pueden reflejar para despues probar abajo
#Probar con Casualty.Class, o X1.Road.Class
scaled_dataset<-scale(prcomp(clean_dataset[,-class_index],retx=T)$x) #no tiene sentido aplicar log no? lo saque por que tengo muchos valores iguales a 0. A menos que solo aplique log a los valores de fecha y hora?






#---------------------k-means----------------------------
cckmeans<-kmeans(scaled_dataset,cent=3)
plot(scaled_dataset, col=cckmeans$cluster, pch=as.numeric(clean_dataset[,class_index]))

    #---------------Casuality.Severity-----------------------------
plot(scaled_dataset,pch=as.numeric(clean_dataset[,class_index]),col=cckmeans$cluster,main = "Casualty.Severity - kmeans")
cont.table <- table(cckmeans$cluster,as.numeric(clean_dataset[,class_index]))
print("k-means Casuality.Severity")
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #----------------------------------------------------

#---------------------------------------------------------

#MUY MAL NO SIRVE, ME CATEGORIZA TODO COMO GRAVE (ES DONDE HAY MAS DATOS)
#Capaz sirva para otra variable/clasificacion
#O conviene mejor clasificarlo para mantener el error de que sea grave y se clasifique mal al minimo?
#------------------h-clust-single-------------------------
cchlust<-hclust(dist(scaled_dataset),method="single") #"single" "average" "complete"
plot(cchlust)

    #----------------Casuality.Severity-------------------------------
plot(scaled_dataset,pch=as.numeric(clean_dataset[,class_index]),col=cutree(cchlust,k=3),main = "Casualty.Severity - hclust single")
cont.table <- table(cutree(cchlust,k=3),as.numeric(clean_dataset[,class_index]))
print("h-clust single Casuality.Severity")
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------

#LO MISMO
#------------------h-clust-average-------------------------
cchlust<-hclust(dist(scaled_dataset),method="average") #"single" "average" "complete"
plot(cchlust)

    #----------------Casuality.Severity-------------------------------
plot(scaled_dataset,pch=as.numeric(clean_dataset[,class_index]),col=cutree(cchlust,k=3),main = "Casualty.Severity - hclust average")
cont.table <- table(cutree(cchlust,k=3),as.numeric(clean_dataset[,class_index]))
print("h-clust average Casuality.Severity")
# Find optimal match between the two classifications
class.match <- matchClasses(as.matrix(cont.table),method="exact")
# Print the confusion table, with rows permuted to maximize the diagonal
print(cont.table[,class.match])
    #---------------------------------------------------------

#---------------------------------------------------------

#LO MISMO
#------------------h-clust-complete-------------------------
cchlust<-hclust(dist(scaled_dataset),method="complete") #"single" "average" "complete"
plot(cchlust)


    #----------------Casuality.Severity-------------------------------
plot(scaled_dataset,pch=as.numeric(clean_dataset[,class_index]),col=cutree(cchlust,k=3),main = "Casualty.Severity - hclust complete")
cont.table <- table(cutree(cchlust,k=3),as.numeric(clean_dataset[,class_index]))
print("h-clust complete Casuality.Severity")
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