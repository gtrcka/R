dir <- "C:/Users/gabi_/OneDrive/Documentos/R"
install.packages("dplyr")
library(dplyr)
setwd(dir)
getwd()

iris_ds <- read.table("Iris.csv", header=T, sep=",",fill=T)
iris_ds[1:25,2:6]
head(iris_ds)

#Selecciono algunas columnas
iris_ds %>% select(SepalWidthCm, SepalLengthCm, PetalLengthCm , PetalWidthCm, Species  )
#Cambio el tipo de dato de una columna
iris_ds %>% mutate(Id=as.character(Id)) ->iris_ds_1
class(iris_ds$Id)
class(iris_ds_1$Id)
#Obtengo un subconjunto por filtro
iris_ds_2 <- subset (iris_ds,PetalLengthCm<2)
#Ordeno por columnas
iris_ds_2 %>% arrange(PetalLengthCm,SepalLengthCm)

#Obtengo SepalLengthCm por cada especie
subset(iris_ds, Species == 'Iris-setosa') %>% select(SepalLengthCm) -> sepalLengthSetosa
subset(iris_ds, Species == 'Iris-virginica') %>% select(SepalLengthCm) -> sepalLengthVirginica
subset(iris_ds, Species == 'Iris-versicolor') %>% select(SepalLengthCm) -> sepalLengthVersicolor
sepalLengthSetosa<-sepalLengthSetosa$SepalLengthCm
sepalLengthVirginica<-sepalLengthVirginica$SepalLengthCm
sepalLengthVersicolor<-sepalLengthVersicolor$SepalLengthCm

#Media del sepalLength por especie
mean(sepalLengthSetosa, na.rm=T)
mean(sepalLengthVirginica)
mean(sepalLengthVersicolor)

#Cuantiles del sepalLength por especia
sepalLengthSetosa
quantile(sepalLengthSetosa, c(0.25,0.50,0.75), na.rm=T)
quantile(sepalLengthVirginica, c(0.1,0.2,0.4,0.6,0.8))
quantile(sepalLengthVersicolor)

#fivenum - Función que corresponde a los cinco números de Tukey
fivenum(sepalLengthVersicolor)

#summary() - Función para explorar el dataset
summary(iris_ds %>% select(SepalWidthCm, SepalLengthCm, PetalLengthCm , PetalWidthCm  ))

#Varianza y Desviación Standard
var(sepalLengthSetosa, na.rm=T)
sd(sepalLengthSetosa)
var(sepalLengthVirginica)
sd(sepalLengthVirginica)
var(sepalLengthVersicolor)
sd(sepalLengthVersicolor)

#skewness() y kurtosis() - Funciones para obtener coeficiente de asimetría y el grado de empinamiento(curtosis) que presenta
install.packages("e1071")
library(e1071)
skewness(sepalLengthSetosa) 
kurtosis(sepalLengthSetosa)
par(mar=c(5,5,5,5))

#Graficos
#Histograma
hist(sepalLengthVirginica, breaks=c(4, 5,6,7,8), scale="density",main="Histograma con intervalos", col="green")

#Gráficos de barras
barplot(sepalLengthVirginica)
barplot(sepalLengthVirginica, col=rainbow(sepalLengthVirginica), horiz=T, xlab="Frecuencia")

#Gráficos de líneas con plot()
#Gráfico de sectores pie() 
#Gráfico de dispersión (plot(x =  , y=  ))
#Boxplot o diagrama de caja boxplot()

getwd()