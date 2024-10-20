dir <- "C:/Users/gabi_/OneDrive/Documentos/R"
#install.packages("dplyr")
#install.packages("ggplot2")
install.packages("gridExtra")
library(gridExtra)
library(dplyr)
library(ggplot2)
setwd(dir)
getwd()

iris_ds <- read.table("Iris.csv", header=T, sep=",",fill=T)
iris_ds[1:25,2:6]
head(iris_ds)
#summary() - Función para explorar el dataset
summary(iris_ds %>% select(SepalWidthCm, SepalLengthCm, PetalLengthCm , PetalWidthCm  ))

#1.Estudio de los datos:
#Cuento registros por specie
iris_ds %>% count(Species)
table(iris_ds$Species)

#Medias
#Medias SepalWidthCm
aggregate(SepalWidthCm ~ Species, data = iris_ds, FUN = mean) %>% arrange(SepalWidthCm)

#Medias SepalLengthCm
aggregate(SepalLengthCm ~ Species, data = iris_ds, FUN = mean) %>% arrange(SepalLengthCm)

#Medias PetalWidthCm
aggregate(PetalWidthCm ~ Species, data = iris_ds, FUN = mean) %>% arrange(PetalWidthCm)

#Medias PetalLengthCm
aggregate(PetalLengthCm ~ Species, data = iris_ds, FUN = mean) %>% arrange(PetalLengthCm)

#Desviaciones estandar
#Desviación estandar SepalWidthCm
aggregate(SepalWidthCm ~ Species, data = iris_ds, FUN = sd) %>% arrange(SepalWidthCm)

#Desviación estandar SepalLengthCm
aggregate(SepalLengthCm ~ Species, data = iris_ds, FUN = sd) %>% arrange(SepalLengthCm)

#Desviación estandar PetalWidthCm
aggregate(PetalWidthCm ~ Species, data = iris_ds, FUN = sd) %>% arrange(PetalWidthCm)

#Desviación estandar PetalLengthCm
aggregate(PetalLengthCm ~ Species, data = iris_ds, FUN = sd) %>% arrange(PetalLengthCm)

#Box-plot - ggplot
p1 <- ggplot(data = iris_ds, aes(x = Species, y = SepalWidthCm, color = Species)) +
  geom_boxplot() +
  theme_bw()
p2 <- ggplot(data = iris_ds, aes(x = Species, y = SepalLengthCm, color = Species)) +
  geom_boxplot() +
  theme_bw()
p3 <- ggplot(data = iris_ds, aes(x = Species, y = PetalWidthCm, color = Species)) +
  geom_boxplot() +
  theme_bw()
p4 <- ggplot(data = iris_ds, aes(x = Species, y = PetalLengthCm, color = Species)) +
  geom_boxplot() +
  theme_bw()
combined_plot <-  grid.arrange(p1 , p2 , p3 , p4 ,ncol = 2)

#Estudio de normalidad SepalWidthCm
par(mfrow = c(2,2), mar = c(5, 4, 4, 2) + 0.1)

qqnorm(iris_ds[iris_ds$Species == "Iris-setosa","SepalWidthCm"], main = "Iris-setosa")
qqline(iris_ds[iris_ds$Species == "Iris-setosa","SepalWidthCm"])
qqnorm(iris_ds[iris_ds$Species == "Iris-versicolor","SepalWidthCm"], main = "Iris-versicolor")
qqline(iris_ds[iris_ds$Species == "Iris-versicolor","SepalWidthCm"])
qqnorm(iris_ds[iris_ds$Species == "Iris-virginica","SepalWidthCm"], main = "Iris-virginica")
qqline(iris_ds[iris_ds$Species == "Iris-virginica","SepalWidthCm"])

iris_ds<-cbind(iris_ds,iris_ds$SepalLengthCm/iris_ds$SepalWidthCm)
iris_ds<-cbind(iris_ds,iris_ds$PetalLengthCm/iris_ds$PetalWidthCm)
colnames(iris_ds)[7]<- "Ratio Sepal"
colnames(iris_ds)[8]<- "Ratio Petal"


iris_ds
