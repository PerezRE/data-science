#Postwork 2

#1.-Importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 2019/2020 
#de la primera división de la liga española a R

liga.2020<-read.csv("SP1.csv")
liga.2019<-read.csv("SP2.csv")
liga.2018<-read.csv("SP3.csv")

#2.-Obten una mejor idea de las características de los data frames al usar las 
#funciones: str, head, View y summary

#Temporada 2019-2020
str(liga.2020)
head(liga.2020)
View(liga.2020)
summary(liga.2020)

#Temporada 2018-2019
str(liga.2019)
head(liga.2019)
View(liga.2019)
summary(liga.2019)

#Temporada 2017-2018
str(liga.2018)
head(liga.2018)
View(liga.2018)
summary(liga.2018)