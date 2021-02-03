library(tidyverse)

#Importamos los datos
datos<-read.csv("DatosJuegos.csv")
juegos<-read.csv("metacritic_pc.csv")

#Estructura de los datos
str(datos)
str(juegos)

#Hacemos las transformaciones necesarias para combinar el dataset
juegos<- juegos%>%
  mutate(nombre=str_replace_all(gsub(" ","",toupper(juegos$Title)),c("-"="",":"="","'"="")))%>%
  arrange(nombre)%>%
  select(-Release.Date)


datos<- datos%>%mutate(nombre=str_replace_all(gsub(" ","",toupper(datos$name)),c("-"="",":"="","'"="")))%>%
  arrange(nombre)

data.Juegos<-merge(datos,juegos,by="nombre")

data.Juegos<-select(data.Juegos,-nombre)

write.csv(data.Juegos,file="DatosJuegosV2.csv")