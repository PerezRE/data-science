library(tidyverse)

#Importamos los datos
datos<-read.csv("compose_data.csv")
juegos<-read.csv("games-features.csv")

#Estructura de los datos
str(datos)
str(juegos)

#Hacemos las transformaciones necesarias para combinar el dataset
juegos<- juegos%>%
          mutate(nombre=str_replace_all(gsub(" ","",toupper(juegos$QueryName)),c("-"="",":"="")))%>%
          arrange(nombre)%>%
          select(SteamSpyOwners,PriceInitial,nombre)


datos<- datos%>%mutate(nombre=str_replace_all(gsub(" ","",toupper(datos$name)),c("-"="",":"="")))%>%
        arrange(nombre)

#Juntamos los dataframes por la columna nombre
data.Juegos<-merge(datos,juegos,by="nombre")

#Eliminamos la columna que creamos para juntarlos
data.Juegos<-select(data.Juegos,-nombre,-platforms,-categories,-genres)

#Generamos el archivo csv sobre el que trabajaremos
write.csv(data.Juegos,file="DatosJuegos.csv")