library(tidyverse)

# Abre el documento a decomponer
data <- read.csv("https://raw.githubusercontent.com/PerezRE/datascience/main/Proyecto/data/Steam_games.csv", header = TRUE, encoding = "UTF-8")

# ========================================================================================
# Función para extraer opciones únicas

optUniques <- function(vect){
  
  filter.one <- unique(vect)
  
  optu <-c()
  for (i in 1:length(filter.one)) {
    optu <- c( optu, unlist(strsplit( filter.one[i], ";" )))  
  }
  
  return(unique(optu))
}

# =========================================================================================
# Generador de columnas logicas

logicalCols <- function(names, vec, nameInject){
  
  data<-c()
  for (i in 1:length(vec)) {                         # Número de registros
    
    vLogical <- rep(FALSE, times = length(names))    # Vector FALSE
    rCurrent <- unlist(strsplit( vec[i], ";" ))      # Descompone la cadena de cada fila
    
    # Verifica coincidencia de manera ordenada
    
    for (j in 1:length(rCurrent)) {
      vLogical[which(names == rCurrent[j])] = TRUE;  # Asigna coincidencias
    }
    
    data <- rbind(data, vLogical)                    # Une los datos
  }
  
  for (i in 1:length(names)) {                       # Asigna nombre clave a la columna
    names[i]<-paste0(nameInject, names[i])     
  }
  colnames(data)<-names
  
  return(data)                                       # Retorna columna
}

# ======================== Extrae vectores de búsqueda de datos ===========================

platformss     <- optUniques(data$platforms)
categoriess    <- optUniques(data$categories)
genress        <- optUniques(data$genres)

# ========================= Genera nuevas columnas para unir ==============================

platformss_c  <- logicalCols(platformss, data$platforms,  "platform_")
categoriess_c <- logicalCols(categoriess,data$categories, "categorie_")
genress_c     <- logicalCols(genress,    data$genres,     "genre_")


# Une columnas
data <- cbind(data, platformss_c, categoriess_c, genress_c)

#==========================Unir con otro dataset======================================#
#importamos el dataset con el que vamos a unir
juegos<-read.csv("https://raw.githubusercontent.com/PerezRE/datascience/main/Proyecto/data/metacritic_pc.csv")

#Hacemos las transformaciones necesarias para combinar el dataset
juegos<- juegos%>%
  mutate(nombre=str_replace_all(gsub(" ","",toupper(juegos$Title)),c("-"="",":"="","'"="")))%>%
  arrange(nombre)%>%
  select(-Release.Date)


datos<- data%>%mutate(nombre=str_replace_all(gsub(" ","",toupper(data$name)),c("-"="",":"="","'"="")))%>%
  arrange(nombre)

#Juntamos los datastes por nombres
data.Juegos<-merge(datos,juegos,by="nombre")

#Eliminamos lo que no necesitamos
data.Juegos<-select(data.Juegos,-nombre,-platforms,-categories,-genres)
data.Juegos<-select(data.Juegos,-nombre)
data.Juegos<-select(data.Juegos,-Title)
data.Juegos<-select(data.Juegos,-Columna1)
data.Juegos<-select(data.Juegos,-appid)

#imprimimos el csv
write.csv(data.Juegos,file="DatosJuegos.csv")
