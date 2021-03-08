install.packages("geometry")
library(dplyr)
library(ggplot2)
library(geometry)
library(data.table)

# Vector normal
norm_vec <- function(x) sqrt(sum(x^2))

# Cosine angle
cos_sim <- function(a, b) {
  norm_a <- norm_vec(a)
  norm_b <- norm_vec(b)
  # 0 no similitud | 1 similitud
  return(dot(a, b) / (norm_a * norm_b))
}

# Dataset
videogames <- na.omit(read.csv("https://raw.githubusercontent.com/PerezRE/datascience/main/Proyecto/data/dataset.csv", header=TRUE))

#Aseguramos dejar a los desarrolladores independientes en el data frame
videogames<- videogames %>% filter(genre_Indie == TRUE)

#Filtrado para eliminar columnas vacías creadas durante la unión de los data sets
s<-c()
for (i in 1:length(videogames)) {
  if(is.logical(videogames[,i])){
    if(sum(videogames[,i]) == 0){
      s <- c(s,i)
    }
  }
}
videogames <- videogames[,-s]

#Corrige las filas y las colmnas del data frame
videogames <- videogames[!duplicated(videogames),]
videogames <- videogames %>% mutate(release_date = as.Date(release_date, format = "%d/%m/%Y"))
videogames <- arrange(videogames, release_date)

# ========================== De aquí en adelante fue lo que se copio ===========================#

Simil <- function(x){
  
  # Seleccionar variables definidas para categorias.
  datos <- videogames %>% select(starts_with(x))
  
  # Convertir de tipo lógico a númerico.
  datos <- na.omit(lapply(datos[, colnames(datos)], as.numeric))
  datos <- as.data.frame(do.call("cbind",datos))
  
  # Entrada simualada de los generos de un videojuego de un "usuario". (Vector generado de forma aleatoria).
  input <- floor(runif(nrow(datos), min=0, max=2))
  
  # A) Se calculan las similitudes a partir del vector input, donde cada índice del vector representa una categoria.
  # Para cada juego (row-fila) en datos, hacer:
  #   similarities.append(videojuego_id, cos_sim(input, datos[row,]))
  
  similarities <- as.data.frame(apply(datos, 2, cos_sim, input))
  colnames(similarities) <- "similitud"
  
  # similarities.sort.desc()
  similarities <- arrange(similarities, desc(similitud))
  
  # obtener los primeros n videojuegos similares y mostrarlos.
  
  # i.e. if cos_sim(input, datos[row, ]) == 0: 
  #         "No hay similitud, son vectores ortogonales."
  #       elseif cos_sim == 1:
  #         "Es 100% seguro que al usuario le gusten las categorias marcadas como 1 en datos[row,] (Del Videojuego en sí)."
  #       else cos_sim()
  #         "Hay cierto grado de similitud entre el input y el videojuego (datos[row,])."

  similarities <- similarities %>% 
    plyr::mutate(mensaje = 
                   dplyr::case_when(similitud == 0 ~ "No hay similitud, son vectores ortogonales",
                                    similitud == 1 ~ "Es 100% seguro que al usuario le guste",
                                    TRUE ~ "Hay cierto grado de similitud"))
  return(similarities)
}

# Categorías de juegos
categories <- Simil("categorie_")

# Generos de juegos
genres <- Simil("genre_")
