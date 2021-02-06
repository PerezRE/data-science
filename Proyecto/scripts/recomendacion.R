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

# Seleccionar variables definidas para categorias.
categories <- videogames %>% select(starts_with("Categorie"))

# Convertir de tipo lógico a númerico.
categories <- na.omit(lapply(categories[, colnames(categories)], as.numeric))

# Eliminar columna de NA's generadas en el paso anterior.
categories$categories <- NULL

# Entrada simualada de los generos de un videojuego de un "usuario". (Vector generado de forma aleatoria).
input <- floor(runif(length(categories), min=0, max=2))

# A) Se calculan las similitudes a partir del vector input, donde cada índice del vector representa una categoria.
# Para cada juego (row-fila) en categories, hacer:
#   similarities.append(videojuego_id, cos_sim(input, categories[row,]))
# similarities.sort.desc()
# obtener los primeros n videojuegos similares y mostrarlos.

# i.e. if cos_sim(input, categories[row, ]) == 0: 
#         "No hay similitud, son vectores ortogonales."
#       elseif cos_sim == 1:
#         "Es 100% seguro que al usuario le gusten las categorias marcadas como 1 en categories[row,] (Del Videojuego en sí)."
#       else cos_sim()
#         "Hay cierto grado de similitud entre el input y el videojuego (categories[row,])."


# Generos de videojuegos.
# Seleccionar variables definidas para generos.
genres <- videogames %>% select(starts_with("genre"))

# Convertir de tipo lógico a númerico.
genres <- na.omit(lapply(genres[, colnames(genres)], as.numeric))

# Eliminar columna de NA's generadas en el paso anterior.
genres$genres <- NULL

# Entrada simualada de generos de un "usuario". (Vector generado de forma aleatoria).
input <- floor(runif(length(genres), min=0, max=2))

# Aplicar mismo algoritmo que en el inciso A).