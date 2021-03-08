# Obtener las probablidades de que cada género sea jugado.

# Contraste de hipótesis (Aquí no se que carajos se hace, porque sé como interpretar este tema).

# De diversos géneros, determinar, ¿Qué tan viable es que el genero/videojuego sea jugado por horas?

# Determinar si el precio es predecido a partir de las variables: 
#   positive_ratings, negative_ratings, plataforma (Con esto, creo que sería generar tres modelos, es decir, un modelo para cada plataforma: linux, windows, mac),
#   average_playtime, meadian_playtime, achivements, release_date.
# Y: Price (Precio): el precio (en USD) del videojuego.
# X1: positive_ratings: Valuación positiva del jugador.
# X2: negative_ratings: Valuación negativa del jugador.
# X3: plataforma: Linux, Windows, Mac.
# X4: average_playtime: Horas jugadas por promedio.
# X5: achivements: Logros desbloqueables.
# X6: release_date: Fecha de lanzamiento.
model <- lm(videogames$price ~ videogames$achievements + videogames$release_date)
summary(model)

# Recomendaciones

# Vector normal
norm_vec <- function(x) sqrt(sum(x^2))

# Cosine angle
cos_sim <- function(a, b) {
  norm_a <- norm_vec(a)
  norm_b <- norm_vec(b)
  # 0 no similitud | 1 similitud
  return(dot(a, b) / (norm_a * norm_b))
}

# Similitudes
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


# ¿Cuales son las categorías más implementadas en los juegos?
categories<- videogames %>% select(starts_with('categorie_'))
colnames(categories) <- gsub('categorie_','',names(categories))

categories<- as.data.frame(cbind(names(categories),apply(categories, 2, sum)))
categories<- categories %>% 
  mutate(V2 = as.numeric(V2))

ggplot(categories, aes(V1,V2, fill = V1)) +
  geom_bar(stat = 'identity') +
  labs(x = 'Categoria', y = 'Cantidad de Juegos', title = 'Frecuencia de Categorias') +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90)
  )

summary(categories)
tail(categories %>% arrange(V2))
