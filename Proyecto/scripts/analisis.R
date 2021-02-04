library(dplyr)
library(ggplot2)

# TODO: Escribir las interrogantes en el archivo Readme.md (solo están algunas)

# Leemos los datos
videogames <- na.omit(read.csv("https://raw.githubusercontent.com/PerezRE/datascience/main/Proyecto/data/videojuegos_2.csv", header=TRUE))

# Eliminar variables redundantes e innecesarias
videogames <- videogames[, -which(names(videogames) %in% c("X.1", "X", "appid", "Columna1", "Title"))]

# Revisamos la estructura del data frame
class(videogames); str(videogames);

# Visualizamos un resumen para cada variable de los datos obtenidos.
summary(videogames)

# ¿Qué videojuego es el mejor/peor valorado?
columns <- c("name", "positive_ratings", "negative_ratings", "release_date")
videogames[which.min(videogames$positive_ratings), columns]
videogames[which.min(videogames$negative_ratings), columns]
videogames[which.max(videogames$positive_ratings), columns]
videogames[which.max(videogames$negative_ratings), columns]

# ¿Qué videojuego ha sido el más/menos jugado?
columns <- c("name", "release_date", "average_playtime", "median_playtime")
videogames[which.min(videogames$average_playtime), columns]
videogames[which.max(videogames$average_playtime), columns]
videogames[which.min(videogames$median_playtime), columns]
videogames[which.max(videogames$median_playtime), columns]

# ¿Qué género resulta más/menos rentable?


# ¿Qué género se ha jugado en los últimos años?
## ¿A través de series de tiempo?

# ¿Qué videojuego resultó más económico y caro?
columns <- c("name", "release_date", "price", "publisher")
videogames[which.min(videogames$price), columns]
videogames[which.max(videogames$price), columns]

# ¿Cuál género resulta más caro/económico?
# TODO:
# 1. Obtener los generos.
# 2. Ver cual resulta más caro & económico.

# Análisis exploratorio
# TODO:
# 1. Graficar cada genero contra las horas jugadas
  # Del ejemplo: my_scatplot + facet_wrap("cyl")
  # Del ejemplo: my_scatplot + facet_grid(am~cyl)
# 2. Graficar genero contra los precios
# 3. Graficar series de tiempo para generos (eje de las abscisas) contra precios (eje de las ordenadas).
# 4. Graficar series de tiempo para generos (eje de las abscisas) contra horas jugadas (eje de las ordenadas) y analizar si hay una tendencia a la alta en un par de generos.
# 5. Analizar si existe alguna correlación entre el genero y las horas jugadas.

# 6. (Intento de) Graficar un histograma de las horas jugadas y el nombre del videojuego.
ggplot(videogames) + 
  aes(average_playtime) + 
  geom_histogram(binwidth = 10, col="white", fill="orange", alpha=0.8) + 
  ggtitle("Histograma de horas jugadas en promedio") + 
  ylab("Horas jugadas en promedio") + 
  xlab("Videojuego") + 
  theme_light()

# 7. Histograma de ratings positivos.
ggplot(videogames) +
  aes(positive_ratings) +
  geom_histogram(binwidth = 10, col="orange", fill="yellow", alpha=0.8) + 
  ggtitle("Histograma de Ratings positivos") + 
  ylab("Rating") + 
  xlab("Videojuego") + 
  theme_light()

# Histograma de ratings negativos.
ggplot(videogames) +
  aes(negative_ratings) +
  geom_histogram(binwidth = 10, col="orange", fill="yellow", alpha=0.8) + 
  ggtitle("Histograma de Ratings positivos") + 
  ylab("Rating") + 
  xlab("Videojuego") + 
  theme_light()

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