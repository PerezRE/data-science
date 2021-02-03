library(dplyr)
library(ggplot2)

# TODO: Escribir las interrogantes en el archivo Readme.md (solo están algunas)

# Leemos los datos
videogames <- na.omit(read.csv("https://raw.githubusercontent.com/PerezRE/datascience/main/Proyecto/data/videojuegos_2.csv"))

# Revisamos la estructura del data frame
class(videogames); str(videogames);
# Visualizamos un resumen para cada variable de los datos obtenidos.
summary(videogames)

# ¿Qué género/videojuego resultó con mayor/menor descuento?


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
# 3. Graficar series de tiempo para generos (eje x) contra precios (eje y).
# 4. Graficar series de tiempo para generos (eje x) contra horas jugadas (eje Y) y analizar si hay una tendencia a la alta en un par de generos.
# 5. Analizar si existe alguna correlación entre el genero y las horas jugadas.

# Tratando de graficar un histograma de las horas jugadas y el nombre del videojuego.
plot(videogames$positive_ratings)
ggplot(videogames) + 
  aes(average_playtime) + 
  geom_histogram(binwidth = 10, col="white", fill="orange", alpha=0.8) + 
  ggtitle("Histograma de horas jugadas en promedio") + 
  ylab("Horas jugadas en promedio") + 
  xlab("Videojuego") + 
  theme_light()


dev.off()

# Obtener las probablidades de que cada género sea jugado.

# Contraste de hipótesis (Aquí no se que carajos se hace, porque sé como interpretar este tema).

# De diversos géneros, obtener, ¿Qué tan viable es que el genero/videojuego sea jugado por horas?



# Determinar el precio dado 
## TODO: 
# Del ejepmplo: m2 <- lm(Price ~ Food + Decor + East)
# summary(m2)

# Determinar si el precio es predecido a partir de las variables: 
#   positive_ratings, negative_ratings, plataforma (Con esto, creo que sería generar tres modelos, es decir, un modelo para cada plataforma: linux, windows, mac), 
#   average_playtime, meadian_playtime, achivements.

# Y: Price (Precio): el precio (en USD) de la cena
# X1: positive_ratings: Valuación del cliente de la comida (sacado de 30)
# X2: negative_ratings: Valuación del cliente de la decoración (sacado de 30)
# X3: plataforma: Valuación del cliente del servicio (sacado de 30)
# X4: average_playtime: variable dummy: 1 (0) si el restaurante está al este (oeste) de la quinta avenida
# X5: achivements: variable dummy: 1 (0) si el restaurante está al este (oeste) de la quinta avenida