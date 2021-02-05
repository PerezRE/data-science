library(dplyr)
library(ggplot2)
library(tidyverse)

# TODO: Escribir las interrogantes en el archivo Readme.md (solo están algunas)

# Leemos los datos
videogames <- na.omit(read.csv("https://raw.githubusercontent.com/PerezRE/datascience/main/Proyecto/data/dataset.csv", header=TRUE))

videogames<-videogames%>% filter(genre_Indie=TRUE)
# Revisamos la estructura del data frame
class(videogames); str(videogames);

# Visualizamos un resumen para cada variable de los dtouch Proye  btenidos.
summary(videogames)

# Correción del data frame
videogames <- na.omit(videogames)
videogames <- videogames[!duplicated(videogames$name),]
videogames <- videogames %>% mutate(release_date = as.Date(release_date, format = "%Y-%m-%d"))
videogames <- arrange(videogames, release_date)
names(videogames)


#=================== ¿Qué videojuego ha sido el más/menos jugado?==========================
columns <- c("name", "release_date", "average_playtime", "median_playtime")
#Menos jugado tomando el promedio
videogames[which.min(filter(videogames,average_playtime>0)$average_playtime), columns]
#Mas jugado tomando el promedio
videogames[which.max(videogames$average_playtime), columns]
#El menos jugado tomando la media
videogames[which.min(filter(videogames,median_playtime>0)$median_playtime), columns]
#El mas jugado tomando la media 
videogames[which.max(videogames$median_playtime), columns]

#Grafica de average_playtime

#Grafica de median_playtime

#===================== ¿Qué videojuego es el mejor/peor valorado?==========================
columns <- c("name", "positive_ratings", "negative_ratings", "release_date")
videogames[which.min(videogames$positive_ratings), columns]
videogames[which.min(videogames$negative_ratings), columns]
videogames[which.max(videogames$positive_ratings), columns]
videogames[which.max(videogames$negative_ratings), columns]

# ¿Cual es el desarrollador mas popular?

# # ¿Cual es el videojuego mas caro/barato?
columns <- c("name", "release_date", "price", "publisher")
videogames[which.min(videogames$price), columns]
videogames[which.max(videogames$price), columns]

#¿Que desarrollador obtiene los mejores puntajes de la critica/usuarios?

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