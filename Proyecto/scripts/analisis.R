library(dplyr)
library(ggplot2)
library(tidyverse)

# TODO: Escribir las interrogantes en el archivo Readme.md (solo están algunas)

#Leemos los datos del data set generado en el anterior script
videogames <- na.omit(read.csv("https://raw.githubusercontent.com/PerezRE/datascience/main/Proyecto/data/dataset.csv", header=TRUE))

#Aseguramos dejar a los desarrolladores independientes en el data frame
videogames<- videogames %>% filter(genre_Indie == TRUE)

#Corrige las filas y las colmnas del data frame
videogames <- videogames[!duplicated(videogames),]
videogames <- videogames %>% mutate(release_date = as.Date(release_date, format = "%Y-%m-%d"))
videogames <- arrange(videogames, release_date)

# Revisamos la estructura del data frame
class(videogames); str(videogames);

# Visualiza un resumen para cada columna
summary(videogames)

# PREGUNTAS...
#=================== ¿Qué videojuego ha sido el más/menos jugado?==========================

columns <- c("name", "release_date", "average_playtime", "median_playtime")
#Menos jugado tomando el promedio
menos.jugado.a <- videogames[which.min(filter(videogames,average_playtime>0)$average_playtime), columns]
#Mas jugado tomando el promedio
mas.jugado.a <- videogames[which.max(videogames$average_playtime), columns]
#El menos jugado tomando la media
menos.jugado.m <- videogames[which.min(filter(videogames,median_playtime>0)$median_playtime), columns]
#El mas jugado tomando la media 
mas.jugado.m <- videogames[which.max(videogames$median_playtime), columns]

#Grafica de average_playtime
annotation <- data.frame(
  x = c(10,10),
  y = c(menos.jugado.a$average_playtime+1500,mas.jugado.a$average_playtime-1500),
  label = c(paste0("< min:",menos.jugado.a$average_playtime, " > ", menos.jugado.a$name), 
            paste0("< max:",mas.jugado.a$average_playtime," > ", mas.jugado.a$name))
)

ggplot(videogames, aes(x=0, y=average_playtime)) +
  geom_boxplot(width = 10, outlier.color = "#f5b041", outlier.alpha = 0.6, size = 0.5) +
  ggtitle("Tiempo promedio jugado") +
  labs(y = "Tiempo[Horas]") +
  theme_minimal() +
  coord_cartesian(xlim = c(-20,20)) +
  geom_hline(yintercept = c(menos.jugado.a$average_playtime, mas.jugado.a$average_playtime), 
             color = c('#a569bd','#12a083'), 
             linetype = 1,
             alpha = 0.5,
             size = .8) +
  geom_label(data=annotation, aes( x=x, y=y, label=label),
             color = c('#a569bd','#12a083'), 
             size=3 , angle=45, fontface="bold" )

#Gráfica de median_playtime
annotation <- data.frame(
  x = c(10,10),
  y = c(menos.jugado.m$median_playtime+1500,mas.jugado.m$median_playtime-1500),
  label = c(paste0("< min:", menos.jugado.m$median_playtime, " > ", menos.jugado.m$name), 
            paste0("< max:", mas.jugado.m$median_playtime,   " > ", mas.jugado.m$name))
)

ggplot(videogames, aes(x=0, y=median_playtime)) +
  geom_boxplot(width = 10, outlier.color = "#9ccc65", outlier.alpha = 0.6, size = 0.5) +
  ggtitle('Medias del tiempo jugado') +
  labs(y = 'Tiempo:[Horas]') +
  theme_minimal() +
  coord_cartesian(xlim = c(-20,20)) +
  geom_hline(yintercept = c(menos.jugado.m$median_playtime, mas.jugado.m$median_playtime), 
             color = c('#2980b9','#a1887f'), 
             linetype = 1,
             alpha = 0.5,
             size = .8) +
  geom_label(data=annotation, aes( x=x, y=y, label=label),
             color= c('#2980b9','#a1887f'), 
             size=3 , angle=45, fontface="bold" )

#===================== ¿Qué videojuego es el mejor/peor valorado?==========================
columns <- c('name', 'positive_ratings', 'negative_ratings', 'release_date')
videogames[which.min(videogames$positive_ratings), columns]
videogames[which.min(videogames$negative_ratings), columns]

videogames[which.max(videogames$positive_ratings), columns]
videogames[which.max(videogames$negative_ratings), columns]


# ¿Cual es el desarrollador mas popular?

# Desarrolladores con más juegos publicados
desarrolladores<-as.data.frame(table(videogames$developer))
names(desarrolladores)<-c('desarrolador','frecuencia')
summary(desarrolladores)
head(desarrolladores%>%arrange(desc(frecuencia)))

# # ¿Cual es el videojuego mas caro/barato?
columns <- c('name', 'release_date', 'price', 'publisher')
videogames[which.min(videogames$price), columns]
videogames[which.max(videogames$price), columns]

# ¿Cuales son los géneros más implementados en los juegos?
data.genres<- videogames %>% select(grep(pattern = "genre_", names(videogames)))
colnames(data.genres) <- gsub("genre_","",names(data.genres))

data.genres<- as.data.frame(cbind(names(data.genres),as.vector(colSums(data.genres[,names(data.genres)]))))
data.genres<- data.genres %>% mutate(V2 = as.numeric(V2)) %>% filter(V2>0 & V1!="Indie")

ggplot(data.genres, aes(V1,V2, fill = V1)) +
  geom_bar(stat = 'identity') +
  labs(x = 'Género', y = 'Cantidad de Juegos', title = 'Frecuencia de Géneros') +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90)
  )
summary(data.genres)
tail(data.genres%>%arrange(V2))

# ¿Cuales son las categorías más implementadas en los juegos?
data.categories<- videogames %>% select(grep(pattern = "categorie_", names(videogames)))
colnames(data.categories) <- gsub('categorie_','',names(data.categories))

data.categories<- as.data.frame(cbind(names(data.categories),as.vector(colSums(data.categories[,names(data.categories)]))))
data.categories<- data.categories %>% mutate(V2 = as.numeric(V2)) %>% filter(V2>0)

ggplot(data.categories, aes(V1,V2, fill = V1)) +
  geom_bar(stat = 'identity') +
  labs(x = 'Categoria', y = 'Cantidad de Juegos', title = 'Frecuencia de Categorias') +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90)
  )

summary(data.categories)
tail(data.categories %>% arrange(V2))


# ¿Cual es la plataforma preferida por los desarrolladores?
data.plataforms<- videogames %>% select(grep(pattern = 'platform_', names(videogames)))
colnames(data.plataforms) <- gsub('platform_','',names(data.plataforms))

data.plataforms<- as.data.frame(cbind(names(data.plataforms),as.vector(colSums(data.plataforms[,names(data.plataforms)]))))
data.plataforms<- data.plataforms %>% mutate(V2 = as.numeric(V2)) %>% filter(V2>0)

ggplot(data.plataforms, aes(V1,V2, fill = V1)) +
  geom_bar(stat = 'identity') +
  labs(x = 'Plataformas', y = 'Cantidad de Juegos', title = 'Plataformas') +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90)
  )

summary(data.plataforms)
tail(data.plataforms %>% arrange(V2))


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
ggplot(videogames, aes(average_playtime)) + 
  geom_histogram(binwidth = 10, col="white", fill="orange", alpha=0.8) + 
  ggtitle("Histograma de horas jugadas en promedio") + 
  ylab("Horas jugadas en promedio") + 
  xlab("Videojuego") + 
  theme_minimal()

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