## Postwork Sesión 4

# Ahora investigarás la dependencia o independencia del número de goles anotados por el equipo de 
# casa y el número de goles anotados por el equipo visitante mediante un procedimiento denominado 
# bootstrap, revisa bibliografía en internet para que tengas nociones de este desarrollo.


# 1. Ya hemos estimado las probabilidades conjuntas de que el equipo de casa anote X=x goles 
#    (x=0,1,... ,8), y el equipo visitante anote Y=y goles (y=0,1,... ,6), en un partido. 

#setwd(choose.dir(caption = "Selecciona el directorio de trabajo"))      # Establecer directorio de trabajo

#leagues <- read.csv("data_frame_postwork2.csv", header = T)             # Carga data frame creado 
leagues  <- read.csv("https://raw.githubusercontent.com/PerezRE/datascience/main/Postworks%20Lenguaje%20R/Postwork%20Sesi%C3%B3n%202/data_frame_postwork2.csv", header = T)

# Resumen postwork 2-3
h.marginal <- as.data.frame(prop.table(table(leagues$FTHG)))
colnames(h.marginal) <- c("Casa", "Marginal")
h.marginal                                                           # Marginales de los goles del equipo local

a.marginal <- as.data.frame(prop.table(table(leagues$FTAG)))
colnames(a.marginal) <- c("Visita", "Marginal")
a.marginal                                                           # Marginales de los goles del equipo visitante

conj.table <- ftable(leagues$FTHG, leagues$FTAG)                         # Tabla de frecuencias
addmargins(conj.table)                                               # Muestra la tabla con marginales

conj.table <- as.data.frame(prop.table(conj.table))                  # Tabla de frecuencias relativas
colnames(conj.table) <- c("Casa", "Visita","Probabilidad_Conjunta")
conj.table

# * Obtén una tabla de cocientes al dividir estas probabilidades conjuntas por el producto 
#   de las probabilidades marginales correspondientes.

product_marginal <- c()
quotients <- c()
i <- 1

for (a in 1:length( a.marginal$Marginal )) {
  for (h in 1:length( h.marginal$Marginal )) {
    product_marginal[i] <- a.marginal$Marginal[a] * h.marginal$Marginal[h]
    quotients[i] <- conj.table$Probabilidad[i] / product_marginal[i]
    i <- i+1
  }  
}

options(scipen = 999) 
table.quotients <- cbind( conj.table,product_marginal, quotients )

# Tabla de cocientes
table.quotients                                                     

# 2. Mediante un procedimiento de boostrap, obtén más quotients similares a los 
#    obtenidos en la tabla del punto anterior. Esto para tener una idea de las 
#    distribuciones de la cual vienen los quotients en la tabla anterior. 

# *  Menciona en cuáles casos le parece razonable suponer que los quotients de la 
#    tabla en el punto 1, son iguales a 1 (en tal caso tendríamos independencia 
#    de las variables aleatorias X y Y).

media    <- mean(tabla.quotients$quotients) 
bootstrap <- replicate(n = 10000, sample( tabla.quotients$quotients, replace = T ))
medias   <- apply(bootstrap, 2, mean)

# Varianza 
sqrt(sum((medias-media)^2)/(ncol(bootstrap)-1))

# Visualización de datos

library(ggplot2)

# Muestra
data <- ggplot() + geom_histogram(aes(tabla.quotients$quotients), bins = 20, color = "#0efd90", fill = "#049352") +
  ggtitle("Distribución de la muestra")
data

# Con bootstrap
bootstrapping <- ggplot() + geom_histogram(aes(medias), bins = 20, color = "#3a8cfa", fill = "#044193") + 
  geom_vline(aes(xintercept = media), color = "#fab13a", size = 1) +
  ggtitle("Distribución de medias con boostrap")

bootstrapping
