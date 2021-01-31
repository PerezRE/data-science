# Postwork 4

# Ahora investigarás la dependencia o independencia del número de goles anotados por el equipo de 
# casa y el número de goles anotados por el equipo visitante mediante un procedimiento denominado 
# bootstrap, revisa bibliografía en internet para que tengas nociones de este desarrollo.


# 1. Ya hemos estimado las probabilidades conjuntas de que el equipo de casa anote X=x goles 
#    (x=0,1,... ,8), y el equipo visitante anote Y=y goles (y=0,1,... ,6), en un partido. 

#setwd(choose.dir(caption = "Selecciona el directorio de trabajo"))    # Establecer directorio de trabajo

#ligas <- read.csv("data_frame_postwork2.csv", header = T)            # Carga data frame creado 
ligas  <- read.csv("https://raw.githubusercontent.com/PerezRE/datascience/Develop/Postworks%20Lenguaje%20R/Postwork%20Sesi%C3%B3n%202/data_frame_postwork2.csv", header = T)

# Resumen postwork 2-3
h.marginal <- as.data.frame(prop.table(table(ligas$FTHG)))
colnames(h.marginal) <- c("Casa", "Marginal")
h.marginal                                                           # Marginales de los goles del equipo local

a.marginal <- as.data.frame(prop.table(table(ligas$FTAG)))
colnames(a.marginal) <- c("Visita", "Marginal")
a.marginal                                                           # Marginales de los goles del equipo visitante

conj.table <- ftable(ligas$FTHG, ligas$FTAG)                         # Tabla de frecuencias
addmargins(conj.table)                                               # Muestra la tabla con marginales

conj.table <- as.data.frame(prop.table(conj.table))                  # Tabla de frecuencias relativas
colnames(conj.table) <- c("Casa", "Visita","Probabilidad_Conjunta")
conj.table

# * Obtén una tabla de cocientes al dividir estas probabilidades conjuntas por el producto 
#   de las probabilidades marginales correspondientes.

producto_marginal <- c()
cocientes <- c()
i <- 1

for (a in 1:length( a.marginal$Marginal )) {
  for (h in 1:length( h.marginal$Marginal )) {
    producto_marginal[i] <- a.marginal$Marginal[a] * h.marginal$Marginal[h]
    cocientes[i] <- conj.table$Probabilidad[i] / producto_marginal[i]
    i <- i+1
  }  
}

options(scipen = 999) 

tabla.cocientes <- cbind( conj.table,producto_marginal, cocientes )
tabla.cocientes                                                     

# 2. Mediante un procedimiento de boostrap, obtén más cocientes similares a los 
#    obtenidos en la tabla del punto anterior. Esto para tener una idea de las 
#    distribuciones de la cual vienen los cocientes en la tabla anterior. 

# *  Menciona en cuáles casos le parece razonable suponer que los cocientes de la 
#    tabla en el punto 1, son iguales a 1 (en tal caso tendríamos independencia 
#    de las variables aleatorias X y Y).

media    <- mean(tabla.cocientes$cocientes) 
boostrap <- replicate(n = 10000, sample( tabla.cocientes$cocientes, replace = T ))
medias   <- apply(boostrap, 2, mean)

# Varianza - Error estandar
sqrt(sum((medias-media)^2)/ncol(boostrap))

# Visualización de datos
# Muestra
data <- ggplot() + geom_histogram(aes(tabla.cocientes$cocientes), bins = 20, color = "#0efd90", fill = "#049352") +
  ggtitle("Distribución de la muestra")
data

# Con boostrap
histograma <- ggplot() + geom_histogram(aes(medias), bins = 20, color = "#3a8cfa", fill = "#044193") + 
  geom_vline(aes(xintercept = media), color = "#fab13a", size = 1) +
  ggtitle("Distribución de medias con boostrap")

histograma

# Se puede rechazar que X y Y sean independientes
