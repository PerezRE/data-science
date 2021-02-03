## Postwork Sesión 3

# 1. Con el último data frame obtenido en el postwork de la sesión 2, elabora tablas de frecuencias 
# relativas para estimar las siguientes probabilidades:

# setwd(choose.dir(caption = "Selecciona el directorio de trabajo"))    # Ayuda a establecer directorio de trabajo

# leagues <- read.csv("data_frame_postwork2.csv", header = T)            # Carga último data frame creado
leagues <- read.csv("https://raw.githubusercontent.com/PerezRE/datascience/main/Postworks%20Lenguaje%20R/Postwork%20Sesi%C3%B3n%202/data_frame_postwork2.csv", header = T)

# * La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x=0,1,2,)

h.freq.table <- prop.table(table(leagues$FTHG))
h.freq.table <- as.data.frame(h.freq.table)
h.freq.table

# * La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y=0,1,2,)

a.freq.table <- prop.table(table(leagues$FTAG))
a.freq.table <- as.data.frame(a.freq.table)
a.freq.table

# * La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como 
#   visitante anote y goles (x=0,1,2,, y=0,1,2,)

conj.freq.table <- ftable(leagues$FTHG, leagues$FTAG)
conj.freq.table <- prop.table(conj.freq.table)
addmargins(conj.freq.table)                          # Permite ver que los marginales estén correctos
conj.freq.table <- as.data.frame(conj.freq.table)

# 2. Realiza lo siguiente:

# * Un gráfico de barras para las probabilidades marginales estimadas del número de goles que anota el 
# equipo de casa

library(ggplot2)

marginal.home <- ggplot(h.freq.table, aes(x = Var1, y = Freq, fill = Var1)) + 
  geom_bar(stat = "identity") +
  ggtitle("Equipo Local - Probabilidad marginal") +
  labs(x = "Goles", y = "Probabilidad") + 
  theme(
    legend.position="none"
  )


marginal.home

# * Un gráfico de barras para las probabilidades marginales estimadas del número de goles que anota el 
# equipo visitante

marginal.away <- ggplot(a.freq.table, aes(x = Var1, y = Freq, fill = Var1)) + 
  geom_bar(stat = "identity") +
  ggtitle("Equipo Visitante - Probabilida marginal") +
  labs(x = "Goles", y = "Probabilidad") +
  theme(
    legend.position="none"
  )

marginal.away

# * Un HeatMap para las probabilidades conjuntas estimadas de los números de goles que anotan el 
# equipo de casa y el equipo visitante en un partido.

heatmap.conj <- ggplot(conj.freq.table, aes(x = Var1 , y = Var2, fill = Freq)) + 
  geom_tile() +
  ggtitle("Probabilidades conjuntas") +
  scale_fill_gradientn(colours = rainbow(5)) +
  labs( x = "FTHG", y = "FTAG")

heatmap.conj
