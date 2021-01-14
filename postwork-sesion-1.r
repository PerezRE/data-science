## Postwork Sesion 1

# Importa los datos de soccer de la temporada 2019/2020 de la primera división de la liga española a R, los datos los puedes encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php
liga.futbol = data.frame(read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv"))

# Del data frame que resulta de importar los datos a R, extrae las columnas que contienen los números de goles anotados por los equipos que jugaron en casa (FTHG) y los goles anotados por los equipos que jugaron como visitante (FTAG)
home.goals <- liga.futbol[,"FTHG"]
away.goals <- liga.futbol[,"FTAG"]

# Consulta cómo funciona la función table en R al ejecutar en la consola ?table
?table

#Posteriormente elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:
h.freq.table <- table(home.goals)
a.freq.table <- table(away.goals)

total.home.goals <- sum(h.freq.table)
total.away.goals <- sum(a.freq.table)


# * La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x = 0, 1, 2, ...)
i <- 0 # Número de goles
for (x in h.freq.table) {
  print(paste("P(", i, "): ", x / total.home.goals))
  i <- i+1
}

# * La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y = 0, 1, 2, ...)
i <- 0 # Número de goles
for (y in a.freq.table) {
  print(paste("P(", i, "): ", y / total.away.goals))
  i <- i+1
}

# * La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y goles (x = 0, 1, 2, ..., y = 0, 1, 2, ...)
#Aqui saque la probabilidad para cada x y cada y dentro de las tablas de fecuencias y todo lo guarde en una matriz 
vector.goles<-c()

for(x in h.freq.table){
  for(y in a.freq.table){
    vector.goles<-c(vector.goles,((x/total.home.goals)*(y/total.away.goals)))
  }
}

matriz.goles<-matrix(vector.goles,length(h.freq.table),length(a.freq.table))
row.names(matriz.goles)<-c(0,1,2,3,4,5,6)
colnames(matriz.goles)<-c(0,1,2,3,4)
matriz.goles