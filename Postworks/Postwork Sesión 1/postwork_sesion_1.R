## Postwork Sesión 1

# 1. Importa los datos de soccer de la temporada 2019/2020 de la primera división de la liga española a R, los datos los puedes encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php
league.futbol = data.frame(read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv"))

# 2. Del data frame que resulta de importar los datos a R, extrae las columnas que contienen los números de goles anotados por los equipos que jugaron en casa (FTHG) y los goles anotados por los equipos que jugaron como visitante (FTAG)
home.goals <- league.futbol$FTHG
away.goals <- league.futbol$FTAG

# 3. Consulta cómo funciona la función table en R al ejecutar en la consola ?table
?table

# Posteriormente elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:
h.freq.table <- table(home.goals)
a.freq.table <- table(away.goals)

addmargins(h.freq.table)
addmargins(a.freq.table)

# * La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x = 0, 1, 2, ...)
marginal.home <- prop.table(h.freq.table)
as.data.frame(marginal.home)

# * La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y = 0, 1, 2, ...)
marginal.away <- prop.table(a.freq.table)
as.data.frame(marginal.away)

# * La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y goles (x = 0, 1, 2, ..., y = 0, 1, 2, ...)
# Aqui saque la probabilidad para cada x y cada y dentro de las tablas de fecuencias y todo lo guarde en una matriz 

contingency.table <- ftable(home.goals,away.goals)
contingency  <- prop.table(contingency.table)
contingency  <- as.matrix(contingency)
contingency
