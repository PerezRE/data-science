## Postwork Sesión 2

#1. Importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 2019/2020 
#   de la primera división de la liga española a R

# Manejo individual
league.2019.2020 <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")
league.2018.2019 <- read.csv("https://www.football-data.co.uk/mmz4281/1819/SP1.csv")
league.2017.2018 <- read.csv("https://www.football-data.co.uk/mmz4281/1718/SP1.csv")

# Manejo en forma de lista
leagues <- list(league.2019.2020, league.2018.2019, league.2017.2018)

#2. Obten una mejor idea de las características de los data frames al usar las 
#funciones: str, head, View y summary

#Temporada 2019-2020
str(league.2019.2020)
head(league.2019.2020)
View(league.2019.2020)
summary(league.2019.2020)

#Temporada 2018-2019
str(league.2018.2019)
head(league.2018.2019)
View(league.2018.2019)
summary(league.2018.2019)

#Temporada 2017-2018
str(league.2017.2018)
head(league.2017.2018)
View(league.2017.2018)
summary(league.2017.2018)

# 3. Con la función select del paquete dplyr selecciona únicamente las columnas 
# Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR; esto para cada uno de los data frames. 
# (Hint: también puedes usar lapply

library(dplyr)

# Individual
league.2019.2020 <- league.2019.2020 %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
league.2018.2019 <- league.2018.2019 %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
league.2017.2018 <- league.2017.2018 %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)

# De manera geneal con lapply
leagues <- lapply(leagues, select, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)

# 4. Asegúrate de que los elementos de las columnas correspondientes de los nuevos 
# data frames sean del mismo tipo (Hint 1: usa as.Date y mutate para arreglar las fechas).

# Contenido de apoyo

# Símbolo       # Significado
# %d            día (numérico, de 0 a 31)
# %a            día de la semana abreviado a tres letras
# %A            día de la semana (nombre completo)
# %m            mes (numérico de 0 a 12)
# %b            mes (nombre abreviado a tres letras)
# %B            mes (nombre completo)
# %y            año (con dos dígitos)
# %Y            año (con cuatro dígitos)

# Individual
league.2019.2020 <- league.2019.2020 %>% mutate(Date = as.Date(Date, format = "%d/%m/%Y"))
league.2018.2019 <- league.2018.2019 %>% mutate(Date = as.Date(Date, format = "%d/%m/%Y"))
league.2017.2018 <- league.2017.2018 %>% mutate(Date = as.Date(Date, format = "%d/%m/%y"))  # Este formato es diferente

# Utilizando la lista 

leagues[1:2] <- leagues[1:2] %>% lapply(mutate, Date = as.Date(Date, format = "%d/%m/%Y"))
leagues[3]   <- leagues[3]   %>% lapply(mutate, Date = as.Date(Date, format = "%d/%m/%y"))  # Este formato es diferente

# * Con ayuda de la función rbind forma un único data frame que contenga las seis 
# columnas mencionadas en el punto 3 (Hint 2: la función do.call podría ser utilizada).

# Unión de los individuales
leagues.union <- rbind(league.2019.2020, league.2018.2019, league.2017.2018)

# Unión utilizando do.call
leagues.union.do <- do.call("rbind", leagues)


# ======================== Ordenar data frame por fecha ===============================

leagues.union    <- arrange(leagues.union,    Date)
leagues.union.do <- arrange(leagues.union.do, Date)

# Comprueba que da el mismo resultado de las dos formas
identical(leagues.union, leagues.union.do)

head(leagues.union)
# ================================ Paso adicional =====================================

# Guarda data frame como csv para postwork 3
write.csv( leagues.union, file = "data_frame_postwork2.csv", row.names = F)


