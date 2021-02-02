# Postwork 6

# Importa el conjunto de datos match.data.csv a R y realiza lo siguiente:
  
# 1. Agrega una nueva columna sumagoles que contenga la suma de goles por partido.

library(lubridate) 
library(dplyr)
library(ggplot2)

url <- "https://raw.githubusercontent.com/beduExpert/Programacion-con-R-Santander/master/Sesion-06/Postwork/match.data.csv"

match.data.csv <- read.csv(url)
match.data.csv <- arrange(match.data.csv, date)
match.data.csv <- match.data.csv %>% mutate( date = as.Date(date, "%Y-%m-%d"), sumagoles = home.score + away.score )
match.data.csv


# 2. Obtén el promedio por mes de la suma de goles.

match.data.csv <- match.data.csv %>% 
                  group_by(year(date), month(date)) %>% 
                  summarise(promedio_goles_mes = mean(sumagoles)) %>%
                  rename(year.date = "year(date)", month.date = "month(date)" ) %>% 
                  mutate(dates = as.Date(paste0(year.date,"-",month.date,"-01"), "%Y-%m-%d"))

match.data.csv <- match.data.csv[,-1:-2]  # Quita la columna de años y meses
match.data.csv

# 3. Crea la serie de tiempo del promedio por mes de la suma de goles hasta diciembre de 2019.

date.start   <- match.data.csv$dates[1]
time.serie <- ts(match.data.csv$promedio_goles_mes, start = c(year(date.start), month(date.start), day(date.start)), 
                end = c(2019, 12, 1), frequency = 12)

plot(time.serie, xlab = "Años", ylab = "Promedio")
title(main = "ts: promedio de goles por mes")

