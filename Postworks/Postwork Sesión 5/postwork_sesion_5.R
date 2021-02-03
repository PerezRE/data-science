# Postwork5

# 1. A partir del conjunto de datos de soccer de la liga española de las temporadas 2017/2018, 2018/2019 y 2019/2020, 
# crea el data frame SmallData, que contenga las columnas date, home.team, home.score, away.team y away.score; 
# esto lo puede hacer con ayuda de la función select del paquete dplyr. Luego establece un directorio de trabajo y 
# con ayuda de la función write.csv guarda el data frame como un archivo csv con nombre soccer.csv. Puedes colocar 
# como argumento row.names = FALSE en write.csv.

library(dplyr)

league.2019.2020 <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")
league.2018.2019 <- read.csv("https://www.football-data.co.uk/mmz4281/1819/SP1.csv")
league.2017.2018 <- read.csv("https://www.football-data.co.uk/mmz4281/1718/SP1.csv")

SmallData <- list(league.2019.2020, league.2018.2019, league.2017.2018)

SmallData <- lapply(SmallData, select, Date, HomeTeam, FTHG, AwayTeam,  FTAG)

SmallData[1:2] <- SmallData[1:2] %>% 
  lapply(mutate, Date = as.Date(Date, format = "%d/%m/%Y"))

SmallData[3]   <- SmallData[3]   %>% 
  lapply(mutate, Date = as.Date(Date, format = "%d/%m/%y"))
  
SmallData   <- SmallData   %>% 
  lapply(rename, date = Date, home.team = HomeTeam, home.score = FTHG, away.team = AwayTeam, away.score = FTAG)

SmallData <- do.call("rbind", SmallData)

write.csv(SmallData, file = "soccer.csv", row.names = F)

# 2. Con la función create.fbRanks.dataframes del paquete fbRanks importe el archivo 
#    soccer.csv a R y al mismo tiempo asignelo a una variable llamada listasoccer. 
#    Se creará una lista con los elementos scores y teams que son data frames listos 
#    para la función rank.teams. Asigna estos data frames a variables llamadas anotaciones y equipos.

#install.packages("fbRanks")
library(fbRanks)

# Abrir archivo desde equipo
# listasoccer <- create.fbRanks.dataframes(scores.file = "soccer.csv", date.format="%Y-%m-%d")

# Abrir archivo desde repositorio
url.soccer <- "https://raw.githubusercontent.com/PerezRE/datascience/main/Postworks%20Lenguaje%20R/Postwork%20Sesi%C3%B3n%205/soccer.csv"
listasoccer <- create.fbRanks.dataframes(scores.file = url.soccer, date.format="%Y-%m-%d")

anotaciones <- listasoccer$scores
equipos     <- listasoccer$teams

# 3. Con ayuda de la función unique crea un vector de fechas (fecha) que no se repitan y que correspondan a las fechas en las que 
#    se jugaron partidos. Crea una variable llamada n que contenga el número de fechas diferentes. Posteriormente, con la función 
#    rank.teams y usando como argumentos los data frames anotaciones y equipos, crea un ranking de equipos usando unicamente datos 
#    desde la fecha inicial y hasta la penúltima fecha en la que se jugaron partidos, estas fechas las deberá especificar en max.date 
#    y min.date. Guarda los resultados con el nombre ranking.

fecha <- unique(SmallData$date)
fecha <- sort(fecha)

n <- length(fecha)

ranking <- rank.teams(scores = anotaciones, 
                      teams = equipos, 
                      max.date = fecha[n-1], 
                      min.date = fecha[1], 
                      date.format = "%Y-%m-%d")

# 4. Finalmente estima las probabilidades de los eventos, el equipo de casa gana, el equipo visitante gana o el resultado es 
# un empate para los partidos que se jugaron en la última fecha del vector de fechas fecha. Esto lo puedes hacer con ayuda 
# de la función predict y usando como argumentos ranking y fecha[n] que deberá especificar en date

predict(ranking, date = fecha[n])
