
# Información general -----------------------------------------------------

## Curso: Fundamentos de R para CCSS y Gestión Pública
## Asunto: Práctica Calificada N° 1
## Grupo: N°6
## Integrantes: 
## - HINOJOSA CAHUANA, PERCY ALBERTH
## - HUANCA MARTINEZ, JORGE ALBERTO
## - HUANCAYA IDONE, CESAR DANTE
## - TINTAYA ORIHUELA, MEIR ALVARO

# Pregunta 1 --------------------------------------------------------------

## Ejercicio a ----
## Abra la data del repositorio GitHub y describa brevemente.
## Asegurarse que el objeto posea la clase tibble.


## Ejercicio b ----
## Cree una nueva variable (de nombre “fecha”) 
## usando la variable “FECHA_VACUNACION”.
## Puede utilizar la función ymd () del paquete “lubridate”


## Ejercicio c ----
## Cree una nueva data que agregue los datos según fecha
## para saber el total de vacunados por día.
## ¿Qué día se realizó la mayor cantidad de vacunas en Apurímac?


## Ejercicio d ----
## ¿Cuáles son los tres distritos que tiene una mayor población vacunada
## con la 3ra dosis?


# Pregunta 2 --------------------------------------------------------------

## Ejercicio a ----
## Abra las siguientes bases de datos GitHub: 
## WorldCupMatches.csv y WorldCups.csv


## Ejercicio b ----
## Genere una lista con los países donde se ha realizado una copa mundial.
## Sólo deben figurar valores únicos.


## Ejercicio c ----
## Presente cuál fue el estadio que ha acogido a la mayor cantidad
## de espectadores o asistentes en la historia de los mundiales


## Ejercicio d ----
## Indique cuál es el referee que más partidos ha arbitrado


## Ejercicio e ----
library(tidyverse)
## Cuál es el partido en el que se anotaron más goles
#importando los datos de WorldCupMatches
WorldCupMatches <- "https://raw.githubusercontent.com/ChristianChiroqueR/Diplomado-2022-Fundamentos-R/main/BDs/WorldCupMatches.csv"
class(WorldCupMatches)
str(WorldCupMatches)
WorldCupMatches
view(WorldCupMatches)

#importando los datos de WorldCups
WorldCups <- "https://raw.githubusercontent.com/ChristianChiroqueR/Diplomado-2022-Fundamentos-R/main/BDs/WorldCups.csv"
class(WorldCups)
str(WorldCups)
WorldCups
library(tidyverse)
WorldCupMatches <- "https://raw.githubusercontent.com/ChristianChiroqueR/Diplomado-2022-Fundamentos-R/main/BDs/WorldCupMatches.csv"
class(WorldCupMatches)
str(WorldCupMatches)
WorldCupMatches
view(WorldCupMatches)
view(WorldCupMatches)
summarize(WorldCupMatches)
## aca se creo una variable que suma los goles de local y visitante
ejg3<- WorldCupMatches |>  mutate(goles_mas=rowSums(WorldCupMatches[ ,c(7,8)])) #Nueva variable: suma de goles
View(ejg3)
## una vez creada la variable se procede a ver el que tenga mas goles aplicando la funcion max de summarize
ejg3 |>
  summarise(max(goles_mas))
View(ejg3)

## Ejercicio f ----
## En la base “WordCupMatches” cree las siguientes nuevas variables:
## equipo ganador, equipo 2do puesto y equipo 3er puesto
## para este caso se considero como equipo 1er puesto y segundo 
## a los partidos donde se llego a la fase final y la variable 3er
## puesto a los partidos que llegaron a semi finales
library(tidyverse)
WorldCupMatches <- "https://raw.githubusercontent.com/ChristianChiroqueR/Diplomado-2022-Fundamentos-R/main/BDs/WorldCupMatches.csv"

ejf1<- WorldCupMatches |>  mutate(equipo_ganador=Stage=="Final")
View(ejf1)

ejf2<- WorldCupMatches |>  mutate(equipo_2do_puesto=Stage=="Final")
View(ejf2)

ejf3<- WorldCupMatches |>  mutate(equipo_3er_puesto=Stage=="Semi-Finals")
View(ejf3)


## Ejercicio g ----
## En la base “WorldCups” genere dos columnas que indiquen: 
## 1) el número de golesanotados por el campeón (primer puesto), y
## 2) el número de goles anotados por equipo que quedó segundo puesto 
library(tidyverse)
enlace2 <- "https://raw.githubusercontent.com/ChristianChiroqueR/Diplomado-2022-Fundamentos-R/main/BDs/WorldCups.csv"
WorldCups <- read.csv(enlace2)

enlace1 <- "https://raw.githubusercontent.com/ChristianChiroqueR/Diplomado-2022-Fundamentos-R/main/BDs/WorldCupMatches.csv"
WorldCupMatches<- read.csv(enlace1)
library(dplyr)

#para esto se debera unir los dos df
WorldCups_full <- WorldCups |>
  full_join(WorldCupMatches, by = "Year")
view(WorldCups_full)
##ahora se genero una variable booleana si el ganador coincidia con su condicion de local
ejg2 <- WorldCups_full |>
  mutate(ganador_home=Winner==Home.Team.Name)
View(ejg2)
##en este caso se suma los goles anotados por el equipo local si este era el campeon
ejg2 |>
  filter(ganador_home == "True") |> group_by(Year) |> summarise(Total_Home = sum(Home.Team.Goals))

##ahora se genero una variable booleana si el ganador coincidia con su condicion de visitante
ejg3 <- WorldCups_full |>
  mutate(ganador_away=Winner==Away.Team.Name)
View(ejg3)  

##en este caso se suma los goles anotados por el equipo visitante si este era el campeon
ejg2 |>
  filter(ganador_away == "True") |> group_by(Year) |> summarise(Total_Home = sum(Away.Team.Goals))  

  ##para el caso del sub campeon
##se genero una variable booleana si el sub campeo coincidia con su condicion de local
ejg4 <- WorldCups_full |>
  mutate(Sub_campeon_home=Runners.Up==Home.Team.Name)
View(ejg4)
##en este caso se suma los goles anotados por el equipo local si este corresponde
ejg4 |>
  filter(Sub_campeon_home == "True") |> group_by(Year) |> summarise(Total_Home = sum(Home.Team.Goals))

##ahora se genero una variable booleana si el subcampeon coincidia con su condicion de visitante
ejg5 <- WorldCups_full |>
  mutate(Sub_campeon_away=Runners.Up==Away.Team.Name)
View(ejg5)  

##en este caso se suma los goles anotados por el equipo visitante si este era el campeon
ejg4 |>
  filter(Sub_campeon_away == "True") |> group_by(Year) |> summarise(Total_Home = sum(Away.Team.Goals))


# Pregunta 3 --------------------------------------------------------------

## Ejercicio a ----
## Suba la base de datos que utilizarán en su trabajo final a un repositorio de GitHub
## (cree una cuenta para su grupo), cárguela y presente una breve descripción


## Ejercicio b ----
## Elija una variable numérica y cree una tabla con los siguientes descriptivos:
## mínimo, máximo, media, mediana, desviación estándar y rango intercuartílico


## Ejercicio c ----
## Elija una variable categórica y cree una tabla donde muestre
## la frecuencia de las categorías y su porcentaje.

