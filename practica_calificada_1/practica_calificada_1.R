
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
# Se usa la funcion libreria para activar el paquete readr con el fin de usar las funciones de importacion de datos
library(readr)
# Se crea el objeto enlace1 para WorldCupMatches.csv
enlace1 <- "https://raw.githubusercontent.com/ChristianChiroqueR/Diplomado-2022-Fundamentos-R/main/BDs/WorldCupMatches.csv"
# Se crea el objeto enlace2 para WorldCup.csv
enlace2 <- "https://raw.githubusercontent.com/ChristianChiroqueR/Diplomado-2022-Fundamentos-R/main/BDs/WorldCups.csv"
# Se genera las funciones para leer las bases de datos en formato CSV
WCMatches <- read_csv(enlace1)
WC <- read_csv(enlace2)
# Se genera las funciones para revisar y abrir las bases de datos en formato dataframe
str(WCMatches)
WCMatches
str(WC)
WC

## Ejercicio b ----
## Genere una lista con los países donde se ha realizado una copa mundial.
## Sólo deben figurar valores únicos.

# Se activa la libreria para manipulación de datos
library(dplyr)
# Se usa la siguiente función para seleccionar una o más variables de la base de datos según corresponda. 
# En este caso, .data= permite identificar el DataFrame desesado, WC, luego se especifica el nombre de la variable.
# Revisando la base de datos, se identificó que "Country" en WC refiere a los países donde se realizó una copa mundial.
select(.data= WC, Country)

## Ejercicio c ----
## Presente cuál fue el estadio que ha acogido a la mayor cantidad
## de espectadores o asistentes en la historia de los mundiales


## Ejercicio d ----
## Indique cuál es el referee que más partidos ha arbitrado


## Ejercicio e ----
## Cuál es el partido en el que se anotaron más goles


## Ejercicio f ----
## En la base “WordCupMatches” cree las siguientes nuevas variables:
## equipo ganador, equipo 2do puesto y equipo 3er puesto


## Ejercicio g ----
## En la base “WorldCups” genere dos columnas que indiquen: 
## 1) el número de golesanotados por el campeón (primer puesto), y
## 2) el número de goles anotados por equipo que quedó segundo puesto 


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

