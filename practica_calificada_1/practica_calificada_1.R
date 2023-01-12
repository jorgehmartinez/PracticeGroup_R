
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
###Descarga de Library
library(rio)
library(tidyverse)
library(datos)
###Archvio desde Github
link<-"https://raw.githubusercontent.com/ChristianChiroqueR/Diplomado-2021---R-Intermedio/main/vacunados_apurimac.csv"
vacunas_GH<-read.csv(link) #Asignamos la data anterior al objeto vacunas_GH
class(vacunas_GH) #visualiza el tipo de datos del objeto vacunas_GH
head(vacunas_GH) #visualiza las primeras filas de vacunas_GH
vacunas_tb<-as.tibble(vacunas_GH) #Convierte a la clase de datos tibble
class(vacunas_tb) #verifica que los datos del objeto sean del tipo tibble
head(vacunas_tb) #visualiza las 6 primeras filas de vacunas_tb
names(vacunas_tb) #visualiza los nombres de todas las variables.

## Ejercicio b ----
## Cree una nueva variable (de nombre “fecha”) 
## usando la variable “FECHA_VACUNACION”.
## Puede utilizar la función ymd () del paquete “lubridate”
View(vacunas_tb) #Visualiza la base de datos
library(lubridate) 
FECHA<-vacunas_tb[[6]] #Crea variable FECHA en base a FECHA_VACUNACION
FECHA<-ymd(FECHA) #Convierte la variable FECHA de integer a date
class(FECHA) #verifica el tipo de dato de la variable fecha

## Ejercicio c ----
## Cree una nueva data que agregue los datos según fecha
## para saber el total de vacunados por día.
## ¿Qué día se realizó la mayor cantidad de vacunas en Apurímac?
vacunas_vf<-vacunas_tb |> 
  mutate(FECHA) |> #crea una nueva data incorporando la variable FECHA
  count(FECHA)  |> #Cuenta el numero de vacunas por fecha
  arrange(desc(n)) #Ordena de descendentemente el número de vacunas
vacunas_vf #visuaiza el resultado
#El dia que se tuvo mayor número de vacunas fue el 28/8/2021

## Ejercicio d ----
## ¿Cuáles son los tres distritos que tiene una mayor población vacunada
## con la 3ra dosis?

tres_vacunas<-vacunas_vf |> #Asigna al objeto tres_vacunas aquellos q datos q cuentan con la tercera dosis
  filter(DOSIS==3) |> #filtra aquellos distritos q sólo tienen l terdera dosis
  count(DISTRITO) |> #cuenta el numero de distritos con tercera dosis
  arrange(desc(n)) # ordena de forma descendente los datos
tres_vacunas #visualiza el resultado
#LOs distritos con mayor número de vacunados con tercera dosis son: ABANCAY (11805),ANDAHUAYLAS(5833) y TALAVERA (2264)  

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
library(tidyverse)
# Se usa la siguiente función para seleccionar una o más variables de la base de datos según corresponda. 
# En este caso, .data= permite identificar el DataFrame desesado, WC, luego se especifica el nombre de la variable.
# Revisando la base de datos, se identificó que "Country" en WC refiere a los países donde se realizó una copa mundial.
select(.data= WC, Country)

## Ejercicio c ----
## Presente cuál fue el estadio que ha acogido a la mayor cantidad
## de espectadores o asistentes en la historia de los mundiales
# Para este ejercicio se usará el operador Pipe
# Se usa la funcion summarise ya que contiene diversos cálculos estadísticos, como valor máximo.
# En este caso, se usa el argumento na.rm para indicar si se incluye los valores perdidos. Para ello, se indica na.rm=TRUE para no devuelva NA como resultado. 
WCMatches |> 
  summarise(max(Attendance, na.rm=TRUE))
# Luego de identificar el máximo valor de asistentes en la historia del mundial, 
# se usará la función filter para identificar el caso correspondiente
# y luego se seleccionará el estadio que acogio a esa cantidad máxima de asistentes
WCMatches |> 
  filter(Attendance == 173850) |> 
  select(Stadium, Attendance)

## Ejercicio d ----
## Indique cuál es el referee que más partidos ha arbitrado
# Se usa la función count para contar la variable solicitada en la pregunta. 
# Además, dado que existe más 357 árbitos, se uso el argumento sort para ordenar de mayor a menor segun la frecuencia.
# De esa manera, se identifica en la fila 2 el árbitro o referee con más partidos arbitrados.
WCMatches |> 
  count(Referee, sort = TRUE)

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
#para obtener lo solicitado se debe cumplir las siguientes condiciones:
#si Home.Team.Name es igual a Winner se sumara Home.Team.Goals
#si Away.Team.Name es igual a Winner se sumara Away.Team.Goals

WorldCups_full |>
  filter(Winner==Home.Team.Name | Winner==Away.Team.Name)


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

