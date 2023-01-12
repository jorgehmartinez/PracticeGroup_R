
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


## Ejercicio b ----
## Genere una lista con los países donde se ha realizado una copa mundial.
## Sólo deben figurar valores únicos.


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

