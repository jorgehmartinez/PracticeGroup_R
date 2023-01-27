# Información general -----------------------------------------------------

## Curso: Fundamentos de R para CCSS y Gestión Pública
## Asunto: Práctica Calificada N° 2
## Grupo: N°6
## Integrantes: 
## - HINOJOSA CAHUANA, PERCY ALBERTH
## - HUANCA MARTINEZ, JORGE ALBERTO
## - HUANCAYA IDONE, CESAR DANTE
## - TINTAYA ORIHUELA, MEIR ALVARO

# Pregunta 1 --------------------------------------------------------------
## Ejercicio a ----
### Librerias necesarias para el desarrollo de la pregunta
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plyr)
### install.packages("gifski")
library(gifski) # paquete para guardar Gif
### install.packages("zoo")
library(zoo) # paquete para crear media móvil 
### Extracción de los datos de COVID-19
enlace <- "https://files.minsa.gob.pe/s/eRqxR35ZCxrzNgr/download"
### Asignamos la data anterior al objeto casos_covid y usamos el argumento sep para indicar que el separador es ";".
casos_covid <- read.csv(enlace, sep = ";")
head(casos_covid) #Aquí se visualiza las primeras filas de casos_covid
### Transformamos la varaible Fecha_Resultado a un formato de fecha
### A su vez, creamos una variable en base al año y mes
casos_covidR <- casos_covid |> 
  mutate(fecha_n=ymd(casos_covid$FECHA_RESULTADO)) |> 
  mutate(año=as.character(year(fecha_n))) |> 
  mutate(mes=month(fecha_n, label=TRUE)) |> 
  mutate(año_mes = paste(año, mes, sep = "-")) |> 
  arrange(fecha_n)
### Con lo anterior, se procede a crear la variable "n_meses"
casos_covidR$n_meses <- mapvalues(casos_covidR$año_mes, 
                                  from = c("2020-Mar",	"2020-Abr",	"2020-May",	"2020-Jun",	
                                           "2020-Jul","2020-Ago",	"2020-Set",	"2020-Oct",	
                                           "2020-Nov",	"2020-Dic","2021-Ene",	"2021-Feb",	
                                           "2021-Mar",	"2021-Abr",	"2021-May","2021-Jun",	
                                           "2021-Jul",	"2021-Ago",	"2021-Set",	"2021-Oct",	
                                           "2021-Nov", "2021-Dic",	"2022-Ene",	"2022-Feb",
                                           "2022-Mar",	"2022-Abr",	"2022-May",	"2022-Jun",
                                           "2022-Jul",	"2022-Ago",	"2022-Set",	"2022-Oct",
                                           "2022-Nov",	"2022-Dic",	"2023-Ene"),
                                  to = c("1",	"2",	"3",	"4",
                                         "5",	"6",	"7",	"8",
                                         "9",	"10",	"11",	"12",
                                         "13",	"14",	"15",	"16",
                                         "17",	"18",	"19",	"20",
                                         "21",	"22",	"23",	"24",
                                         "25",	"26",	"27",	"28",
                                         "29",	"30",	"31",	"32",
                                         "33",	"34",	"35"))
### Se transforma el dato a número 
casos_covidR$n_meses <- as.integer(casos_covidR$n_meses)
class(casos_covidR$n_meses)
### Se genera un nuevo objeto con los departamentos y el conteo por mes de los casos positivos
casos_mes <- casos_covidR |> 
  arrange(DEPARTAMENTO) |> 
  select(n_meses, DEPARTAMENTO) |> 
  filter(DEPARTAMENTO %in% c("AMAZONAS", "APURIMAC", 
                             "AYACUCHO", "HUANCAVELICA", "PASCO", "TACNA")) |>
  group_by(n_meses) |> 
  count()
### Para generar el gráfico de lineas y puntos en función a la información del mes 1 al mes 21
grafico <- casos_mes |> 
  filter(n_meses %in% c("1",	"2",	"3",	"4",
                     "5",	"6",	"7",	"8",
                     "9",	"10",	"11",	"12",
                     "13",	"14",	"15",	"16",
                     "17",	"18",	"19",	"20",
                     "21")) |> 
  ggplot() + 
  aes(x = n_meses, y = freq) +
  geom_line(size = 1, alpha = 0.8) + 
  geom_point() +
  aes(color=DEPARTAMENTO) + #Se colorea según departamento
  labs(x = "Meses", y = "Casos Positivos",  col = "") 
### Para visualizar el gráfico
grafico
### Usar el paquete para la animación del gráfico de puntos.
library(gganimate)
### Se asignar nombre y se usa la función de transicion de tiempo
gif_p1A <- grafico +
  transition_reveal(n_meses)
gif_p1A
### Análisis: El gráfico permite tener información de la evolución mesual de los casos positivos de COVID
### según 6 regiones. Se puede inferir que al mes 6 se registra un alta cantidad casos positivos, sobre todo en Tacna,
### Amazonas y Ayacucho, caso contrario sucede con la cantidad de casos positivos en Pasco.
### registra menor cantidad de casos positivos. Por otro lado, solo Apurimac registra
### un alto número casos positvos en el mes 14. No obstante, desde el mes 16 en todas las regiones
### seleccionadas empieza un descenso de nuevos casos positivos de COVID. De hecho, en el mes 21,
### el número de nuevos casos positivos de COVID no supera los 1000 al mes. 

## Ejercicio b ----
### Creación de la variable macroregión
casos_mes$Macroregion <- revalue(casos_mes$DEPARTAMENTO, c("AMAZONAS"="SELVA", 
                                                           "APURIMAC"="SIERRA",
                                                           "TACNA"="SIERRA",
                                                           "AYACUCHO"="CENTRO",
                                                           "HUANCAVELICA"="CENTRO",
                                                           "PASCO"="CENTRO"))
### Creación de la variable media movil
casos_mes$media_movil <- rollmean(casos_mes$freq,k=3,fill=NA, align = "center")
### Generación del gráfico
grafico2 <- casos_mes |> 
  filter(n_meses %in% c("1",	"2",	"3",	"4",
                        "5",	"6",	"7",	"8",
                        "9",	"10",	"11",	"12",
                        "13",	"14",	"15",	"16",
                        "17",	"18",	"19",	"20",
                        "21")) |> 
  ggplot() + 
  aes(x = n_meses, y = media_movil) +
  geom_line(size = 1, alpha = 0.8) + 
  geom_point() +
  aes(color=Macroregion) + #Se colorea según macroregion
  labs(x = "Meses", y = "Casos Positivos",  col = "") 
### visualización del segundo gráfico o plot
grafico2
### animación del segundo gráfico
gif_p1B <- grafico2 +
  transition_reveal(n_meses)
gif_p1B
#### Análisis: A partir de la agrupación de regiones y el control de las fluctuaciones mensuales, se 
#### inferir que a nivel macrorregional, el registro de nuevos casos positivos de COVID sigue
#### una tendencia similar hasta el mes 13. A su vez, se puede inferir que en la macroregión selva se 
#### registra una mayor cantidad de casos positivos a COVID entre el mes 14 y 15.

# Pregunta 2 --------------------------------------------------------------
### Extracción de los datos de LAPOP PERU
enlace2 <- "http://datasets.americasbarometer.org/database/files/PER_2021_LAPOP_AmericasBarometer_v1.2_w.dta"
library(haven) # Paquete para importación de datos SPSS
lapop_peru <- read_dta(enlace2)
names(lapop_peru)
### Se selecciona las variables necesarias para el análisis
sublapop <- lapop_peru |> 
  select(q2, q1tb, prov1t, b2, it1, cses6n, ur1new, ing4, gi0n, anestg)
### Abrir paquetes de datos necesarios para el análisis de valores perdidos
library(pacman)
p_load("VIM","DEoptimR","minqa","nloptr","simputation", "mice", "tidyverse", "DMwR2", "naniar")
### Diagnóstico general de los valores perdidos ----
any_na(sublapop) # Confirmamos que sí existen valores perdidos
pct_miss(sublapop) # Se observa que existe un 6% de datos perdidos en toda la base de datos
### Se solicita un gráficos para identificar las variables y frecuencias de NA.
gg_miss_upset(sublapop)
matrixplot(sublapop)
#### Análisis: Si nos quedamos con el porcentaje de 6% de datos perdidos, no es un problema tan grave de pérdida de datos
#### no obstante, el gráfico señala que la variable b2 "¿Hasta qué punto tiene usted 
#### respeto por las instituciones políticas de Perú?" tiene un alto número de datos perdidos, cerca a los 1500.  
### Análisis de datos perdidos entre ING4 e IT1 ----
### Diagnósitico visual
VIM::pbox(sublapop[5:8], pos=1)
### Prueba t de medias para evaluar el mecanismo de la variable IT1
### H0: No hay diferencia
### H1: Hay diferencia 
t.test(ing4 ~ is.na(it1), data=sublapop)
#### Análisis: Desde el diagnóstivo visual, el diagrama de caja de la it1 comparado
#### con los diagramas de caja de ing4 con y sin datos perdidos no presentan diferencias. A su vez, 
#### resisando los resultados de la prueba T de medias, se registra que el p-value no es menor a 0.05, 
#### por tanto, no se puede rechazar la H0, es decir, los valores perdidos de ing4 no son afectados 
#### por la distribución de valores perdidos de it1.
#### diferencias e