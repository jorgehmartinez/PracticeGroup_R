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
## 
### Librerias necesarias para el desarrollo de la pregunta
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(plyr)
# install.packages("gifski")
library(gifski)
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
## Solo presenta la información del mes 1 al mes 21
grafico <- casos_mes |> 
  filter(n_meses %in% c("1",	"2",	"3",	"4",
                     "5",	"6",	"7",	"8",
                     "9",	"10",	"11",	"12",
                     "13",	"14",	"15",	"16",
                     "17",	"18",	"19",	"20",
                     "21")) |> 
  ggplot() + 
  aes(x = n_meses, y = freq) +
  geom_point(alpha = 0.8) + 
  aes(color=DEPARTAMENTO) +
  labs(x = "Meses", y = "Casos Positivos",  col = "") 

grafico

### Usar el paquete para la animación del gráfico de puntos.
library(gganimate)
### Se asignar nombre y se usa la función de transicion de tiempo
gif_p1A <- grafico +
  transition_time(n_meses)
gif_p1A
## Ejercicio b ----


# Pregunta 2 --------------------------------------------------------------


