# Información general ----
## El presente trabajo tiene como autores a:
## - Hinojosa Cahuana, Percy Alberth
## - Huanca Martinez, Jorge Alberto
## - Huancaya Idone, Cesar Dante
## - Tintaya Orihuela, Meir Alvaro

## Importar librerías ----
## Con pacman se puede gestionar la instalación y carga de otros paquetes de R. 
library(pacman)
pacman::p_load(tidyverse, ggrepel, showtext, ggtext)
# Importar data a partir de un enlace
enlace <- "https://raw.githubusercontent.com/jorgehmartinez/PracticeGroup_R/main/trabajo_aplicativo_integrador/evolucion_prevalencia_salud.csv"

# Leer la data y guardarla en un objeto denominado "salud mental"
salud_mental <- read.csv2(enlace)

# Se transforma el tipo de la variable "valor" a numérica,
# dado que es necesario para la función geom_line, para ello usamos el pipe y la
# función mutate
salud_mental <- salud_mental |> 
  mutate(valor = as.double(valor))

## Gráfico de prevalencia general ----
salud_mental |> 
  # Redondear el valor de la prevalencia a un decimal
  mutate(valor = round(valor, 1)) |> 
  # Se usa la función dplyr::filter para filtrar por el segmento General
  filter(segmento == "General") |> 
  # Definir las coordenadas (año y valor de prevalencia)
  # colorear las líneas según el nombre del problema de salud mental
  ggplot(aes(x = year, 
             y = valor,
             col = variable)) +
  # Se agrega un gráfico de línea, definir el tamaño y el diseño de línea 
  geom_line(size = 1.5,
            linetype = "dashed") +
  # Se agrega un gráfico de puntos, definir el tamaño de los puntos
  # Se utiliza para visualizar la cifra en el gráfico
  geom_point(size = 3) +
  # Se define la etiqueta del título, quitar etiquetas para los ejes
  labs(title = "Proporción de docentes que declaran haber experimentado\n estrés, ansiedad y/o depresión (2014-2021)",
       x = NULL,
       y = NULL,
       col = NULL) +
  # Se elige el tema del gráfico mediante el paquete hrbrthemes
  hrbrthemes::theme_ipsum() + 
  # usar ggrepel para evitar que se superpongan las etiquetas
  # las etiquetas se modifican mediante el paquete scales
  ggrepel::geom_label_repel(aes(label = scales::percent(valor/100)), show.legend = F) +
  # se modifica los límites y los puntos de corte de los ejes
  scale_x_continuous(
    limits = c(2014, 2021),
    breaks = seq(2014, 2021, by = 1)
  ) +
  scale_y_continuous(
    limits = c(0, 65),
    breaks = seq(10,70,10)
  ) +
  # se eligen los colores específicos de las líneas del gráfico,
  # esta elección se realiza a partir de una escala
  scale_color_manual(
    values = c(rcartocolor::carto_pal(n = 10, name = "Bold")[1:11-1], "#10454F") 
  ) +
  # se define el tema del gráfico, especificando la posición de la leyenda
  # además se especifican los elementos del título: tamaño y posición
  theme(legend.position = "top",
        plot.title = element_text(face = "bold", size = 20, vjust = -0.5, hjust = 0.5))

## Gráfico de prevalencia según sexo----
salud_mental |> 
  # Se redonde el valor
  mutate(valor = round(valor, 1)) |> 
  filter(segmento %in% c("Hombres", "Mujeres")) |> 
  ggplot(aes(x = year, 
             y = valor,
             col = segmento)) +
  geom_line(size = 1.5,
            linetype = "solid") +
  geom_point(size = 3) +
  labs(title = "Proporción de docentes que declaran haber experimentado\n estrés, ansiedad y/o depresión según sexo",
       x = NULL,
       y = NULL,
       color = "Sexo") +
  # se divide el gráfico a partir de las categorías de "variable" en este caso: estrés, depresión y ansiedad,
  # la cual previamente fue filtrada con valores específicos
  facet_wrap(~variable) +
  hrbrthemes::theme_ipsum() +
  scale_y_continuous(
    limits = c(0, 70),
    breaks = seq(5,70,5)
  ) +
  # se eligen los colores específicos de las líneas del gráfico,
  # esta elección se realiza a partir de una escala
  scale_color_manual(values = c("#008F8C", "#F2AE30")) +
  theme(plot.title = element_text(face = "bold", size = 20, vjust = 3, hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(size=14),
        legend.text = element_text(size=14))

## Gráfico de prevalencia según área ----
salud_mental |> 
  mutate(valor = round(valor, 1)) |> 
  filter(segmento %in% c("Urbana", "Rural")) |> 
  ggplot(aes(x = year, 
             y = valor,
             col = segmento)) +
  geom_line(size = 1.5,
            linetype = "solid") +
  geom_point(size = 3) +
  labs(title = "Proporción de docentes que declaran haber experimentado\n estrés, ansiedad y/o depresión según área",
       x = NULL,
       y = NULL,
       color = "Área") +
  # se divide el gráfico a partir de las categorías de "variable" en este caso: estrés, depresión y ansiedad ,
  # la cual previamente fue filtrada con valores específicos
  facet_wrap(~variable) +
  hrbrthemes::theme_ipsum() +
  scale_y_continuous(
    limits = c(0, 70),
    breaks = seq(5,70,5)
  ) +
  scale_color_manual(values = c("#008F8C", "#F2AE30")) +
  theme(plot.title = element_text(face = "bold", size = 20, vjust = 3, hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(size=14),
        legend.text = element_text(size=14))

## Gráfico de prevalencia según nivel educativo----
salud_mental |> 
  mutate(valor = round(valor, 1)) |> 
  filter(segmento %in% c("Inicial", "Primaria", "Secundaria")) |> 
  ggplot(aes(x = year, 
             y = valor,
             col = segmento)) +
  geom_line(size = 1.5,
            linetype = "solid") +
  geom_point(size = 3) +
  labs(title = "Proporción de docentes que declaran haber experimentado\n estrés, ansiedad y/o depresión según nivel educativo",
       x = NULL,
       y = NULL,
       color = "Nivel Educativo") +
  facet_wrap(~variable) +
  hrbrthemes::theme_ipsum() +
  scale_y_continuous(
    limits = c(0, 50),
    breaks = seq(5,70,5)
  ) +
  scale_color_manual(values = c("#008F8C", "#F2AE30", "#8C1F28")) +
  theme(plot.title = element_text(face = "bold", size = 20, vjust = 3, hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(size=14),
        legend.text = element_text(size=14))