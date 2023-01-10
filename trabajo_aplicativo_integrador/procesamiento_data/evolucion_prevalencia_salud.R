
# Importar librerías ------------------------------------------------------
pacman::p_load(tidyverse, ggrepel, showtext, ggtext)

# Importar data -----------------------------------------------------------
estres       <- readxl::read_excel("data/evolucion_prevalencia_salud.xlsx", sheet = "estres") 
depresion    <- readxl::read_excel("data/evolucion_prevalencia_salud.xlsx", sheet = "depresion")
ansiedad     <- readxl::read_excel("data/evolucion_prevalencia_salud.xlsx", sheet = "ansiedad") 
salud_mental <- rbind(estres, depresion, ansiedad) |> clean_names()

# Generar gráficos --------------------------------------------------------

## Gráfico de prevalencia general ----
prevalencia_general <- salud_mental |> 
  mutate(valor = round(valor, 1)) |> 
  filter(segmento == "General") |> 
  ggplot(aes(x = year, 
             y = valor,
             col = variable)) +
  geom_line(size = 1.5,
            linetype = "dashed") +
  geom_point(size = 3) +
  labs(title = "Proporción de docentes que declaran haber experimentado\n estrés, ansiedad y/o depresión (2014-2021)",
       x = NULL,
       y = NULL,
       col = NULL) +
  hrbrthemes::theme_ipsum() + 
  ggrepel::geom_label_repel(aes(label = scales::percent(valor/100)), show.legend = F) +
  
  scale_x_continuous(
    limits = c(2014, 2021),
    breaks = seq(2014, 2021, by = 1)
  ) +
  scale_y_continuous(
    limits = c(0, 65),
    breaks = seq(10,70,10)
  ) +
  scale_color_manual(
    values = c(rcartocolor::carto_pal(n = 10, name = "Bold")[1:11-1], "#10454F") 
  ) +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold", size = 20, vjust = -0.5, hjust = 0.5))

## Gráfico de prevalencia según sexo ----
prevalencia_sexo <- salud_mental |> 
  mutate(valor = round(valor, 1)) |> 
  filter(segmento %in% c("Hombres", "Mujeres")) |> 
  ggplot(aes(x = year, 
             y = valor,
             col = segmento)) +
  geom_line(size = 1.5,
            linetype = "solid") +
  geom_point(size = 3) +
  labs(title = "Prevalencia de trastornos de salud mental en docentes según sexo (2014-2021)",
       x = NULL,
       y = NULL) +
  facet_wrap(~variable) +
  hrbrthemes::theme_ipsum() +
  scale_y_continuous(
    limits = c(0, 70),
    breaks = seq(5,70,5)
  ) +
  scale_color_manual(values = c("#008F8C", "#F2AE30")) 

## Gráfico de prevalencia según área ----
prevalencia_area <- salud_mental |> 
  mutate(valor = round(valor, 1)) |> 
  filter(segmento %in% c("Urbana", "Rural")) |> 
  ggplot(aes(x = year, 
             y = valor,
             col = segmento)) +
  geom_line(size = 1.5,
            linetype = "solid") +
  geom_point(size = 3) +
  labs(title = "Prevalencia de trastornos de salud mental en docentes según área (2014-2021)",
       x = NULL,
       y = NULL) +
  facet_wrap(~variable) +
  hrbrthemes::theme_ipsum() +
  scale_y_continuous(
    limits = c(0, 70),
    breaks = seq(5,70,5)
  ) +
  scale_color_manual(values = c("#008F8C", "#F2AE30"))

## Gráfico de prevalencia según nivel educativo ----
#prevalencia_ne <- 
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


# Exportar gráficos -------------------------------------------------------
ggsave(filename = "prevalencia_general.jpg", prevalencia_general, height = 7, width = 10)
ggsave(filename = "prevalencia_sexo.jpg", prevalencia_sexo, height = 7, width = 12)
ggsave(filename = "prevalencia_area.jpg", prevalencia_area, height = 7, width = 12)
ggsave(filename = "prevalencia_ne.jpg", prevalencia_ne, height = 7, width = 12)

