library(tidyverse)
library(ggplot2)
source("~/Collahuasi/indice-perfilamiento/proceso_funciones.R")
Sys.setlocale(category = "LC_TIME", locale="es_ES.UTF-8") #meses en castellano
prensa <- arrow::read_feather("~/Collahuasi/seguimiento-scraping/P4_prensa/scraping/resultados_scraping/prensa_limpia.feather")

#detectar temáticas en base de prensa
prensa_texto_temas <- prensa |> 
  mutate(texto = paste(titulo, bajada, cuerpo)) |> 
  select(titulo, texto, fecha = fecha_f, fuente) |> 
  detectar_temas("texto") |> 
  tidyr::pivot_longer(cols=starts_with("mención"), names_to ="temas_originales", values_to="temas_detectados")
#multidplyr esta hueá

#contar temas detectados por día y obtener promedio
prensa_conteo <- prensa_texto_temas |> 
  filter(fecha <= lubridate::today()) |> 
  group_by(fecha) |> 
  count(temas_detectados) |> 
  filter(!is.na(temas_detectados)) |> 
  mutate(año = lubridate::year(fecha),
         mes = lubridate::month(fecha),
         semana = lubridate::week(fecha)) |> 
  filter(año >= 2019) |> 
  #porcentaje diario
  group_by(fecha) |> 
  mutate(p = n/sum(n))

prensa |> distinct(url, .keep_all = T) |> count(fuente)
prensa |> filter(fuente == "estrella") |> select(pagina)


#noticias obtenidas por día ----
prensa_conteo |> 
  filter(fecha >= "2022-02-01") |> 
  group_by(fecha) |> 
  summarize(n = mean(n)) |> 
  ggplot(aes(fecha, n)) +
  geom_area()

prensa |> 
  filter(fecha_f >= "2021-02-01") |> 
  group_by(fecha_f, fuente) |> 
  count() |> 
  summarize(n = sum(n)) |> 
  ggplot(aes(fecha_f, n, fill = fuente)) +
  geom_col(position = "stack", width = 2) +
  theme(legend.position = "bottom")
  
#noticias obtenidas por año
prensa_conteo |> 
  group_by(año) |> 
  summarize(n = sum(n)) |> 
  ggplot(aes(año, n)) +
  geom_col()


prensa_conteo |> 
  filter(año == 2022) |> 
  filter(temas_detectados == "Minería") |> 
  ggplot(aes(fecha, n)) +
  geom_area()


#—----

#opciones comunes a los gráficos
conf_grafico <- function() {
  list(  viridis::scale_color_viridis(discrete = T),
                         viridis::scale_fill_viridis(discrete = T),
                         scale_x_date(date_breaks = "months", labels = function (x) format(x, "%B %y")),
                         scale_alpha_manual(values = c(1, 0.4)),
                         theme_void(),
                         theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                               axis.text.y = element_blank(),
                               axis.title = element_blank(),
                               legend.title = element_blank())
  )
}

#función para destacar algunos segmentos según el porcentaje que obtienen
calcular_p_significativos_prensa <- function(x) {
  x |> 
    #disminuir el margen para temas significativos (para que menos menciones los hagan destacar)
    mutate(significativa = case_when(temas_detectados == "Collahuasi" & p >= 0.02 ~ "Destacada",
                                     temas_detectados == "Ambiente" & p >= 0.1 ~ "Destacada",
                                     temas_detectados == "Minería" & p >= 0.05 ~ "Destacada",
                                     p >= 0.2 ~ "Destacada",
                                     TRUE ~ "Normal"),
           #aumentar el margen que tienen que tener para que sean significativos (para des-marcar temas muy frecuentes)
           significativa = case_when(temas_detectados == "Ingresos" & p <= 0.3 ~ "Normal",
                                     temas_detectados == "Laboral" & p <= 0.3 ~ "Normal",
                                     TRUE ~ significativa))
}

filtro_temas_prensa = c("Aguas", "Ambiente", "Collahuasi", "Educación", "Laboral", "Minería", "Protestas", "Social")

#porcentajes diarios
g_prensa_diaria <- prensa_conteo |> 
  filter(temas_detectados %in% filtro_temas_prensa) |> 
  filter(año == 2022,
         fecha <= lubridate::today() - 1,
         mes >= (lubridate::today() |> lubridate::month())-4) |> 
  ggplot(aes(fecha, p, fill=temas_detectados, col=temas_detectados)) +
  geom_col(width = 1.2) +
  conf_grafico() +
  labs(title = "Temáticas en prensa")


#porcentajes diarios destacados ----
g_prensa_diaria_destacada <- prensa_conteo |> 
  filter(temas_detectados %in% filtro_temas_prensa) |> 
  filter(año == 2022,
         mes >= (lubridate::today() |> lubridate::month())-4,
         fecha <= lubridate::today() - 1) |> 
  calcular_p_significativos_prensa() |> 
  ggplot(aes(fecha,
             p, fill=temas_detectados,
             alpha = significativa)) +
  geom_col(width = 1.2) +
  conf_grafico() +
  labs(title = "Temáticas en prensa")




#ver ----
g_prensa_diaria
g_prensa_diaria_destacada
