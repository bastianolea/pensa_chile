library(tidyverse)
library(purrr)

#library(ggplot2)
#source("~/Collahuasi/collahuasi-indice-dashboard/proceso_funciones.R")
#Sys.setlocale(category = "LC_TIME", locale="es_ES.UTF-8") #meses en castellano
prensa <- arrow::read_feather("~/Collahuasi/seguimiento-scraping/P4_prensa/scraping/resultados_scraping/prensa_limpia.feather")



prensa2 <- prensa |> 
  mutate(texto = paste(titulo, bajada, cuerpo)) |> 
  select(texto, fecha_f, fecha_scraping, fuente)
  
prensa3 <- prensa2 |> 
  mutate(t_1 = str_detect(texto, regex("collahuasi", ignore_case = T)),
         t_2 = str_detect(texto, regex("minería", ignore_case = T)))

prensa3 |> 
  filter(t_2)

#términos
terminos <- c("fundacion collahuasi",
"matías aylwin",
"luciano malhue",
"jorge gómez",
"gaetano maniello",
"collahuasi",
"cmdic",
"impulsa mujer",
"impulsa tarapaca")

terminos <- c("constitución", "constituyente",
              "presidente", "gobierno", 
              "economía", "mercado", "inflación",
              "minería", "minera", "cobre", "collahuasi",
              "tarapacá", "alto hospicio", "colchane")

#loop búsqueda
resultado_terminos <- map(terminos |> set_names(), ~{
  tictoc::tic()  
  resultados <- str_detect(prensa2$texto[1:10000], regex(.x, ignore_case = TRUE))
  tictoc::toc()
  return(resultados)
  })

#guardar resultados
prensa3 <- prensa2 |> 
  slice(1:10000) |> 
  bind_cols(resultado_terminos)

#buscar
prensa3 |> 
  filter(gobierno) |> 
  pull(texto)
