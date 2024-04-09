# PRENSA: PASO 2
# en este script se cargan los datos de prensa y se tokenizan, es decir, se transforma hacia una estructura donde cada fila contiene una sola palabra
# luego se eliminan stopwords y palabras inútiles, y se guarda el resultado
# input: prensa_datos.feather
# output: prensa_palabras.feather

library(dplyr)
library(purrr)
library(furrr)
library(tidytext)
library(beepr)
source("funciones_scraping.r")

plan(multisession, workers = 2)
# options(future.globals.maxSize = 2048*1024^2)

tictoc::tic()


# cargar datos ----
# source("prensa_cargar_datos.R") #(ejecutar si es que hay datos nuevos)
if (!exists("datos_prensa")) datos_prensa <- arrow::read_feather("datos/prensa_datos.feather")

# preparar datos ----
# datos_prensa_2 <- datos_prensa |> 
#   filter(año >= 2019) |>
#   # mutate(id = 1:n()) |> 
#   select(id, fuente, fecha, año, titulo, bajada, cuerpo_limpio, url) |> 
#   mutate(across(c(fuente, titulo, bajada), as.factor)) |> 
#   mutate(texto = paste(titulo, bajada, cuerpo_limpio))

# library(data.table)
# library(dtplyr)
# 
# datos_prensa__prueba <- datos_prensa_2 |> 
#   slice(1:1000)

# tokenizar palabras ----

# # tokeniazr palabras en un solo hilo
# prensa_palabras_tokenizadas <- datos_prensa_2 |>
#   select(id, fuente, fecha, año, texto) |>
#   unnest_tokens(output = palabra, input = texto,
#                 token = "words", drop = T); beep()

# # tokenizar palabras en múltiples hilos (para ello, dividimos los datos en piezas iguales para procesar paralelamente)
# prensa_palabras_tokenizadas <- datos_prensa_2 |> 
#   select(id, fuente, fecha, año, texto) |>
#   mutate(grupos = (row_number()-1) %/% (n()/4)) |> # 8 grupos de igual cantidad de filas
#   group_by(grupos) |> 
#   group_split() |> 
#   future_map(~unnest_tokens(.x,
#                             output = palabra, input = texto,
#                             token = "words", drop = T)) |> 
#   list_rbind(); beep()

datos_prensa_split <- datos_prensa |>
  filter(año >= 2018) |>
  # slice(1:100000) |> 
  # select(any_of(c("id", "fuente", "fecha", "año", "titulo", "bajada", "cuerpo_limpio", "url", "grupos"))) |>
  # select(id, fuente, fecha, año, titulo, bajada, cuerpo_limpio) |>
  select(id, cuerpo_limpio) |> 
  mutate(grupos = (row_number()-1) %/% (n()/30)) |> # 8 grupos de igual cantidad de filas
  group_by(grupos) |>
  group_split()


future_walk(datos_prensa_split, \(parte) {
  # parte <- datos_prensa[[1]]
  
  palabras <- parte |> 
    unnest_tokens(output = palabra, input = cuerpo_limpio,
                  token = "words", drop = T) |> 
    filter(!palabra %in% stopwords) |> 
    filter(palabra != "") |> 
    filter(!palabra %in% palabras_eliminar)
  
  grupo <- parte$grupos[1]
  
  # guardar resultados individuales
  readr::write_rds(palabras, 
                   file = paste0("datos/preprocesados_palabras/palabras_", grupo, ".rds"), compress = "gz")
})

plan(multisession)

prensa_palabras_tokenizadas <- map(fs::dir_ls("datos/preprocesados_palabras/"), readr::read_rds) |> 
  list_rbind()

 
# 
# prensa_palabras <- prensa_palabras_tokenizadas |> 
#   filter(!palabra %in% stopwords) |> 
#   filter(palabra != "") |> 
#   filter(!palabra %in% palabras_eliminar); beep()

# prensa_palabras_2
message(paste("total de palabras:", nrow(prensa_palabras_tokenizadas)))


## guardar ----
arrow::write_feather(prensa_palabras_tokenizadas, "datos/prensa_palabras.feather")

tictoc::toc()
plan(multisession)
remove(prensa_palabras_tokenizadas)
