# PRENSA: PASO 3
# se cargan los datos tokenizados en el paso anterior, se cuenta la frecuencia de palabras por noticia, y se guarda el resultado
# input: prensa_palabras.feather (paso 2)
# output: prensa_palabras_conteo.parquet
# tiempo aprox: 55 minutos

library(dplyr)
library(purrr)
library(furrr)
library(widyr)
library(beepr)

tictoc::tic()
plan(multisession, workers = 4)

# cargar texto procesado ----
# source("prensa_procesar_texto.R") #(ejecutar si hay datos nuevos)
if (!exists("prensa_palabras")) prensa_palabras <- arrow::read_feather("datos/prensa_palabras.feather")

# conteo de palabras por noticia, multiprocesador
prensa_palabras_conteo <- prensa_palabras |> 
  group_by(id) |> 
  group_split() |> 
  future_map(~count(.x, id, palabra)) |> 
  list_rbind(); beep()

# prensa_palabras_conteo
plan(multisession)

# guardar ----
arrow::write_parquet(prensa_palabras_conteo, "datos/prensa_palabras_conteo.parquet")

tictoc::toc()