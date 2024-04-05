library(dplyr)
library(purrr)
library(furrr)
library(widyr)
library(beepr)

plan(multisession, workers = 7)

# cargar datos ----
# source("prensa_procesar_texto.R")
prensa_palabras <- arrow::read_feather("datos/prensa_palabras.feather")

# conteo de palabras por noticia, multiprocesador
prensa_palabras_conteo <- prensa_palabras |> 
  # filter(año >= 2023) |> 
  group_by(fuente) |> 
  group_split() |> 
  future_map(~count(.x, id, palabra)) |> 
  list_rbind(); beep()

prensa_palabras_conteo
plan(multisession)

# guardar ----
arrow::write_parquet(prensa_palabras_conteo, "datos/prensa_palabras_conteo.parquet")