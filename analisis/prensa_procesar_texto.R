# PRENSA: PASO 2
# en este script se cargan los datos de prensa y se tokenizan, es decir, se transforma hacia una estructura donde cada fila contiene una sola palabra
# luego se eliminan stopwords y palabras inútiles, y se guarda el resultado
# input: prensa_datos.feather
# output: prensa_palabras.feather
# tiempo aproximado: 7 minutos

library(dplyr)
library(purrr)
library(furrr)
library(tidytext)
source("funciones_scraping.r")

plan(multisession, workers = 2)
# options(future.globals.maxSize = 2048*1024^2)

tictoc::tic()

# cargar datos ----
# source("prensa_cargar_datos.R") #(ejecutar si es que hay datos nuevos)
if (!exists("datos_prensa")) datos_prensa <- arrow::read_feather("datos/prensa_datos.feather")

# dividir dataframe en partes para poder procesar en paralelo
datos_prensa_split <- datos_prensa |>
  filter(año >= 2018) |>
  # slice(1:100000) |> 
  select(id, cuerpo_limpio) |> 
  mutate(grupos = (row_number()-1) %/% (n()/30)) |> # 8 grupos de igual cantidad de filas
  group_by(grupos) |>
  group_split()

# tokenización de texto por partes, multiprocesador
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
rm(datos_prensa_split)

# cargar piezas individuales
prensa_palabras <- map(fs::dir_ls("datos/preprocesados_palabras/"), readr::read_rds) |> 
  list_rbind()

message(paste("total de palabras:", nrow(prensa_palabras)))


## guardar ----
arrow::write_feather(prensa_palabras, "datos/prensa_palabras.feather")

remove(prensa_palabras)
beepr::beep()
tictoc::toc()