library(dplyr)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
library(httr)
library(jsonlite)
source("funciones_scraping.r")

# request
json_file <- "https://www.biobiochile.cl/lista/api/get-todo-sin-robin?limit=1600"
# json_file <- "https://www.biobiochile.cl/lista/api/get-todo-sin-robin?limit=1000&offset=999&"
# https://www.biobiochile.cl/lista/api/get-todo-sin-robin?limit=10&offset=30&categorias=group-nacional&t=1711596848816
# json_file <- "https://www.biobiochile.cl/lista/api/get-todo?limit=1700&offset=500"

# 
# json_file <- "https://www.biobiochile.cl/static/news.json?t=1712171198277"
# # https://www.biobiochile.cl/lista/api/get-todo-sin-robin?limit=10&offset=20&categorias=group-nacional&t=1712171261154
# json_file <- "https://www.biobiochile.cl/lista/api/get-todo-sin-robin?limit=10&offset=200&categorias=group-nacional"
# json_file <-"https://www.biobiochile.cl/static/news.json?t=1712171378290"

# obtener datos
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

# limpiar
json_data |> tibble() |> glimpse()

resultados_biobio <- json_data |> 
  tibble() |>
  select(titulo = post_title,
         bajada = post_excerpt,
         cuerpo = post_content,
         fecha = post_date_date, 
         fecha_texto = post_date_txt,
         categoria = primary,
         categoria2 = secondary,
         url = post_URL) |> 
  mutate(fecha_scraping = lubridate::today(),
         fuente = "biobio")

# convertir html a texto en columna cuerpo
resultados_biobio_2 <- resultados_biobio |> 
  rowwise() |> 
  mutate(cuerpo = read_html(as.character(cuerpo)) |> html_text2())

# guardar ----
dir.create("resultados/biobio/")

readr::write_rds(resultados_biobio_2,
                 glue("resultados/biobio/biobio_cron_{lubridate::today()}.rds"))

message(glue("listo cron biobio {lubridate::now()}"))