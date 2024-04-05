library(dplyr)
library(stringr)
library(ggplot2)
library(multidplyr)
library(tidytext)
library(beepr)

# cargar datos ----
# source("prensa_cargar_datos.R")
datos_prensa <- arrow::read_feather("datos/datos_prensa.feather")

# preparar texto ----
datos_prensa_2 <- datos_prensa |> 
  filter(año >= 2014) |> 
  mutate(id = 1:n()) |> 
  select(id, fuente, fecha, año, titulo, bajada, cuerpo_limpio, url) |> 
  mutate(across(c(fuente, titulo, bajada), as.factor)) |> 
  mutate(texto = paste(titulo, bajada, cuerpo_limpio))

# tokenizar palabras ----
prensa_palabras <- datos_prensa_2 |> 
  select(id, fuente, fecha, año, texto) |> 
  unnest_tokens(output = palabra, input = texto,
                token = "words", drop = T); beep()

# filtrar palabras ----
stopwords <- readr::read_lines("~/Documents/Apps Shiny/lira_popular/datos/stopwords_español.txt") #tidytext::get_stopwords("es") |> pull(word)

palabras_eliminar = c("right", "left", "top", "align", "gnews", "px", "twitter", "com", "pic", "font", "height", "width",
                      "pred", "fs", "us", "april", "flickr", "datawrapper", "data", "fried", "ftx", "medium", "exante", "server", "family", "loc", "lon", "mag", "prof", "lat", "gpt", "banner", "donación",
                      "rectangle", "container", "img", "display", "sans", "end", "weight", "content", "rem", "flex", "border", "bottom", "margin", "padding", "center", 
                      "radius", "text", "síguenos", "solid", "items", "dadada", "droidsans", "justify", "serif", "push", "function", "cmd", "div", "googletag", "ad",
                      "protected", "email")

prensa_palabras_2 <- prensa_palabras |> 
  filter(!palabra %in% stopwords) |> 
  filter(palabra != "") |> 
  filter(!palabra %in% palabras_eliminar)

prensa_palabras_2

## guardar ----
arrow::write_feather(prensa_palabras_2, "datos/prensa_palabras.feather")
