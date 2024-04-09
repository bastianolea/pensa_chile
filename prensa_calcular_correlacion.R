# a partir del conteo de palabras por noticia, calcular correlación entre términos, indicando qué términos aparecen cercanos a otros
# https://uc-r.github.io/word_relationships#corr

library(dplyr)
library(purrr)
library(furrr)
library(widyr)
library(beepr)

tictoc::tic()

# cargar ----
# source("prensa_calcular_conteo.R")

if (!exists("prensa_palabras_conteo")) {
prensa_palabras_conteo <- arrow::read_parquet("datos/prensa_palabras_conteo.parquet")
}

# calcular correlación entre palabras, por noticia
correlacion <- prensa_palabras_conteo |> 
  group_by(palabra) |> 
  filter(n() >= 1000) |> #solo palabras que aparezcan más de n veces a través del corpus
  pairwise_cor(palabra, id)  |> 
  filter(!is.na(correlation)); beep()

# guardar ----
arrow::write_parquet(correlacion, "datos/prensa_correlacion.parquet")

tictoc::toc()

# pruebas ----
correlacion |> 
  filter(item1 == "delincuencia") |> 
  arrange(desc(correlation)) |> 
  head(30) |> print(n=Inf)

correlacion |> 
  filter(item1 == "robo") |> 
  arrange(desc(correlation)) |> 
  head(10)

correlacion |> 
  filter(item1 == "arma") |> 
  arrange(desc(correlation)) |> 
  head(10)

correlacion |> 
  filter(item1 == "corrupción") |> 
  arrange(desc(correlation)) |> 
  head(10)

correlacion |> 
  filter(item1 == "fraude") |> 
  arrange(desc(correlation)) |> 
  head(10)

correlacion |> 
  filter(item1 == "jadue") |> 
  arrange(desc(correlation)) |> 
  head(10)

correlacion |> 
  filter(item1 == "kast") |> 
  arrange(desc(correlation)) |> 
  head(10)

correlacion |> 
  filter(correlation > .9) |> 
  arrange(desc(correlation)) |> 
  head(20)

# graficar ----

correlacion |> 
  filter(!is.na(correlation),
         correlation <= .95, correlation >= .9) |> 
  graph_from_data_frame() |> 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()