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

# if (!exists("prensa_palabras_conteo")) {
#   prensa_palabras_conteo <- arrow::read_parquet("datos/prensa_palabras_conteo.parquet")
# }
# 
# # calcular correlación entre palabras, por noticia
# correlacion <- prensa_palabras_conteo |> 
#   group_by(palabra) |> 
#   filter(n() >= 1000) |> #solo palabras que aparezcan más de n veces a través del corpus
#   pairwise_cor(palabra, id)  |> 
#   filter(!is.na(correlation)); beep()
# 
# # guardar ----
# arrow::write_parquet(correlacion, "datos/prensa_correlacion.parquet")

# tictoc::toc()

correlacion <- arrow::read_parquet("datos/prensa_correlacion.parquet")

obtener_termino_correlacion <- function(correlacion, termino) {
  correlacion |> 
    filter(item1 == termino) |> 
    arrange(desc(correlation)) |> 
    head(10) |> 
    print() |> 
    pull(item2) |> 
    dput()
}

# obtener correlaciones ----
correlacion |> obtener_termino_correlacion("robo")
correlacion |> obtener_termino_correlacion("portonazo")
correlacion |> obtener_termino_correlacion("secuestro")
correlacion |> obtener_termino_correlacion("delincuencia")
correlacion |> obtener_termino_correlacion("asalto")
correlacion |> obtener_termino_correlacion("delincuente")

correlacion |> obtener_termino_correlacion("arma")
correlacion |> obtener_termino_correlacion("pistola")
correlacion |> obtener_termino_correlacion("homicidio")
correlacion |> obtener_termino_correlacion("asesinato")


correlacion |> 
  filter(item1 == "muerte") |> 
  arrange(desc(correlation)) |> 
  head(10) |> 
  print() |> 
  pull(item2) |> 
  dput()



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