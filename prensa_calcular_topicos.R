
# prensa_palabras <- arrow::read_feather("datos_prensa_palabras.feather")

# cargar datos
# source("prensa_calcular_conteo.R")
prensa_palabras_conteo <- arrow::read_parquet("datos/prensa_palabras_conteo.parquet")

# crear document text matrix ----
prensa_dtm <- tidytext::cast_dtm(data = prensa_palabras_conteo, 
                                 document = id, 
                                 term = palabra, 
                                 value = n)
# prensa_dtm

## guardar ----
readr::write_rds(prensa_dtm, "datos/prensa_palabras_dtm.rds")
# rm(prensa_palabras_conteo)


# modelar temas ----
prensa_lda <- topicmodels::LDA(prensa_dtm, k = 5)

## guardar ----
readr::write_rds(prensa_lda, "datos/prensa_palabras_lda.rds")


## calcular word-topic probabilities ----
prensa_topics <- tidytext::tidy(prensa_lda, matrix = "beta")

### guardar ----
#lista de palabras y sus respectivos gammas por topico
readr::write_rds(prensa_topics, "datos/prensa_diccionario_topicos.rds")

### top terminos por tópico ----
prensa_top_terminos <- prensa_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>% 
  ungroup() %>%
  arrange(topic, -beta)

#### graficar topicos ----
# library(ggplot2)
# 
# prensa_top_terminos %>%
#   mutate(term = reorder_within(term, beta, topic)) %>%
#   ggplot(aes(beta, term, fill = factor(topic))) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~ topic, scales = "free") +
#   scale_y_reordered()

### probabilidad de topico por documento ----
# per-document-per-topic probabilities
prensa_documents <- tidy(prensa_lda, matrix = "gamma")

prensa_documents |> 
  arrange(as.numeric(document), desc(gamma)) |> 
  filter(gamma > 0.4) |>
  group_by(document) |> 
  mutate(n_topics = n()) |> 
  filter(n_topics > 1)
#algunos documentos no clasifican 100% en un solo topico


#pivotar a columnas para indicar el tópico al que pertenece cada documento
prensa_topicos <- prensa_documents |> 
  arrange(as.numeric(document), desc(gamma)) |>
  group_by(document) |> 
  mutate(topico = max(gamma),
         topico = ifelse(topico == gamma, topic, NA)) |> 
  tidyr::fill(topico) |> 
  tidyr::pivot_wider(id_cols = c(document, topico), names_from = topic, names_prefix = "topico_", values_from = gamma) |> 
  mutate(document = as.integer(document)) |> 
  rename(id = document)


#### guardar ----

#documentos por id, topico al cual pertenecen, y gammas de cada topico para el documento
readr::write_rds(prensa_topicos, "datos/prensa_topicos.rds")



# #lista de palabras unicas y el topico que tiene mayor gamma
# prensa_topics |> 
#   group_by(term) |> 
#   slice_max(beta) |> 
#   arrow::write_feather("datos/prensa_diccionario_topicos_unicos.feather")
# 




# prensa_palabras_conteo <- prensa_palabras |> 
#   group_by(fecha) |> 
#   count(palabra) |> 
#   mutate(p = n/sum(n)) |> 
#   group_by(palabra) |> 
#   mutate(n_abs = n()) |> 
#   ungroup() |> 
#   arrange(desc(n)) |> 
#   mutate(rank = 1:n(),
#          class = if_else(rank <= 10000, "alto", "bajo")) |> 
#   group_by(class) |> 
#   mutate(p_abs = n_abs/sum(n_abs),
#          p_abs = if_else(class == "alto", p_abs, 0))
# 
# 
# prensa_palabras_conteo |> 
#   filter(p_abs > 0.000001 & p > 0.005) |> 
#   ggplot(aes(fecha, p)) +
#   geom_text(aes(label = palabra), size = 2, angle = 90,check_overlap = T)
# 
# 
# prensa_palabras_conteo |> 
#   filter(palabra %in% c("portonazo", "robo", "asalto", "secuestro", "delincuencia")) |> 
#   group_by(palabra) |> 
#   mutate(p = slider::slide_dbl(p, mean, .before = 30)) |> 
#   ggplot(aes(fecha, p, color = palabra)) +
#   geom_line() +
#   geom_text(aes(label = palabra), size = 2, angle = 90,check_overlap = T)


# arrow::write_parquet(prensa_palabras_conteo, "datos_prensa_palabras_conteo.feather")
# prensa_palabras_conteo <- arrow::read_parquet("datos_prensa_palabras_conteo.feather")
