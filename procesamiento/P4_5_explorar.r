
#noticias por día ----
prensa_limpia |> 
  bind_rows() |> 
  filter(lubridate::year(fecha_f) >= 2022) |> 
  ggplot(aes(fecha_f)) +
  geom_histogram(aes(fill=fuente), show.legend = F) +
  facet_wrap(~fuente, ncol=1) 

# prensa_limpia |> 
#   bind_rows() |> 
#   filter(fuente == "latercera") |> 
#   select(url)

#palabras mas frecuentes ----
prensa_palabras |> 
  bind_rows() |> 
  select(fuente, texto, palabras, fecha_scraping) |> 
  distinct() |> 
  filter(palabras != "na") |> 
  group_by(fuente) |> 
  count(palabras) |> 
  mutate(p = n /sum(n)) |> 
  #filter(n >= 3) |> 
  #filter(p 
  arrange(desc(p)) |> 
  slice_max(order_by = n, n = 10) |> 
  ungroup() |> 
  #group_by(fuente) |> 
  mutate(#palabras = as.factor(palabras),
    fuente = as.factor(fuente),
         #palabras = fct_reorder(palabras, p)) |> 
         palabras = tidytext::reorder_within(palabras, p, fuente)) |> 
  #print()
  #graficar
  ggplot(aes(y = palabras, x = p)) +
  geom_col() +
  #coord_flip() +
  tidytext::scale_y_reordered() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  facet_wrap(~fuente, scales = "free_y") +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle=90, vjust=0.5))




#terminos ----
terminos_conceptos <- c("diputad", "senado", "parlamentar")

terminos_temas <- c("minería",
                    "minera",
                    "collahuasi",
                    "energía",
                    "agua",
                    "hídrico",
                    "ambiente",
                    "naturale",
                    "recursos",
                    "trabajo",
                    "empleo")

terminos_apellidos <- c("Ebensperger",
                        "Soria",
                        "Galleguillos",
                        "Moraga",
                        "Trisotti",
                        "Astudillo",
                        "Ramírez",
                        "Trisotti")

#buscar termino y ver en que fuentes sale
prensa_limpia |> 
  bind_rows() |> 
  filter(str_detect(texto, "Soria"))



#parlamentarios por fecha y fuente ----
prensa_limpia |> 
  bind_rows() |> 
  mutate(fecha_scraping = lubridate::as_date(fecha_scraping)) |> 
  distinct(titulo, .keep_all = T) |> 
  mutate(mención_1 = case_when(str_detect(texto, stringr::regex("Ebensperger")) ~ "Ebensperger"),
         mención_2 = case_when(str_detect(texto, stringr::regex("Soria")) ~ "Soria"),
         mención_3 = case_when(str_detect(texto, stringr::regex("Galleguillos")) ~ "Galleguillos"),
         mención_4 = case_when(str_detect(texto, stringr::regex("Moraga")) ~ "Moraga"),
         mención_6 = case_when(str_detect(texto, stringr::regex("Astudillo")) ~ "Astudillo"),
         mención_7 = case_when(str_detect(texto, stringr::regex("Ramírez")) ~ "Ramírez"),
         mención_8 = case_when(str_detect(texto, stringr::regex("Trisotti")) ~ "Trisotti")) |> 
  tidyr::pivot_longer(cols = starts_with("mención"), 
                      names_to = "temas", 
                      values_to = "parlamentarios") |> 
  filter(!is.na(parlamentarios)) |> 
  select(-temas) |> 
  filter(lubridate::year(fecha_f) >= 2022) |> 
  #graficar
  ggplot(aes(x = fecha_f, y = fuente, col = parlamentarios)) +
  geom_jitter(alpha = 0.8, width = 0, height = 0.2) +
  scale_x_date(date_breaks = "weeks", date_minor_breaks = "days") +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle=90, vjust=0.5))


#procentaje parlamentarios por día ----
datos_fff <- prensa_limpia |> 
  bind_rows() |> 
  #mutate(fecha_scraping = lubridate::as_date(fecha_scraping)) |> 
  #distinct(titulo, .keep_all = T) |> 
  mutate(mención_1 = case_when(str_detect(texto, stringr::regex("Ebensperger")) ~ "Ebensperger"),
         mención_2 = case_when(str_detect(texto, stringr::regex("Soria")) ~ "Soria"),
         mención_3 = case_when(str_detect(texto, stringr::regex("Galleguillos")) ~ "Galleguillos"),
         mención_4 = case_when(str_detect(texto, stringr::regex("Moraga")) ~ "Moraga"),
         mención_6 = case_when(str_detect(texto, stringr::regex("Astudillo")) ~ "Astudillo"),
         mención_7 = case_when(str_detect(texto, stringr::regex("Ramírez")) ~ "Ramírez"),
         mención_8 = case_when(str_detect(texto, stringr::regex("Trisotti")) ~ "Trisotti")) |> 
  tidyr::pivot_longer(cols = starts_with("mención"), 
                      names_to = "temas", 
                      values_to = "parlamentarios") |> 
  filter(!is.na(parlamentarios)) |> 
  select(-temas) |> 
  filter(lubridate::year(fecha_f) >= 2022)

datos_fff |>
  filter(lubridate::month(fecha_f) == 4) |> 
  group_by(fecha_f) |> 
  count(parlamentarios) |> 
  mutate(p = n/sum(n)) |> 
  #graficar
  ggplot(aes(x = fecha_f, 
             #y = fuente, 
             y = p,
             fill = parlamentarios)) +
  #geom_jitter(alpha = 0.8, width = 0, height = 0.2) +
  geom_col(position = "stack") +
  scale_x_date(date_breaks = "weeks", date_minor_breaks = "days") +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle=90, vjust=0.5),
        legend.position = "bottom")


#violin mejor????
datos_fff |>
  filter(lubridate::month(fecha_f) == 4) |> 
  group_by(fecha_f) |> 
  count(parlamentarios) |> 
  mutate(p = n/sum(n)) |> 
  #graficar
  ggplot(aes(x = fecha_f, 
             #y = fuente, 
  #           y = p,
             fill = parlamentarios)) +
  geom_violin() +
  #geom_jitter(alpha = 0.8, width = 0, height = 0.2) +
  #geom_col(position = "stack") +
  scale_x_date(date_breaks = "weeks", date_minor_breaks = "days") +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle=90, vjust=0.5),
        legend.position = "bottom")





#temas por fecha y fuente ----
source("/home/bastian/Collahuasi/collahuasi-indice-dashboard/proceso_funciones.R")

prensa_limpia |> 
  bind_rows() |> 
  #filter(fuente != "estrella") |> 
  mutate(fecha_scraping = lubridate::as_date(fecha_scraping)) |> 
  distinct(titulo, .keep_all = T) |> 
  detectar_temas(input = "texto") |> 
  filtrar_temas() |> 
  tidyr::pivot_longer(cols=starts_with("mención"), names_to ="temas_originales", values_to="temas_detectados") |> 
  select(-temas_originales) |> 
  filter(!is.na(temas_detectados)) |> 
  filter(lubridate::year(fecha_f) >= 2022) |> 
  #graficar
  ggplot(aes(x = fecha_scraping, y = temas_detectados, col = temas_detectados)) +
  geom_jitter(alpha = 0.8, width = 0) +
  scale_x_date(date_breaks = "weeks", date_minor_breaks = "days")



#buscar termino y contar palabras por fuente
prensa_palabras |> 
  bind_rows() |> 
  filter(str_detect(texto, "Soria")) |> 
  group_by(fuente) |> 
  count(palabras) |> 
  filter(n >= 10) |> 
  arrange(desc(n)) |> 
  print(n=100)
