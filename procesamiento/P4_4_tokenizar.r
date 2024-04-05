#tokenizar ----
stopwords_extra <- c("alto", "hospicio", "iquique", "región", "tarapacá",
                     "comuna", "región",
                     "casos", "años","año", "personas",
                     "favoritos", "escuchar", "email", "compartir", "leer", "articulo", "completo",
                     "mil", "pesos", "na")

#loop
prensa_palabras <- map(ruta_scraping |> purrr::set_names(), ~{
  #x_archivo <- ruta_scraping[3]
  x_archivo <- .x
  message(glue("tokenizando {x_archivo}"))
  
  #limpiado <- prensa$soyiquique |>   
  palabras <- prensa_limpia[[x_archivo]] |> 
    unnest_tokens(output = palabras, input = texto,
                  token = "regex", pattern = " ",
                  to_lower = T, drop = F) |> 
    #filtrar stopwords
    filter(!palabras %in% c(stopwords, stopwords_extra)) |> 
    #poner identificador
    mutate(fuente = x_archivo)
  
  return(palabras)
})

message("OK tokenizado")