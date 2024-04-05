#genera fechas en columna fecha_f y limpia los casos
#retorna la lista prensa_limpia 
#source("~/Collahuasi/seguimiento-scraping/P4_prensa/P4_2_unir.r")

revisar = FALSE #opción para que diga si hay missing en la fecha de cada paso

#fechas ----

prensa_fechas <- map(ruta_scraping |> purrr::set_names(), ~{
  #fuente <- ruta_scraping[4]
  #fuente <- "lakalle"
  fuente <- .x
  message(glue("extrayendo fecha {fuente}"))
  
  if (fuente == "estrella") {
    prensa_fecha <- prensa |> 
      #prensa |> 
      pluck(fuente) |> 
      #pluck("estrella") |> 
      mutate(fecha2 = fecha |> lubridate::ymd()) |> 
      rename(fecha_f = fecha2) |> 
      select(-fecha) |> 
      mutate(bajada = case_when(!is.na(bajada) & is.na(bajada_1) & is.na(bajada_2) ~ bajada,
                                !is.na(bajada_1) & !is.na(bajada_2) ~ paste(bajada_1, bajada_2),
                                TRUE ~ paste(bajada, bajada_1, bajada_2))) |> 
      select(-bajada_1, -bajada_2) |> 
      select(-pagina)
    
  } else if (fuente == "cooperativa" | fuente == "cooperativa_pais") {
    prensa_fecha <- prensa |> 
      #la hueá no tenía url, pero ahora le integré esa columna
      pluck(fuente) |> 
      #pluck(ruta_scraping[1]) |> 
      mutate(dia = str_extract(fecha, "\\d+"),
             tmp = str_extract(fecha, "\\d+.*\\d{4}"),
             año = str_extract(tmp, "\\d{4}"),
             mes_t = str_remove(tmp, "\\d{4}") |>
               str_remove("^\\d+") |>
               str_remove_all(" de ") |>
               str_trim(),
             mes = stringi::stri_replace_all_fixed(mes_t |> tolower(), c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre") %>% tolower(),
                                                   c(1:12) %>% as.character(),
                                                   vectorize_all = F),
             fecha2 = lubridate::ymd(paste(año, mes, dia))) |> 
      rename(fecha_f = fecha2) |> 
      select(-(dia:mes)) |> 
      select(-fecha)
    
    
    
  } else if  (fuente == "latercera" | fuente == "latercera_pais") {
    
    #pre limpiar fechas
    fuente_e = fuente
    #las que vienen con las fechas limpias
    fechas_listas <- prensa |> 
      #pluck("latercera_pais") |> 
      pluck(fuente) |> 
      #arrange(desc(fecha_scraping)) |> 
      #select(fecha) |> 
      filter(stringr::str_detect(fecha, "\\d{4}-\\d{2}-\\d{2}"))
      
    fechas_pre <- prensa |> 
      #pluck("latercera_pais") |> 
      pluck(fuente) |> 
      filter(!stringr::str_detect(fecha, "\\d{4}-\\d{2}-\\d{2}")) |> #las que no tienen fecha lista
      filter(!is.na(fecha2)) |> #fecha2 es la más completa, dejar esta (hay noticias que no la tienen por un error antiguo que quedó entrwe los resultados)
      mutate(fecha3 = str_remove_all(fecha2, "\\d+\\:\\d+.*(AM|PM)")) |> 
      mutate(fecha4 = str_remove(fecha3, "Tiempo de lectura\\: \\d+ (minutos|minuto)") |> str_trim()) |> 
      mutate(hace = str_detect(fecha4, "Hace|hace")) |> 
      mutate(fecha5 = str_remove_all(fecha4, "(h|H)ace \\d+ (horas|hora|minutos|minuto)")) |> 
      mutate(fecha6 = str_remove_all(fecha5, "\\d+ (minutos|minuto)") |> str_trim())
    
    #obtener fechas cuando son del mismo día (y por ende dicen "hace 1 hora")
    fechas_hace <- fechas_pre |> 
      filter(hace) |> 
      mutate(minutos = str_detect(fecha4, "minuto")) |> 
      mutate(fecha_minutos = case_when(minutos == T ~ as_date(fecha_scraping))) |> 
      mutate(horas = case_when(minutos == F ~ str_extract(fecha4, "(Hace|hace) \\d+ (horas|hora)") |> str_extract("\\d+") |> readr::parse_integer())) |> 
      mutate(fecha_horas = case_when(!is.na(horas) ~ as_date(fecha_scraping)-hours(horas))) |> 
      #generar fechas
      mutate(fecha_hace = case_when(!is.na(fecha_minutos) ~ fecha_minutos,
                                    !is.na(fecha_horas) ~ fecha_horas)) |> 
      select(everything(), -starts_with("fecha"), fecha_scraping, fecha_hace,
             -horas, -minutos, -hace)
    
    #obtener fechas cuando están en texto completo
    fechas_general <- fechas_pre |> 
      filter(!hace) |> 
      #obtener año (4 digitos)
      mutate(año = str_extract(fecha6, "\\d{4}$"),
             #fecha sin el año
             fecha7 = str_remove(fecha6, "\\d{4}$") |> str_trim()) |> 
      #sacar palabras "de" para que el mes sea la última palabra
      mutate(fecha8 = str_remove_all(fecha7, "de") |> str_trim()) |>
      #extraer mes
      mutate(mes = str_extract(fecha8, "\\w+$")) |> 
      #extraer día sacando el mes de la fecha y obteniendo 
      mutate(fecha9 = str_remove(fecha8, "\\w+$") |> str_trim(),
             dia = str_extract(fecha9, "\\d+$")) |>
      #detectar meses
      #transformar meses de texto a numero
      mutate(mes_corto = stringi::stri_replace_all_fixed(mes |> tolower(), 
                                                         pattern = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre") |>
                                                           tolower() |> str_trunc(3, ellipsis = ""), 
                                                         replacement = c(1:12) |> as.character(), vectorize_all = F),
             mes_corto = str_extract(mes_corto, "\\d+")) |> 
      mutate(mes_largo = stringi::stri_replace_all_fixed(mes |> tolower(), 
                                                         pattern = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre") |>
                                                           tolower(), 
                                                         replacement = c(1:12) |> as.character(), vectorize_all = F),
             mes_largo = str_extract(mes_largo, "\\d+")) |> 
      mutate(mes_num = case_when(is.na(mes_largo) ~ mes_corto,
                                 !is.na(mes_largo) ~ mes_largo)) |> 
      #generar fecha
      mutate(fecha_general = ymd(paste(año, mes_num, dia))) |> 
      select(everything(), -starts_with("fecha"), fecha_scraping, fecha_general,
             -hace, -starts_with("mes"), -dia, -año)
    
    #re unir datos
    prensa_fecha <- bind_rows(fechas_listas |> rename(fecha_f = fecha) |> mutate(fecha_f = ymd(fecha_f)),
                              fechas_general |> rename(fecha_f = fecha_general),
                              fechas_hace |> rename(fecha_f = fecha_hace)
                              ) |> 
      mutate(fecha_f = if_else(is.na(fecha_f), as_date(fecha_scraping), fecha_f)) |> 
      mutate(fuente = all_of(fuente_e)) |> 
      select(-fecha2, -fecha_textual, -fecha_original)
      #select(fecha_hace, fecha_general, fecha_scraping) |> 
      #pasar a fecha_f
      # mutate(fecha_f = case_when(!is.na(fechas_listas) ~ fecha,
      #                            is.na(fecha_hace) & !is.na(fecha_general) ~ fecha_general,
      #                            !is.na(fecha_hace) ~ fecha_hace,
      #                            is.na(fecha_hace) & is.na(fecha_general) ~ as_date(fecha_scraping))) |> 
      #select(-fecha_hace, -fecha_general)
    
    
  } else if (fuente == "radiopaulina") {
    prensa_fecha <- prensa |> 
      pluck(fuente) |> 
      #pluck(ruta_scraping[4]) |> 
      mutate(fecha2 = lubridate::dmy(fecha)) |> 
      rename(fecha_f = fecha2) |> 
      select(-fecha)
    
  } else if (fuente == "soyiquique") {
    prensa_fecha <- prensa |> 
      pluck(fuente) |> 
      #pluck(ruta_scraping[5]) |> 
      mutate(fecha2 = str_extract(url, "\\d{4}/\\d+/\\d+"),
             fecha3 = lubridate::ymd(fecha2)) |> 
      rename(fecha_f = fecha3) |> 
      select(-fecha2, -fecha) |> 
      distinct(url, .keep_all = TRUE)
    # 
    # prensa |> 
    #   pluck("soyiquique") |> 
    #   slice(30) |> 
    #   pull(titulo)
    # 
    # prensa |> 
    #   pluck("soyiquique") |> 
    #   filter(stringr::str_detect(titulo,
    # "Tras discusión hombre ebrio atropella a su conviviente en tomas")) |> pull(url)
    
    
  } else if (fuente == "tarapacaonline") {
    prensa_fecha <- prensa |> 
      pluck(fuente) |> 
      #pluck(ruta_scraping[6]) |> 
      filter(url != "NA") |> 
      mutate(fecha2 = str_extract(url, "\\d{4}/\\d+/\\d+"),
             fecha3 = lubridate::ymd(fecha2)) |> 
      rename(fecha_f = fecha3) |> 
      select(-fecha2, -fecha) |> 
      mutate(bajada = "") #crear columna vacía porque el scraping no obtiene bajada
    
  } else if (fuente == "cnnchile") {
    prensa_fecha <- prensa |>
      #prensa |>
      pluck(fuente) |> 
      ##pluck("cnnchile") |> 
      mutate(fecha2 = str_extract(fecha, "\\d+\\.\\d+\\.\\d+")) |> 
      mutate(fecha_f = lubridate::dmy(fecha2)) |> 
      select(-fecha, -fecha2)
    
  } else if (fuente == "elmostrador") {
    prensa_fecha <- prensa |>
      pluck(fuente) |>
      # prensa |>
      #   pluck("elmostrador") |> 
      # #limpiar textos
      # mutate(titulo = str_remove_all(titulo, "\n"),
      #        titulo = str_trim(titulo),
      #        titulo = str_squish(titulo)) |>
      # mutate(cuerpo = str_remove_all(cuerpo, "\n"),
      #        cuerpo = str_trim(cuerpo),
      #        cuerpo = str_squish(cuerpo)) |> 
      #eliminar numeros que no son fecha
      mutate(fecha2 = str_remove(fecha, "Latinoamérica21")) |> 
      #extraer fecha
      mutate(fecha3 = str_extract(fecha2, "\\d+ \\w+.*\\d{4}")) |> 
      #numerizar meses
      mutate(fecha4 = stringi::stri_replace_all_fixed(fecha3 |> tolower(), 
                                                      pattern = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre") |>
                                                        tolower(), #|> #str_trunc(3, ellipsis = ""), 
                                                      replacement = c(1:12) |> as.character(),
                                                      vectorize_all = F)) |> 
      mutate(fecha_f = lubridate::dmy(fecha4)) |> 
      select(-fecha, -fecha2, -fecha3, -fecha4)
    # select(starts_with("fecha"), -fecha_scraping) |> 
    # filter(is.na(fecha_f))
    
    
    
  } else if (fuente == "elciudadano") {
    prensa_fecha <- prensa |> 
      pluck(fuente) |> 
      mutate(fecha_f = lubridate::as_date(fecha)) |> 
      select(-fecha)
    
    
  } else if (fuente == "elreportero") {
    prensa_fecha <- prensa |> 
      #prensa |> 
      pluck(fuente) |> 
      #pluck("elreportero") |> 
      mutate(fecha2 = stringi::stri_replace_all_fixed(fecha |> tolower(), 
                                                      pattern = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre") |>
                                                        tolower(),
                                                      replacement = c(1:12) |> as.character(),
                                                      vectorize_all = F)) |> 
      mutate(fecha3 = fecha2 |> lubridate::dmy()) |> 
      rename(fecha_f = fecha3) |> 
      select(-fecha, -fecha2)
    
    
  } else if (fuente == "elsol") {
    prensa_fecha <- prensa |> 
      #prensa |> 
      pluck(fuente) |> 
      #pluck("elsol") |> 
      #select(fecha) |> 
      mutate(fecha2 = stringr::str_remove_all(fecha, "\t") |> stringr::str_remove_all("\n")) |> 
      mutate(fecha3 = stringi::stri_replace_all_fixed(fecha2 |> tolower(), 
                                                      pattern = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre") |>
                                                        tolower(),
                                                      replacement = c(1:12) |> as.character(),
                                                      vectorize_all = F)) |> 
      mutate(fecha4 = lubridate::mdy(fecha3)) |> 
      rename(fecha_f = fecha4) |> 
      select(-fecha, -fecha2, -fecha3)
    
    
    
  } else if (fuente == "lakalle") {
    prensa_fecha <- prensa |> 
      #prensa |> 
      pluck(fuente) |> 
      #pluck("lakalle") |> 
      #select(fecha_scraping, fecha) |> 
      mutate(fecha_scraping = lubridate::ymd(fecha_scraping)) |> 
      mutate(fecha2 = stringr::str_remove_all(fecha, "\n") |> stringr::str_squish()) |> 
      select(-fecha) |> 
      mutate(horas = case_when(stringr::str_detect(fecha2, "Hace \\d+") ~ stringr::str_extract(fecha2, "\\d+") |> readr::parse_integer())) |>
      #fechas de foy y ayer
      mutate(fecha_hoy_ayer = case_when(stringr::str_detect(fecha2, "Hoy") ~ as_date(fecha_scraping),
                                        stringr::str_detect(fecha2, "Ayer") ~ as_date(fecha_scraping)-1)) |> 
      #fechas de "hace"
      mutate(fecha_hace = case_when(stringr::str_detect(fecha2, "Hace \\d+") ~ as_date(fecha_scraping)-hours(horas))) |> 
      #filter(is.na(fecha_hoy_ayer) & is.na(fecha_hace)) |> 
      #fecha de los que tienen fecha dentro
      mutate(fecha_dentro = case_when(stringr::str_detect(fecha2, "\\d{4}") ~ stringr::str_remove_all(fecha2, " de") |> tolower() |> 
                                        stringi::stri_replace_all_fixed(pattern = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre") |>
                                                                          tolower(),
                                                                        replacement = c(1:12) |> as.character(),
                                                                        vectorize_all = F) |> 
                                        stringr::str_extract("\\d+ \\d+ \\d{4}") |> 
                                        lubridate::dmy())
      ) |> 
      #filter(is.na(fecha_dentro)) |> 
      #fecha de los que dicen el día
      mutate(dia = case_when(stringr::str_detect(fecha2, "El \\w+ Por") ~ stringi::stri_replace_all_fixed(fecha2, pattern = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo") |>
                                                                                                            tolower(),
                                                                                                          replacement = c(1:7) |> as.character(),
                                                                                                          vectorize_all = F) |> 
                               stringr::str_extract("El \\d+") |> 
                               stringr::str_extract("\\d+") |> 
                               readr::parse_integer())) |> 
      mutate(inicio_semana = as_date(fecha_scraping) |> lubridate::floor_date(unit = "week", week_start = 1)) |> 
      mutate(fecha_dia = inicio_semana + lubridate::days(dia-1)) |> 
      select(-horas, -dia, -inicio_semana) |> 
      #unir fechas
      mutate(fecha_f = coalesce(fecha_hoy_ayer, fecha_hace, fecha_dentro, fecha_dia)) |> 
      select(-fecha2, -fecha_hoy_ayer, -fecha_hace, -fecha_dentro, -fecha_dia)
    
    
  } else if (fuente == "longino") {
    prensa_fecha <- prensa |> 
      #prensa |> 
      pluck(fuente) |> 
      #pluck("longino") |> 
      #select(fecha_scraping, fecha) |> 
      mutate(fecha2 = stringr::str_remove_all(fecha, "\t") |> stringr::str_remove_all("\n")) |> 
      mutate(fecha3 = stringi::stri_replace_all_fixed(fecha2, pattern = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre") |>
                                                        tolower(),
                                                      replacement = c(1:12) |> as.character(),
                                                      vectorize_all = F)) |> 
      mutate(fecha4 = lubridate::mdy(fecha3)) |> 
      rename(fecha_f = fecha4) |> 
      select(-fecha, -fecha2, -fecha3)
    
    
    
  } else if (fuente == "t13") {
    prensa_fecha <- prensa |> 
      #prensa |> 
      pluck(fuente) |> 
      #pluck("t13") |> 
      #select(fecha_scraping, fecha) |>
      mutate(fecha_f = as_date(fecha)) |> 
      select(-fecha)
    
    
  } else if (fuente == "agricultura") {
    #la fecha viene codificada en formato fecha desde el sitio, un manjar
    prensa_fecha <- prensa |> 
      #prensa |> 
      pluck(fuente) |> 
      #pluck("t13") |> 
      #select(fecha_scraping, fecha) |>
      mutate(fecha_f = as_date(fecha)) |> 
      select(-fecha)
    
    
  } else if (fuente == "adnradio") {
    #en este, por alguna razón las fechas de scraping son siempre las del día en curso... así que se saca la fecha desde la url
    prensa_fecha <- prensa |> 
      #prensa |> 
      pluck(fuente) |> 
      #pluck("adnradio") |> 
      #select(titulo, fecha, url) |> 
      #distinct(fecha, fecha_scraping)
      #select(url) |> 
      mutate(fecha2 = str_remove(url, "https://www.adnradio.cl/")) |> 
      #select(fecha2) |> 
      # mutate(fecha2_2 = str_extract(fecha2, "^.*/\\d{4}")) |>  #no va a funcionar en el año 2100
      # mutate(año = str_extract(fecha2_2, "\\d{4}")) |> 
      # mutate(fecha3 = str_remove(fecha2, fecha2_2)) |> 
      # mutate(fecha4 = str_extract(fecha3, "/\\d{2}/\\d{2}")) |> 
      # mutate(fecha5 = paste0(año, fecha4)) |> 
      # mutate(fecha_f = ymd(fecha5)) |> 
      # filter(is.na(fecha_f))
      mutate(fecha3 = str_extract(fecha2, "\\d{4}/\\d{2}/\\d{2}/")) |> 
      mutate(fecha_f = ymd(fecha3)) |> 
      select(-fecha2, -fecha3) |> 
      select(-fecha)
    
    
  } else if (fuente == "biobio" | fuente == "biobio_pais") {
    #la fecha viene codificada en formato fecha desde el sitio, un manjar
    prensa_fecha <- prensa |> 
      #prensa |> 
      #pluck("biobio") |> 
      pluck(fuente) |> 
      mutate(fecha_f = as_date(fecha)) |> 
      select(-fecha)
    
    
  } else if (fuente == "lahora") {
    #la fecha se obtiene desde las url
    prensa_fecha <- prensa |> 
      #prensa |> 
      pluck(fuente) |> 
      mutate(fecha_f = ymd(fecha)) |> 
      select(-fecha)
    
    
  } else if (fuente == "emol" | fuente == "emol_pais") {
    #la fecha se obtiene desde las url
    prensa_fecha <- prensa |> 
      #prensa |> 
      pluck(fuente) |> 
      mutate(fecha_f = ymd(fecha)) |> 
      select(-fecha)
    
    
  } else if (fuente == "diariofinanciero" | fuente == "diariofinanciero_pais") {
    #la fecha se obtiene desde las url
    prensa_fecha <- prensa |> 
      #prensa |> 
      pluck(fuente) |> 
      mutate(fecha_f = ymd(fecha)) |> 
      select(-fecha)
    
    
  }
  
  message(glue("{nrow(prensa_fecha) |> format(big.mark = '.', decimal.mark = ',')} elementos en {fuente}"))
  
  return(prensa_fecha)
})


#revisar ----
if (revisar == TRUE) {
  
  walk(names(prensa_fechas), ~{
    n_missing <- prensa_fechas[[.x]] |> select(fecha_f) |> filter(is.na(fecha_f)) |> nrow()
    message(.x, ": ", n_missing, " missings en fecha_f")
  })
  
  #revisar columnas presentes
  walk(names(prensa_fechas), ~{
    message(.x)
    print("fecha_original" %in% names(prensa_fechas[[.x]]))
  })
}


#limpiar ----
prensa_limpia <- map(ruta_scraping |> purrr::set_names(), ~{
  #x_archivo <- ruta_scraping[4]
  x_archivo <- .x
  message(glue("limpiando {x_archivo}"))
  
  #la columna de fecha que importa es fecha_f
  
  limpiado <- prensa_fechas[[x_archivo]] |> 
    #reconocer primer scraping, si es que hay duplicados
    #group_by(url) |> 
    #mutate(fecha_scraping_primer = min(fecha_scraping, na.rm = T)) |> 
    ungroup() |> 
    #eliminar insuficientes
    filter(!is.na(titulo)) %>%
    #formato fecha original, para mantener columna "fecha" si es que está (en realidad esto no sirve para nada, así que se desactiva)
    #{ if ("fecha" %in% names(.)) mutate(., fecha_original = as.character(fecha)) |> select(-fecha) else .} %>%
    #eliminar duplicados
    arrange(desc(fecha_f)) |> 
    distinct(titulo, .keep_all = T) |> 
    #limpiar texto
    mutate(across(c(where(is.character), -url), ~ #str_remove_all(.x, "[:punct:]") |> #puntuación
                    #str_remove_all("\\d+") |> #números
                    #str_remove_all("NA") |>
                    stringr::str_remove_all(.x, "\t") |>
                    stringr::str_remove_all("\n") |>
                    stringr::str_remove_all("\\|") |> #símbolos
                    #str_remove_all("\\[VIDEO\\] ") |> 
                    tidyr::replace_na("") |> 
                    stringr::str_squish() |> 
                    stringr::str_trim())) %>%
    ##arreglar columnas de estrella, que tiene dos bajadas
    #{ if (any(names(.) %in% "bajada_1")) mutate(., bajada = paste(bajada_1, bajada_2)) else . } |> 
    #poner bajada vacía para tarapacaonline que no tiene
    #{ if (any(names(.) %in% "bajada") == FALSE) mutate(., bajada = "") else . } |> 
    #unir texto
    #mutate(texto = paste(titulo, bajada, cuerpo)) |> 
    #poner identificador
    mutate(fuente = x_archivo) |> 
    #poner local/nacional
    mutate(escala = definir_escala(fuente)) |> 
    mutate(fecha_scraping = as_date(fecha_scraping))
  
  return(limpiado)
  message("OK limpieza")
})
