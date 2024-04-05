# carga todos los reusltados individuales, los une y los limpia
tictoc::tic()

library(fs)
library(future)
library(purrr)
library(furrr)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)

plan(multisession, workers = 4)


# datos ----
carpetas_modulos <- dir_ls("resultados")
archivos_modulos <- map(carpetas_modulos, ~dir_ls(.x, regexp = ".rds"))


# carga ----
# cargar resultados de módulos 
modulos_cargados <- future_map(archivos_modulos, \(archivo_modulo) {
  # archivo_modulo <- archivos_modulos[[1]]
  
  # cargar todos los archivos de la carpeta del módulo
  resultados_modulo <- map(unlist(archivo_modulo), read_rds)
  
  # por cada pieza de datos del módulo, convertir a dataframe si es necesario
  modulo_cargado <- map(resultados_modulo, \(resultado_modulo) {
    # resultado_modulo <- resultados_modulo[[2]]
    
    # clase_interna <- resultado_modulo |> pluck(1) |> class()
    
    # # aplanar listas
    # if ("tbl" %in% class(resultado_modulo)) {
    #   resultado_modulo2 <- resultado_modulo
    # } else if (class(resultado_modulo) == "list") {
    #   resultado_modulo2 <- resultado_modulo #|> list_flatten()
    # }
    
    # convertir los que son lista a dataframes
    if ("list" %in% class(resultado_modulo)) {
      # pegar partes de la lista, si fueran más de 1 por columna
      resultado_modulo3 <- map(1:length(resultado_modulo), ~lapply(resultado_modulo[[.x]], paste, collapse ="\n"))
      # resultado_modulo3 <- map(resultado_modulo2, ~lapply(.x, paste, collapse ="\n")) |> list_flatten()
      
      # resultado_modulo3[[1]] |> tibble::enframe() |> tidyr::pivot_wider(values_fn = ~paste(.x, collapse = ""))
      
      # resultado_modulo4 <- map(resultado_modulo3, ~{
      #   tibble::enframe(.x) |> tidyr::pivot_wider(values_fn = ~paste(.x, collapse = ""))
      # })
      # resultado_modulo4 <- map(resultado_modulo3, ~as_tibble(.x, .name_repair = "unique"))
      # resultado_modulo3  |> bind_rows()
      # resultado_modulo3  |> list_flatten() |> bind_rows()
      resultado_modulo4 <- map(resultado_modulo3, bind_rows)
      resultado_modulo <- resultado_modulo4 |> list_rbind() |> mutate(across(everything(), as.character))
      
    }
    
    if (!"tbl" %in% class(resultado_modulo)) return(NULL) #solo retornar si funciona
    
    resultado_modulo_pre <- resultado_modulo |> mutate(across(everything(), as.character))
    
    return(resultado_modulo_pre)
  })
  
  datos <- modulo_cargado |> 
    list_rbind()
  
  return(datos)
}); beepr::beep()

# modulos_cargados |> bind_rows() |> count(fuente)

# revisar fechas
# modulos_cargados[["resultados/meganoticias"]] |>
#   select(url, fecha) |> distinct() |> 
#   # filter(is.na(fecha)) |> 
#   mutate(fecha2 = str_extract(url,  "(?<=(-))(\\d{2}-\\d+-\\d{4})(?=\\.html)") ) |> 
#   filter(is.na(fecha2))

# modulos_cargados[["resultados/latercera"]] |>
#   select(url, fecha, fecha2, fecha_textual) |> 
#   distinct() |>
#   filter(is.na(fecha2))
# en la tercera, las columnas de opinión vienen con la fecha distinta, y no se obtiene


# limpieza ----
datos_prensa <- future_map(modulos_cargados, \(modulo) {
  # modulo <- modulos_cargados[[2]]
  
  datos_2 <- modulo |>
    distinct(url, .keep_all = TRUE) |> 
    mutate(fuente = str_remove(fuente, "_pais")) |> 
    filter(nchar(titulo) > 20)
  
  # que tengan todas las columnas necesarias
  if (!"bajada" %in% names(datos_2)) datos_2 <- datos_2 |> mutate(bajada = NA_character_)
  
  ## limpiar fechas ----
  datos_3 <- datos_2 |> 
    # extracción de fecha por fuente
    mutate(fecha2 = as_date(fecha)) |> 
    mutate(fecha2 = if_else(fuente == "cnnchile", str_extract(url, "\\d{8}") |> as_date(), fecha2)) |> 
    mutate(fecha2 = if_else(fuente == "adnradio", str_extract(url, "(?<=(\\/))(\\d{4}\\/\\d{2}\\/\\d{2})(?=\\/)") |> ymd(), fecha2)) |> 
    mutate(fecha2 = if_else(fuente == "24horas", dmy(fecha), fecha2)) |> 
    mutate(fecha2 = if_else(fuente == "exante", ymd(fecha), fecha2)) |> 
    mutate(fecha2 = if_else(fuente == "meganoticias" & is.na(fecha2), str_extract(url, "(?<=(-))(\\d{2}-\\d+-\\d{4})(?=\\.html)") |> dmy(), fecha2)) |> 
    # extracción general
    mutate(fecha2 = if_else(is.na(fecha2), str_extract(fecha, "\\d{4}.\\d{2}.\\d{2}") |> ymd(), fecha2)) |> 
    mutate(fecha2 = if_else(is.na(fecha2), str_extract(fecha, "\\d{2}.\\d{2}.\\d{4}") |> dmy(), fecha2)) |> 
    mutate(fecha2 = if_else(is.na(fecha2), str_extract(fecha, "\\d{2}.\\d{1}.\\d{4}") |> dmy(), fecha2)) |> 
    mutate(fecha2 = if_else(is.na(fecha2), str_extract(url, "\\d{4}-\\d{2}-\\d{2}") |> ymd(), fecha2)) |> 
    mutate(fecha2 = if_else(is.na(fecha2), str_extract(url, "\\d{2}-\\d{2}-\\d{4}") |> dmy(), fecha2)) |> 
    mutate(fecha2 = if_else(is.na(fecha2), str_extract(url, "\\d{4}\\/\\d{2}\\/\\d{2}") |> ymd(), fecha2)) |> 
    mutate(fecha2 = if_else(is.na(fecha2), str_extract(url, "\\d{2}\\/\\d{2}\\/\\d{4}") |> dmy(), fecha2)) |>
    # excepciones por fuente
    mutate(fecha2 = if_else(fuente == "elciudadano" & is.na(fecha2), paste("2024", str_extract(url, "(?<=(\\/))\\d{2}\\/\\d{2}(?=\\/)")) |> ymd(), fecha2)) |>
    mutate(fecha_original = fecha, 
           fecha = fecha2,
           año = year(fecha))
  
  ## eliminar textos ----
  datos_4 <- datos_3 |> 
    mutate(cuerpo_limpio = tolower(cuerpo),
           cuerpo_limpio = str_replace_all(cuerpo_limpio, "dfp:|\\n|\\r", " ")) |> 
    mutate(cuerpo_limpio = str_replace_all(cuerpo_limpio, "[0-9]", " "),
           cuerpo_limpio = str_replace_all(cuerpo_limpio, "[[:punct:]]", " "),
           cuerpo_limpio = str_replace_all(cuerpo_limpio, "\\||\\<|\\>|@|-|—|\\{|\\}|\\[|\\]|\\=|“", " "),
           cuerpo_limpio = str_trim(cuerpo_limpio) |> str_squish() |> tolower()) |> 
    mutate(titulo = str_trim(titulo),
           bajada = str_trim(bajada))
  
  # finalizar
  datos_prensa <- datos_4 |> 
    ungroup() |> 
    select(fuente, fecha, titulo, bajada, cuerpo, cuerpo_limpio, url, año) 
  
  return(datos_prensa)
}) |> list_rbind(); beepr::beep()


# revisar ----
# 
# datos_prensa |> 
#   filter(is.na(fecha)) |> 
#   count(fuente)
# 
# max(datos_prensa$fecha, na.rm = T)
# 
# datos_prensa |> filter(is.na(fecha))
# 
# # noticias por año
# datos_prensa |> 
#   mutate(año = year(fecha)) |> 
#   count(año) |> 
#   arrange(desc(año))
# 
# # noticias por fuente
# datos_prensa |> 
#   count(fuente) |> 
#   arrange(desc(n))
# 
# # noticias por mes
# datos_prensa |> 
#   mutate(fecha = floor_date(fecha, "month")) |> 
#   count(fecha) |> 
#   arrange(desc(fecha))


# guardar ----
arrow::write_feather(datos_prensa |> 
                       arrange(desc(fecha)) |> 
                       filter(!is.na(fecha)), 
                     "datos/prensa_datos.feather")

tictoc::toc()
plan(multisession)
