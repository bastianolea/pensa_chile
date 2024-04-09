# PRENSA: PASO 1
# carga todos los resultados individuales en una lista por fuente, donde cada elemento de la lista es un dataframe con una noticia por fila,
# luego revisa cada elemento de la lista y separa los con demasiadas filas en partes más pequeñas, 
# y finalmente procesa esas partes paralelamente, donde se interpretan las fechas y se limpia el texto, y los resultados se guardan al disco.
# finalmente, se cargan las partes preprocesadas y se escriben como una sola base.
# input: archivos rds individuales de la carpeta resultados
# output: prensa_datos.feather
# tiempo estimado: 30 minutos aprox.

tictoc::tic()

library(fs)
library(future)
library(purrr)
library(furrr)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
source("funciones_scraping.r")

plan(multisession, workers = 4)


# datos ----
carpetas_modulos <- dir_ls("resultados")
archivos_modulos <- map(carpetas_modulos, ~dir_ls(.x, regexp = ".rds"))


# carga ----
# cargar resultados de módulos 
modulos_cargados <- future_map(archivos_modulos, \(archivo_modulo) {
  # archivo_modulo <- archivos_modulos[[3]]
  # archivo_modulo <- archivos_modulos[["resultados/emol"]]
  
  # cargar todos los archivos de la carpeta del módulo
  resultados_modulo <- map(unlist(archivo_modulo), read_rds)
  
  # por cada pieza de datos del módulo, convertir a dataframe si es necesario
  modulo_cargado <- map(resultados_modulo, \(resultado_modulo) {
    # resultado_modulo <- resultados_modulo[[3]]
    
    # revisar si datos son válidos
    if ("list" %in% class(resultado_modulo)) {
      if (length(resultado_modulo) == 0) return(NULL)
    }
    if ("tbl" %in% class(resultado_modulo)) {
      if (nrow(resultado_modulo) == 0) return(NULL)
    }
    
    # convertir los que son lista a dataframes
    if ("list" %in% class(resultado_modulo)) {
      # pegar partes de la lista, si fueran más de 1 por columna
      resultado_modulo3 <- map(1:length(resultado_modulo), ~lapply(resultado_modulo[[.x]], paste, collapse ="\n"))
      
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


# map(modulos_cargados, nrow)

# separar fuentes con muchos datos en partes mas pequeñas para luego procesarlas
modulos_cargados <- map(modulos_cargados, \(modulo) {
  # modulo <- modulos_cargados[[2]]
  
  filas <- nrow(modulo)
  grupos <- filas %/% 10000 #un grupo cada 10000 observaciones
  
  if (filas >= 10000) {
  modulo <- modulo |> 
    mutate(grupos = (row_number()-1) %/% (n()/grupos)) |> 
    group_by(grupos) |> 
    group_split()
  } else {
    modulo <- modulo |> 
      mutate(grupos = 0)
  }
    
  return(modulo)
}) |> list_flatten()


# modulos_cargados_split[1:4]

# limpieza ----
future_walk(modulos_cargados, \(modulo) {
  # modulo <- modulos_cargados[[1]]
  
  datos_2 <- modulo |>
    distinct(url, .keep_all = TRUE) |> 
    mutate(fuente = str_remove(fuente, "_pais")) |> 
    filter(nchar(titulo) > 20)
  
  if (nrow(datos_2) == 0) return(NULL)
  
  # que tengan todas las columnas necesarias
  if (!"bajada" %in% names(datos_2)) datos_2 <- datos_2 |> mutate(bajada = NA_character_)
  
  ## limpiar fechas ----
  datos_3 <- datos_2 |> 
    # extracción de fecha por fuente
    mutate(fecha2 = as_date(fecha)) |> 
    # mutate(fecha2 = if_else(fuente == "cnnchile", str_extract(url, "\\d{8}") |> as_date(), fecha2)) |> 
    # mutate(fecha2 = if_else(fuente == "adnradio", str_extract(url, "(?<=(\\/))(\\d{4}\\/\\d{2}\\/\\d{2})(?=\\/)") |> ymd(), fecha2)) |> 
    # mutate(fecha2 = if_else(fuente == "24horas", dmy(fecha), fecha2)) |> 
    # mutate(fecha2 = if_else(fuente == "exante", ymd(fecha), fecha2)) |> 
    # mutate(fecha2 = if_else(fuente == "meganoticias" & is.na(fecha2), str_extract(url, "(?<=(-))(\\d{2}-\\d+-\\d{4})(?=\\.html)") |> dmy(), fecha2)) |> 
    mutate(fecha2 = case_when(fuente == "cnnchile" ~ str_extract(url, "\\d{8}") |> as_date(),
                              fuente == "adnradio" ~ str_extract(url, "(?<=(\\/))(\\d{4}\\/\\d{2}\\/\\d{2})(?=\\/)") |> ymd(),
                              fuente == "24horas" ~ dmy(fecha),
                              fuente == "exante" ~ ymd(fecha),
                              fuente == "meganoticias" & is.na(fecha2) ~ str_extract(url, "(?<=(-))(\\d{2}-\\d+-\\d{4})(?=\\.html)") |> dmy(), 
                              .default = fecha2)) |> 
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
    # ordenar
    mutate(fecha_original = fecha, 
           fecha = fecha2,
           año = year(fecha))
  
  ## eliminar textos ----
  datos_4 <- datos_3 |> 
    mutate(titulo = limpiar_texto_poquito(titulo),
           bajada = limpiar_texto_poquito(bajada),
           cuerpo = limpiar_texto_poquito(cuerpo)) |> 
    mutate(cuerpo_limpio = limpiar_texto(cuerpo),
           bajada_limpia = limpiar_texto(bajada),
           titulo_limpio = limpiar_texto(titulo))
  
  # finalizar
  datos_prensa <- datos_4 |> 
    select(fuente, fecha, titulo, bajada, cuerpo, cuerpo_limpio, url, año) |> 
    # crear ids unicos por noticia
    rowwise() |>
    mutate(id = rlang::hash(url)) |> 
    ungroup()
  
  fuente <- unique(datos_prensa$fuente)
  grupo <- unique(datos_4$grupos)
  
  # guardar resultados individuales
  readr::write_rds(datos_prensa, 
                   file = paste0("datos/preprocesados/", fuente, "_", grupo, ".rds"))
  remove(datos_prensa, datos_4, datos_3, datos_2, modulo)
  # beepr::beep()
})

plan(multisession)

# unir ----
datos_prensa <- map(dir_ls("datos/preprocesados"), read_rds) |> 
  list_rbind()

# revisar ----
# noticias por fuente
# datos_prensa |>
#   count(fuente) |>
#   arrange(desc(n))
# 
# # noticias por año
# datos_prensa |>
#   mutate(año = year(fecha)) |>
#   count(año) |>
#   arrange(desc(año))
# 
# # noticias sin fecha por fuente
# datos_prensa |>
#   filter(is.na(fecha)) |>
#   count(fuente)
# 
# # noticias por mes
# datos_prensa |>
#   mutate(fecha = floor_date(fecha, "month")) |>
#   count(fecha) |>
#   arrange(desc(fecha))


# guardar ----
arrow::write_feather(datos_prensa, "datos/prensa_datos.feather")

beepr::beep()
tictoc::toc()
plan(multisession)
remove(modulos_cargados, resultados_modulo, modulo_cargado)