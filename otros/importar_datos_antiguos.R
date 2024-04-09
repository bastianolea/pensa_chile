# carga todos los reusltados individuales, los une y los limpia

library(fs)
library(purrr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

# cargar ----

# obtener rutas de archivos de resultados
.fuente = "agricultura"

ruta <- glue("/Users/baolea/resultados_scraping/{.fuente}/")

resultados <- dir_ls(ruta, recurse = T, regexp = ".rds")

# cargar datos, convertir los que son lista a dataframes
datos_tmp <- map(resultados, \(dato) {
  # browser()
  dato <- readr::read_rds(dato)
  
  if (length(dato) == 0) return(NULL)
  
  if ("list" %in% class(dato)) {
    dato_plano <- dato
    # message(length(dato_plano))
    dato_plano_2 <- map(1:length(dato_plano), ~lapply(dato_plano[[.x]], paste, collapse ="\n"))
    
    dato <- bind_rows(dato_plano_2)
  }
  
  dato <- dato |> 
    mutate(across(everything(), as.character))
  
  return(dato)
}, .progress = TRUE)


datos_tmp_2 <- datos_tmp |> 
  list_rbind() |> 
  distinct(url, .keep_all = TRUE) |> 
  mutate(fuente = ifelse(is.na(fuente), .fuente, fuente)) |> 
  mutate(fuente = str_remove(fuente, "_pais"))

datos_tmp_2 |> 
  count(fuente)

.fuente2 <- str_remove(.fuente, "_pais")

ruta_guardado <- glue("~/Documents/Apps Shiny/prensa/resultados/{.fuente2}/{.fuente2}_cron_2022.rds")

# guardar
readr::write_rds(datos_tmp_2, ruta_guardado, compress = "gz")
