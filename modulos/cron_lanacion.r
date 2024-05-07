library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones_scraping.r")

#enlaces ----
enlaces <- paste0("https://www.lanacion.cl/nacional/page/", 1:3); hist = ""

# para descargar noticias anteriores
# enlaces <- paste0("https://www.lanacion.cl/nacional/page/", 4:500); hist = "_h_a"
# enlaces <- paste0("https://www.lanacion.cl/nacional/page/", 501:1000); hist = "_h_b"
# enlaces <- paste0("https://www.lanacion.cl/nacional/page/", 1001:1500); hist = "_h_c"
# enlaces <- paste0("https://www.lanacion.cl/nacional/page/", 1501:2000); hist = "_h_d"
# enlaces <- paste0("https://www.lanacion.cl/nacional/page/", 2001:2500); hist = "_h_e"
# enlaces <- paste0("https://www.lanacion.cl/nacional/page/", 2501:3000); hist = "_h_f"
# enlaces <- paste0("https://www.lanacion.cl/nacional/page/", 3001:3100); hist = "_h_g"
# enlaces <- paste0("https://www.lanacion.cl/nacional/page/", 3101:3200); hist = "_h_h"
# enlaces <- paste0("https://www.lanacion.cl/nacional/page/", 3201:3300); hist = "_h_i"
# hasta 3587

# obtener enlaces de noticias ----
resultados_enlaces <- map(enlaces, \(enlace) {
  # enlace <- enlaces[1]
  
  tryCatch({
    if (is.null(revisar_url(enlace))) return(NULL)  
    
    sesion <- bow(enlace) |> scrape()
    
    noticias <- sesion |> 
      html_elements(".entry-title") |> 
      html_elements("a") |> 
      html_attr("href")
    
    message(glue("Se obtuvieron {length(noticias)} noticias en {enlace}"))
    return(noticias)
  },
  error = function(e) {
    message("error en scraping lanacion: ", e)
    return(NULL)}
  )
})


enlaces_lanacion <- resultados_enlaces |> 
  unlist() |> 
  unique()


# scraping ----
resultados_lanacion <- map_df(enlaces_lanacion, \(enlace) {
  # enlace <- enlaces_lanacion[3]
  # enlace <- "https://www.lanacion.cl/ministra-arredondo-debera-enfrentar-demanda-por-violacion-de-derechos-humanos-por-polemica-en-bafona/"
  
  tryCatch({
    if (is.null(revisar_url(enlace))) return(NULL)  
    
    noticia <- bow(enlace) |> scrape()
    
    #elementos
    titulo <- noticia |> html_elements(".tdb-title-text") |> html_text2()
    
    bajada <- noticia |> html_elements(".wpb_wrapper") |> html_elements(".extracto") |> html_text2()
    
    fecha_texto <- noticia |> html_elements(".entry-date") |> html_text2()
    
    
    
    mes <- fecha_texto[1] |> str_extract("\\w+") |> 
      recode("Enero" = "1", "Febrero" = "2", "Marzo" = "3",
             "Abril" = "4", "Mayo" = "5", "Junio" = "6",
             "Julio" = "7", "Agosto" = "8", "Septiembre" = "9",
             "Octubre" = "10", "Noviembre" = "11", "Diciembre" = "12")
    
    dia_año <- fecha_texto[1] |> 
      str_extract("\\d+, \\d{4}") |> 
      str_remove(",")
    
    año <- dia_año |> str_extract("\\d{4}$")
    
    dia <- dia_año |> str_extract("^\\d+")
    
    fecha <- paste(año, mes, dia)
    
    cuerpo <- noticia |> html_elements(".tdb-block-inner") |> html_elements("p") |> html_text2() |> 
      paste(collapse = "\n")
    
    #unir
    noticia_data <- tibble("titulo" = titulo |> validar_elementos(),
                           "bajada" = bajada |> validar_elementos(),
                           "fecha"  = fecha  |> validar_elementos(),
                           "cuerpo" = cuerpo |> validar_elementos(),
                           "fuente" = "lanacion",
                           "url" = enlace,
                           "fecha_scraping" = lubridate::now())
    return(noticia_data)
  },
  error = function(e) {
    message("error en scraping lanacion: ", e)
    return(NULL)}
  )
})

# guardar ----
dir.create("resultados/lanacion/", showWarnings = F)

readr::write_rds(resultados_lanacion, 
                 glue("resultados/lanacion/lanacion_cron_{lubridate::today()}{hist}.rds"))

message(glue("listo cron lanacion {lubridate::now()}"))
