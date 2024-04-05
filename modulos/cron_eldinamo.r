library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones_scraping.r")

#enlaces ----
enlaces <- paste0("https://www.eldinamo.cl/pais/page/", 1:3); hist = ""

# para descargar noticias anteriores
# enlaces <- paste0("https://www.eldinamo.cl/pais/page/", 6:200); hist = "_h"
# enlaces <- paste0("https://www.eldinamo.cl/pais/page/", 201:500); hist = "_h_b"
# enlaces <- paste0("https://www.eldinamo.cl/pais/page/", 500:1000); hist = "_h_a"
# enlaces <- paste0("https://www.eldinamo.cl/pais/page/", 1001:2000); hist = "_h_b"


# obtener enlaces de noticias ----
resultados_enlaces <- map(enlaces, \(enlace) {
  # enlace <- enlaces[1]
  
  tryCatch({
    if (is.null(revisar_url(enlace))) return(NULL)  
    
    sesion <- bow(enlace) |> scrape()
    
    noticias <- sesion |> 
      html_elements(".titulares") |> 
      html_elements("a") |> 
      html_attr("href")
    
    message(glue("Se obtuvieron {length(noticias)} noticias en {enlace}"))
    return(noticias)
  },
  error = function(e) {
    message("error en scraping eldinamo: ", e)
    return(NULL)}
  )
})


enlaces_eldinamo <- resultados_enlaces |> 
  unlist() |> 
  unique()


# scraping ----
resultados_eldinamo <- map_df(enlaces_eldinamo, \(enlace) {
  # enlace <- enlaces_dinamo[3]
  
  tryCatch({
    if (is.null(revisar_url(enlace))) return(NULL)  
    
    noticia <- bow(enlace) |> scrape()
    
    #elementos
    titulo <- noticia |> html_elements(".principal") |> html_elements("h1") |> html_text()
    
    bajada <- noticia |> html_elements(".principal") |> html_elements(".bajada") |> html_text()
    
    fecha <- enlace |> str_extract("\\d{4}\\/\\d{2}\\/\\d{2}")
    
    cuerpo <- noticia |> html_elements(".the-content") |> html_elements("p") |> html_text2() |> 
      paste(collapse = "\n")
    
    #unir
    noticia_data <- tibble("titulo" = titulo |> validar_elementos(),
                           "bajada" = bajada |> validar_elementos(),
                           "fecha"  = fecha  |> validar_elementos(),
                           "cuerpo" = cuerpo |> validar_elementos(),
                           "fuente" = "eldinamo",
                           "url" = enlace,
                           "fecha_scraping" = lubridate::now())
    return(noticia_data)
  },
  error = function(e) {
    message("error en scraping eldinamo: ", e)
    return(NULL)}
  )
})

# guardar ----
dir.create("resultados/eldinamo/", showWarnings = F)

readr::write_rds(resultados_eldinamo, 
                 glue("resultados/eldinamo/eldinamo_cron_{lubridate::today()}{hist}.rds"))

message(glue("listo cron eldinamo {lubridate::now()}"))
