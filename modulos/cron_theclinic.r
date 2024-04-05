library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones_scraping.r")

#enlaces ----

enlaces <- c("https://www.theclinic.cl/noticias/politica/",
             paste0("https://www.theclinic.cl/noticias/politica/page/", 2:3),
             "https://www.theclinic.cl/noticias/reportajes/",
             paste0("https://www.theclinic.cl/noticias/reportajes/page/", 2:3),
             "https://www.theclinic.cl/lo-ultimo/",
             paste0("https://www.theclinic.cl/lo-ultimo/page/", 2:3)); hist = ""

# para descargar noticias anteriores
# enlaces <- c(paste0("https://www.theclinic.cl/noticias/politica/page/", 4:354),
#             paste0("https://www.theclinic.cl/noticias/reportajes/page/", 4:200),
#             paste0("https://www.theclinic.cl/lo-ultimo/page/", 4:1000)); hist = "_h_a"

# enlaces <- c(paste0("https://www.theclinic.cl/lo-ultimo/page/", 1001:2000)); hist = "_h_b"


# obtener enlaces de noticias ----
resultados_enlaces <- map(enlaces, \(enlace) {
  # enlace <- enlaces[1]
  
  tryCatch({
    # if (is.null(revisar_url(enlace))) return(NULL)  
    
    sesion <- bow(enlace) |> scrape()
    
    enlaces <- sesion |> 
      html_elements(".titulares") |> 
      html_elements("a") |> 
      html_attr("href")
    
    message(glue("Se obtuvieron {length(enlaces)} noticias en {enlace}"))
    return(enlaces)
  },
  error = function(e) {
    message("error en scraping theclinic: ", e)
    return(NULL)
  })
})


enlaces_theclinic <- resultados_enlaces |> 
  unlist() |> 
  unique()


# scraping ----
resultados_theclinic <- map_df(enlaces_theclinic, \(enlace) {
  # enlace <- enlaces_theclinic[3]
  # enlace <- "https://www.theclinic.cl/2024/04/03/daniel-jadue-formalizacion-frente-amplio-cde/"
  
  #revisar si existe la página
  # if (is.null(revisar_url(enlace))) return(NULL)  
  message("scraping ", enlace)
  
  tryCatch({
    noticia <- bow(enlace) |> scrape()
    
    #elementos
    titulo <- noticia |> html_elements(".principal") |> html_elements("h1") |> html_text2()
    
    bajada <- noticia |> html_elements(".principal") |> html_elements(".bajada") |> html_text2()
    
    fecha <- enlace |> str_extract("\\d{4}\\/\\d{2}\\/\\d{2}")
    
    cuerpo <- noticia |> html_elements(".the-content") |> html_elements("p") |> html_text2() |> 
      paste(collapse = "\n")
    
    #unir
    noticia_data <- tibble("titulo" = titulo |> validar_elementos(),
                           "bajada" = bajada |> validar_elementos(),
                           "fecha" = fecha |> validar_elementos(),
                           "cuerpo" = cuerpo |> validar_elementos(),
                           "fuente" = "theclinic",
                           "url" = enlace,
                           "fecha_scraping" = lubridate::now())
    return(noticia_data)
  },
  error = function(e) {
    message("Error en scraping theclinic: ", e)
    return(NULL)}
  )
})

# guardar ----
dir.create("resultados/theclinic/", showWarnings = F)

readr::write_rds(resultados_theclinic, 
                 glue("resultados/theclinic/theclinic_cron_{lubridate::today()}{hist}.rds"))

message(glue("listo cron theclinic {lubridate::now()}"))