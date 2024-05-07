library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones_scraping.r")

#enlaces ----
enlaces <- paste0("https://www.meganoticias.cl/nacional/?page=", 1:5); hist = ""

# para descargar noticias anteriores
# enlaces <- paste0("https://www.meganoticias.cl/nacional/?page=", 100:1500)
# enlaces <- paste0("https://www.meganoticias.cl/nacional/?page=", 1500:2000); hist = "_h"
# enlaces <- paste0("https://www.meganoticias.cl/nacional/?page=", 2000:2500); hist = "_h_b"
# enlaces <- paste0("https://www.meganoticias.cl/nacional/?page=", 2500:3000); hist = "_h_c"
# enlaces <- paste0("https://www.meganoticias.cl/nacional/?page=", 3000:3200); hist = "_h_d"
# enlaces <- paste0("https://www.meganoticias.cl/nacional/?page=", 3200:3800); hist = "_h_a"
# enlaces <- paste0("https://www.meganoticias.cl/nacional/?page=", 3801:4000); hist = "_h_b"

# enlaces <- paste0("https://www.meganoticias.cl/nacional/?page=", 4000:4200); hist = "_h_a"
# enlaces <- paste0("https://www.meganoticias.cl/nacional/?page=", 4201:4400); hist = "_h_b"
# enlaces <- paste0("https://www.meganoticias.cl/nacional/?page=", 4401:4600); hist = "_h_c"
# enlaces <- paste0("https://www.meganoticias.cl/nacional/?page=", 4601:4800); hist = "_h_d"
# enlaces <- paste0("https://www.meganoticias.cl/nacional/?page=", 4801:5000); hist = "_h_e"


resultados_enlaces <- purrr::map_df(enlaces, \(enlace) {
  #enlace <- enlaces[1]
  
  if (is.null(revisar_url(enlace))) return(NULL)  
  
  sesion <- session(enlace) |> 
    read_html()
  
  noticias_sesion <- sesion |> 
    html_elements("figcaption") |> 
    html_elements("a") |> 
    html_attr("href")
  
  noticias_link <- noticias_sesion |> 
    tibble() |> 
    filter(nchar(noticias_sesion) > 40) |> 
    distinct()
  
  message(glue("Se obtuvieron {nrow(noticias_link)} noticias en {enlace}"))
  
  return(noticias_link)
})

enlaces_mega <- resultados_enlaces |>
  pull(noticias_sesion) |>
  unique()


#loop ----
resultados_meganoticias <- map_df(enlaces_mega, \(enlace) {
  # enlace <- enlaces_mega[3]
  
  message(glue("scraping {enlace}"))
  
  #revisar si existe la página
  if (is.null(revisar_url(enlace))) return(NULL)  
  
  tryCatch({
    noticia <- enlace |> bow() |> scrape()
    
    #elementos
    x_titulo <- noticia |> html_elements("h1") |> html_text()
    
    #fecha desde codigo
    x_fecha <- noticia |> 
      html_elements(".fechaHora") |> 
      html_elements("time") |> 
      html_attr("datetime") |> 
      stringr::str_trim() |> 
      stringr::str_extract("^\\d{4}-\\d+-\\d+") |> 
      as.character()
    
    x_cuerpo <- noticia |> html_elements(".contenido-nota") |> html_elements("p") |> html_text2() |> 
      paste(collapse = "\n")
    
    #unir
    noticia_data <- tibble("titulo" = x_titulo |> validar_elementos(),
                           "fecha" = x_fecha |> validar_elementos(),
                           "cuerpo" = x_cuerpo |>  validar_elementos(),
                           "fuente" = "meganoticias",
                           "url" = enlace,
                           "fecha_scraping" = lubridate::now())
    Sys.sleep(1)
    return(noticia_data)
  },
  error = function(e) {
    message("Error en scraping meganoticias: ", e)
    return(NULL)}
  )
})

# guardar ----
dir.create("resultados/meganoticias/")

readr::write_rds(resultados_meganoticias, 
                 glue("resultados/meganoticias/meganoticias_cron_{lubridate::today()}{hist}.rds"))

message(glue("listo cron meganoticias {lubridate::now()}"))
