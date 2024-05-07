library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones_scraping.r")

#enlaces ----
enlaces <- c(paste0("https://www.latercera.com/etiqueta/nacional/page/", 1:5),
             "https://www.latercera.com/etiqueta/politica/",
             "https://www.latercera.com/etiqueta/economia/"); hist = ""

# para descargar noticias anteriores
# enlaces <- paste0("https://www.latercera.com/etiqueta/nacional/page/", 100:592); hist = "_h" #ok

# enlaces <- paste0("https://www.latercera.com/etiqueta/politica/page/", 210:400); hist = "_h_a" #hasta 667 (p 210 es aprox final de 2022)
# enlaces <- paste0("https://www.latercera.com/etiqueta/politica/page/", 401:667); hist = "_h_b"

# enlaces <- paste0("https://www.latercera.com/categoria/la-tercera-pm/page/", 190:400); hist = "_h_c" #hasta 667 (p 190 es aprox final de 2022)
# enlaces <- paste0("https://www.latercera.com/categoria/la-tercera-pm/page/", 401:667); hist = "_h_d"


# la tercera nacional solo llega a la página 592, y desde la página 530 aprox (desde febrero de 2022 hacia atrás), cada día empeiza a tener muchas menos noticias

resultados_enlaces <- purrr::map_df(enlaces, \(enlace) {
  
  if (is.null(revisar_url(enlace))) return(NULL)  
  
  noticias_sesion <- session(enlace) |> 
    read_html() |>
    html_elements(".hl") |> 
    html_elements("a") |> 
    html_attr("href")
  
  noticias_link <- noticias_sesion |> 
    tibble() |> 
    filter(nchar(noticias_sesion) > 40,
           !str_detect(noticias_sesion, "practico/noticia")) |> 
    distinct()
  
  message(glue("Se obtuvieron {nrow(noticias_link)} noticias en {enlace}"))
  
  return(noticias_link)
})


enlaces_lt <- resultados_enlaces |>
  pull(noticias_sesion) |>
  unique()

#loop ----
resultados_latercera <- map_df(enlaces_lt, \(enlace) {
  
  enlace <- glue("https://www.latercera.com{enlace}")
  #x_url <- enlaces_lt[6]
  message(glue("scraping {enlace}"))
  
  #revisar si existe la página
  if (is.null(revisar_url(enlace))) return(NULL)  
  
  tryCatch({
    noticia_lt <- enlace |> bow() |> scrape()
    
    #elementos
    x_titulo <- noticia_lt |> html_elements(".defecto") |> html_elements(".hl") |> html_text()
    
    #intentar si no tiene título
    if (length(x_titulo) == 0) { 
      x_titulo <- noticia_lt |> html_elements(".titulares") |> html_elements(".hl") |> html_text()
      
      #salir si no tiene título
      if (length(x_titulo) == 0) { 
        message("sin titulo, next") 
        return(NULL) 
      }
    }
    
    x_bajada <- noticia_lt |> html_elements(".defecto") |> html_elements(".excerpt") |> html_text()
    
    #intentar si no tiene texto
    if (length(x_bajada) == 0) { 
      x_bajada <- noticia_lt |> html_elements(".excerpt") |> html_text()
    }
    
    #fecha textual
    x_fecha_0 <- noticia_lt |> html_elements(".d-flex-center") |> html_text() |> unique()
    x_fecha_texto <- x_fecha_0 |> str_remove("^\\w+ \\w+ ") |> str_remove("Tiempo de lectura.*$") |> str_trim()
    
    #fecha desde codigo
    #<time datetime="Sat Jan 06 2018 13:09:38 GMT-0300 (-03)" title="6 ene 2018" class="p-left-10 "><small><b>6 ene 2018</b> 01:09 PM</small></time>
    x_fecha <- noticia_lt |> 
      html_elements(".d-flex") |> 
      html_elements("time") |> 
      html_attr("datetime") |> 
      stringr::str_remove("^\\w+") |> 
      stringr::str_trim() |> 
      stringr::str_extract("^\\w+ \\w+ \\w+") |> 
      lubridate::mdy() |> 
      as.character()
    
    x_cuerpo <- noticia_lt |> html_elements(".abody-col") |> html_elements(".paragraph") |> html_text() |> 
      paste(collapse = "\n")
    
    #unir
    noticia_data <- tibble("titulo" = x_titulo |> validar_elementos(),
                           "bajada" = x_bajada |> validar_elementos(),
                           "fecha" = x_fecha |> validar_elementos(),
                           "fecha_original" = x_fecha_0 |> validar_elementos(),
                           "fecha_textual" = x_fecha_texto |> validar_elementos(),
                           "cuerpo" = x_cuerpo |>  validar_elementos(),
                           "fuente" = "latercera_pais",
                           "url" = enlace,
                           "fecha_scraping" = lubridate::now())
    
    Sys.sleep(1)
    return(noticia_data)
  },
  error = function(e) {
    message("Error en scraping latercera: ", e)
    return(NULL)}
  )
})

# guardar ----
# resultados_latercera
dir.create("resultados/latercera/")

readr::write_rds(resultados_latercera, 
                 glue("resultados/latercera/latercera_cron_{lubridate::today()}{hist}.rds"))

message(glue("listo cron latercera {lubridate::now()}"))
