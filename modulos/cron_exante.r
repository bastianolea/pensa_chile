library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones_scraping.r")

#enlaces ----
# enlaces <- "https://www.ex-ante.cl/category/nacional/2024/03/10/"

# para obtener fechas anteriores
fecha_inicio <- today() %m-% days(3); hist = ""
fecha_fin <- today()
# fecha_inicio <- today() %m-% months(6); hist = "_h"

# fecha_fin <- today() %m-% months(6)
# fecha_inicio <- today() %m-% months(30); hist = "_hb"

fechas <- seq.Date(from = fecha_inicio, to = fecha_fin, 
                   by = "days") |> 
  format("%Y/%m/%d")

enlaces <- paste0("http://www.ex-ante.cl/category/nacional/", fechas, "/")


## enlaces noticias ----
resultados_enlaces <- purrr::map(enlaces, \(enlace) {
  # enlace <- enlaces[152]
  
  # if (is.null(revisar_url(enlace))) return(NULL)  
  
  sesion <- bow(enlace) |> 
    scrape()
  
  noticias_enlaces <- sesion |> 
    html_elements(".titulo_grande") |> 
    html_elements("a") |> 
    html_attr("href")
  
  message(glue("Se obtuvieron {length(noticias_enlaces)} noticias en {enlace}"))
  
  return(noticias_enlaces)
})

enlaces_exante <- resultados_enlaces |>
  unlist() |>
  unique()


#scraping ----
resultados_exante <- map_df(enlaces_exante, \(enlace) {
  # enlace <- enlaces_exante[2]
  
  # if (is.null(revisar_url(enlace))) return(NULL)   
  message("scraping ", enlace)
  
  #obtener respuesta
  respuesta <- bow(url = enlace, force = TRUE, verbose = TRUE) |>
    tryCatch(error = function(error) {
      warning(glue("error en {enlace}: error en respuesta"))
      warning(error)
      return(NULL)
    })
  
  noticia <- scrape(respuesta)
  
  if (is.null(noticia)) {
    warning(glue("error en scrape {enlace}: scrape null"))
    return(NULL)
  }
  
  x_titulo <- noticia |> 
    html_elements("h1") |> 
    html_text2()
  
  x_fecha_texto <- noticia |> 
    html_elements(".fecha") |> 
    html_text2()
  
  mes <- x_fecha_texto |> str_extract("\\w+") |> 
    recode("Enero" = "1", "Febrero" = "2", "Marzo" = "3",
                             "Abril" = "4", "Mayo" = "5", "Junio" = "6",
                             "Julio" = "7", "Agosto" = "8", "Septiembre" = "9",
                             "Octubre" = "10", "Noviembre" = "11", "Diciembre" = "12")
  
  dia_año <-  x_fecha_texto |> 
    str_extract("\\d+, \\d{4}") |> 
    str_remove(",")
  
  año <- dia_año |> str_extract("\\d{4}$")
  
  dia <- dia_año |> str_extract("^\\d+")
  
  x_fecha <- paste(año, mes, dia)
    
  x_bajada <- noticia |> 
    html_elements(".contenido-noticia") |> 
    html_elements("h4") |> 
    html_text2()
  
  x_texto <- noticia |> 
    html_elements(".contenido-noticia") |> 
    html_elements("p") |> 
    html_text2() |> 
    paste(collapse = "\n")
  
  resultado <- tibble("titulo" = x_titulo[1],
                      "fecha" = x_fecha[1],
                      "fecha_original" = x_fecha_texto[1],
                      "fecha_scraping" = lubridate::now(),
                      "bajada" = x_bajada[1],
                      "cuerpo" = x_texto[1],
                      "fuente" = "exante",
                      "url" = enlace)
  
  return(resultado)
})

#guardar ----
# resultados_exante
dir.create("resultados/exante/", showWarnings = F)

readr::write_rds(resultados_exante, 
                 glue("resultados/exante/exante_cron_{lubridate::today()}{hist}.rds"))

message(glue("listo cron exante {lubridate::now()}"))
