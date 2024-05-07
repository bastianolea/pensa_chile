library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones_scraping.r")

#enlaces ----

enlaces <- c("https://www.chvnoticias.cl/nacional/",
  "https://www.chvnoticias.cl/casos-policiales/",
  "https://www.chvnoticias.cl/reportajes/",
  "https://www.chvnoticias.cl/cazanoticias/",
  "https://www.chvnoticias.cl/economia/")


## enlaces noticias ----
resultados_enlaces <- purrr::map(enlaces, \(enlace) {
  # enlace <- enlaces[2]
  
  if (is.null(revisar_url(enlace))) return(NULL)  
  
  sesion <- session(enlace) |> 
    read_html()
  
  noticias_enlaces <- sesion |> 
    html_elements(".category-card__title") |> 
    html_elements("a") |> 
    html_attr("href")
  
  message(glue("Se obtuvieron {length(noticias_enlaces)} noticias en {enlace}"))
  
  return(noticias_enlaces)
})

enlaces_chvnoticias <- resultados_enlaces |>
  unlist() |>
  unique()


#scraping ----
resultados_chvnoticias <- map_df(enlaces_chvnoticias, \(enlace) {
  # enlace <- enlaces_chvnoticias[2]
  
  if (is.null(revisar_url(enlace))) return(NULL)   
  
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
    html_elements(".the-single__title") |> 
    html_text2()

  codigo <- enlace |> str_extract("\\d{8}") |> str_split(pattern = "") |> unlist()
  
  x_fecha <- paste0(c(codigo[1:4], "-", codigo[5:6], "-", codigo[7:8]), collapse = "")
  
  x_bajada <- noticia |> 
    html_elements(".the-single__excerpt") |> 
    html_text2()
  
  x_texto <- noticia |> 
    html_elements(".the-single-section__text") |> 
    html_elements("p") |> 
    html_text2() |> 
    paste(collapse = "\n")
  
  if (length(x_texto) == 0) {
    x_texto <- noticia |> 
      html_elements(".the-single-box") |> 
      html_elements("p") |> 
      html_text2() |> 
      paste(collapse = "\n")
  }
  
  resultado <- tibble("titulo" = x_titulo[1],
                      "fecha" = x_fecha[1],
                      "fecha_scraping" = lubridate::now(),
                      "bajada" = x_bajada[1],
                      "cuerpo" = x_texto[1],
                      "fuente" = "chvnoticias",
                      "url" = enlace)
  
  return(resultado)
})

#guardar ----

dir.create("resultados/chvnoticias/")

readr::write_rds(resultados_chvnoticias, 
                 glue("resultados/chvnoticias/chvnoticias_cron_{lubridate::today()}.rds"))

message(glue("listo cron chvnoticias {lubridate::now()}"))
