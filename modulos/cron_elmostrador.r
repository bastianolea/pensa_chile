library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones_scraping.r")

# este scraping no funciona, porque sin importar qué enlace le pongo, obtiene las mismas 10 noticias

#enlaces ----
# secciones <- c(paste0("https://www.elmostrador.cl/categoria/pais/page/", 1:3), #"https://www.elmostrador.cl/noticias/pais/",
#                paste0("https://www.elmostrador.cl/categoria/opinion/page/", 1:3),
#                paste0("https://www.elmostrador.cl/categoria/dia/page/", 1:3),
#                paste0("https://www.elmostrador.cl/categoria/mercados/page/", 1:3)
# ); hist = ""

# para descargar noticias anteriores
# secciones <- paste0("https://www.elmostrador.cl/categoria/pais/page/", 1:500); hist = "_h"
# secciones <- paste0("https://www.elmostrador.cl/categoria/pais/page/", 500:4000); hist = "_h_b"
# secciones <- paste0("https://www.elmostrador.cl/categoria/pais/page/", 4000:5000); hist = "_h_a"
# secciones <- paste0("https://www.elmostrador.cl/categoria/pais/page/", 5001:6000); hist = "_h_b"
# ultima pagina es 11385

secciones <- paste0("https://www.elmostrador.cl/categoria/pais/page/", 1); hist = ""


resultados_enlaces <- map(secciones, \(enlace_seccion) {
  # enlace_seccion <- secciones[1]
  
  tryCatch({
  #revisar url válida
  if (is.null(revisar_url(enlace_seccion))) return(NULL)
  
  seccion <- bow(enlace_seccion) |> scrape()
  
  enlaces <- seccion |>
    html_elements("h4") |>
    html_elements("a") |>
    html_attr("href")
  
  message(glue("Se obtuvieron {length(enlaces)} noticias en {enlace_seccion}"))
  return(enlaces)
  },
  error = function(e) {
    message("Error en scraping: ", e)
    return(NULL)
  })
})

enlaces_elmostrador <- resultados_enlaces |> unlist()


#loop ----
resultados_elmostrador <- map(enlaces_elmostrador, \(enlace) {
  #enlace <- enlaces_elmostrador[5]
  
  tryCatch({
  #revisar si existe la página
  if (is.null(revisar_url(enlace))) return(NULL) 
  
  #scraping
  noticia <- enlace |> bow() |> scrape()
  
  # #en opinion y mercado
  # noticia_titulo_a <- noticia |>
  #   html_elements(".avatar-y-titulo") |>
  #   html_text2()
  # 
  # #en pais o destacado
  # noticia_titulo_b <- noticia |>
  #   html_elements(".titulo-single") |>
  #   html_text2()
  
  noticia_titulo <- noticia |>
    html_elements(".d-the-single__title") |> 
    html_text2()
  
  #   noticia_titulo <- ifelse(length(noticia_titulo_a) != 0,
  #                            noticia_titulo_a,
  #                            noticia_titulo_b)
  
  # #fecha
  # noticia_fecha_a <- noticia |>
  #   html_elements(".autor-y-fecha") |>
  #   html_text()
  
  noticia_fecha <- noticia |>
    html_elements(".d-the-single__date") |> 
    html_attr("datetime")
  
  # #bajada (puede no tener)
  # noticia_bajada <- noticia |>
  #   html_elements(".bloque-principal") |>
  #   html_elements("figcaption") |>
  #   html_text()
  
  noticia_bajada <- noticia |>
    html_elements(".d-the-single__excerpt") |> 
    html_text2()
  
  # #texto
  # noticia_texto <- noticia |>
  #   html_elements(".bloque-principal") |>
  #   html_elements("#noticia") |>
  #   html_text() |> 
  #   paste(collapse = "\n")
  
  noticia_texto <-  noticia |>
    html_elements(".d-the-single__wrapper") |> 
    html_text2()
  
  noticia_tabla <- tibble("titulo" = noticia_titulo |> validar_elementos(),
                          "bajada" = noticia_bajada |> validar_elementos(),
                          "cuerpo" = noticia_texto |> validar_elementos(),
                          "fecha" = noticia_fecha |> validar_elementos(),
                          "fecha_scraping" = lubridate::today(),
                          "fuente" = "elmostrador",
                          "url" = enlace)
  
  return(noticia_tabla)
},
error = function(e) {
  message("Error en scraping: ", e)
  return(NULL)
})
})

# guardar ----
dir.create("resultados/elmostrador/")

readr::write_rds(resultados_elmostrador,
                 glue("resultados/elmostrador/elmostrador_cron_{lubridate::today()}{hist}.rds"))

message(glue("listo cron elmostrador {lubridate::now()}"))
