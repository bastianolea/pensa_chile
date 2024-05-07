library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones_scraping.r")

#enlaces ----
secciones <- paste0("https://www.elciudadano.com/chile/page/", 1:3); hist = ""

# para descargar hacia atrás
# secciones <- paste0("https://www.elciudadano.com/chile/page/", 200:1000); hist = "_h"
# secciones <- paste0("https://www.elciudadano.com/chile/page/", 1001:1800); hist = "_h_a"
# secciones <- paste0("https://www.elciudadano.com/chile/page/", 1801:2500); hist = "_h_b"
# secciones <- paste0("https://www.elciudadano.com/chile/page/", 2501:3200); hist = "_h_c"

#loop de enlaces
resultados_links <- map_df(secciones, \(enlace_seccion) {
  # enlace_seccion <- secciones[4]

  if (is.null(revisar_url(enlace_seccion))) return(NULL)

  noticias_seccion <- bow(enlace_seccion) |>
    scrape()

  noticias_seccion_links <- noticias_seccion |>
    html_elements(".col-md-9") |>
    html_elements("a") |>
    html_attr("href") |>
    unique()

  noticias_seccion_links_2 <- noticias_seccion_links[nchar(noticias_seccion_links) > 60]

  noticias_links <- tibble("enlace" = noticias_seccion_links_2,
                           "origen" = enlace_seccion)

  message(glue("Se obtuvieron {nrow(noticias_links)} noticias en {enlace_seccion}"))
  return(noticias_links)
})

#loop ----
resultados_elciudadano <- map(resultados_links$enlace, \(enlace) {
  # enlace <- resultados_links$enlace[6]
  # enlace <- "https://www.elciudadano.com/actualidad/quienes-son-los-grandes-contaminadores-del-aire-publican-lista-de-87-plantas-industriales-sujetas-al-impuesto-verde/03/28/"

  #revisar si existe la página
  if (is.null(revisar_url(enlace))) return(NULL) 

  #scraping
  noticia <- enlace |> bow() |> scrape()

  noticia_titulo <- noticia |>
    #html_elements(".order-md-2") |>
    html_elements(".mb-4") |>
    html_text2()

  noticia_fecha <- noticia |>
    html_elements(".time-now-") |>
    html_attr("data-date") |> 
    str_extract("\\d{4}-\\d{2}-\\d{2}")

  noticia_bajada <- noticia |>
    html_elements(".order-md-2") |>
    html_elements(".the-excerpt-") |>
    html_text2()

  #texto
  noticia_texto <- noticia |>
    html_elements(".pt-3-") |>
    html_elements("p") |>
    html_text2() |>
    paste(collapse = "\n")

  noticia_tabla <- list("titulo" = noticia_titulo[1],
                          "bajada" = noticia_bajada[1],
                          "cuerpo" = noticia_texto[1],
                          "fecha" = noticia_fecha[1],
                          "fecha_scraping" = lubridate::today(),
                          "fuente" = "elciudadano",
                          "url" = enlace)

  return(noticia_tabla)
})

# guardar ----
dir.create("resultados/elciudadano/")
# resultados_elciudadano

readr::write_rds(resultados_elciudadano,
                 glue("resultados/elciudadano/elciudadano_cron_{lubridate::today()}{hist}.rds"))

message(glue("listo cron elciudadano {lubridate::now()}"))
