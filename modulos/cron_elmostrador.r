library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones_scraping.r")

#enlaces ----
secciones <- c(paste0("https://www.elmostrador.cl/categoria/pais/page/", 1:3), #"https://www.elmostrador.cl/noticias/pais/",
               "https://www.elmostrador.cl/claves/congreso/",
               "https://www.elmostrador.cl/claves/politica/"); hist = ""

# para descargar noticias anteriores
# secciones <- paste0("https://www.elmostrador.cl/categoria/pais/page/", 1:500); hist = "_h"
# secciones <- paste0("https://www.elmostrador.cl/categoria/pais/page/", 500:4000); hist = "_h_b"
# secciones <- paste0("https://www.elmostrador.cl/categoria/pais/page/", 4000:5000); hist = "_h_a"
# secciones <- paste0("https://www.elmostrador.cl/categoria/pais/page/", 5001:6000); hist = "_h_b"

#loop para encontrar noticias en cada sección
resultados_links <- map_df(secciones, \(enlace_seccion) {
  # enlace_seccion <- secciones[1]

  #revisar url válida
  if (is.null(revisar_url(enlace_seccion))) return(NULL)

  noticias_seccion <- bow(enlace_seccion) |>
    scrape()

  noticias_seccion_links <- noticias_seccion |>
    html_elements("h4") |>
    html_elements("a") |>
    html_attr("href")

  noticias_links <- tibble("enlace" = noticias_seccion_links,
                           "origen" = enlace_seccion)

  message(glue("Se obtuvieron {nrow(noticias_links)} noticias en {enlace_seccion}"))
  return(noticias_links)
})


#loop ----
resultados_elmostrador <- map(resultados_links$enlace, ~{
  #enlace <- resultados_links$enlace[6]
  enlace <- .x

  #revisar si existe la página
  if (is.null(revisar_url(enlace))) return(NULL) 

  #scraping
  noticia <- enlace |> bow() |> scrape()

  # noticia <- "https://www.elmostrador.cl/dia/2019/04/09/clientes-mas-protegidos-diputados-aprueban-proyecto-sobre-fraudes-en-tarjetas-y-pagos-electronicos/" |>
  #   bow() |> scrape()
  #
  # noticia_0c <- "https://www.elmostrador.cl/noticias/opinion/columnas/2021/10/20/una-clase-politica-desorientada/" |>
  #   bow() |> scrape()

  #en opinion y mercado
  noticia_titulo_a <- noticia |>
    html_elements(".avatar-y-titulo") |>
    html_text()

  #en pais o destacado
  noticia_titulo_b <- noticia |>
    html_elements(".titulo-single") |>
    html_text()

  noticia_titulo <- ifelse(length(noticia_titulo_a) != 0,
                           noticia_titulo_a,
                           noticia_titulo_b)

  #fecha
  noticia_fecha <- noticia |>
    html_elements(".autor-y-fecha") |>
    html_text()

  #bajada (puede no tener)
  noticia_bajada <- noticia |>
    html_elements(".bloque-principal") |>
    html_elements("figcaption") |>
    html_text()

  #texto
  noticia_texto <- noticia |>
    html_elements(".bloque-principal") |>
    html_elements("#noticia") |>
    html_text() |> 
    paste(collapse = "\n")

  noticia_url <- enlace

  noticia_tabla <- list("titulo" = noticia_titulo,
                          "bajada" = noticia_bajada,
                          "cuerpo" = noticia_texto,
                          "fecha" = noticia_fecha,
                          "fecha_scraping" = lubridate::today(),
                          "fuente" = "elmostrador",
                          "url" = noticia_url)

  return(noticia_tabla)
})

# guardar ----
dir.create("resultados/elmostrador/")

readr::write_rds(resultados_elmostrador,
                 glue("resultados/elmostrador/elmostrador_cron_{lubridate::today()}{hist}.rds"))

message(glue("listo cron elmostrador {lubridate::now()}"))
