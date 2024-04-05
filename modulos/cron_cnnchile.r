library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones_scraping.r")

#enlaces ----
# secciones <- "https://www.cnnchile.com/category/pais/"
secciones <- paste0("https://www.cnnchile.com/category/pais/page/", 1:5); hist = ""

# para descargar noticias anteriores
# secciones <- paste0("https://www.cnnchile.com/category/pais/page/", 100:1500)
# secciones <- paste0("https://www.cnnchile.com/category/pais/page/", 1500:2000); hist = "_h"
# secciones <- paste0("https://www.cnnchile.com/category/pais/page/", 2000:2500); hist = "_h_b"
# secciones <- paste0("https://www.cnnchile.com/category/pais/page/", 2500:3000); hist = "_h_c"
# secciones <- paste0("https://www.cnnchile.com/category/pais/page/", 3001:3500); hist = "_h_d"



#loop enlaces
resultados_links <- map_df(secciones, ~{
  enlace_seccion <- .x
  #enlace_seccion <- secciones

  #revisar si existe la página
  if (is.null(revisar_url(enlace_seccion))) return(NULL)

  noticias_seccion <- bow(enlace_seccion) |>
    scrape()

  noticias_seccion_links <- noticias_seccion |>
    html_elements(".inner-list") |>
    html_elements(".inner-item") |>
    #html_elements("h4") |>
    html_elements("a") |>
    html_attr("href") |>
    unique()

  noticias_links <- tibble("enlace" = noticias_seccion_links,
                           "origen" = enlace_seccion)

  message(glue("Se obtuvieron {nrow(noticias_links)} noticias en {enlace_seccion}"))
  return(noticias_links)
})
# 
# # resultados_links
# # 
# # resultados_links |> readr::write_rds("~/Collahuasi/seguimiento-scraping/P4_prensa/scraping/resultados_scraping/cnnchile/links_extendidos.rds")
#resultados_links <-  readr::read_rds("~/Collahuasi/seguimiento-scraping/P4_prensa/scraping/resultados_scraping/cnnchile/links_extendidos.rds")


#loop ----
resultados_cnnchile <- map(resultados_links$enlace, ~{
  #enlace <- resultados_links$enlace[6]
  enlace <- .x

  #revisar si existe la página
  if (is.null(revisar_url(enlace))) return(NULL) 

  #scraping
  noticia <- enlace |> bow() |> scrape()

  noticia_titulo <- noticia |>
    html_elements(".main-single-header") |>
    html_elements(".main-single-header__title") |>
    html_text()

  noticia_fecha <- noticia |>
    html_elements(".main-single-about") |>
    html_elements(".main-single__date") |>
    html_text()

  noticia_bajada <- noticia |>
    html_elements(".main-single-header") |>
    html_elements("p") |>
    html_text()

  #texto
  noticia_texto <- noticia |>
    html_elements(".main-single-body__content") |>
    html_elements("p") |>
    html_text() |>
    paste(collapse="\n")

  noticia_url <- enlace

  noticia_tabla <- list("titulo" = noticia_titulo,
                          "bajada" = noticia_bajada,
                          "cuerpo" = noticia_texto,
                          "fecha" = noticia_fecha,
                          "fecha_scraping" = lubridate::today(),
                          "fuente" = "cnnchile",
                          "url" = noticia_url)

  return(noticia_tabla)
})

# guardar ----
# resultados_cnnchile
dir.create("resultados/cnnchile/")

readr::write_rds(resultados_cnnchile,
                 glue("resultados/cnnchile/cnnchile_cron_{lubridate::today()}{hist}.rds"))

message(glue("listo cron cnnchile {lubridate::now()}"))
