library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones_scraping.r")

#enlaces ----
categorias <- c("https://www.adnradio.cl/noticias/nacional/",
                "https://www.adnradio.cl/noticias/politica/")

#loop enlaces ----
resultados_links <- map_df(categorias, \(enlace_categoria) {
  # enlace_categoria <- categorias[1]
  
  if (is.null(revisar_url(enlace_categoria))) return(NULL) 
  
  enlace_categoria_1 <- enlace_categoria |> bow() |> 
    scrape()
  
  enlaces <- enlace_categoria_1 |>
    html_elements("h3") |> 
    html_elements("a") |> 
    html_attr("href") |> 
    unique()
  
  noticias_links <- tibble("enlace" = paste0("https://www.adnradio.cl", enlaces),
                           "origen" = enlace_categoria) |> 
    filter(!stringr::str_detect(enlace, "facebook\\.com"),
           !stringr::str_detect(enlace, "series-y-peliculas"),
           !stringr::str_detect(enlace, "wa\\.me"),
           !stringr::str_detect(enlace, "twitter\\.com")) |> 
    filter(nchar(enlace) > 30)
  
  
  message(glue("Se obtuvieron {nrow(noticias_links)} noticias en {enlace_categoria}"))
  return(noticias_links)
})


#loop ----
resultados_adnradio <- map(unique(resultados_links$enlace), \(enlace) {
  # enlace <- resultados_links$enlace[4]
  
  #revisar si existe la página
  if (is.null(revisar_url(enlace))) return(NULL)   
  
  
  #scraping
  scraping <- function() {
    tryCatch(bow(enlace) |> scrape(),
             error = function() stop("error")
    )
  }
  
  #repetir 3 veces, esperando 15 segundos entre cada una
  noticia <- tryCatch(retry::retry(scraping(), when = "error", interval = 15, max_tries = 3, timeout = 60),
                      error = function(e) { stop("error: ", e); return(NULL) }
  )

  
  noticia_titulo <- noticia |> 
    html_elements("header") |> 
    html_elements("h1") |> 
    html_text()
  
  noticia_bajada <- noticia |>
    html_elements("header") |> 
    html_elements("h3") |>
    html_text() |> 
    purrr::pluck(1)
  
  # noticia_fecha <- noticia |> 
  #   html_elements(".cnt-byline") |> 
  #   html_elements(".a_ti") |> 
  #   html_attr("datetime") |> 
  #   unique() |> 
  #   purrr::pluck(1)
  
  noticia_fecha <- enlace |> 
    str_extract("\\d{4}/\\d{2}/\\d{2}")
  
  noticia_cuerpo <- noticia |> 
    html_elements(".cnt-txt") |> 
    html_elements("p") |> 
    html_text() |> 
    paste(collapse = "\n")
  
  noticia_tabla <- list("titulo" = noticia_titulo,
                        "bajada" = noticia_bajada,
                        "cuerpo" = noticia_cuerpo,
                        "fecha" = noticia_fecha,
                        "fecha_scraping" = lubridate::today(),
                        "fuente" = "adnradio",
                        "url" = enlace)
  
  return(noticia_tabla)
})

# guardar ----
dir.create("resultados/adnradio/")
#resultados_adnradio

readr::write_rds(resultados_adnradio,
                 glue("resultados/adnradio/adnradio_cron_{lubridate::today()}.rds"))

message(glue("listo cron adnradio {lubridate::now()}"))
