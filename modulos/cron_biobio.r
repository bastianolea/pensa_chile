library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones_scraping.r")

#enlaces ----
secciones <- c("https://www.biobiochile.cl/lista/busca-2020/categorias/nacional",
               "https://www.biobiochile.cl/lista/busca-2020/categorias/economia",
               "https://www.biobiochile.cl/lista/categorias/opinion")

# para bajar noticias antiguas, scrapear por api
# scraping_prensa("modulos/cron_biobio_api.r")

#loop para encontrar noticias en cada sección
resultados_links <- map_df(secciones, \(enlace_seccion) {
  
  #revisar url válida
  if (is.null(revisar_url(enlace_seccion))) return(NULL)
  
  noticias_seccion <- bow(enlace_seccion) |>
    scrape()
  
  noticias_seccion_links <- noticias_seccion |>
    html_elements(".article-content-container") |>
    html_elements("a") |>
    html_attr("href")
  
  noticias_links <- tibble("enlace" = noticias_seccion_links,
                           "origen" = enlace_seccion) |> 
    filter(nchar(enlace) > 30) |> 
    filter(!stringr::str_detect(enlace, "/autores/"))
  
  message(glue("Se obtuvieron {nrow(noticias_links)} noticias en {enlace_seccion}"))
  return(noticias_links)
})

resultados_links


#loop ----
resultados_biobio <- map(resultados_links$enlace, \(enlace) {
  # enlace <- resultados_links$enlace[6]
  # enlace <- "https://www.biobiochile.cl/noticias/nacional/region-de-valparaiso/2024/03/28/suben-a-21-los-casos-de-dengue-en-rapa-nui-alcalde-pide-al-minsal-invertir-para-prevenir-enfermedad.shtml"
  
  #revisar si existe la página
  if (is.null(revisar_url(enlace))) return(NULL) 
  
  #scraping
  noticia <- enlace |> bow() |> scrape()
  
  noticia_titulo <- noticia |>
    html_elements(".post-title") |>
    html_text2() |> 
    pluck(1)
  
  #fecha
  # noticia_fecha <- noticia |>
  #   html_elements(".post-date") |>
  #   html_text()
  
  noticia_fecha <- noticia |> 
    html_elements("meta") |> 
    #html_attrs()
    html_attrs_dfr() |> #obtiene todas las propiedades
    tibble() |> 
    filter(itemprop == "datePublished") |> 
    pull(content) |> 
    lubridate::as_date()
  #<meta itemprop="datePublished" content="2022-09-13T09:57:11-03:00">
  
  #bajada (puede no tener)
  noticia_bajada <- noticia |>
    html_elements(".post-excerpt") |>
    #html_elements("figcaption") |>
    html_text2() |> 
    paste(collapse = "\n")
  
  #texto
  noticia_texto <- noticia |>
    html_elements(".clearfix") |>
    html_elements("p") |> 
    #html_elements("#noticia") |>
    html_text2() |> 
    paste(collapse = "\n")
  
  noticia_tabla <- list("titulo" = noticia_titulo,
                        "bajada" = noticia_bajada,
                        "cuerpo" = noticia_texto,
                        "fecha" = noticia_fecha,
                        "fecha_scraping" = lubridate::today(),
                        "fuente" = "biobio_pais",
                        "escala" = "nacional",
                        "url" = enlace)
  
  return(noticia_tabla)
})

# guardar ----
dir.create("resultados/biobio/")

readr::write_rds(resultados_biobio,
                 glue("resultados/biobio/biobio_cron_{lubridate::today()}.rds"))

message(glue("listo cron biobio {lubridate::now()}"))