library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones_scraping.r")


# enlaces ----
categorias <- "https://www.t13.cl/lo-ultimo"

#loop enlaces ----
resultados_links <- map_df(categorias, ~{
  enlace_categoria <- .x
  
  enlace_categoria_1 <- enlace_categoria |> bow() |> 
    scrape()
  
  enlaces <- enlace_categoria_1 |>
    html_elements("#last-news-container") |> 
    html_elements(".item-article") |> 
    html_attr("href")
  
  noticias_links <- tibble("enlace" = enlaces,
                           "origen" = enlace_categoria) |> 
    filter(!stringr::str_detect(enlace, "/videos/"))
  
  message(glue("Se obtuvieron {nrow(noticias_links)} noticias en {enlace_categoria}"))
  return(noticias_links)
})


#loop ----
resultados_t13 <- map(resultados_links$enlace, ~{
  #enlace <- glue("https://www.t13.cl{resultados_links$enlace[1]}")
  enlace <- glue("https://www.t13.cl{.x}")
  
  #revisar si existe la página
  if (is.null(revisar_url(enlace))) return(NULL)   
  
  #scraping
  noticia <- enlace |> bow() |> scrape()
  
  noticia_titulo <- noticia |> 
    html_elements(".article-component__header-title") |> 
    html_text()
  
  noticia_bajada <- noticia |> 
    html_elements(".article-component__lead") |> 
    html_text()
  
  noticia_fecha <- noticia |> 
    html_elements(".article-component__info") |> 
    html_elements("time") |> 
    html_attr("datetime")
  
  noticia_cuerpo <- noticia |> 
    html_elements(".article-component__body") |> 
    html_elements("p") |> 
    html_text() |> 
    paste(collapse = "\n")
  
  noticia_url <- enlace
  
  noticia_tabla <- list("titulo" = noticia_titulo,
                        "bajada" = noticia_bajada,
                        "cuerpo" = noticia_cuerpo,
                        "fecha" = noticia_fecha,
                        "fecha_scraping" = lubridate::today(),
                        "fuente" = "t13",
                        "url" = noticia_url)
  
  return(noticia_tabla)
})

# guardar ----
#resultados_t13
dir.create("resultados/t13/")

readr::write_rds(resultados_t13,
                 glue("resultados/t13/t13_cron_{lubridate::today()}.rds"))

message(glue("listo cron t13 {lubridate::now()}"))
