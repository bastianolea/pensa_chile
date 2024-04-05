library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones_scraping.r")

#enlaces ----
categorias <- c("https://lahora.cl/cronica/",
                "https://lahora.cl/tv/")

#loop enlaces ----
resultados_links <- map_df(categorias, ~{
  #enlace_categoria <- categorias[1]
  enlace_categoria <- .x
  
  enlace_categoria_1 <- enlace_categoria |> bow() |> 
    scrape()
  
  enlaces <- enlace_categoria_1 |>
    html_elements(".post-title") |> 
    html_elements("a") |> 
    html_attr("href") |> 
    unique()
  
  noticias_links <- tibble("enlace" = enlaces,
                           "origen" = enlace_categoria) |> 
    filter(nchar(enlace) > 30)
  
  message(glue("Se obtuvieron {nrow(noticias_links)} noticias en {enlace_categoria}"))
  return(noticias_links)
})


enlaces <- resultados_links |> 
  mutate(url = paste0("https://lahora.cl", enlace)) |> 
  pull(url) |> 
  unique()

#loop ----
resultados_lahora <- map(enlaces, \(enlace) {
  # enlace <- enlaces[4]
  
  #revisar si existe la página
  if (is.null(revisar_url(enlace))) return(NULL)   
  
  # #scraping
  noticia <- tryCatch({
    enlace |> bow() |> scrape()
    },
    error = function(e) {
      message("error en scrape: ", e); return(NULL)
      }
  )
  
  noticia_titulo <- noticia |> 
    html_elements(".entry-header") |> 
    html_elements(".post-title") |> 
    html_text()
  
  noticia_bajada <- noticia |>
    html_elements(".single-excerpt") |> 
    html_text()
  
  noticia_fecha <- enlace |> 
    stringr::str_extract("\\d{4}.\\d{2}.\\d{2}") |> 
    lubridate::ymd()
  
  noticia_cuerpo <- noticia |> 
    html_elements(".entry-main-content") |> 
    html_elements("p") |> 
    html_text() |> 
    paste(collapse = "\n")
  
  noticia_url <- enlace
  
  noticia_tabla <- list("titulo" = noticia_titulo,
                        "bajada" = noticia_bajada,
                        "cuerpo" = noticia_cuerpo,
                        "fecha" = noticia_fecha,
                        "fecha_scraping" = lubridate::today(),
                        "fuente" = "lahora",
                        "url" = noticia_url)
  
  return(noticia_tabla)
})

# guardar ----
#resultados_lahora
dir.create("resultados/lahora/")

readr::write_rds(resultados_lahora,
                 glue("resultados/lahora/lahora_cron_{lubridate::today()}.rds"))

message(glue("listo cron lahora {lubridate::now()}"))
