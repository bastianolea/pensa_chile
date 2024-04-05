library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones_scraping.r")

#enlaces ----
categorias = "https://www.df.cl"

#loop enlaces inicio ----
resultados_links <- map_df(categorias, ~{
  enlace_categoria <- .x

  #revisar si existe la página
  if (is.null(revisar_url(enlace_categoria))) return(NULL)

  enlace_categoria_1 <- enlace_categoria |> bow() |> scrape()

  enlaces <- enlace_categoria_1 |>
    html_elements(".pad-lr") |>
    html_elements(".col-md-12") |>
    html_elements("a") |>
    html_attr("href") |>
    unique() |> 
    stringr::str_trim()

  
  noticias_links <- tibble("enlace" = enlaces,
                           "origen" = enlace_categoria) |>
    filter(nchar(enlace) > 20) |> 
    filter(!stringr::str_detect(enlace, "taxport")) |> 
    filter(!stringr::str_detect(enlace, "total-brands")) |> 
    filter(!stringr::str_detect(enlace, "dfsud"))

  message(glue("Se obtuvieron {nrow(noticias_links)} noticias en {enlace_categoria}"))
  return(noticias_links)
})

#loop enlaces categorías ----
categoria_mineria = "https://www.df.cl/noticias/site/tax/port/all/taxport_1_2__1.html"
categoria_medioambiente = "https://www.df.cl/noticias/site/tax/port/all/taxport_1_10__1.html"
categoria_politica = "https://www.df.cl/noticias/site/tax/port/all/taxport_2_16__1.html"
categoria_energia = "https://www.df.cl/noticias/site/tax/port/all/taxport_1_3__1.html"

categorias_2 <- c(categoria_mineria,
                  categoria_medioambiente,
                  categoria_politica,
                  categoria_energia)

resultados_links_categorias <- map_df(categorias_2, ~{
  #enlace_categoria <- categorias_2[3]
  enlace_categoria <- .x
  
  #revisar si existe la página
  if (is.null(revisar_url(enlace_categoria))) return(NULL)
  
  enlace_categoria_2 <- enlace_categoria |> bow() |> scrape()
  
  enlaces <- enlace_categoria_2 |>
    html_elements("#wrap-noticias") |>
  #  html_elements(".col-md-12") |>
    html_elements("a") |>
    html_attr("href") |>
    unique() |> 
    stringr::str_trim()
  
  
  noticias_links <- tibble("enlace" = enlaces,
                           "origen" = enlace_categoria) |>
    filter(nchar(enlace) > 20) |> 
    filter(!stringr::str_detect(enlace, "taxport")) |> 
    filter(!stringr::str_detect(enlace, "total-brands")) |> 
    filter(!stringr::str_detect(enlace, "dfsud"))
  
  message(glue("Se obtuvieron {nrow(noticias_links)} noticias en {enlace_categoria}"))
  return(noticias_links)
})

#unir ----
resultados_links_2 <- bind_rows(resultados_links,
                                resultados_links_categorias)


#loop ----
resultados_diariofinanciero <- map(unique(resultados_links_2$enlace), \(enlace) {
  enlace <- if_else(str_detect(enlace, "^http"), 
                    enlace,
                    paste0("https://www.df.cl", enlace))
  
  #revisar si existe la página
  if (is.null(revisar_url(enlace))) return(NULL)   
  
  tryCatch({
    noticia <- enlace |> bow() |> scrape()
    
    titulo_noticia <- noticia |> 
      html_elements("#titulo_articulo") |> 
      html_text()
    
    bajada_noticia <- noticia |> 
      html_elements(".bajada") |> 
      html_text()
    
    cuerpo_noticia <- noticia |> 
      html_elements(".CUERPO") |> 
      html_elements("p") |> 
      html_text() |> 
      paste(collapse = "\n")
    
    fecha_noticia <- noticia |> 
      html_elements("script") |> 
      html_attrs_dfr() |> #obtiene todas las propiedades
      tibble() |>
      filter(type == "application/ld+json") |>
      pull(`.text`) |>
      #   stringr::str_remove_all("^.*datePublished") |>
      # print()
      stringr::str_extract('"datePublished\":\"\\d{4}-\\d{2}-\\d{2}') |> 
      stringr::str_extract('\\d{4}-\\d{2}-\\d{2}') |> 
      lubridate::ymd() |> 
      as.character()
    
    #<script type="application/ld+json">
    #"datePublished":"2022-09-07T15:50:25Z",
    
    
    noticia_tabla <- list("titulo" = titulo_noticia,
                          "bajada" = bajada_noticia,
                          "cuerpo" = cuerpo_noticia,
                          "fecha" = fecha_noticia,
                          "fecha_scraping" = lubridate::today(),
                          "fuente" = "diariofinanciero",
                          "url" = enlace)
    
    return(noticia_tabla)
  },
  error = function(e) {
    message("Error en ", enlace, ": ", e)
    return(NULL)
  })
})

# guardar ----
dir.create("resultados/diariofinanciero/")
readr::write_rds(resultados_diariofinanciero,
                 glue("resultados/diariofinanciero/diariofinanciero_cron_{lubridate::today()}.rds"))

message(glue("listo cron diariofinanciero {lubridate::now()}"))