library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones_scraping.r")

#enlaces ----
categorias <- c("https://www.radioagricultura.cl/nacional/",
                "https://www.radioagricultura.cl/economia/",
                "https://www.radioagricultura.cl/se-dijo-en-agricultura/",
                "https://www.radioagricultura.cl/columnas-de-opinion/")

#loop enlaces ----
resultados_links <- map_df(categorias, \(enlace_categoria) {
  
  enlace_categoria_1 <- enlace_categoria |> 
    bow() |> 
    scrape()
  
  enlaces <- enlace_categoria_1 |>
    html_elements(".entry-title") |> 
    html_elements("a") |> 
    html_attr("href")
  
  noticias_links <- tibble("enlace" = enlaces,
                           "origen" = enlace_categoria)
  
  
  message(glue("Se obtuvieron {nrow(noticias_links)} noticias en {enlace_categoria}"))
  return(noticias_links)
})


#loop ----
resultados_agricultura <- map(resultados_links$enlace, \(enlace) {
  #enlace <- glue("https://lakalle.cl{resultados_links$enlace[6]}")
  #enlace <- resultados_links$enlace[39]
  
  #revisar si existe la página
  if (is.null(revisar_url(enlace))) return(NULL)   
  
  #scraping
  noticia <- enlace |> bow() |> scrape()
  
  noticia_titulo <- noticia |> 
    html_elements(".entry-header") |> 
    html_elements(".entry-title") |> 
    html_text2()
  
  # noticia_bajada <- noticia |>
  #   html_elements(".contenido") |>
  #   html_elements("header") |>
  #   html_elements(".detalle") |>
  #   html_text()
  
  noticia_fecha <- noticia |> 
    html_elements(".entry-meta") |> 
    #html_elements("time") |> 
    html_elements(".published") |> 
    html_attr("datetime")
  
  noticia_cuerpo <- noticia |> 
    html_elements(".entry-content") |> 
    html_elements("p") |> 
    html_text() |> 
    paste(collapse = "\n")
  
  noticia_tabla <- list("titulo" = noticia_titulo,
                        "bajada" = "",
                        "cuerpo" = noticia_cuerpo,
                        "fecha" = noticia_fecha,
                        "fecha_scraping" = lubridate::today(),
                        "fuente" = "agricultura",
                        "url" = enlace)
  
  return(noticia_tabla)
})

# guardar ----
#resultados_agricultura
dir.create("resultados/agricultura/")

readr::write_rds(resultados_agricultura,
                 glue("resultados/agricultura/agricultura_cron_{lubridate::today()}.rds"))

message(glue("listo cron agricultura {lubridate::now()}"))
