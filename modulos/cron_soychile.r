# no lo puedo hacer funcionar para el cuerpo


library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones_scraping.r")

#enlaces ----
enlaces <- paste0("https://www.soychile.cl"); hist = ""

# obtener enlaces de noticias ----
resultados_enlaces <- map(enlaces, \(enlace) {
  # enlace <- enlaces[1]
  
  tryCatch({
    if (is.null(revisar_url(enlace))) return(NULL)  
    
    sesion <- bow(enlace) |> scrape()
    
    noticias <- sesion |> 
      html_elements(".media-heading") |> 
      html_elements("a") |> 
      html_attr("href")
    
    noticias2 <- paste0("https://www.soychile.cl", noticias)
    message(glue("Se obtuvieron {length(noticias2)} noticias en {enlace}"))
    return(noticias2)
  },
  error = function(e) {
    message("error en scraping soychile: ", e)
    return(NULL)}
  )
})


enlaces_soychile <- resultados_enlaces |> 
  unlist() |> 
  unique()


# scraping ----
resultados_soychile <- map_df(enlaces_soychile, \(enlace) {
  # enlace <- enlaces_soychile[3]
  
  tryCatch({
    if (is.null(revisar_url(enlace))) return(NULL)  
    
    noticia <- bow(enlace) |> scrape()
    
    #elementos
    titulo <- noticia |> html_elements(".note-inner-title") |> html_text2()
    
    bajada <- noticia |> html_elements(".note-inner-desc") |> html_text2()
    
    fecha <- enlace |> str_extract("\\d{4}\\/\\d{2}\\/\\d{2}")
    
    cuerpo <- noticia |> html_elements(".note-inner-text") |> html_text2() |> 
      paste(collapse = "\n")
    # no se puede obtener el texto, pero hace unos llamados json y todavía no cacho
    
    
    #unir
    noticia_data <- tibble("titulo" = titulo |> validar_elementos(),
                           "bajada" = bajada |> validar_elementos(),
                           "fecha"  = fecha  |> validar_elementos(),
                           "cuerpo" = cuerpo |> validar_elementos(),
                           "fuente" = "soychile",
                           "url" = enlace,
                           "fecha_scraping" = lubridate::now())
    return(noticia_data)
  },
  error = function(e) {
    message("error en scraping soychile: ", e)
    return(NULL)}
  )
})

# guardar ----
dir.create("resultados/soychile/", showWarnings = F)

readr::write_rds(resultados_soychile, 
                 glue("resultados/soychile/soychile_cron_{lubridate::today()}{hist}.rds"))

message(glue("listo cron soychile {lubridate::now()}"))
