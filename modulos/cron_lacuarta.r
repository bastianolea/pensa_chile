library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones_scraping.r")

#enlaces ----
enlaces <- paste0("https://www.lacuarta.com/cronica/", 1:9); hist = ""
# no deja descargar más allá de la 9
# https://www.lacuarta.com/cronica/7/?iu=21671350435%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F&google_preview=sPw6VemE%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F&gdfp_req=1%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F/
# https://www.lacuarta.com/cronica/8/?google_preview=8BehrtiYgS8YuNPKmAYwuO__nwaIAYCAgOCEwfqAsQE%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F&gdfp_req=1%2F%2F%2F%2F%2F&iu=21671350435%2F%2F%2F%2F%2F%2F%2F/
# para descargar noticias anteriores


# obtener enlaces de noticias ----
resultados_enlaces <- map(enlaces, \(enlace) {
  # enlace <- enlaces[1]
  
  tryCatch({
    if (is.null(revisar_url(enlace))) return(NULL)  
    
    sesion <- bow(enlace) |> scrape()
    
    noticias <- sesion |> 
      html_elements(".article-title") |> 
      html_elements("a") |> 
      html_attr("href")
    
    noticias <- paste0("https://www.lacuarta.com", noticias)
    
    message(glue("Se obtuvieron {length(noticias)} noticias en {enlace}"))
    return(noticias)
  },
  error = function(e) {
    message("error en scraping lacuarta: ", e)
    return(NULL)}
  )
})


enlaces_noticias <- resultados_enlaces |> 
  unlist() |> 
  unique()


# scraping ----
resultados_noticias <- map_df(enlaces_noticias, \(enlace) {
  # enlace <- enlaces_noticias[3]
  
  tryCatch({
    if (is.null(revisar_url(enlace))) return(NULL)  
    
    noticia <- bow(enlace) |> scrape()
    
    #elementos
    titulo <- noticia |> html_elements(".story-title") |> html_elements("h1") |> html_text2()
    
    bajada <- noticia |> html_elements(".story-subtitle") |> html_elements(".h3") |> html_text2()
    
    fecha_texto <- noticia |> html_elements(".story-subtitle") |> html_elements(".date") |> html_text2()
    
    mes <- fecha_texto |> str_extract("(?<=(de ))\\w+") |> 
      recode("enero" = "1", "febrero" = "2", "marzo" = "3",
             "abril" = "4", "mayo" = "5", "junio" = "6",
             "julio" = "7", "agosto" = "8", "septiembre" = "9",
             "octubre" = "10", "noviembre" = "11", "diciembre" = "12")
    
    año <- fecha_texto |> str_extract("\\d{4}$")
    
    dia <- fecha_texto |> str_extract("\\d+")
    
    fecha <- paste(año, mes, dia)
    
    cuerpo <- noticia |> html_elements(".body") |> html_elements("p") |> html_text2() |> paste(collapse = "\n")
    
    #unir
    noticia_data <- tibble("titulo" = titulo |> validar_elementos(),
                           "bajada" = bajada |> validar_elementos(),
                           "fecha"  = fecha  |> validar_elementos(),
                           "cuerpo" = cuerpo |> validar_elementos(),
                           "fuente" = "lacuarta",
                           "url" = enlace,
                           "fecha_scraping" = lubridate::now())
    return(noticia_data)
  },
  error = function(e) {
    message("error en scraping lacuarta: ", e)
    return(NULL)}
  )
})

# guardar ----
dir.create("resultados/lacuarta/", showWarnings = F)

readr::write_rds(resultados_noticias, 
                 glue("resultados/lacuarta/lacuarta_cron_{lubridate::today()}{hist}.rds"))

message(glue("listo cron lacuarta {lubridate::now()}"))
