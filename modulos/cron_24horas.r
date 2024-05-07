library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones_scraping.r")

#enlaces ----
enlaces <- paste0("https://www.24horas.cl/actualidad/nacional/p/", 1:5); hist = ""

# para descargar noticias anteriores
# enlaces <- paste0("https://www.24horas.cl/actualidad/nacional/p/", 4:2000); hist = "_h"

# obtener enlaces de noticias ----
resultados_enlaces <- map(enlaces, \(enlace) {
  # enlace <- enlaces[1]
  
  tryCatch({
  if (is.null(revisar_url(enlace))) return(NULL)  
  
  sesion <- bow(enlace) |> scrape()
  
  enlaces_a <- sesion |> 
    html_elements(".cont-txt") |> 
    html_elements("a") |> 
    html_attr("href")
  
  enlaces_b <- sesion |> 
    html_elements(".box-dest") |> 
    html_elements("a") |> 
    html_attr("href")
  
  enlaces <- c(enlaces_a, enlaces_b)
  # enlaces <- paste0("https://www.24horas.cl", enlaces)
  
  message(glue("Se obtuvieron {length(enlaces)} noticias en {enlace}"))
  
  return(enlaces)
  },
  error = function(e) {
    message("error en scraping 24horas: ", e)
    return(NULL)
  })
})


enlaces_24horas <- resultados_enlaces |> 
  unlist() |> 
  unique()


# scraping ----
resultados_24horas <- map_df(enlaces_24horas, \(enlace) {
  # enlace <- enlaces_24horas[3]
  # enlace <- "/actualidad/nacional/espacios-de-formacion-seguros-colegio-de-terapeutas-ocupacionales-suicidio-estudiante"
  enlace <- paste0("https://www.24horas.cl", enlace)
  
  #revisar si existe la página
  if (is.null(revisar_url(enlace))) return(NULL)  
  
  tryCatch({
    noticia <- bow(enlace) |> scrape()
    
    #elementos
    titulo <- noticia |> html_elements(".titular") |> html_elements(".tit") |> html_text2()
    
    bajada <- noticia |> html_elements(".titular") |> html_elements(".baj") |> html_text2()
    
    fecha_0 <- noticia |> html_elements(".fecha") |> html_text()
    
    dia <- fecha_0 |> str_extract("(?<=( ))(\\d+)")
    mes_texto <- fecha_0 |> str_extract("(?<=( de ))(\\w+)") |> tolower()
    mes <- recode(mes_texto, "enero" = "1", "febrero" = "2", "marzo" = "3",
             "abril" = "4", "mayo" = "5", "junio" = "6",
             "julio" = "7", "agosto" = "8", "septiembre" = "9",
             "octubre" = "10", "noviembre" = "11", "diciembre" = "12")
    año <- fecha_0 |> str_extract("\\d{4}")
    fecha <- paste(dia, mes, año)
    
    cuerpo <- noticia |> html_elements(".CUERPO") |> html_elements("p") |> html_text2() |> 
      paste(collapse = "\n") |> str_remove_all("Ver más|\\{\\{_txt_titular\\}\\}")
    
    #unir
    noticia_data <- tibble("titulo" = titulo |> validar_elementos(),
                           "bajada" = bajada |> validar_elementos(),
                           "fecha" = fecha |> validar_elementos(),
                           "cuerpo" = cuerpo |> validar_elementos(),
                           "fuente" = "24horas",
                           "url" = enlace,
                           "fecha_scraping" = lubridate::now())
    return(noticia_data)
  },
  error = function(e) {
    message("Error en scraping 24horas: ", e)
    return(NULL)}
  )
})

# guardar ----
dir.create("resultados/24horas/", showWarnings = F)

readr::write_rds(resultados_24horas, 
                 glue("resultados/24horas/24horas_cron_{lubridate::today()}{hist}.rds"))

message(glue("listo cron 24horas {lubridate::now()}"))