library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
library(jsonlite)
source("funciones_scraping.r")

# enlaces api ----
offset = 0; hist = ""
size = 50

# obtener enlaces anteriores
# size = 50; offset = 50; hist = "_h_a"
# size = 100; offset = 100; hist = "_h_b"
# size = 100; offset = 200; hist = "_h_c"
# size = 100; offset = 300; hist = "_h_d"
# size = 100; offset = 400; hist = "_h_e"
# size = 100; offset = 500; hist = "_h_f"
# size = 100; offset = 600; hist = "_h_g"
# size = 100; offset = 700; hist = "_h_h"
# size = 100; offset = 800; hist = "_h_i"
# size = 100; offset = 900; hist = "_h_j"
# size = 100; offset = 1000; hist = "_h_k"
# size = 100; offset = 1100; hist = "_h_l"
# size = 100; offset = 1200; hist = "_h_m"
# size = 100; offset = 1300; hist = "_h_n"
# size = 100; offset = 1400; hist = "_h_o"
# size = 100; offset = 1500; hist = "_h_p"
# size = 100; offset = 1600; hist = "_h_q"
# size = 100; offset = 1700; hist = "_h_r"
# size = 100; offset = 1800; hist = "_h_s"
# size = 100; offset = 1900; hist = "_h_t"
# size = 100; offset = 2000; hist = "_h_u" # 2023/09/29

# hacer query y obtener noticias
# json_query <- glue("https://www.publimetro.cl/pf/api/v3/content/fetch/story-feed-sections?query=%7B%22excludeSections%22%3A%22%22%2C%22feature%22%3A%22results-list%22%2C%22feedOffset%22%3A{offset}%2C%22feedSize%22%3A{size}%2C%22includeSections%22%3A%22%2Fnoticias%22%7D&filter=%7Bcontent_elements%7B_id%2Ccredits%7Bby%7B_id%2Cadditional_properties%7Boriginal%7Bbyline%7D%7D%2Cname%2Ctype%2Curl%7D%7D%2Cdescription%7Bbasic%7D%2Cdisplay_date%2Cheadlines%7Bbasic%7D%2Clabel%7Bbasic%7Bdisplay%2Ctext%2Curl%7D%7D%2Cowner%7Bsponsored%7D%2Cpromo_items%7Bbasic%7Bresized_params%7B158x89%2C274x154%7D%2Ctype%2Curl%7D%2Clead_art%7Bpromo_items%7Bbasic%7Bresized_params%7B158x89%2C274x154%7D%2Ctype%2Curl%7D%7D%2Ctype%7D%7D%2Ctype%2Cwebsites%7Bmwnchile%7Bwebsite_section%7B_id%2Cname%7D%2Cwebsite_url%7D%7D%7D%2Ccount%2Cnext%7D&d=453&_website=mwnchile")
json_query <- glue("https://www.publimetro.cl/pf/api/v3/content/fetch/story-feed-sections?query=%7B%22excludeSections%22%3A%22%22%2C%22feature%22%3A%22results-list%22%2C%22feedOffset%22%3A{offset}%2C%22feedSize%22%3A{size}%2C%22includeSections%22%3A%22%2Fnoticias%22%7D")

json_data <- fromJSON(paste(readLines(json_query), collapse=""))

enlaces <- paste0("https://www.publimetro.cl", json_data$content_elements$websites$mwnchile$website_url)


# pasos <- 1:5; hist = "_h_v"
# pasos <- 6:10; hist = "_h_w"
# pasos <- 11:15; hist = "_h_x"
# pasos <- 16:20; hist = "_h_y"
# pasos <- 21:30; hist = "_h_z"
# 
# # para obtener noticias anteriores
# enlaces_json <- map(pasos, \(valor) {
#   message("paso ", valor)
#   offset = 2000+(100*valor)
#   size = 100
#   
#   json_query <- glue("https://www.publimetro.cl/pf/api/v3/content/fetch/story-feed-sections?query=%7B%22excludeSections%22%3A%22%22%2C%22feature%22%3A%22results-list%22%2C%22feedOffset%22%3A{offset}%2C%22feedSize%22%3A{size}%2C%22includeSections%22%3A%22%2Fnoticias%22%7D&filter=%7Bcontent_elements%7B_id%2Ccredits%7Bby%7B_id%2Cadditional_properties%7Boriginal%7Bbyline%7D%7D%2Cname%2Ctype%2Curl%7D%7D%2Cdescription%7Bbasic%7D%2Cdisplay_date%2Cheadlines%7Bbasic%7D%2Clabel%7Bbasic%7Bdisplay%2Ctext%2Curl%7D%7D%2Cowner%7Bsponsored%7D%2Cpromo_items%7Bbasic%7Bresized_params%7B158x89%2C274x154%7D%2Ctype%2Curl%7D%2Clead_art%7Bpromo_items%7Bbasic%7Bresized_params%7B158x89%2C274x154%7D%2Ctype%2Curl%7D%7D%2Ctype%7D%7D%2Ctype%2Cwebsites%7Bmwnchile%7Bwebsite_section%7B_id%2Cname%7D%2Cwebsite_url%7D%7D%7D%2Ccount%2Cnext%7D&d=453&_website=mwnchile")
#   
#   json_data <- fromJSON(paste(readLines(json_query), collapse=""))
#   
#   enlaces <- paste0("https://www.publimetro.cl", json_data$content_elements$websites$mwnchile$website_url)
#   
#   message(glue("se obtuvieron {length(enlaces)} noticias con offset {offset}")) |> try()
#   Sys.sleep(1)
#   return(enlaces)
# })
# 
# enlaces <- enlaces_json |> unlist() |> unique()


# scraping ----
resultados_publimetro <- map_df(enlaces, \(enlace) {
  # enlace <- enlaces[30]
  
  tryCatch({
    if (is.null(revisar_url(enlace))) return(NULL)  
    
    noticia <- bow(enlace) |> scrape()
    
    #elementos
    titulo <- noticia |> html_elements(".headline") |> html_text2()
    
    bajada <- noticia |> html_elements(".sub-headline") |> html_text2()
    
    fecha <- enlace |> str_extract("\\d{4}\\/\\d{2}\\/\\d{2}")
    
    cuerpo <- noticia |> html_elements(".body-paragraph") |> html_text2() |> paste(collapse = "\n")
    
    #unir
    noticia_data <- tibble("titulo" = titulo |> validar_elementos(),
                           "bajada" = bajada |> validar_elementos(),
                           "fecha"  = fecha  |> validar_elementos(),
                           "cuerpo" = cuerpo |> validar_elementos(),
                           "fuente" = "publimetro",
                           "url" = enlace,
                           "fecha_scraping" = lubridate::now())
    return(noticia_data)
  },
  error = function(e) {
    message("error en scraping publimetro: ", e)
    return(NULL)}
  )
})

# guardar ----
dir.create("resultados/publimetro/", showWarnings = F)

readr::write_rds(resultados_publimetro, 
                 glue("resultados/publimetro/publimetro_cron_{lubridate::today()}{hist}.rds"))

message(glue("listo cron publimetro {lubridate::now()}"))
