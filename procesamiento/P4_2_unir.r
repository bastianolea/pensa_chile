#va carpeta por carpeta uniendo los scrapings diarios, y los guarda en la lista con el nombre de la carpeta

ruta_scraping <- "/home/bastian//Collahuasi/seguimiento-scraping/P4_prensa/scraping/resultados_scraping" |> 
  list.dirs(full.names = F, recursive = F)

#ruta_scraping <- ruta_scraping[ruta_scraping != "elciudadano"]

#cargar todo ----
prensa <- map(ruta_scraping |> purrr::set_names(), ~{
    #x_carpeta <- ruta_scraping[4]
    #x_carpeta <- "soyiquique"
    x_carpeta <- .x
    
    #definir ruta
    x_carpeta_ruta <- paste0("/home/bastian/Collahuasi/seguimiento-scraping/P4_prensa/scraping/resultados_scraping/",
                             x_carpeta)
    message("uniendo ", x_carpeta)
    
    #obtener archivos dentro de la carpeta
    x_archivos <- x_carpeta_ruta |> list.files(full.names = T, pattern = "cron") 
    
    message(glue("{length(x_archivos)} archivos en {x_carpeta}"))

    #cargar todos los archivos
    # x_cargados <- map(x_archivos, ~{
    #   ##.x <- "/home/bastian/Collahuasi/seguimiento-scraping/P4_prensa/scraping/resultados_scraping/biobio_pais/biobio_pais_cron_2023-06-08.rds"
    #   #x <- "/home/bastian/Collahuasi/seguimiento-scraping/P4_prensa/scraping/resultados_scraping/biobio_pais/biobio_pais_cron_2023-06-07.rds"
    #   #message(.x)
    #   dato <- readr::read_rds(.x)
    # })
    x_cargados <- map(x_archivos, readRDS)
    
    # x_cargados[[1]] |> pull(url)
    # x_cargados[[1]] |> distinct(url, .keep_all = T)
    
    #problema: algunos scraping tienen variables con largo > 1 entonces no permiten hacer dataframe donde 1 fila sea 1 noticia
    #solución: aplicar lmap para pegar todos los contenidos de las variables, cosa que queden length 1
    
    #x_cargados[[3]][[1]] #a este nivel, hay que hacer que cada elemento sea de largo 1
    #aplanar lista para que cada elemento de la lista corresponda a una noticia
    x_cargados_plana <- x_cargados |> purrr::list_flatten()
    
    #x_cargados_plana[[1]]
  
    #para evitar problema de que pega todos los elementos
    if (x_carpeta %in% c("radiopaulina", "latercera_pais", "latercera", "tarapacaonline", 
                         "emol", "estrella", "cooperativa", "cooperativa_pais", "emol_pais", "soyiquique")) {
      x_cargados_corregida <- x_cargados_plana
      } else {
        #función que a cada variable de cada noticia aplica un paste para dejarla length 1
    x_cargados_corregida <- map(1:length(x_cargados_plana), ~lapply(x_cargados_plana[[.x]], paste, collapse ="\n"))
    #esto produce errores en soyiquique y Radio Cooperativa (regional)
      }
    
    # x_cargados_corregida[[1]]
    # x_cargados_corregida[1]
    # x_cargados[1]
    #aplicar función a cada elemento de la lista
    #map(x_cargados_plana, lapply(x_cargados2[[1]], paste, collapse ="\n")
    #purrr::lmap(x_cargados2[[1]], ~(paste(.x, collapse ="\n")))
    
    x_cargados_corregida_2 <- bind_rows(x_cargados_corregida)
    #length(unique(x_cargados_corregida_2$url))
    
    # #hay una noticia donde no hay fecha, o quizás una con dos titulos
    # 
    # x_cargados <- map(x_archivos |> set_names(), ~{
    #   #message(.x)
    #   data <- readr::read_rds(.x)
    #   #if (length(data$fecha) == 0) return(NULL)
    #   
    #   errores <- map(1:length(data), ~{
    #     if (length(data[[1]]$titulo) != 1) {
    #       return(.x)
    #     } else {
    #       return(0)
    #     }
    #   })
    #   return(errores)
    #   }, .progress = TRUE)
    
    # x_cargados2 <- x_cargados |> list_flatten()
    
    #ver si hay alguna que no sea, y dónde
  #  names(x_cargados2)[lapply(x_cargados2, as.character) != 0]
    
    # .x = "/home/bastian/Collahuasi/seguimiento-scraping/P4_prensa/scraping/resultados_scraping/biobio_pais/biobio_pais_cron_2023-05-03.rds"
    # x_archivos = c(
    # "/home/bastian/Collahuasi/seguimiento-scraping/P4_prensa/scraping/resultados_scraping/biobio_pais/biobio_pais_cron_2023-05-03.rds",
    # "/home/bastian/Collahuasi/seguimiento-scraping/P4_prensa/scraping/resultados_scraping/biobio_pais/biobio_pais_cron_2023-05-02.rds",
    # "/home/bastian/Collahuasi/seguimiento-scraping/P4_prensa/scraping/resultados_scraping/biobio_pais/biobio_pais_cron_2023-05-01.rds"
    # )
    
    #return(x_cargados)
    return(x_cargados_corregida_2)
  })

message("OK, todo cargado")



