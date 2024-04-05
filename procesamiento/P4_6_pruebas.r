library(dplyr); library(stringr)

source("~/Collahuasi/seguimiento-scraping/P4_prensa/P4_2_unir.r")
source("~/Collahuasi/seguimiento-scraping/P4_prensa/P4_3_limpiar.r")
source("~/Collahuasi/seguimiento-scraping/P4_prensa/P4_4_tokenizar.r")

prensa_palabras

prensa_fechas$latercera |> 
  select(fecha) |> 
  head(20) |> 
  mutate(año = str_extract(fecha, "\\d{4}")) |> 
  mutate(mes = str_extract(fecha, "^\\w{3} "),
         mes2 = str_extract(fecha, " \\w{3} ")) |> 
  print(n=Inf)

prensa_fechas$latercera |> 
  filter(is.na(fecha_f)) |> 
  select(fecha, fecha_scraping, fecha2, fecha_f) |> 
  #si la fecha dice "hace x horas", reconocer, obtener horas, y restarselas a la fecha del scraping
  mutate(hace = stringr::str_extract(fecha2, "Hace \\d+ hora") |> stringr::str_extract("\\d+") |> readr::parse_integer()) |> 
  mutate(fecha_hace = case_when(is.na(fecha_f) & !is.na(hace) ~ fecha_scraping-lubridate::hours(hace)))

prensa$latercera |> pull(bajada)


prensa_limpia$latercera |> 
  select(url, fecha_f)


prensa_limpia$tarapacaonline |> 
  print(n=100)
  select(fecha, fecha_f)

  
  prensa_palabras$radiopaulina |> count(fuente)
  prensa_palabras$latercera |> count(fuente)
  
  prensa_palabras |> 
    bind_rows() |> 
    count(fuente)
    
  
  prensa_limpia$latercera |> 
    arrange(desc(fecha_scraping))
  
  prensa_limpia$latercera |> 
    summarize(max(fecha_f))
  
prensa_palabras2 <- prensa_palabras |> 
  bind_rows() |> 
  filter(fecha_f >= today()-days(40))

prensa_palabras2 |>
  count(fuente)

prensa_palabras2 |> 
  group_by(fuente) |> 
  count(palabras) |> 
  group_by(fuente) |> 
  mutate(p = n/sum(n)) |> 
  ungroup() |> 
  filter(p > 0.002) |> 
  arrange(fuente, desc(p)) |> 
  group_by(fuente) |> 
  slice(1:10) |> 
  print(n=Inf)
  
