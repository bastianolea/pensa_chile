library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
source("funciones_scraping.r")


# realizar scraping ----

scraping_prensa("modulos/cron_latercera.r") #hist

scraping_prensa("modulos/cron_meganoticias.r") #hist

scraping_prensa("modulos/cron_eldinamo.r") #hist

scraping_prensa("modulos/cron_theclinic.r") #hist

scraping_prensa("modulos/cron_elciudadano.r") #hist

scraping_prensa("modulos/cron_radiouchile.r") #hist

# scraping_prensa("modulos/cron_emol_h.r") #hist

scraping_prensa("modulos/cron_24horas.r") #hist

scraping_prensa("modulos/cron_cnnchile.r") #hist

scraping_prensa("modulos/cron_elmostrador.r") #(requiere selenium)

scraping_prensa("modulos/cron_exante.r") #hist

scraping_prensa("modulos/cron_elsiglo.r") #hist

scraping_prensa("modulos/cron_ciper.r") #hist

scraping_prensa("modulos/cron_diariofinanciero.r") #histórico solo hasta página 20

scraping_prensa("modulos/cron_cooperativa.r")

scraping_prensa("modulos/cron_chvnoticias.r")

scraping_prensa("modulos/cron_t13.r")

scraping_prensa("modulos/cron_agricultura.r")

scraping_prensa("modulos/cron_biobio.r")

scraping_prensa("modulos/cron_lahora.r")

scraping_prensa("modulos/cron_adnradio.r") 






# 
# #revisión ----
# #chequear si se guardaron los archivos
# revisar_resultados("/home/bastian/Collahuasi/seguimiento-scraping/P4_prensa/scraping/resultados_scraping")
# #revisa si hay archivos nuevos el día actual
# 
# #—----
# 
# #ambos procesos funcionan a partir de la ruta (que se obtiene en 2_unir); es decir que se ejecutan en base al nombre de las carpetas en P4_prensa/scraping/resultados_scraping
# 
# #unir ----
# message("uniendo...")
# source("/home/bastian/Collahuasi/seguimiento-scraping/P4_prensa/P4_2_unir.r")
# 
# #limpiar ----
# message("limpiando...")
# source("/home/bastian/Collahuasi/seguimiento-scraping/P4_prensa/P4_3_limpiar.r")
# 
# #prensa_limpia$biobio
# #prensa_limpia$biobio_pais
# # prensa_limpia$latercera_pais |> 
# # summarize(min(fecha_f),
# #           max(fecha_f))
# 
# #nrow(prensa$soyiquique)
# #nrow(prensa_limpia$soyiquique)
# 
# #guardar ----
# prensa_limpia_2 <- bind_rows(prensa_limpia)
# 
# message(glue("{nrow(prensa_limpia_2) |> format(big.mark = '.', decimal.mark = ',')} noticias en total"))
# conteo <- count(prensa_limpia_2, escala)
# message(glue("{conteo$n[1] |> format(big.mark = '.', decimal.mark = ',')} noticias locales y {conteo$n[2] |> format(big.mark = '.', decimal.mark = ',')} noticias nacionales"))
# 
# message("guardando...")
# arrow::write_feather(prensa_limpia_2, 
#                      "/home/bastian/Collahuasi/seguimiento-scraping/P4_prensa/scraping/resultados_scraping/prensa_limpia.feather")
# 
# message("listo ", lubridate::today())
# message("-----------------")
