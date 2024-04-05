library(dplyr)
library(rvest)
library(polite)
library(stringr)
library(purrr)
library(glue)
library(lubridate)

#scraping ----
#source("~/Collahuasi/seguimiento-scraping/P4_prensa/P4_prensa_cron.r")

#unir ----
source("/home/bastian/Collahuasi/seguimiento-scraping/P4_prensa/P4_2_unir.r")
prensa

#limpiar ----
source("/home/bastian/Collahuasi/seguimiento-scraping/P4_prensa/P4_3_limpiar.r")
prensa_fechas
prensa_limpia
