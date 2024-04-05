# prensa
# ejcución en orden:

# obtener todos los datos mediante web scraping
source("prensa_obtener_datos.R")

# cargar datos scrapeados y guardarlos en una sola base, una noticia por fila
source("prensa_cargar_datos.R")

# transformar datos de prensa en base tokenizada por palabras para análisis de texto
source("prensa_procesar_texto.R")

# conteo de palabras por noticia
source("prensa_calcular_conteo.R")

# correlación entre palabras dentro de noticias, retorna base con palabras y sus pares correlacionados
source("prensa_calcular_correlacion.R")

# procesamiento de noticias para identificar topicos mediante machine learning
source("prensa_calcular_topicos.R")