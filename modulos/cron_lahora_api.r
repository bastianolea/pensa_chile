# posible pero no puedo interpretar los datos

library(RCurl)

data <- getURL(url = "https://lahora.cl/exec/buscador/index.php",
       postfields = "por=category&slug=cronica&desde=18&cantidad=6",
       httpheader = c(Connection = "close"
       )
)


fromJSON(paste(data, collapse=";"))

read.table(text = data, sep = ";")

fromJSON(content(data, type = "text"))