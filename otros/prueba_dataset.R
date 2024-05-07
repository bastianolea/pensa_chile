library(dplyr)
library(dbplyr)
library(DBI)
library(RPostgreSQL)
showMethods("dbConnect")
getMethod(dbConnect, "PostgreSQLDriver")

if (!exists("datos_prensa")) {
  datos_prensa <- arrow::read_feather("datos/prensa_datos.feather")
}

con <- dbConnect(RSQLite::SQLite(), "datos/sqlite_database")
copy_to(con, datos_prensa)
# dbWriteTable(con, "datos_prensa", datos_prensa)

data <- tbl(con, "datos_prensa") |> 
  filter(fuente == "elsiglo") |> 
  collect()

data |> 
  mutate(fecha = as_date(fecha)) |> 
  rowwise() |>
  mutate(id = rlang::hash(url))
  mutate(id = 1:n(),
         id = paste(fuente, año, id, sep = "_")) |> 
  # mutate(id = paste(fuente, año, month(fecha), day(fecha), 1:n(), sep = "_")) |> 
  select(fuente, url, id)

# rlang::hash("mapacheses")

  data |> 
    ungroup() |> 
    mutate(n = n(),
           n2 = n/8)
  
  data %>% 
    mutate(grupos = (row_number()-1) %/% (n()/8)) |> 
    group_by(grupos) |> 
    # summarize(n = n()) |> 
    group_split()
  
  
# 
# datos_prensa
# 
# 
# library(arrow)
# library(dplyr)
# 
# datos_prensa <- arrow::open_dataset("datos/prensa_dataset", format = "feather")
# 
# datos_prensa |> 
#   filter(fuente == "latercera")
