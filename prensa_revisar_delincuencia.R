library(dplyr)
library(dbplyr)
library(ggplot2)
library(lubridate)
library(slider)

# cargar datos ----
if (!exists("prensa_palabras_conteo")) prensa_palabras_conteo <- arrow::read_parquet("datos/prensa_palabras_conteo.parquet")

# definir conceptos para temas ----
palabras_delincuencia = c("homicidio", "homicidios", "asesinato", "asesinatos", 
                          "hurto", "hurtos", "hurtó", "hurtaron",
                          "robo", "robos", "robar", "robando", 
                          "asalto", "asaltaron", "asaltar",
                          "ladrón", "ladrones",
                          "arma", "calibre",
                          "secuestro", "secuestro", "secuestrado", "secuestran",
                          "delito", "delitos", "delincuente", "delincuentes", "delincuencia", 
                          "crimen", "criminal", "criminales", 
                          "narcotráfico", "narco", "droga",
                          "barricada", "protesta", "saqueo", "saquearon",
                          "ebriedad", "desórdenes", "incivilidad", "incivilidades"
)

# detectar temas ----

# detectar noticias sobre delincuencia
id_noticias_tema_delincuencia <- prensa_palabras_conteo |> 
  # slice(1:5000000) |> 
  mutate(tema_delincuencia = if_else(palabra %in% palabras_delincuencia, T, F)) |> 
  group_by(id) |> 
  summarize(tema_delincuencia = sum(tema_delincuencia)/n()) |> 
  filter(tema_delincuencia > 0.03) |> 
  pull(id)


# ver temas en datos ----
if (!exists("datos_prensa")) datos_prensa <- arrow::read_feather("datos/prensa_datos.feather")


datos_prensa_filt <- datos_prensa |> 
  filter(año >= 2019)


# obtener cantidad de noticias al día sobre delincuencia
ts_noticias_delincuencia_fuente <- datos_prensa_filt |> 
  filter(id %in% id_noticias_tema_delincuencia) |> 
  count(fecha, fuente) 

# obtener cantidad total de noticias al día
ts_conteo_noticias_fuente <- datos_prensa_filt |> 
  count(fecha, fuente) 

# revisar visualmente
# ts_conteo_noticias |> 
#   ggplot(aes(fecha, n)) +
#   geom_col()

# unir totales de noticias con noticias del tema, para obtener proporción de noticias sobre el tema
ts_noticias_delincuencia_fuente_prop <- ts_conteo_noticias_fuente |> 
  rename(total = n) |> 
  left_join(ts_noticias_delincuencia_fuente, by = c("fecha", "fuente")) |> 
  filter(!is.na(fecha),
         fecha >= "2018-01-01") |> 
  mutate(n = if_else(is.na(n), 0, n)) |> 
  group_by(fuente) |> 
  mutate(p = n/total) |> 
  arrange(fuente, desc(fecha)) 

# sumar noticias por mes
ts_noticias_delincuencia_fuente_prop_mes <- ts_noticias_delincuencia_fuente_prop |> 
  mutate(fecha = lubridate::floor_date(fecha, "month")) |> 
  group_by(fecha, fuente) |> 
  summarize(total = sum(total),
            n = sum(n)) |> 
  mutate(p = n/total) |> 
  arrange(desc(fecha)) |> 
  mutate(p_s = slide_dbl(p, mean, .before = 3))

ts_noticias_delincuencia_fuente_prop_mes |> 
  ggplot(aes(x = fecha, fill = fuente)) +
  geom_col(aes(y = total)) +
  guides(fill = guide_legend(position = "bottom", ncol = 7))

# totales
ts_noticias_delincuencia_fuente_prop_mes |> 
  group_by(fuente) |> 
  mutate(total_total = n()) |> 
  filter(total_total > 10) |> 
  mutate(p = if_else(p == 0, NA, p)) |> 
  # 
  ggplot(aes(x = fecha, fill = fuente)) +
  geom_area(aes(y = total), fill = "black") +
  geom_area(aes(y = n), fill = "red") +
  guides(fill = guide_legend(position = "bottom", ncol = 7)) +
  facet_wrap(~fuente, scales = "free_y")

# porcentaje
ts_noticias_delincuencia_fuente_prop_mes |> 
  group_by(fuente) |> 
  mutate(total_total = n()) |> 
  filter(total_total > 10) |> 
  mutate(p = if_else(p == 0, NA, p)) |> 
  # 
  ggplot(aes(x = fecha, fill = fuente)) +
  geom_area(aes(y = 1), fill = "black") +
  geom_col(aes(y = p), fill = "red") +
  guides(fill = guide_legend(position = "bottom", ncol = 7)) +
  facet_wrap(~fuente, scales = "free_y")


ts_noticias_delincuencia_fuente_prop_mes |> 
  filter(fuente == "eldinamo") |> 
  ggplot(aes(x = fecha, fill = fuente)) +
  geom_area(aes(y = 1), fill = "black") +
  geom_col(aes(y = p), fill = "red")
  