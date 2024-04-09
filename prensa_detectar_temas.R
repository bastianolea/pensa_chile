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

palabras_corrupcion = c("corrupción", "corrupto", 
                        "cohecho", 
                        "fundación", "fundaciones", "convenios", 
                        "investigación", "investigativa", 
                        "fraude", "fraudulento", "lavado", 
                        "fisco",
                        "allanamiento", "allanar", "allanaron",
                        "malversación", "malversado", 
                        "abandono", "tráfico",
                        "estafa", "desleal", "falsificación",
                        "formalizado", "formalizada", "formalización")


# detectar temas ----

# detectar noticias sobre delincuencia
id_noticias_tema_delincuencia <- prensa_palabras_conteo |> 
  # slice(1:5000000) |> 
  mutate(tema_delincuencia = if_else(palabra %in% palabras_delincuencia, T, F)) |> 
  group_by(id) |> 
  summarize(tema_delincuencia = sum(tema_delincuencia)/n()) |> 
  filter(tema_delincuencia > 0.03) |> 
  pull(id)

# # detectar noticias sobre corrupción
# id_noticias_tema_corrupcion <- prensa_palabras_conteo |> 
#   # slice(1:5000000) |> 
#   mutate(tema_corrupcion = if_else(palabra %in% palabras_corrupcion, T, F)) |> 
#   group_by(id) |> 
#   summarize(tema_corrupcion = sum(tema_corrupcion)/n()) |> 
#   filter(tema_corrupcion > 0.04) |> 
#   pull(id)

# con estos vectores se pueden obtener noticias que representan cada tema

# # revisar un id específico
# prensa_palabras_conteo |> 
#   filter(id == "006026ffaaee40e12cf0321a74cb76ae") |> 
#   print(n=Inf)



# ver temas en datos ----
if (!exists("datos_prensa")) datos_prensa <- arrow::read_feather("datos/prensa_datos.feather")


# sql <- DBI::dbConnect(RSQLite::SQLite(), "datos/sqlite_database")
# if (!DBI::dbExistsTable(sql, "datos_prensa")) copy_to(sql, datos_prensa)

# revisar casos en datos
# datos_prensa |> 
# filter(id %in% id_noticias_tema_corrupcion)

# tbl(sql, "datos_prensa") |> 
#   filter(id %in% id_noticias_tema_corrupcion)
# 
# tbl(sql, "datos_prensa") |> 
#   filter(id %in% id_noticias_tema_delincuencia)

# revision <- tbl(sql, "datos_prensa") |> 
#   filter(año == 2018) |> 
#   # filter(id %in% id_noticias_tema_delincuencia) |> 
#   group_by(fecha, fuente) |> 
#   count() |> 
#   collect()
#   
# revision |> 
#   mutate(fecha = as_date(fecha)) |> 
#   filter(fecha >= "2018-07-01",
#          fecha < "2018-12-01") |> 
#   ggplot(aes(fecha, n, color = fuente)) +
#   geom_line()


# calcular temas como porcentaje de noticias -----
datos_prensa_filt <- datos_prensa |> 
  filter(año >= 2019)
  # filter(fuente != "latercera",
  #        fuente != "lahora")

# obtener cantidad de noticias al día sobre delincuencia
ts_noticias_delincuencia <- datos_prensa_filt |> 
  filter(id %in% id_noticias_tema_delincuencia) |> 
  count(fecha) 

# obtener cantidad total de noticias al día
ts_conteo_noticias <- datos_prensa_filt |> 
  count(fecha) 

# revisar visualmente
# ts_conteo_noticias |> 
#   ggplot(aes(fecha, n)) +
#   geom_col()

# unir totales de noticias con noticias del tema, para obtener proporción de noticias sobre el tema
ts_noticias_delincuencia_prop <- ts_conteo_noticias |> 
  rename(total = n) |> 
  left_join(ts_noticias_delincuencia, by = "fecha") |> #by = c("fecha", "fuente")) |> 
  filter(!is.na(fecha),
         fecha >= "2018-01-01") |> 
  # filter(!fuente %in% c("elciudadano", "radiouchile", "exante", "diariofinanciero", "ciper")) |> #excluir medios que no suelen reportar delincuencia (menos del 1% de sus noticias)
  mutate(n = if_else(is.na(n), 0, n)) |> 
  mutate(p = n/total) |> 
  arrange(desc(fecha)) 

# sumar noticias por mes
ts_noticias_delincuencia_prop_total_mes <- ts_noticias_delincuencia_prop |> 
  mutate(fecha = lubridate::floor_date(fecha, "month")) |> 
  group_by(fecha) |> 
  summarize(total = sum(total),
            n = sum(n)) |> 
  mutate(p = n/total) |> 
  arrange(desc(fecha)) |> 
  mutate(p_s = slide_dbl(p, mean, .before = 3))

# revisar visualmente
# ts_noticias_delincuencia_prop_total_mes |> 
#   ggplot(aes(fecha)) +
#   geom_col(aes(y = total)) +
#   geom_col(aes(y = n), fill = "red2")

# cargar datos de delincuencia
delincuencia <- arrow::read_parquet("~/Documents/Apps Shiny/delincuencia_chile/app/cead_delincuencia.parquet")

# contar datos de delincuencia por mes, para todo el país
delincuencia_total <- delincuencia |> 
  group_by(fecha) |> 
  summarize(n = sum(delito_n)) |> 
  filter(fecha >= "2019-01-01") |> 
  mutate(miles = n/1000,
         miles2 = miles/100)

# unir noticias de delincuencia con delitos
datos_consolidados <- ts_noticias_delincuencia_prop_total_mes |> 
  mutate(valor = p_s,
         tipo = "Noticias") |> 
  bind_rows(delincuencia_total |> 
              mutate(valor = miles/2800, #escala para que partan los dos gráficos en el mismo intercepto
                     tipo = "Delitos"))

# grafico de noticias versus delitos ----
datos_consolidados |> 
  ggplot(aes(fecha, valor, color = tipo)) +
  geom_vline(xintercept = c("2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01") |> as_date(), alpha = .2, linewidth = .5) +
  geom_label(data = ~.x |> group_by(tipo) |> filter(fecha == max(fecha)),
             aes(label = paste(" ", tipo)), hjust = 0, color = "black", label.size = 0, label.padding = unit(.1, "mm"), show.legend = F) +
  geom_line(linewidth = 1.8, alpha = .8, lineend = "round") +
  annotate("label", y = 0.0085, x = c("2019-07-01", "2020-07-01", "2021-07-01", "2022-07-01", "2023-07-01", "2024-07-01") |> as_date(), 
           label = 2019:2024, label.size = 0, hjust = 0.5, size = 3.5, color = "grey40") +
  scale_y_continuous(name = "Noticias sobre delincuencia (porcentaje de noticias al mes)", labels = scales::percent,
                     sec.axis = sec_axis(~ ., labels = \(labels) labels*3000, name = "Delitos reportados al mes en todo el país (miles)")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%m", expand = expansion(c(0, 0.05))) +
  theme_minimal() +
  coord_cartesian(clip = "off") + #ylim = c(.01, .055)) +
  guides(color = guide_legend(position = "bottom", title = "", nrow = 1)) +
  theme(#axis.text.x = element_text(angle = -90, vjust = .5),
        panel.grid.minor.x = element_blank(), 
        axis.title.x = element_blank(), plot.caption = element_text(lineheight = .5),
        plot.title.position = "plot",
        plot.caption.position = "plot", 
        legend.box.margin = margin(t = -6, b = -2),
        legend.text = element_text(margin = margin(l = 2, r = 6))) +
  labs(title = "Cobertura de delincuencia en prensa versus la realidad",
       subtitle = "Noticias acerca de delincuencia, comparadas con la cantidad real de delitos reportados en Chile.",
       caption = "Autoría: Bastián Olea Herrera, data scientist, magíster en Sociología (PUC).\n
       Fuentes: Estadítsicas delictuales obtenidas desde CEAD (en base a datos reportados por PDI y Carabineros).\n
       Noticias obtenidas de sitios web de prensa como La Tercera, Emol, CNN, Meganoticias, El Mostrador, ADN y 15 más")

ggsave(glue::glue("graficos/prensa_delincuencia_vs_reporte2.jpg"), 
       width = 10, height = 8)
