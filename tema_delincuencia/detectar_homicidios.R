library(dplyr)
library(dbplyr)
library(ggplot2)
library(lubridate)
library(slider)

# cargar datos ----
if (!exists("prensa_palabras_conteo")) prensa_palabras_conteo <- arrow::read_parquet("datos/prensa_palabras_conteo.parquet")

# definir conceptos para temas ----
# palabras_delincuencia = c("homicidio", "homicidios", "asesinato", "asesinatos", "asesinó", 
#                           "hurto", "hurtos", "hurtó", "hurtaron",
#                           "robo", "robos", "robar", "robando", "robaron",
#                           "asalto", "asaltaron", "asaltar", "asaltó", "asaltantes", "asaltante",
#                           "arma", "armas", "calibre", "pistola", "revolver", "revólver",
#                           "secuestro", "secuestro", "secuestrado", "secuestran",
#                           "delito", "delitos", "delincuencia", 
#                           "delincuente", "delincuentes", "ladrón", "ladrones", "antisocial", "antisociales",
#                           "crimen", "criminal", "criminales", 
#                           "narcotráfico", "narco", "droga",
#                           "barricada", "protesta", "saqueo", "saquearon",
#                           "ebriedad", "ebrio", "ebria", "desórdenes", "incivilidad", "incivilidades"
# )

palabras_homicidios = c("homicidio", "homicidios",  
                        "asesinato", "asesinatos", "asesinó", 
                        "fallecido", "falleció",
                        # correlaciones de homicidio
                        c("frustrado", "calificado", "víctima", "homicidios", "crimen", "muerte", "delito", "arma", "asesinato", "autor"),
                        # correlaciones de asesinato
                        c("crimen", "homicidio", "asesinado", "muerte", "condenado", "asesinada")
)

palabras_delincuencia = c(
  # robo
  c("intimidación", "delincuentes", "robos",
    "antisociales", "encargo", "carabineros", "habitado", "receptación"),
  # asalto
  c("asaltantes", "delincuentes", "antisociales", "robo", "violento", 
    "intimidaron", "vehículo", "arma", "huyeron"),
  # delincuencia
  c("narcotráfico", "seguridad", "inseguridad", "organizado", 
    "policías", "combatir", "crimen", "violencia", "delincuentes"),
  #delincuente
  c("robo", "delincuentes", "sujeto", "arma", "asalto", "robar", 
    "carabineros", "carabinero", "vehículo", "policial"),
  # portonazo
  c("vehículo", "delincuentes", "robo", "antisociales", "auto", 
    "sujetos", "asaltantes", "intimidaron", "automóvil", "violento")
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
conteo_delincuencia <- prensa_palabras_conteo |> 
  # slice(1:50000) |>
  mutate(tema_delincuencia = if_else(palabra %in% palabras_delincuencia, T, F)) |> 
  mutate(tema_delincuencia_n = if_else(tema_delincuencia, n, 0)) |> 
  group_by(id) |> 
  summarize(tema_delincuencia_n = sum(tema_delincuencia_n),
            n = sum(n)) |> 
  mutate(tema_delincuencia = tema_delincuencia_n/n)

conteo_homicidios <- prensa_palabras_conteo |> 
  # slice(1:50000) |>
  mutate(tema_homicidios = if_else(palabra %in% palabras_homicidios, T, F)) |> 
  mutate(tema_homicidios_n = if_else(tema_homicidios, n, 0)) |> 
  group_by(id) |> 
  summarize(tema_homicidios_n = sum(tema_homicidios_n),
            n = sum(n)) |> 
  mutate(tema_homicidios = tema_homicidios_n/n)

# id_noticias_tema_delincuencia <- prensa_palabras_conteo |> 
#   slice(1:50000) |>
#   mutate(tema_delincuencia = if_else(palabra %in% palabras_delincuencia, T, F)) |> 
#   group_by(id) |> 
#   summarize(tema_delincuencia = sum(tema_delincuencia)/n()) |> 
#   filter(tema_delincuencia > 0.03) |> 
#   pull(id)

# vector de ids de noticias con más de x% de palabras de delincuencia
id_noticias_tema_delincuencia <- conteo_delincuencia |> 
  filter(tema_delincuencia > 0.03) |> 
  pull(id)

id_noticias_tema_homicidios <- conteo_homicidios |> 
  filter(tema_homicidios > 0.03) |> 
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

# revisar una noticia por su id 
datos_prensa |> filter(id == "001b295087391e8d2168252c4e923015") |> glimpse()


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
  filter(año >= 2019) |> 
  filter(!fuente %in% c("24horas", "chvnoticias", "lacuarta", 
                        "biobio", "cooperativa", "diariofinanciero", "agricultura")) #fuentes sin noticias en años pasados
# filter(fuente != "latercera",
#        fuente != "lahora")

# obtener cantidad de noticias al día sobre delincuencia
noticias_delincuencia_conteo <- datos_prensa_filt |> 
  filter(id %in% id_noticias_tema_delincuencia) |> 
  count(fecha) 

noticias_homicidios_conteo <- datos_prensa_filt |> 
  filter(id %in% id_noticias_tema_homicidios) |> 
  count(fecha) 

# obtener cantidad total de noticias al día
noticias_conteo <- datos_prensa_filt |> 
  count(fecha) 

# revisar visualmente
# ts_conteo_noticias |> 
#   ggplot(aes(fecha, n)) +
#   geom_col()

# unir totales de noticias con noticias del tema, para obtener proporción de noticias sobre el tema
noticias_prop <- noticias_conteo |> 
  rename(total = n) |> 
  left_join(noticias_delincuencia_conteo, by = "fecha") |> #by = c("fecha", "fuente")) |> 
  filter(!is.na(fecha),
         fecha >= "2018-01-01") |> 
  # filter(!fuente %in% c("elciudadano", "radiouchile", "exante", "diariofinanciero", "ciper")) |> #excluir medios que no suelen reportar delincuencia (menos del 1% de sus noticias)
  mutate(n = if_else(is.na(n), 0, n)) |> 
  mutate(p = n/total) |> 
  arrange(desc(fecha)) 

# sumar noticias por mes
noticias_prop_mes <- noticias_prop |> 
  mutate(fecha = floor_date(fecha, "month")) |> 
  group_by(fecha) |> 
  summarize(total = sum(total),
            n = sum(n)) |> 
  mutate(p = n/total)

noticias_prop_mes_suavizado <- noticias_prop_mes |>   
  arrange(fecha) |> 
  mutate(p_s = slide_dbl(p, mean, .before = 3)) #suavizar datos diarios

# revisar visualmente
# noticias_prop_mes |>
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

escala_eje_2 = 2150 #escala para que partan los dos gráficos en el mismo intercepto

# unir noticias de delincuencia con delitos
datos_consolidados <- bind_rows(
  noticias_prop_mes_suavizado |> mutate(valor = p_s, tipo = "Noticias"),
  delincuencia_total |> mutate(valor = miles/escala_eje_2, tipo = "Delitos")) |> 
  select(fecha, valor, tipo)

# grafico de noticias versus delitos ----
datos_consolidados |> 
  ggplot(aes(fecha, valor, color = tipo)) +
  geom_vline(xintercept = c("2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01") |> as_date(), alpha = .2, linewidth = .5) +
  geom_label(data = ~.x |> group_by(tipo) |> filter(fecha == max(fecha)),
             aes(label = paste(" ", tipo)), hjust = 0, color = "black", label.size = 0, label.padding = unit(.1, "mm"), show.legend = F) +
  geom_line(linewidth = 1.8, alpha = .7, lineend = "round") +
  annotate("label", y = 0.018, x = c("2019-07-01", "2020-07-01", "2021-07-01", "2022-07-01", "2023-07-01", "2024-07-01") |> as_date(), 
           label = 2019:2024, label.size = 0, hjust = 0.5, size = 3.5, color = "grey40") +
  scale_y_continuous(name = "Noticias sobre delincuencia (porcentaje de noticias al mes)", 
                     labels = ~scales::percent(.x, accuracy = 1), 
                     breaks = c(0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09), 
                     minor_breaks = c(50, 100, 150, 200)/escala_eje_2,
                     sec.axis = sec_axis(~ ., 
                                         breaks = c(50, 75, 100, 125, 150, 175, 200)/escala_eje_2,
                                         labels = \(labels) round(labels*escala_eje_2, 0), 
                                         name = "Delitos reportados al mes en todo el país (miles)")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%m", expand = expansion(c(0, 0.05))) +
  # scale_color_manual(values = c(color_destacado, color_secundario)) +
  scale_color_manual(values = c("#D96621", "#179992")) +
  theme_minimal() +
  coord_cartesian(clip = "off", ylim = c(.02, .092)) +
  guides(color = guide_legend(position = "bottom", title = "", nrow = 1, reverse = T)) +
  theme(#axis.text.x = element_text(angle = -90, vjust = .5),
    panel.grid.minor.x = element_blank(), 
    axis.title.x = element_blank(), plot.caption = element_text(lineheight = .5),
    plot.title.position = "plot",
    plot.caption.position = "plot", 
    legend.box.margin = margin(t = -6, b = -2),
    legend.text = element_text(margin = margin(l = 2, r = 6))) +
  labs(title = "Cobertura de delincuencia en prensa versus estadísticas de delincuencia",
       subtitle = "Noticias acerca de delincuencia, comparadas con la cantidad real de delitos reportados en Chile.",
       caption = "Autoría: Bastián Olea Herrera, data scientist, magíster en Sociología (PUC).\n
       Fuentes: Estadítsicas delictuales obtenidas desde CEAD (en base a datos reportados por PDI y Carabineros).\n
       Noticias obtenidas de sitios web de prensa como La Tercera, Emol, CNN, Meganoticias, El Mostrador, ADN y 15 más")

ggsave(glue::glue("graficos/prensa_delincuencia_vs_reporte3b.jpg"), 
       width = 10, height = 8)


# library(shades)
# c("#317773" |> brightness(0.6) |> saturation(0.85),
#   "#cf5a13" |> brightness(0.85) |> saturation(0.85)) |> 
#   swatch()


# grafico de noticias, homicidios y tasa de homicidios ----
# contar datos de delincuencia por mes, para todo el país
delincuencia_homicidios <- delincuencia |> 
  filter(delito == "Homicidios") |> 
  filter(fecha >= "2019-01-01") |> 
  group_by(fecha) |> 
  summarize(n = sum(delito_n)) |> 
  arrange(fecha) |> 
  mutate(n_s = slide_dbl(n, mean, .before = 3))

escala_eje_2 = 0.00158 # escala_eje_2 = 600 #escala para que partan los dos gráficos en el mismo intercepto

# unir noticias de delincuencia con delitos
datos_consolidados_homicidios <- bind_rows(
  noticias_prop_mes_suavizado |> mutate(valor = p_s, tipo = "Noticias"),
  delincuencia_homicidios |> mutate(valor = n_s, tipo = "Delitos")) |> 
  select(fecha, valor, tipo)

datos_consolidados_homicidios |> 
  ggplot(aes(fecha, valor, color = tipo)) +
  geom_vline(xintercept = c("2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01") |> as_date(), alpha = .2, linewidth = .5) +
  geom_label(data = ~.x |> group_by(tipo) |> filter(fecha == max(fecha)),
             aes(label = paste(" ", tipo)), hjust = 0, color = "black", label.size = 0, label.padding = unit(.1, "mm"), show.legend = F) +
  geom_line(linewidth = 1.8, alpha = .7, lineend = "round") +
  annotate("label", y = 0.008, x = c("2019-07-01", "2020-07-01", "2021-07-01", "2022-07-01", "2023-07-01", "2024-07-01") |> as_date(), 
           label = 2019:2024, label.size = 0, hjust = 0.5, size = 3.5, color = "grey40") +
  scale_y_continuous(name = "Noticias sobre delincuencia (porcentaje de noticias al mes)", 
                     labels = ~scales::percent(.x, accuracy = 1), 
                     breaks = c(0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09), 
                     minor_breaks = c(50, 100, 150, 200)/escala_eje_2,
                     sec.axis = sec_axis(~ ., breaks = c(50, 75, 100, 125, 150, 175, 200)/escala_eje_2, labels = \(labels) round(labels*escala_eje_2, 0), name = "Delitos reportados al mes en todo el país (miles)")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%m", expand = expansion(c(0, 0.05))) +
  scale_color_manual(values = c("#D96621", "#179992")) +
  theme_minimal() +
  # coord_cartesian(clip = "off", ylim = c(.01, .115)) +
  guides(color = guide_legend(position = "bottom", title = "", nrow = 1, reverse = T)) +
  theme(panel.grid.minor.x = element_blank(), 
        axis.title.x = element_blank(), plot.caption = element_text(lineheight = .5),
        plot.title.position = "plot", plot.caption.position = "plot", 
        legend.box.margin = margin(t = -6, b = -2), legend.text = element_text(margin = margin(l = 2, r = 6))) +
  labs(title = "Cobertura de delincuencia en prensa versus estadísticas de delincuencia",
       subtitle = "Noticias acerca de delincuencia, comparadas con la cantidad real de delitos reportados en Chile.",
       caption = "Autoría: Bastián Olea Herrera, data scientist, magíster en Sociología (PUC).\n
       Fuentes: Estadítsicas delictuales obtenidas desde CEAD (en base a datos reportados por PDI y Carabineros).\n
       Noticias obtenidas de sitios web de prensa como La Tercera, Emol, CNN, Meganoticias, El Mostrador, ADN y 15 más")

# ggsave(glue::glue("graficos/prensa_delincuencia_vs_reporte3b.jpg"), 
#        width = 10, height = 8)


# library(shades)
# c("#317773" |> brightness(0.6) |> saturation(0.85),
#   "#cf5a13" |> brightness(0.85) |> saturation(0.85)) |> 
#   swatch()

# cargar datos del censo, para tasas
censo <- arrow::read_parquet("~/Documents/Apps Shiny/delincuencia_chile/app/censo_proyecciones_año.parquet")

censo_anual <- censo |> 
  group_by(año) |> 
  summarize(pob = sum(población))

delincuencia_homicidios_tasa <- delincuencia_homicidios |> 
  mutate(año = year(fecha)) |> 
  left_join(censo_anual, by = "año") |> 
  mutate(tasa = (n/pob)*100000)

escala_eje_2 = 0.05 # escala_eje_2 = 600 #escala para que partan los dos gráficos en el mismo intercepto

# unir noticias de delincuencia con delitos
datos_consolidados_homicidios_tasa <- bind_rows(
  noticias_prop_mes_suavizado |> mutate(valor = p_s, tipo = "Noticias"),
  delincuencia_homicidios_tasa |> mutate(valor = tasa+0.048, tipo = "Homicidios (tasa)"),
  delincuencia_homicidios |> mutate(valor = n_s*escala_eje_2, tipo = "Homicidios total")) |> 
  select(fecha, valor, tipo)

datos_consolidados_homicidios_tasa |> 
  ggplot(aes(fecha, valor, color = tipo)) +
  geom_vline(xintercept = c("2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01") |> as_date(), alpha = .2, linewidth = .5) +
  geom_label(data = ~.x |> group_by(tipo) |> filter(fecha == max(fecha)),
             aes(label = paste(" ", tipo)), hjust = 0, color = "black", label.size = 0, label.padding = unit(.1, "mm"), show.legend = F) +
  geom_line(linewidth = 1.8, alpha = .7, lineend = "round") +
  annotate("label", y = 0.008, x = c("2019-07-01", "2020-07-01", "2021-07-01", "2022-07-01", "2023-07-01", "2024-07-01") |> as_date(), 
           label = 2019:2024, label.size = 0, hjust = 0.5, size = 3.5, color = "grey40") +
  scale_y_continuous(name = "Noticias sobre delincuencia (porcentaje de noticias al mes)", 
                     labels = ~scales::percent(.x, accuracy = 1), 
                     breaks = c(0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09), 
                     minor_breaks = c(50, 100, 150, 200)/escala_eje_2,
                     sec.axis = sec_axis(~ ., breaks = c(50, 75, 100, 125, 150, 175, 200)/escala_eje_2, labels = \(labels) round(labels*escala_eje_2, 0), name = "Delitos reportados al mes en todo el país (miles)")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%m", expand = expansion(c(0, 0.05))) +
  # scale_color_manual(values = c("#D96621", "#179992")) +
  theme_minimal() +
  # coord_cartesian(clip = "off", ylim = c(.01, .115)) +
  guides(color = guide_legend(position = "bottom", title = "", nrow = 1, reverse = T)) +
  theme(panel.grid.minor.x = element_blank(), 
        axis.title.x = element_blank(), plot.caption = element_text(lineheight = .5),
        plot.title.position = "plot", plot.caption.position = "plot", 
        legend.box.margin = margin(t = -6, b = -2), legend.text = element_text(margin = margin(l = 2, r = 6))) +
  labs(title = "Cobertura de delincuencia en prensa versus estadísticas de delincuencia",
       subtitle = "Noticias acerca de delincuencia, comparadas con la cantidad real de delitos reportados en Chile.",
       caption = "Autoría: Bastián Olea Herrera, data scientist, magíster en Sociología (PUC).\n
       Fuentes: Estadítsicas delictuales obtenidas desde CEAD (en base a datos reportados por PDI y Carabineros).\n
       Noticias obtenidas de sitios web de prensa como La Tercera, Emol, CNN, Meganoticias, El Mostrador, ADN y 15 más")
