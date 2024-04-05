# source("prensa_cargar_datos.R")
datos_prensa <- arrow::read_feather("datos/datos_prensa.feather")


datos_prensa |> 
  filter(is.na(fecha)) |> 
  count(fuente)

# gráficos ----
library(ggplot2)

# gráfico noticias por fuente y mes
datos_prensa |>
  filter(year(fecha) >= 2020) |> 
  mutate(fecha = floor_date(fecha, "month")) |> 
  ggplot(aes(fecha, fill = fuente)) +
  # geom_bar(position = position_dodge2(preserve = "single")) +
  geom_bar(position = position_stack(), color = "white") +
  scale_x_date(date_breaks = "months", date_labels = "%m", expand = c(0, 0), minor_breaks = NULL) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  guides(fill = guide_legend(position = "bottom", ncol = 7)) +
  labs(y = "noticias", x = NULL, title = "Noticias de medios digitales chilenos") +
  facet_grid(~año, space = "free_x", scales = "free_x", switch = "x") +
  theme(strip.text = element_text(margin = margin(t=20)),
        axis.text.x = element_text(margin = margin(t=-25)),
        legend.margin = margin(t=15), 
        panel.spacing.x = unit(4, "mm"))

ggsave(glue("graficos/datos_prensa_scraping_{today()}.png"), 
       width = 10, height = 8)


# gráfico noticias por fuente y año
datos_prensa |>
  filter(year(fecha) >= 2019) |>
  mutate(fecha = floor_date(fecha, "year")) |>
  ggplot(aes(fecha, fill = fuente)) +
  geom_bar(position = position_dodge2(preserve = "single")) +
  scale_x_date(date_breaks = "years", date_labels = "%Y", expand = c(0, 0), minor_breaks = NULL) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  guides(fill = guide_legend(position = "bottom", ncol = 7)) +
  labs(y = "noticias", x = "años")
