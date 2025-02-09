datos <- readr::read_rds("repositorios.rds")

library(shiny)
library(dplyr)
library(purrr)
library(lubridate)
library(gt)


# add_cyl_color <- function(elementos){
#   div_out <- map(elementos, \(cyl) {
#   add_color <- if (cyl == "social") {
#     "background: hsl(116, 60%, 90%); color: hsl(116, 30%, 25%);"
#   } else if (cyl == "data") {
#     "background: hsl(230, 70%, 90%); color: hsl(230, 45%, 30%);"
#   } else if (cyl == 8) {
#     "background: hsl(350, 70%, 90%); color: hsl(350, 45%, 30%);"
#   }
#   
#   div_out <- htmltools::div(
#     style = paste(
#       "display: inline-block; padding: 2px 12px; border-radius: 15px; font-weight: 600; font-size: 12px;",
#       add_color
#     ),
#     cyl
#   ) |> 
#     as.character()
#   })
#   
#   # browser()
#   # div_out |> as.character() |> list_c()
#   paste(div_out, collapse = " ") |> 
#     gt::html()
# }

source("funciones.R")

datos |> 
  arrange(desc(fecha)) |> 
  relocate(reciente, popular, .after = etiquetas) |> 
  mutate(titulo = ifelse(!is.na(titulo_repo), titulo_repo, titulo)) |> 
  print(n=Inf) |>
  # formato columnas gt
  rowwise() |>
  mutate(etiquetas = list(discard(etiquetas, etiquetas %in% c("data", "shiny", "r", "app", "tiempo", "comunas", "chile")))) |> 
  # columnas
  mutate(tiempo = case_match(tiempo,
                                   "anual" ~ "calendar",
                                   "mensual" ~ "calendar-days")) |> 
  rowwise() |>
  mutate(etiquetas = col_tag(etiquetas),
         titulo = col_link(titulo, enlace),
         # aplicación = col_icon(aplicación, "window-restore"),
         aplicación = col_icon_link(aplicación, "window-restore", enlace_app),
         popular = col_icon(popular, "arrow-up"),
         genero = col_icon(genero, "venus", tooltip = "hola"),
         comunas = col_icon(comunas, "map"),
         reciente = col_icon(reciente, "star")) |> 
  gt() |> 
  fmt_icon(columns = tiempo) |> 
  sub_missing(columns = tiempo, missing_text = "") |> 
  cols_align(columns = titulo, "right") |> 
  cols_align(columns = etiquetas, "left") |> 
  cols_align(columns = c(genero, comunas, aplicación, reciente, popular, tiempo),
             "center") |> 
  fmt_markdown(columns = c(titulo, etiquetas,
                           genero, comunas, aplicación, reciente, popular)) |> 
  cols_hide(c(enlace, estrellas, fecha, enlace_app, titulo_repo)) |> 
  cols_width(descripcion ~ "40%",
             etiquetas ~ "20%") |> 
  # centrar verticalmente texto de celdas
  tab_style(style = "vertical-align:middle", 
            locations = cells_body(columns = everything())
            ) |> 
  tab_style(style = cell_text(color = "black", weight = "bold"),
            locations = cells_body(columns = titulo)) |> 
  cols_label(titulo = "",
             descripcion = "",
             etiquetas = "",
             reciente = "Reciente",
             popular = "Popular",
             genero = "Género",
             aplicación = "App",
             comunas = "Comunas",
             tiempo = "Tiempo") |> 
  # eliminar bordes de arriba y abajo de la tabla
  tab_options(column_labels.border.top.color = "white", 
              column_labels.border.bottom.color = "white",
              table_body.border.bottom.color = "white",
              table.additional_css = "a {color: black !important;}")

tibble("icono" = c("star"), 
       "texto" = c("Recientes")) |> 
  gt()

tribble(~icono, ~titulo, ~explicacion,
        "star", "Recientes", "Código o datos recientemente actualizados",
        "arrow-up", "Popular", "Conjunto de datos popular en GitHub",
        "window-restore", "App", "Aplicación interactiva disponible",
        "venus", "Género", "Datos desagregados por género",
        "map", "Comunas", "Datos desagregados por comunas",
        "calendar-days", "Años", "Temporalidad anual de las observaciones",
        "calendar-days", "Meses", "Temporalidad mensual de las observaciones") |> 
  gt() |> 
  fmt_icon(columns = icono) |> 
  cols_align(columns = icono, "center")
