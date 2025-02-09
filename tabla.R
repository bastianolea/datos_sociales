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

col_tag <- function(elementos) {
  
  # browser()
  div_out <- map(elementos, \(tag) {
    
    div_out <- htmltools::div(
      style = paste(
        "display: inline-block; padding: 3px 8px; border-radius: 15px; font-weight: 600; 
        font-size: 10px; border: 1px solid black; margin: 2px 0px 2px 0px"
      ),
      tag
    )
  })
  # browser()
  div_out |> 
    # shiny::tagList() |> 
    htmltools::div() |>
    as.character() |> 
    gt::html()
}


col_link <- function(texto, enlace) {
 paste0("[", texto, "]",
       "(", enlace, ")")
}

col_icon <- function(value, icon = "arrow-up", color = "black") {
  if (value) {
    logo <- fontawesome::fa(icon, fill = color)
  } else {
    logo <- ""
  }
  
  logo |> as.character() |> gt::html()
}

col_icon_multi <- function(value, icon = "arrow-up", color = "black") {
  if (value) {
    logo <- fontawesome::fa(icon, fill = color)
  } else {
    logo <- ""
  }
  
  logo |> as.character() |> gt::html()
}

datos |> 
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
         aplicación = col_icon(aplicación, "window-restore"),
         popular = col_icon(popular, "arrow-up"),
         genero = col_icon(genero, "venus"),
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
  cols_hide(c(enlace, estrellas, fecha)) |> 
  cols_width(descripcion ~ "40%",
             etiquetas ~ "20%") |> 
  tab_style(style = "vertical-align:middle", 
            locations = cells_body( columns = everything())
            ) 
  # opt_interactive(active = TRUE, 
  #                 use_highlight = TRUE,
  #                 use_sorting = TRUE,
  #                 selection_mode = "single", use_pagination = F,
  #                 use_resizers = TRUE)
