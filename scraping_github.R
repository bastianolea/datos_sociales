library(rvest)
library(dplyr)
library(purrr)
library(lubridate)

paginas <- 1:4

# scraping ----
url <- paste("https://github.com/bastianolea?page=", paginas, "&tab=repositories", sep = "")

repositorios_pagina <- map(url, \(url_pagina) {
  # url_pagina <- url[2]
  pagina <- session(url_pagina) |> read_html()
  
  pagina_2 <- pagina |> 
    # read_html() |> 
    html_elements("#user-repositories-list") |> 
    html_elements("li")
})


# de entre todas las páginas, separar cada elemento como un elemento de una lista
# porque es una lista con un elemento por página, y dentro de cada elemento, x elementos por cada repositorio;
# entonces, se busca dejar una sola lista con tantos elementos como repositorios
repositorios <- map(repositorios_pagina, ~{
  map(.x, ~{
    .x
  })
}) |> 
  list_flatten()


# extraer datos ----
tabla_0 <- map(repositorios, \(repositorio) {
  # repositorio <- repositorios[[6]]
  
  titulo <- repositorio |> html_elements("h3") |> html_elements("a") |> html_text2()
  enlace <- repositorio |> html_elements("h3") |> html_elements("a") |> html_attr("href")
  
  acceso <- repositorio |> html_elements("h3") |> html_elements("span") |> html_text2()
  if (!("Public" %in% acceso)) return(NULL)
  
  descripcion <- repositorio |> html_elements(".col-9") |> html_text2()
  
  etiquetas <- repositorio |> html_elements(".topics-row-container") |> html_elements("a") |> html_text2() |> list()
  
  fecha <- repositorio |> html_elements("relative-time") |> html_attr("datetime") |> as_date()
  
  estrellas <- repositorio |> 
    html_elements("div.col-10.col-lg-9.d-inline-block > div.f6.color-fg-muted.mt-2 > a") |> 
    html_text2() |>  
    as.numeric() |> 
    pluck(1)
  
  
  tibble(titulo,
         enlace,
         descripcion,
         etiquetas,
         estrellas,
         fecha)
})

# filtrar ----
# dejar solo los que contengan datos
tabla_1 <- tabla_0 |> 
  list_rbind() |> 
  distinct() |> 
  rowwise() |> 
  filter("data" %in% unlist(etiquetas)) |> 
  ungroup()

# corregir ----
tabla_2 <- tabla_1 |> 
  mutate(estrellas = ifelse(is.na(estrellas), 0, estrellas))

# calcular
tabla_3 <- tabla_2 |> 
rowwise() |> 
  mutate(aplicación = "app" %in% etiquetas,
         genero = "genero" %in% etiquetas,
         comunas = "comunas" %in% etiquetas,
         tiempo = case_when("meses" %in% etiquetas ~ "mensual",
                                  "tiempo" %in% etiquetas ~ "anual",
                                  .default = "no")) |> 
  ungroup() |> 
  mutate(popular = estrellas > quantile(estrellas, .35, na.rm = TRUE),
         popular = ifelse(is.na(popular), FALSE, popular),
         reciente = fecha >= today() - months(2))





# guardar ----
readr::write_rds(tabla_3, "repositorios.rds")
