library(rvest)
library(dplyr)
library(stringr)
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
  # repositorio <- repositorios[[15]]
  
  titulo <- repositorio |> html_elements("h3") |> html_elements("a") |> html_text2() |> print()
  enlace <- repositorio |> html_elements("h3") |> html_elements("a") |> html_attr("href")
  
  acceso <- repositorio |> html_elements("h3") |> html_elements("span") |> html_text2()
  if (!("Public" %in% acceso)) return(NULL)
  
  descripcion <- repositorio |> html_elements(".col-9") |> html_text2()
  
  etiquetas <- repositorio |> html_elements(".topics-row-container") |> html_elements("a") |> html_text2() |> list()
  # sólo extrae hasta 7 etiquetas, pero se usa solo para detectar si viene "data"
  
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


# scraping repos ----

# scraping de cada repositorio para obtener enlaces y títulos
datos_repos <- map(tabla_1$enlace, \(enlace) {
  # enlace <- tabla_1$enlace[8]
  
  url_repo <- paste0("https://github.com", enlace)
  message(url_repo)
  
  sitio_repo <- url_repo |> 
    read_html()
  
  enlace_app <- sitio_repo |> 
    html_elements(".Layout-sidebar") |> 
    html_elements(".my-3") |> 
    html_elements(".flex-auto") |> 
    html_elements("a") |> 
    html_attr("href")
  
  if (length(enlace_app) == 0) enlace_app <- NA_character_
  
  titulo_repo <- sitio_repo |> 
    html_elements(".markdown-body") |> 
    html_elements("h1") |> 
    html_text() |> 
    pluck(1)
  
  if (length(titulo_repo) == 0) {
    titulo_repo <- sitio_repo |> 
      html_elements(".markdown-body") |> 
      html_elements("h2") |> 
      html_text() |> 
      pluck(1)
  }
  
  if (length(titulo_repo) == 0) titulo_repo <- NA_character_
  
  # enlaces de descarga
  descarga <- sitio_repo |> 
    html_elements(".markdown-body") |> 
    html_elements("a") |> 
    html_attr("href") |> 
    str_subset("github.*(raw|blob).*datos") |> 
    pluck(1)
  # para que el repositorio aparezca con enlace de descarga, tiene que tener un enlace en el readme que apunte a un archivo hosteado en GitHub (con `raw` en el enlace)
  
  if (length(descarga) != 1) descarga <- NA_character_
  
  etiquetas <- sitio_repo |> 
    html_elements(".Layout-sidebar") |> 
    html_elements(".f6") |> 
    html_elements("a") |> 
    html_text2() |> 
    list()
  
  tibble(enlace_app,
         titulo_repo,
         enlace,
         descarga,
         etiquetas)
})

# limpieza
datos_repos_2 <- datos_repos |> 
  list_rbind() |> 
  filter(!is.na(enlace_app) | !is.na(titulo_repo)) |> 
  # corregir nombres de repos
  mutate(titulo_repo = str_replace(titulo_repo, "Visualizador de datos de", "Datos de"),
         titulo_repo = str_replace(titulo_repo, "Visualizaciones de datos", "Datos"),
         titulo_repo = str_replace(titulo_repo, "Visualizador de estadísticas", "Datos"),
         titulo_repo = str_replace(titulo_repo, "Visualizador de estadísticas", "Datos"),
         titulo_repo = str_replace(titulo_repo, "Visualizador de", "Datos de"))

# datos_repos_2 |> select(titulo_repo) |> print(n=Inf)

# agregar datos de repositorios y arreglar enlaces
tabla_4 <- tabla_2 |> 
  select(-etiquetas) |> # la segunda tabla tiene mejores etiquetas
  left_join(datos_repos_2,
            by = "enlace") |> 
  mutate(enlace = paste("https://github.com", enlace, sep = "")) |> 
  mutate(bajar = !is.na(descarga))


# calcular
tabla_5 <- tabla_4 |> 
  rowwise() |> 
  mutate(app = "app" %in% etiquetas,
         genero = "genero" %in% etiquetas,
         comunas = "comunas" %in% etiquetas,
         tiempo = case_when("meses" %in% etiquetas ~ "mensual",
                            "tiempo" %in% etiquetas ~ "anual",
                            .default = "no")) |> 
  ungroup() |> 
  mutate(popular = estrellas > 1 & estrellas > quantile(estrellas, .5, na.rm = TRUE),
         popular = ifelse(is.na(popular), FALSE, popular),
         reciente = fecha >= today() - months(2))

# tabla_4 |>
#   filter(titulo == "femicidios_chile") |>
#   tidyr::unnest(etiquetas)

# tabla_4 |>
#   filter(titulo == "plebiscitos_chile") |> 
#   glimpse()

# guardar ----
readr::write_rds(tabla_5, "repositorios.rds")
