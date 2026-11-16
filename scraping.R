library(gh)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(lubridate)

# obtiene los datos de los repositorios mediante la api de github, usando el paquete {gh}
# requiere un token de github configurado (por ejemplo, con gitcreds::gitcreds_set() o la variable GITHUB_PAT)

usuario <- "bastianolea"

# repositorios ----
# obtener todos los repositorios públicos del usuario (la api pagina automáticamente con .limit = Inf)
repositorios_api <- gh(
  "/users/{usuario}/repos",
  usuario = usuario,
  type = "owner",
  .limit = Inf
)

# extraer datos ----
tabla_0 <- map(repositorios_api, \(repositorio) {
  tibble(
    titulo = repositorio$name,
    enlace = repositorio$html_url,
    descripcion = repositorio$description %||% NA_character_,
    etiquetas = list(unlist(repositorio$topics)),
    estrellas = repositorio$stargazers_count,
    fecha = as_date(repositorio$pushed_at),
    enlace_app = repositorio$homepage %||% NA_character_,
    privado = repositorio$private,
    fork = repositorio$fork
  )
}) |>
  list_rbind()


# filtrar ----
# dejar solo los repositorios públicos que tengan la etiqueta "data"
tabla_1 <- tabla_0 |>
  filter(!privado) |>
  rowwise() |>
  filter("data" %in% etiquetas) |>
  ungroup() |>
  select(-privado, -fork)

# corregir ----
tabla_2 <- tabla_1 |>
  mutate(
    estrellas = ifelse(is.na(estrellas), 0, estrellas),
    enlace_app = ifelse(enlace_app == "", NA_character_, enlace_app)
  )


# readme de cada repositorio ----

# la api de github a veces responde el readme como json (con el markdown en base64)
# y a veces directamente como html renderizado, así que se manejan ambos casos
convertir_readme_a_html <- function(respuesta) {
  if (!is.null(respuesta$content)) {
    # json: el contenido viene codificado en base64
    readme_texto <- respuesta$content |>
      str_remove_all("\n") |>
      charToRaw() |>
      jsonlite::base64_dec() |>
      rawToChar()
  } else {
    # texto: puede ser html o markdown
    readme_texto <- respuesta$message
  }

  Encoding(readme_texto) <- "UTF-8"

  es_html <- str_detect(str_trim(readme_texto), "^<")

  if (!es_html) {
    readme_texto <- commonmark::markdown_html(readme_texto, extensions = TRUE)
  }

  read_html(readme_texto)
}

# obtener el readme para extraer el título y los enlaces de descarga
obtener_readme <- function(repositorio) {
  message(repositorio)

  readme <- tryCatch(
    gh(
      "/repos/{usuario}/{repositorio}/readme",
      usuario = usuario,
      repositorio = repositorio
    ),
    error = \(error) NULL
  )

  if (is.null(readme)) {
    return(tibble(
      titulo = repositorio,
      titulo_repo = NA_character_,
      descarga = NA_character_
    ))
  }

  readme_html <- convertir_readme_a_html(readme)

  # título: primer h1 del readme, o en su defecto el primer h2
  titulo_repo <- readme_html |>
    html_elements("h1") |>
    html_text2() |>
    pluck(1)

  if (length(titulo_repo) == 0) {
    titulo_repo <- readme_html |>
      html_elements("h2") |>
      html_text2() |>
      pluck(1)
  }

  if (length(titulo_repo) == 0) {
    titulo_repo <- NA_character_
  }

  # enlaces de descarga
  # para que el repositorio aparezca con enlace de descarga, tiene que tener un enlace en el readme que apunte a un archivo hosteado en GitHub (con `raw` o `blob` en el enlace)
  descarga <- readme_html |>
    html_elements("a") |>
    html_attr("href") |>
    str_subset("github.*(raw|blob).*datos") |>
    pluck(1)

  if (length(descarga) != 1) {
    descarga <- NA_character_
  }

  tibble(
    titulo = repositorio,
    titulo_repo,
    descarga
  )
}

datos_repos <- map(tabla_2$titulo, obtener_readme) |>
  list_rbind()

# limpieza
datos_repos_2 <- datos_repos |>
  # corregir nombres de repos
  mutate(
    titulo_repo = str_squish(titulo_repo),
    titulo_repo = str_replace(
      titulo_repo,
      "Visualizador de datos de",
      "Datos de"
    ),
    titulo_repo = str_replace(titulo_repo, "Visualizaciones de datos", "Datos"),
    titulo_repo = str_replace(
      titulo_repo,
      "Visualizador de estadísticas",
      "Datos"
    ),
    titulo_repo = str_replace(titulo_repo, "Visualizador de", "Datos de")
  )

# agregar datos de los readme
tabla_4 <- tabla_2 |>
  left_join(datos_repos_2, by = "titulo") |>
  mutate(bajar = !is.na(descarga))


# calcular ----
tabla_5 <- tabla_4 |>
  rowwise() |>
  mutate(
    app = "app" %in% etiquetas, # para que salga como app, tiene que tener el tag "app"
    genero = "genero" %in% etiquetas,
    comunas = "comunas" %in% etiquetas,
    tiempo = case_when(
      "meses" %in% etiquetas ~ "mensual",
      "tiempo" %in% etiquetas ~ "anual",
      .default = "no"
    )
  ) |>
  ungroup() |>
  mutate(
    popular = estrellas > 1 & estrellas > quantile(estrellas, .5, na.rm = TRUE),
    popular = ifelse(is.na(popular), FALSE, popular),
    reciente = fecha >= today() - months(2)
  ) |>
  # mismo orden de columnas que la versión anterior
  relocate(
    titulo,
    enlace,
    descripcion,
    estrellas,
    fecha,
    enlace_app,
    titulo_repo,
    descarga,
    etiquetas,
    bajar
  )


# guardar ----
readr::write_rds(tabla_5, "repositorios.rds")
