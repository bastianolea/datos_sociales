# Datos sociales

Repositorio de mis repositorios de datos. Se trata de una [página web que contiene una tabla](https://bastianolea.github.io/datos_sociales/) con un resumen de todos los conjuntos de datos sociales que tengo en mi repositorio. Cada dato tiene un enlace a su repositorio, donde se encuentran los datos junto al código que obtiene, procesa y limpia los datos, así como otros scripts que pueden servir para analizar, explorar, y visualizar.

La tabla se genera mediante un web scraping de mi cuenta de GitHub. Los contenidos de la tabla se adaptan en base a las etiquetas que tiene cada repositorio, la descripción viene de la descripción de cada repo, y el título viene del readme. Las columnas con iconos también se hacen automáticamente a partir de las etiquetas. La tabla se genera con el [paquete {gt}.](https://gt.rstudio.com) La página se genera con [Quarto](https://github.com/quarto-dev/quarto-r) y se publica automáticamente con GitHub Pages.

Si quieres aprender a generar una página web así con Quarto, [sigue este tutorial que hice.](https://bastianolea.rbind.io/blog/tutorial_quarto_github_pages/)

### Instrucciones

1. Ejecutar `scraping.R`
2. Se obtiene `repositorios.rds`
3. Renderizar `index.qmd`
4. Se genera `docs/index.html`
5. Subir (push) repositorio a GitHub
6. GitHub genera la [página](https://bastianolea.github.io/datos_sociales/) (GitHub Pages)