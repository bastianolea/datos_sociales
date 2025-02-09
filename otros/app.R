# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file
library(shiny)
library(gt)
# pkgload::load_all(".", helpers = FALSE, attach_testthat = FALSE)

gt_tbl <-
  gtcars |>
  gt() |>
  fmt_currency(columns = msrp, decimals = 0) |>
  cols_hide(columns = -c(mfr, model, year, mpg_c, msrp)) |>
  cols_label_with(columns = everything(), fn = toupper) |>
  data_color(columns = msrp, method = "numeric", palette = "viridis") |>
  sub_missing() |>
  opt_interactive(active = TRUE, use_highlight = TRUE, use_compact_mode = TRUE, 
                  selection = "single")
#, selection = "multiple")

ui <- fluidPage(
  gt_output(outputId = "table"),
  verbatimTextOutput("selected")
)

server <- function(input, output, session) {
  output$table <- render_gt(expr = gt_tbl)
  output$selected <- renderPrint({
    input$table
  })
}

shinyApp(ui = ui, server = server)