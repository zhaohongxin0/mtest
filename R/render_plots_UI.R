#' Add a table to a Word document
#'
#' This function adds a table to a Word document with a specified orientation and reference text.
#' The function modifies the input Word document directly and does not return any values.
#'
#' @param plots A list of ggplot objects to be rendered in the Shiny app.
#' @param titles A character string containing the title text to be displayed above each plot. Can include placeholders for dynamic values.
#' @return A Shiny UI element containing the rendered plots and their titles.
#' @examples
#' \dontrun{
#' # In a Shiny app
#' output$plots <- renderUI({
#'   req(input$OK)
#'   isolate({
#'     req(length(input$comp) > 0 || length(input$solo) > 0)
#'     render_plots_UI(plots(), glue("Estimated Marginal Means for {input$dep} by {input$emMeans[[i]]}"))
#'   })
#' })
#' }
#'
#' @export

render_plots_UI <- function(plots, title_glue) {
  titles <- lapply(seq_along(plots), function(i) {
    glue(title_glue)
  })

  plot_UIs <- lapply(seq_along(plots), function(i) {
    tagList(
      br(),
      titles[[i]],
      renderPlot({
        plots[[i]]
      }), br()
    )
  })

  out <- do.call(tagList, unlist(plot_UIs, recursive = FALSE))
  return(out)
}
