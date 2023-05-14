#' Render a list of plots in a Shiny UI
#'
#' This function renders a list of plots in a Shiny UI, along with their titles.
#'
#' @param plots A list of plot objects to be rendered
#' @param title_fun A function that takes an integer i and returns a string for the title of the i-th plot
#' @return A tagList object containing the rendered plots and titles
#' @export
#'
#' @examples
#' \dontrun{
#' # In a Shiny app
#' output$plots <- renderUI({
#'   req(input$OK)
#'   isolate({
#'     req(length(input$comp) > 0 || length(input$solo) > 0)
#'     render_plots_UI(plots(), function(i) {
#'       glue("Estimated Marginal Means for {input$dep} by {input$emMeans[[i]]}")
#'     })
#'   })
#' })
#' }
render_plots_UI <- function(plots, title_fun) {
  titles <- lapply(seq_along(plots), function(i) {
    title_fun(i)
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
