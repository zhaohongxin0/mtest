#' Create UI components for downloading a plot in various formats
#'
#' This function creates UI components with download buttons for PNG, PPT, and PDF formats.
#'
#' @param id A unique ID for the Shiny module
#' @return A tagList with the download buttons for the plot
#' @examples
#' # In a Shiny app
#' # UI part:
#' downloadPlot_UI("plot_downloads")
#'
#' # Server part:
#' downloadPlot_server("plot_downloads", my_plot, 800, 600)
#'
#' # Example of how to use it in a Shiny app
#' library(shiny)
#' library(ggplot2)
#'
#' ui <- fluidPage(
#'   titlePanel("Example Plot Download"),
#'   sidebarLayout(
#'     sidebarPanel(
#'       sliderInput("plot_x", "Plot Width:", 400, 1200, 800),
#'       sliderInput("plot_y", "Plot Height:", 300, 900, 600),
#'       downloadPlot_UI("plot_downloads")
#'     ),
#'     mainPanel(
#'       plotOutput("my_plot")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   my_plot <- reactive({
#'     ggplot(mtcars, aes(x = mpg, y = disp)) +
#'       geom_point() +
#'       theme_minimal()
#'   })
#'
#'   output$my_plot <- renderPlot({
#'     my_plot()
#'   }, width = input$plot_x, height = input$plot_y)
#'
#'   downloadPlot_server("plot_downloads", my_plot(), input$plot_x, input$plot_y)
#' }
#'
#' shinyApp(ui, server)
#'
#' @importFrom shiny NS tagList downloadButton
#' @export


downloadPlot_UI <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(ns("downloadpng"), "点此下载高清PNG大图"),
    downloadButton(ns("downloadppt"), "点此下载可编辑的PPT文件"),
    downloadButton(ns("downloadpdf"), "点此下载PDF矢量图")
  )
}



#' Server logic for downloading a plot in various formats
#'
#' This function handles the server-side logic for downloading a plot in PNG, PPT, and PDF formats.
#'
#' @param id A unique ID for the Shiny module, matching the one used in downloadPlot_UI()
#' @param plot The ggplot2 plot to be downloaded
#' @param x The width of the plot in pixels
#' @param y The height of the plot in pixels
#' @return A module server function
#' @examples
#' # In a Shiny app
#' # UI part:
#' downloadPlot_UI("plot_downloads")
#'
#' # Server part:
#' downloadPlot_server("plot_downloads", my_plot, 800, 600)
#'
#' # Example of how to use it in a Shiny app
#' library(shiny)
#' library(ggplot2)
#'
#' ui <- fluidPage(
#'   titlePanel("Example Plot Download"),
#'   sidebarLayout(
#'     sidebarPanel(
#'       sliderInput("plot_x", "Plot Width:", 400, 1200, 800),
#'       sliderInput("plot_y", "Plot Height:", 300, 900, 600),
#'       downloadPlot_UI("plot_downloads")
#'     ),
#'     mainPanel(
#'       plotOutput("my_plot")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   my_plot <- reactive({
#'     ggplot(mtcars, aes(x = mpg, y = disp)) +
#'       geom_point() +
#'       theme_minimal()
#'   })
#'
#'   output$my_plot <- renderPlot({
#'     my_plot()
#'   }, width = input$plot_x, height = input$plot_y)
#'
#'   downloadPlot_server("plot_downloads", my_plot(), input$plot_x, input$plot_y)
#' }
#'
#' shinyApp(ui, server)
#'
#' @importFrom shiny moduleServer
#' @importFrom ggplot2 ggsave
#' @importFrom eoffice topptx
#' @importFrom ragg agg_png
#' @export

downloadPlot_server <- function(id, plot, x, y) {
  moduleServer(id, function(input, output, session) {
    output$downloadpng <- downloadHandler(
      filename = function() {
        paste("www/plot.png")
      },
      content = function(file) {

        withProgress(message = "写入png文件 ...", {
          incProgress(0.4)

          ragg::agg_png("www/plot.png", width = 4 * x,
                        height = 4 * y, units = "px", scaling = 4)

          plot(plot)

          dev.off()

          file.copy("www/plot.png", file)
        })
      },
      contentType = "application/png"
    )

    output$downloadppt <- downloadHandler(
      filename = function() {
        paste("www/plot.pptx")
      },
      content = function(file) {

        withProgress(message = "写入pptx文件 ...", {
          incProgress(0.4)

          eoffice::topptx(figure = plot,
                          filename = "www/plot.pptx", width = x * 4 / 300, height = y * 4 / 300
          )

          file.copy("www/plot.pptx", file)
        })
      },
      contentType = "application/pptx"
    )

    output$downloadpdf <- downloadHandler(
      filename = function() {
        paste("www/plot.pdf")
      },
      content = function(file) {

        withProgress(message = "写入pdf文件 ...", {
          incProgress(0.4)

          dev.new()
          ggplot2::ggsave("plot.pdf", plot,
                          width = x * 4 / 300, height = y * 4 / 300, device = cairo_pdf, family = "Arial", path = "www")
          dev.off()
          file.copy("www/plot.pdf", file)
        })
      },
      contentType = "application/pdf"
    )
  })
}


