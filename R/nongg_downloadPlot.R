#' Create UI components for downloading a plot in various formats
#'
#' This function creates UI components with download buttons for PNG, PPT, and PDF formats.
#'
#' @param id A unique ID for the Shiny module
#' @return A tagList with the download buttons for the plot
#' @examples
#' # In a Shiny app
#' # UI part:
#' nongg_downloadPlot_UI("plot_downloads")
#'
#' # Server part:
#' nongg_downloadPlot_server("plot_downloads", my_plot_png, my_plot_ppt, my_plot_pdf, 800, 600, "plot")
#'
#' @importFrom shiny NS tagList downloadButton
#' @export

nongg_downloadPlot_UI <- function(id) {
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
#' @param id A unique ID for the Shiny module, matching the one used in adjusted_downloadPlot_UI()
#' @param plot_png The ggplot2 plot to be downloaded as a PNG
#' @param plot_ppt The ggplot2 plot to be downloaded as a PPT
#' @param plot_pdf The ggplot2 plot to be downloaded as a PDF
#' @param x The width of the plot in pixels
#' @param y The height of the plot in pixels
#' @param render A string, either "plot" or "print". When "plot", it uses plot() function to render the plot. When "print", it uses print() function to render the plot.
#' @return A module server function
#' @examples
#' # In a Shiny app
#' # UI part:
#' nongg_downloadPlot_UI("plot_downloads")
#'
#' # Server part:
#' nongg_downloadPlot_server("plot_downloads", my_plot_png, my_plot_ppt, my_plot_pdf, 800, 600, "print")
#'
#' @importFrom shiny moduleServer
#' @importFrom ggplot2 ggsave
#' @importFrom eoffice topptx
#' @importFrom ragg agg_png
#' @export

nongg_downloadPlot_server <- function(id, plot_png, plot_ppt,plot_pdf, x, y, render = "plot") {
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

          if (render == "plot") {
            plot(plot_png)
          } else {
            print(plot_png)
          }

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

          eoffice::topptx(figure = plot_ppt,
                          filename = "www/plot.pptx", width = x * 4 / 300, height = y * 4 / 300)

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

          pdf("www/plot.pdf", width = 8,
              height = 8 * y/x)

          if (render == "plot") {
            plot(plot_pdf)
          } else {
            print(plot_pdf)
          }

          dev.off()
          file.copy("www/plot.pdf", file)
        })
      },
      contentType = "application/pdf"
    )
  })
}
