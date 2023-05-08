# 前置函数，下载图片按钮----
downloadPlot_UI <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(ns("downloadpng"), "点此下载高清PNG大图"),
    downloadButton(ns("downloadppt"), "点此下载可编辑的PPT文件"),
    downloadButton(ns("downloadpdf"), "点此下载PDF矢量图")
  )
}

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
