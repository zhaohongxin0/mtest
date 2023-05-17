#' Add multiple ggplot objects to a Word document
#'
#' This function adds multiple ggplot objects to a Word document with a specified orientation and reference texts.
#' Reference texts will be displayed after all the plots, with a blank line between them.
#'
#' @param word_doc An officer Word document object
#' @param plots A list of ggplot objects to be added to the Word document
#' @param base_width Numeric, the base width of the plots in inches
#' @param x Numeric, the width of the plot in pixels
#' @param y Numeric, the height of the plot in pixels
#' @param orientation Character, the orientation of the page, either "portrait" or "landscape" (default: "portrait")
#' @param reference_texts Character vector or NULL, the reference texts to be added after all the plots (default: NULL)
#' @return The modified officer Word document object with the ggplot objects added
#' @examples
#'
#' ## 下载包含多个图像的Word文档
#' ## 首先，确保已安装并加载所需的包
#' # install.packages("officer")
#' # install.packages("flextable")
#' library(officer)
#' library(flextable)
#'
#' ## 使用downloadHandler生成下载链接
#' output$downloadData <- downloadHandler(
#'   filename = function() {
#'     paste("example.docx")
#'   },
#'   content = function(file) {
#'     tmp <- read_docx()
#'
#'     # 创建一个包含多个图像的列表
#'     plots_list <- list(
#'       rocPlot(),
#'       rocPlot(),
#'       rocPlot()
#'     )
#'
#'     reference_text <- "参考文献：Sing, T., Sander, O., Beerenwinkel, N., & Lengauer, T. (2015). ROCR: Visualizing the Performance of Scoring Classifiers. [R package]. Retrieved from https://cran.r-project.org/package=ROCR."
#'
#'     # 使用add_plots_to_word()函数添加多个图像
#'     tmp <- add_plots_to_word(tmp, plots = plots_list, base_width = 6, x = x, y = y, orientation = "portrait", reference_texts = reference_text)
#'
#'     tmp %>% print(target = file)
#'   },
#'   contentType = "application/docx"
#' )

#' @export

add_plots_to_word <- function(word_doc, plots, base_width, x, y, orientation = "portrait", reference_texts = NULL) {
  tryCatch({
    if (length(plots) > 0) {
      for (i in seq_along(plots)) {
        word_doc <- word_doc %>%
          body_add_plot(plots[[i]], width = base_width, height = base_width * y / x) %>%
          body_add_par(" ")

        if (orientation == "landscape") {
          word_doc <- word_doc %>% body_end_section_landscape(w = 21 / 2.54, h = 29.7 / 2.54)
        } else {
          word_doc <- word_doc %>% body_end_section_portrait(w = 21 / 2.54, h = 29.7 / 2.54)
        }
      }

      if (!is.null(reference_texts)) {
        word_doc <- word_doc %>%
          body_add_par(" ") %>%
          body_add_fpar(fpar(ftext(paste(reference_texts, collapse = "\n"), prop = fp_text(font.size = 8))))
      }
    }
  },
  shiny.silent.error = function(e) {
    word_doc <- word_doc
  })

  return(word_doc)
}
