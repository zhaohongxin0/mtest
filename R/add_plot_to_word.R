#' Add a plot to a Word document
#'
#' This function adds a plot to a Word document with a specified base width, aspect ratio, orientation, and reference text.
#' The function modifies the input Word document directly and does not return any values.
#'
#' @param word_doc A Word document object created by the officer package's read_docx() function.
#' @param plot A ggplot2 plot object to be added to the Word document.
#' @param base_width A numeric value specifying the base width of the plot.
#' @param x A numeric value specifying the original width of the plot.
#' @param y A numeric value specifying the original height of the plot.
#' @param orientation A character string specifying the orientation of the plot. Valid values are "portrait" and "landscape". Default is "portrait".
#' @param reference_text A character string containing the reference text to be added after the plot. Default is an empty string.
#' @return A new officer word object that added our plot to the original word object
#' @examples
#' \dontrun{
#' # In a Shiny app
#' output$downloadData <- downloadHandler(
#'   filename = function() {
#'     paste("example.docx")
#'   },
#'   content = function(file) {
#'     tmp <- read_docx()
#'
#'     tmp <- add_plot_to_word(tmp, rocPlot(), 6, x, y, "portrait", "参考文献：Sing, T., Sander, O., Beerenwinkel, N., & Lengauer, T. (2015). ROCR: Visualizing the Performance of Scoring Classifiers. [R package]. Retrieved from https://cran.r-project.org/package=ROCR.")
#'
#'     tmp %>% print(target = file)
#'   },
#'   contentType = "application/docx"
#' )
#' }
#'
#' @export
add_plot_to_word <- function(word_doc, plot, base_width, x, y, orientation = "portrait", reference_text = "") {
  tryCatch({
    if (isTruthy(plot)) {
      word_doc <- word_doc %>%
        body_add_plot(plot, width = base_width, height = base_width * y / x) %>%
        body_add_par(" ") %>%
        body_add_fpar(fpar(ftext(reference_text, prop = fp_text(font.size = 8))))

      if (orientation == "landscape") {
        word_doc <- word_doc %>% body_end_section_landscape(w = 21 / 2.54, h = 29.7 / 2.54)
      } else {
        word_doc <- word_doc %>% body_end_section_portrait(w = 21 / 2.54, h = 29.7 / 2.54)
      }
    }
  },
  shiny.silent.error = function(e) {
    word_doc <- word_doc
  })

  return(word_doc)
}
