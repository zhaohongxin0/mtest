#' Add a table to a Word document
#'
#' This function adds a table to a Word document with a specified orientation and reference text.
#' The function modifies the input Word document directly and does not return any values.
#'
#' @param word_doc A Word document object created by the officer package's read_docx() function.
#' @param table A flextable object to be added to the Word document.
#' @param orientation A character string specifying the orientation of the table. Valid values are "portrait" and "landscape". Default is "portrait".
#' @param reference_text A character string containing the reference text to be added after the table. Default is an empty string.
#'
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
#'     add_table_to_word(tmp, durbin_table(), "portrait", "参考文献：Fox, J., & Weisberg, S. (2020). car: Companion to Applied Regression. [R package]. Retrieved from https://cran.r-project.org/package=car.")
#'
#'     add_table_to_word(tmp, collin_table(), "portrait", "参考文献：Fox, J., & Weisberg, S. (2020). car: Companion to Applied Regression. [R package]. Retrieved from https://cran.r-project.org/package=car.")
#'
#'     tmp %>% print(target = file)
#'   },
#'   contentType = "application/docx"
#' )
#' }
#'
#' @export
add_table_to_word <- function(word_doc, table, orientation = "portrait", reference_text = "") {

  tryCatch({
    if (isTruthy(table)) {

      page_width <- 29.7 / 2.54
      page_width2 <- 21 / 2.54
      if (orientation == "landscape") {
        max_width = page_width
      } else {
        max_width = page_width2
      }

      fitTable <- table %>% fit_to_width(max_width = max_width, unit = "in")
      word_doc <- word_doc %>%
        body_add_flextable(fitTable) %>%
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

