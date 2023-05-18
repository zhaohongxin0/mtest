#' Add multiple tables to a Word document
#'
#' This function adds multiple tables to a Word document. Each table is followed by the same reference text.
#' The tables are provided as a list of flextable objects. The function also allows you to set the orientation
#' of the Word document (either portrait or landscape).
#'
#' @param word_doc A Word document object created using officer::read_docx().
#' @param tables_list A list of flextable objects to be added to the Word document.
#' @param orientation Character, either "portrait" or "landscape". Default is "portrait".
#' @param reference_text A character string containing the reference text to be added after each table.
#'
#' @return A modified Word document object with the tables and reference text added.
#' @export
#'
#' @examples
#' \dontrun{
#' library(officer)
#' library(flextable)
#' library(magrittr)
#'
#' # Create a sample Word document
#' my_doc <- read_docx()
#'
#' # Create some sample flextables
#' table1 <- flextable(head(mtcars))
#' table2 <- flextable(head(iris))
#' table_list <- list(table1, table2)
#'
#' # Add the tables to the Word document
#' my_doc <- add_tables_to_word(my_doc, tables_list = table_list,
#'                              orientation = "portrait",
#'                              reference_text = "Sample reference text.")
#'
#' # Save the Word document
#' my_doc %>% print(target = "example.docx")
#' }
add_tables_to_word <- function(word_doc, tables_list, orientation = "portrait", reference_text = "") {

  tryCatch({
    tables_list
    if (isTruthy(tables_list)) {

      page_width <- 29.7 / 2.54
      page_width2 <- 21 / 2.54
      if (orientation == "landscape") {
        max_width = page_width
      } else {
        max_width = page_width2
      }

      for (table in tables_list) {
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
    }
  },
  shiny.silent.error = function(e) {
    word_doc <- word_doc
  })

  return(word_doc)
}
