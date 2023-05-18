#' Add paragraphs to a Word document object
#'
#' This function takes a Word document object and a text string as input. It splits the text into paragraphs based on newline characters and inserts them into the Word document object.
#' @param doc Word document object to be modified
#' @param text Text to be split and inserted into the Word document object
#' @return A modified Word document object with the inserted paragraphs
#' @export
#' @examples
#' \dontrun{
#' library(officer)
#'
#' my_doc <- read_docx() # Create a new Word document
#' text_to_insert <- "This is the first paragraph.\nThis is the second paragraph."
#' updated_doc <- add_paragraphs_to_doc(my_doc, text_to_insert) # Insert paragraphs and get the updated document
#' print(updated_doc, target = "output.docx") # Save Word document
#' }
add_paragraphs_to_doc <- function(doc, text) {
  tryCatch({
    paragraphs <- unlist(strsplit(text, "\n"))
    for (paragraph in paragraphs) {
      doc <- doc %>%
        body_add_par(paragraph, style = "Normal")
    }
    doc <- doc %>%
      body_add_par("") %>%
      body_add_par("")
  },
  error = function(e) {
    doc <- doc
  })

  return(doc)
}
