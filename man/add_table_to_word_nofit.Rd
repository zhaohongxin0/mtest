% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_table_to_word_nofit.R
\name{add_table_to_word_nofit}
\alias{add_table_to_word_nofit}
\title{Add a table to a Word document}
\usage{
add_table_to_word_nofit(
  word_doc,
  table,
  long = 29.7,
  short = 21,
  orientation = "portrait",
  reference_text = ""
)
}
\arguments{
\item{word_doc}{A Word document object created by the officer package's read_docx() function.}

\item{table}{A flextable object to be added to the Word document.}

\item{orientation}{A character string specifying the orientation of the table. Valid values are "portrait" and "landscape". Default is "portrait".}

\item{reference_text}{A character string containing the reference text to be added after the table. Default is an empty string.}
}
\value{
A new officer word object that added our table to the original word object
}
\description{
This function adds a table to a Word document with a specified orientation and reference text.
The function modifies the input Word document directly and does not return any values.
}
\examples{
\dontrun{
# In a Shiny app
output$downloadData <- downloadHandler(
  filename = function() {
    paste("example.docx")
  },
  content = function(file) {
    tmp <- read_docx()

    tables_list <- list(durbin_table(), collin_table())
    reference_text <- "参考文献：Fox, J., & Weisberg, S. (2020). car: Companion to Applied Regression. [R package]. Retrieved from https://cran.r-project.org/package=car."

    tmp <- add_tables_to_word(tmp, tables_list, 29.7,21,"portrait", reference_text)

    tmp \%>\% print(target = file)
  },
  contentType = "application/docx"
)
}

}
