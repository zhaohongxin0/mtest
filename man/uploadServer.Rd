% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/upload_data.R
\name{uploadServer}
\alias{uploadServer}
\title{Upload Server for Shiny}
\usage{
uploadServer(input, output, session, id)
}
\arguments{
\item{id}{A character string for the id of the UI elements}
}
\value{
A list of shiny outputs and the imported data
}
\description{
A function to generate a Shiny server for handling uploaded files
}
\examples{
if (interactive()) {
  shinyApp(
    ui = fluidPage(
      uploadUI("uploadId")
    ),
    server = function(input, output, session) {
      imported <- uploadServer("uploadId",output)
    }
  )
}
}
