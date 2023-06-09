% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modify_data.R
\name{modifyUI}
\alias{modifyUI}
\title{Modify Variables UI}
\usage{
modifyUI(id)
}
\arguments{
\item{id}{A string, the namespace of the module.}
}
\value{
Returns a shiny UI object that can be used in UI assembly.
}
\description{
This function generates a Shiny UI module that allows the user to modify variables in a dataset.
}
\details{
修改变量的UI
}
\examples{
if (interactive()) {
  library(shiny)
  ui <- fluidPage(
    mtest::modifyUI("vars")
  )
  server <- function(input, output, session) { }
  shinyApp(ui, server)
}
}
