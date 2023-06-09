% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imputation_functions.R
\name{custom_mode}
\alias{custom_mode}
\title{Calculate mode}
\usage{
custom_mode(x)
}
\arguments{
\item{x}{A numeric vector, factor or other object for which the mode will be calculated}
}
\value{
mode_val The mode of the input vector
}
\description{
Calculate mode
}
\examples{
custom_mode(c(1,2,2,3,4,5,5,5)) # Returns: 5
}
