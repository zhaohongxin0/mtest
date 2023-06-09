% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imputation_functions.R
\name{impute_model}
\alias{impute_model}
\title{Impute values based on model}
\usage{
impute_model(data, model_fill, inplace, suffix)
}
\arguments{
\item{data}{A data frame}

\item{model_fill}{A character string specifying the model-based method for filling NA values}

\item{inplace}{A logical value indicating whether to replace the original variables in the data}

\item{suffix}{A character string to append to the new variables}
}
\value{
data The data frame with imputed values
}
\description{
Impute values based on model
}
\examples{
# Prepare data
data <- data.frame(cat = factor(c("A", "B", "A", NA)), num = c(1, 2, 3, NA))
# Impute values
impute_model(data, "KNN填充法", TRUE, "_imputed")
}
