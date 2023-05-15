#' Extract Numeric Factor Variables from a Dataset
#'
#' This function takes a dataset as input and returns a vector containing the names of all factor variables with purely numeric levels.
#'
#' @param data A data frame containing the dataset to be processed.
#'
#' @return A character vector containing the names of factor variables with purely numeric levels.
#' @export
#'
#' @examples
#' data(iris)
#' numeric_factors <- extract_numeric_factors(iris)
#' print(numeric_factors)
extract_numeric_factors <- function(data) {
  # Initialize result vector
  result <- c()

  # Iterate through all variables in the dataset
  for (column_name in colnames(data)) {
    column_data <- data[[column_name]]

    # Check if the current variable is a factor variable
    if (is.factor(column_data)) {
      # Convert factor levels to numeric
      numeric_levels <- suppressWarnings(as.numeric(as.character(levels(column_data))))

      # Check if all levels are numeric
      if (all(!is.na(numeric_levels))) {
        # Add variable name to result vector
        result <- append(result, column_name)
      }
    }
  }

  return(result)
}
