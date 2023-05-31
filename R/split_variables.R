#' Split Variables Function
#'
#' This function splits the provided variable names into two categories: numeric and non-numeric.
#'
#' @param data A data frame in which to look for the variables.
#' @param variables A character vector of variable names.
#' @return A list containing two character vectors of variable names: `covs` for numeric variables and `factors` for non-numeric variables.
#' @examples
#' \dontrun{
#' data <- data.frame(A = rnorm(100), B = letters[1:100], C = rnorm(100), D = letters[1:100])
#' vars <- c("A", "B", "C", "D")
#' result <- split_variables(data, vars)
#' numeric_vars <- result$covs
#' non_numeric_vars <- result$factors
#' print(numeric_vars)
#' print(non_numeric_vars)
#' }
#' @importFrom dplyr select_if
#' @export

split_variables <- function(data, variables) {
  # 获取数据集中的相关列
  selected_columns <- data[, variables, drop = FALSE]

  # 使用dplyr的select_if()函数和is.numeric判断将numeric和非numeric的变量分开
  numeric_vars <- select_if(selected_columns, is.numeric) %>% colnames()
  non_numeric_vars <- select_if(selected_columns, Negate(is.numeric)) %>% colnames()

  # 创建一个列表分别包含numeric和非numeric变量的向量
  list(covs = numeric_vars, factors = non_numeric_vars)
}
