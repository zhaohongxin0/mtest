#' Lasso variable selection
#'
#' This function uses lasso regression to perform variable selection.
#'
#' @param df A data frame.
#' @param response_var A string, the name of the response variable in the data frame.
#' @param predictor_vars A vector of strings, the names of the predictor variables in the data frame.
#' @param family A string, the model family to be used in the lasso regression. Default is "gaussian".
#' @param lambda_selection A string, the criterion to select the best lambda value. Can be either "lambda.1se" or "lambda.min". Default is "lambda.1se".
#' @param alpha A numeric value, the alpha parameter for the lasso regression. Default is 1.
#' @param nlambda A numeric value, the number of lambda values to be used in the lasso regression. Default is 100.
#' @param nfolds A numeric value, the number of folds to be used in the cross-validation. Default is 10.
#'
#' @return A list with the following components:
#' \itemize{
#'  \item "coef_table": A data frame with the coefficient table from the lasso regression.
#'  \item "non_zero_vars": A vector of strings, the names of the predictor variables with non-zero coefficients.
#'  \item "cv_plot": A plot object, the cross-validation plot from the lasso regression.
#'  \item "lasso_plot": A plot object, the lasso path plot from the lasso regression.
#' }
#'
#' @examples
#' # Use mtcars dataset as an example
#' df <- mtcars
#' response_var <- "mpg"
#' predictor_vars <- setdiff(names(df), response_var)
#'
#' # Run lasso variable selection
#' result <- lasso_variable_selection(df, response_var, predictor_vars)
#'
#' # Print results
#' print(result$coef_table)
#' print(result$non_zero_vars)
#'
#' # Show plots
#' replayPlot(result$cv_plot)
#' replayPlot(result$lasso_plot)
#'
#' @importFrom glmnet cv.glmnet
#' @export
lasso_variable_selection <- function(df, response_var, predictor_vars, family = "gaussian", lambda_selection = "lambda.1se", alpha = 1, nlambda = 100, nfolds = 10) {
  # 删除含有缺失值的行
  df <- df[complete.cases(df[, c(response_var, predictor_vars)]), ]

  # 提取响应变量和预测变量
  y <- df[, response_var]

  df_matrix <- df
  colnames(df_matrix)<-paste0(colnames(df_matrix),"_level_")
  x <- model.matrix(as.formula(paste("~",paste(
    paste0(predictor_vars,"_level_"),collapse = "+"))),
    data=df_matrix)[,-1]


  # 用lasso回归进行建模
  cv_fit <- glmnet::cv.glmnet(x, y, family = family, alpha = alpha, nlambda = nlambda, nfolds = nfolds)

  # 获取最优lambda值
  if (lambda_selection == "lambda.1se") {
    lambda_best <- cv_fit$lambda.1se
  } else if (lambda_selection == "lambda.min") {
    lambda_best <- cv_fit$lambda.min
  }

  # 提取系数
  coef_matrix <- as.matrix(coef(cv_fit, s = lambda_best))
  coef_df <- as.data.frame(coef_matrix)
  names(coef_df) <- c("Coefficient")
  coef_df$variable <- rownames(coef_matrix)

  # 创建一个函数，用于提取原始分类变量名称
  get_original_var_name <- function(var_name) {
    if (grepl("_level_", var_name)) {
      strsplit(var_name, "_level_")[[1]][1]
    } else {
      var_name
    }
  }

  # 提取非零系数的变量，忽略截距项
  non_zero_var_names <- coef_df$variable[coef_df$Coefficient != 0 & coef_df$variable != "(Intercept)"]
  non_zero_vars <- unique(sapply(non_zero_var_names, get_original_var_name))

  # 若没有非零的变量，则返回NULL
  if(length(non_zero_vars) == 0){
    non_zero_vars <- NULL
  }

  # 绘制cv图，并捕获它
  plot(cv_fit)
  cv_plot <- recordPlot()

  # 绘制lasso路径图，并捕获它
  plot(cv_fit$glmnet.fit, xvar = "lambda", label = TRUE)
  abline(v = log(lambda_best), col = "black", lty = 2, lwd = 2)
  lasso_plot <- recordPlot()

  # 生成结果反馈信息
  if (is.null(non_zero_vars)) {
    feedback <- "Lasso回归筛选出来的变量数量为零，请考虑将λ值设为lambda.min试试，或者放弃lasso，不进行变量筛选。"
  } else {
    feedback <- paste("以下变量已经被Lasso回归筛选出来，可以用于下一步的建模分析：", paste(non_zero_vars, collapse = ", "))
  }

  # 返回结果
  return(list("coef_table" = coef_df, "non_zero_vars" = non_zero_vars, "cv_plot" = cv_plot, "lasso_plot" = lasso_plot, "feedback" = feedback))
}





