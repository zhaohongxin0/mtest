#' Calculate mode
#'
#' @param x A numeric vector, factor or other object for which the mode will be calculated
#'
#' @return mode_val The mode of the input vector
#'
#' @examples
#' custom_mode(c(1,2,2,3,4,5,5,5)) # Returns: 5
#' @export
custom_mode <- function(x) {
  tbl <- table(x)
  mode_val <- names(tbl[tbl == max(tbl)])
  if (is.numeric(x)) {
    mode_val <- as.numeric(mode_val)
  } else if (is.factor(x)) {
    mode_val <- as.factor(mode_val)
  }
  return(mode_val)
}

#' Impute values based on statistical measures
#'
#' @param data A data frame
#' @param cat_vars A vector of categorical variables in the data
#' @param num_vars A vector of numerical variables in the data
#' @param cat_fill A character string specifying the fill method for categorical variables
#' @param num_fill A character string specifying the fill method for numerical variables
#' @param inplace A logical value indicating whether to replace the original variables in the data
#' @param suffix A character string to append to the new variables
#'
#' @return data The data frame with imputed values
#'
#' @examples
#' # Prepare data
#' data <- data.frame(cat = factor(c("A", "B", "A", NA)), num = c(1, 2, 3, NA))
#' # Impute values
#' impute_statistical(data, "cat", "num", "众数填充", "均值填充", TRUE, "_imputed")
#' @export
impute_statistical <- function(data, cat_vars, num_vars, cat_fill, num_fill, inplace, suffix) {
  for (var in cat_vars) {
    if (cat_fill == "众数填充") {
      if (inplace) {
        data[[var]] <- as.factor(replace_na(data[[var]], mtest::custom_mode(na.omit(data[[var]]))))
      } else {
        new_var <- paste0(var, suffix)
        data <- data %>% mutate(!!sym(new_var) := as.factor(replace_na(data[[var]], mtest::custom_mode(na.omit(data[[var]]))))) %>%
          relocate(!!sym(new_var), .after = var)
      }
    } else if (cat_fill == "将缺失值单独作为一个分类Unknown") {
      if (inplace) {
        data[[var]] <- forcats::fct_explicit_na(data[[var]], na_level = "Unknown")
      } else {
        new_var <- paste0(var, suffix)
        data <- data %>% mutate(!!sym(new_var) := forcats::fct_explicit_na(data[[var]], na_level = "Unknown")) %>%
          relocate(!!sym(new_var), .after = var)
      }
    }
  }


  for (var in num_vars) {
    if (num_fill == "均值填充") {
      if (inplace) {
        data[[var]] <- replace_na(data[[var]], mean(data[[var]], na.rm = TRUE))
      } else {
        new_var <- paste0(var, suffix)
        data <- data %>% mutate(!!sym(new_var) := replace_na(data[[var]], mean(data[[var]], na.rm = TRUE))) %>%
          relocate(!!sym(new_var), .after = var)
      }
    } else if (num_fill == "中位数填充") {
      if (inplace) {
        data[[var]] <- replace_na(data[[var]], median(data[[var]], na.rm = TRUE))
      } else {
        new_var <- paste0(var, suffix)
        data <- data %>% mutate(!!sym(new_var) := replace_na(data[[var]], median(data[[var]], na.rm = TRUE))) %>%
          relocate(!!sym(new_var), .after = var)
      }
    } else if (num_fill == "众数填充") {
      if (inplace) {
        data[[var]] <- replace_na(data[[var]], mode(data[[var]]))
      } else {
        new_var <- paste0(var, suffix)
        data <- data %>% mutate(!!sym(new_var) := replace_na(data[[var]], mode(data[[var]]))) %>%
          relocate(!!sym(new_var), .after = var)
      }
    } else if (num_fill == "三倍标准差填充") {
      if (inplace) {
        data[[var]] <- replace_na(data[[var]], mean(data[[var]], na.rm = TRUE) + 3 * sd(data[[var]], na.rm = TRUE))
      } else {
        new_var <- paste0(var, suffix)
        data <- data %>% mutate(!!sym(new_var) := replace_na(data[[var]], mean(data[[var]], na.rm = TRUE) + 3 * sd(data[[var]], na.rm = TRUE))) %>%
          relocate(!!sym(new_var), .after = var)
      }
    } else if (num_fill == "负三倍标准差填充") {
      if (inplace) {
        data[[var]] <- replace_na(data[[var]], mean(data[[var]], na.rm = TRUE) - 3 * sd(data[[var]], na.rm = TRUE))
      } else {
        new_var <- paste0(var, suffix)
        data <- data %>% mutate(!!sym(new_var) := replace_na(data[[var]], mean(data[[var]], na.rm = TRUE) - 3 * sd(data[[var]], na.rm = TRUE))) %>%
          relocate(!!sym(new_var), .after = var)
      }
    }
  }

  return(data)
}

#' Impute values based on rules
#'
#' @param data A data frame
#' @param cat_vars A vector of categorical variables in the data
#' @param num_vars A vector of numerical variables in the data
#' @param cat_fill A character string specifying the fill method for categorical variables
#' @param num_fill A character string specifying the fill method for numerical variables
#' @param fixed_value_num A numeric value used to fill NA values in numerical variables
#' @param fixed_value_cat A character value used to fill NA values in categorical variables
#' @param inplace A logical value indicating whether to replace the original variables in the data
#' @param suffix A character string to append to the new variables
#'
#' @return data The data frame with imputed values
#'
#' @examples
#' # Prepare data
#' data <- data.frame(cat = factor(c("A", "B", "A", NA)), num = c(1, 2, 3, NA))
#' # Impute values
#' impute_rules(data, "cat", "num", "纵向缺失值用上一个值替换", "纵向缺失值用上一个值替换", 999, "Unknown", TRUE, "_imputed")
#' @export
impute_rules <- function(data, cat_vars, num_vars, cat_fill, num_fill, fixed_value_num, fixed_value_cat, inplace, suffix) {
  for (var in cat_vars) {
    new_var <- paste0(var, suffix)
    if (cat_fill == "纵向缺失值用上一个值替换") {
      if (inplace) {
        data[[var]] <- na.locf(data[[var]], na.rm = FALSE)
      } else {
        data <- data %>% mutate(!!sym(new_var) := na.locf(data[[var]], na.rm = FALSE)) %>% relocate(!!sym(new_var), .after = var)
      }
    } else if (cat_fill == "纵向缺失值用下一个值替换") {
      if (inplace) {
        data[[var]] <- na.locf(data[[var]], na.rm = FALSE, fromLast = TRUE)
      } else {
        data <- data %>% mutate(!!sym(new_var) := na.locf(data[[var]], na.rm = FALSE, fromLast = TRUE)) %>% relocate(!!sym(new_var), .after = var)
      }
    } else if (cat_fill == "固定值填充") {
      if (inplace) {
        data[[var]] <- forcats::fct_explicit_na(data[[var]], na_level = fixed_value_cat)
        data[[var]] <- replace_na(data[[var]], fixed_value_cat)
      } else {
        data <- data %>% mutate(!!sym(new_var) := forcats::fct_explicit_na(data[[var]], na_level = fixed_value_cat))
        data <- data %>% mutate(!!sym(new_var) := replace_na(data[[new_var]], fixed_value_cat)) %>% relocate(!!sym(new_var), .after = var)
      }
    }
  }

  for (var in num_vars) {
    new_var <- paste0(var, suffix)
    if (num_fill == "纵向缺失值用上一个值替换") {
      if (inplace) {
        data[[var]] <- na.locf(data[[var]], na.rm = FALSE)
      } else {
        data <- data %>% mutate(!!sym(new_var) := na.locf(data[[var]], na.rm = FALSE)) %>% relocate(!!sym(new_var), .after = var)
      }
    } else if (num_fill == "纵向缺失值用下一个值替换") {
      if (inplace) {
        data[[var]] <- na.locf(data[[var]], na.rm = FALSE, fromLast = TRUE)
      } else {
        data <- data %>% mutate(!!sym(new_var) := na.locf(data[[var]], na.rm = FALSE, fromLast = TRUE)) %>% relocate(!!sym(new_var), .after = var)
      }
    } else if (num_fill == "固定值填充") {
      if (inplace) {
        data[[var]] <- replace_na(data[[var]], fixed_value_num)
      } else {
        data <- data %>% mutate(!!sym(new_var) := replace_na(data[[var]], fixed_value_num)) %>% relocate(!!sym(new_var), .after = var)
      }
    }
  }

  return(data)
}

#' Impute values based on model
#'
#' @param data A data frame
#' @param model_fill A character string specifying the model-based method for filling NA values
#' @param inplace A logical value indicating whether to replace the original variables in the data
#' @param suffix A character string to append to the new variables
#'
#' @return data The data frame with imputed values
#'
#' @examples
#' # Prepare data
#' data <- data.frame(cat = factor(c("A", "B", "A", NA)), num = c(1, 2, 3, NA))
#' # Impute values
#' impute_model(data, "KNN填充法", TRUE, "_imputed")
#' @export
impute_model <- function(data, model_fill, inplace, suffix) {
  library(DMwR2)
  library(missForest)

  temp_data <- data

  if (model_fill == "KNN填充法") {
    temp_data <- knnImputation(temp_data)
  } else if (model_fill == "随机森林填充法") {
    temp_data <- missForest(data)$ximp
  }

  if (inplace) {
    return(temp_data)
  }
  else {
    imputed_data <- data
    for (var in colnames(data)) {
      new_var <- paste0(var, suffix)
      imputed_data[[new_var]] <- temp_data[[var]]
    }
    return(imputed_data)

  }
}
