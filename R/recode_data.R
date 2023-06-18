#' @title Recode Variables UI
#' @description This function generates a Shiny UI module for recoding categorical variables.
#' @param id A string, the namespace of the module.
#' @return Returns a shiny UI object that can be used in UI assembly.
#' @export
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   ui <- fluidPage(
#'     mtest::recodeUI("recodeId")
#'   )
#'   server <- function(input, output, session) { }
#'   shinyApp(ui, server)
#' }
recodeUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$b( "对分类变量数据标签重新编码（可对20个水平以下的变量的值进行批量修改，例如 0, 1 修改成 男、女），如果
          不做重编码，请直接跳过，到下一个页面。"),br(),br(),
    "一些传统软件本身有缺陷，比如做 Logistic 回归，做 ROC 曲线，要求应变量必须以0和1来表示，否则
  就没法进行，这就让广大用户养成了用数字来代替原本的文本的习惯，但这样很不好，因为做出来
  的统计表格显示的是数字，达不到发表要求，还要手工改表格内容。",br(),br(),
  "而 MSTATA 的任何分析都不需要用数字去代替文本，大家准备数据的时候，直接用男、女，用有效、无效
  来准备数据就可以了，不要用 0,1 或者 1,2,3 等无意义的数字，这样第一个好处是可以直接生成发表
  级别的表格，第二有利于下一步人工智能根据表格生成论文，只有表格内是有意义的文本，人工智能
  才能读懂从而生成正确的论文。",br(),br(),
  "由于多年养成的习惯，导致EDC软件产生的数据库都用数字来代替文本，我们用户手里的数据库无奈都是些数字，
  那就请利用这个页面，批量把数字转换回文本，一次可以连续替换多个变量，非常方便：",br(),br(),
  tags$hr(style="border-color: grey;"),
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("code_ui"))
      ),
      mainPanel(
        DTOutput(ns("data_table"))
      )
    )
  )
}

#' @title Recode Variables Server
#' @description This function generates a Shiny server module for recoding categorical variables.
#' @param input, output, session The input, output, and session objects of the Shiny server.
#' @param id A string, the namespace of the module.
#' @param data A reactive, the data to be recoded.
#' @return Returns a reactive expression that expresses the recoded data.
#' @export
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   ui <- fluidPage(
#'     mtest::recodeUI("recodeId")
#'   )
#'   server <- function(input, output, session) {
#'     data <- reactive({ data.frame() })
#'     recoded <- mtest::recodeServer(input, output, session, "recodeId", data)
#'   }
#'   shinyApp(ui, server)
#' }
recodeServer <- function(input, output, session, id, data) {
  ns <- NS(id)
  dataset <- reactiveVal()
  observeEvent(data(),{
    dataset(data())
  })
  output[[ns("code_ui")]] <- renderUI({
    req(dataset())  # 确保数据集已加载

    # 选择不重复值个数<=20的变量
    valid_columns <- colnames(dataset())[sapply(dataset(), function(x) length(unique(x[!is.na(x)])) <= 20)]

    tagList(
      selectInput("variable", "选择变量：", choices = valid_columns),
      uiOutput("value_editors"),
      radioButtons("modify_option", "修改方式：", choices = c("原变量基础上修改" = "original", "新生成的变量上修改" = "new"),
                   selected = "new"),
      uiOutput("new_var_name_input"),
      actionButton("save_changes", "保存更改"),
      actionButton("reset", "复位")
    )
  })

  # Generate value editors based on the selected variable
  output$value_editors <- renderUI({
    req(dataset())  # 确保数据集已加载
    unique_values <- unique(dataset()[[input$variable]])
    unique_values <- unique_values[!is.na(unique_values)]  # 排除缺失值
    if (length(unique_values) <= 20) {
      lapply(seq_along(unique_values), function(i) {
        textInput(paste0("value_", i), label = paste0("值 ", i, " (", unique_values[i], ")："), value = unique_values[i])
      })
    } else {
      NULL
    }
  })

  # Dynamically generate the new_var_name input with the default value
  output$new_var_name_input <- renderUI({
    if (input$modify_option == "new") {
      textInput("new_var_name", "新变量名称：", value = paste0(input$variable, "_f"))
    } else {
      NULL
    }
  })

  # Update values and save changes
  observeEvent(input$save_changes, {
    req(dataset())  # 确保数据集已加载
    new_data <- dataset()
    unique_values <- unique(new_data[[input$variable]])
    unique_values <- unique_values[!is.na(unique_values)]  # 排除缺失值
    if (length(unique_values) <= 20) {
      new_values_list <- lapply(seq_along(unique_values),
                                function(i) {
                                  input[[paste0("value_", i)]]
                                })
      value_mapper <- setNames(new_values_list, unique_values)
      if (input$modify_option == "new") {
        new_var_name <- input$new_var_name
        if (new_var_name != "") {
          new_data <- new_data %>%
            mutate(!!new_var_name := new_data[[input$variable]]) %>%
            relocate(!!sym(new_var_name), .after = !!sym(input$variable))
          new_data[[new_var_name]] <- as.factor(new_data[[new_var_name]])
          new_data[[new_var_name]] <- recode(new_data[[new_var_name]], !!!value_mapper)
        }
      } else {
        new_data[[input$variable]] <- as.factor(new_data[[input$variable]])
        new_data[[input$variable]] <- recode(new_data[[input$variable]], !!!value_mapper)
      }
      dataset(new_data)
    }
  })

  # Reset dataset to the initial state
  observeEvent(input$reset, {
    req(data()) # 确保文件已上传

    dataset(data())
  })

  # Display the dataset using DT package
  output[[ns("data_table")]] <- DT::renderDataTable({
    req(dataset()) # 确保数据集已加载
    mtest::downloadDT(dataset(),"数据如下")},server = FALSE)
  return(dataset)
}
