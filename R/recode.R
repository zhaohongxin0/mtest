


# 创建 value_modifier UI 函数
# 创建 value_modifier UI 函数
value_modifier_ui <- function(id, dataset_columns) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("variable"), "选择变量：", choices = dataset_columns),
        uiOutput(ns("value_editors")),
        radioButtons(ns("modify_option"), "修改方式：", choices = c("原变量基础上修改" = "original", "新生成的变量上修改" = "new")),
        uiOutput(ns("new_var_name_input")),
        actionButton(ns("save_changes"), "保存更改"),
        actionButton(ns("reset"), "复位")
      ),
      mainPanel(
        DTOutput(ns("data_table"))
      )
    )
  )
}

# 创建 value_modifier server 函数
value_modifier_server <- function(id, input_dataset_name, output_dataset_name) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 获取输入数据集
    input_dataset <- reactive({
      get(input_dataset_name)
    })

    # 创建一个原始数据集的副本
    original_dataset <- reactiveVal(input_dataset())

    # Generate value editors based on the selected variable
    output$value_editors <- renderUI({
      unique_values <- unique(dataset()[[input$variable]])
      if (length(unique_values) <= 20) {
        lapply(seq_along(unique_values), function(i) {
          textInput(ns(paste0("value_", i)), label = paste0("值 ", i, " (", unique_values[i], ")："), value = unique_values[i])
        })
      } else {
        NULL
      }
    })

    # Dynamically generate the new_var_name input with the default value
    output$new_var_name_input <- renderUI({
      if (input$modify_option == "new") {
        textInput(ns("new_var_name"), "新变量名称：", value = paste0(input$variable, "_f"))
      } else {
        NULL
      }
    })

    observeEvent(input$save_changes, {
      new_data <- dataset()
      unique_values <- unique(new_data[[input$variable]])
      if (length(unique_values) <= 20) {
        new_values_list <- lapply(seq_along(unique_values), function(i) {
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
        output_dataset(new_data)  # 更新输出数据集
        output_dataset_name <- quo_name(enquo(output_dataset))
        assign(output_dataset_name, new_data, envir = parent.frame())  # 更新输出数据集
      }
    })

    # Reset dataset to the initial state
    observeEvent(input$reset, {
      dataset(original_dataset())  # 重置为原始数据集
      output_dataset(original_dataset()) # 重置输出数据集
      output_dataset_name <- quo_name(enquo(output_dataset))
      assign(output_dataset_name, original_dataset(), envir = parent.frame())  # 更新输出数据集
    })

    # Display the dataset using DT package
    output$data_table <- renderDT({
      datatable(
        dataset(),
        options = list(
          language = list(url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Chinese.json"),
          scrollX = TRUE
        )
      )
    })

  })
}
