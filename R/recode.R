# 数据集修改模块 UI
recode_UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("open_modal"), "打开数据集修改器")
  )
}

# 数据集修改模块的服务器端逻辑
recode_server <- function(input, output, session, dataset_name) {
  # Add the following code at the beginning of the server function
  observeEvent(input$open_modal, {
    showModal(
      modalDialog(
        title = "修改数据集",
        sidebarLayout(
          sidebarPanel(
            selectInput(session$ns("variable"), "选择变量：", choices = NULL),
            uiOutput(session$ns("value_editors")),
            radioButtons(session$ns("modify_option"), "修改方式：", choices = c("原变量基础上修改" = "original", "新生成的变量上修改" = "new")),
            conditionalPanel(
              condition = "input.modify_option == 'new'",
              textInput(session$ns("new_var_name"), "新变量名称：", value = "")
            ),
            actionButton(session$ns("save_changes"), "保存更改"),
            actionButton(session$ns("reset"), "复位")
          ),
          mainPanel(
            DTOutput(session$ns("data_table"))
          )
        ),
        size = "l",
        # Add an id to the modal
        id = session$ns("modal")
      )
    )
  })



  # Reactive expression for the dataset
  dataset <- reactiveVal(dataset_name)
  updated_dataset <- reactiveVal(NULL)

  # Update the variable selection input
  observe({
    updateSelectInput(session, "variable", choices = colnames(dataset()))
  })

  # Reactive expression for the dataset
  dataset <- reactiveVal(persons)

  # Generate value editors based on the selected variable
  output$value_editors <- renderUI({
    unique_values <- unique(dataset()[[input$variable]])
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
    }
  })

  # Reset dataset to the initial state
  observeEvent(input$reset, {
    dataset(persons)
  })

  # Display the dataset using DT package
  output$data_table <- DT::renderDataTable({
    mtest::downloadDT(dataset(),"当前数据集如下：")},server = FALSE)


  # Return the updated dataset
  return(updated_dataset)

  # Add the following code at the end of the server function
  observeEvent(input$reset, {
    removeModal(session$ns("modal"))
  })

}
