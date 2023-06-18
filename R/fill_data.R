#' Generate UI for data imputation.
#'
#' This function generates a Shiny UI for the data imputation task. It allows
#' the user to input various parameters for imputation and view the imputed data.
#'
#' @return A shiny fluidPage object that represents the UI of the data imputation task.
#'
#' @examples
#' # This function is designed to be used in a Shiny app and thus cannot be run directly.
#' # Below is an example of how to use it in a Shiny app.
#' #
#' # ui <- shiny::fluidPage(
#' #   mtest::fillUI()
#' # )
#' @export

fillUI <- function() {

fluidPage(
  tags$b("如果您的数据有缺失值，可以在这个页面填补，如果不做缺失值填补，请直接跳过，到下一个页面。"),br(),br(),

  "通常缺失值超过40%的变量，不宜再进行填补，
         可以直接舍弃；请阅读缺失值填补方面的教程后再操作，本页面仅做参数填补和模型填补，如果要
         做多重填补（MI），则需要在统计建模过程进行，不在本页面。",br(),br(),
  tags$hr(style="border-color: grey;"),
  sidebarLayout(
    sidebarPanel(
      tags$b("请选择变量进行缺失值填补，请注意，如果是普通填补，请仅选择需要填补的变量，而
             如果选择模型填补（如KNN、随机森林等），默认对整个数据集进行填补，推荐生成新的变量进行填补，
             分析的时候酌情选用填补后的变量，如果缺失值超过40%的变量，尽管进行了填补，也不宜在后续分析使用。"),br(),br(),
      uiOutput("fill_type"),
      uiOutput("impute"),
      tags$br(),tags$br()
    ),
    mainPanel(
      # style="overflow-x:scroll",

      tags$head(
        tags$style(HTML("
      .scrollable-table {
        width: 100%;
        overflow-x: auto;
      }
    "))
      ),
    h4(tags$b("填补结果如下：")),br(),
    DTOutput("imputed_data"),br(),br(),
    verbatimTextOutput("imputed_Summary")

    )
  )
)
}



#' Perform server-side operations for data imputation.
#'
#' This function performs server-side operations for the data imputation task in Shiny.
#' It processes the user inputs from the UI, performs data imputation, and renders
#' the output for the user.
#'
#' @param input A list of input values from the Shiny UI.
#' @param output A list of output values to be displayed on the Shiny UI.
#' @param session The Shiny session object.
#' @param recoded The dataset to be imputed.
#'
#' @return A reactive expression representing the imputed dataset.
#'
#' @examples
#' # This function is designed to be used in a Shiny app and thus cannot be run directly.
#' # Below is an example of how to use it in a Shiny app.
#' #
#' # shiny::shinyServer(function(input, output, session) {
#' #   imported_data <- mtest::uploadServer(input, output, session, "uploadId")
#' #   modified <- mtest::modifyServer(input, output, session, "vars", imported_data)
#' #   recoded <- mtest::recodeServer(input, output, session, "recodeId", modified)
#' #   imputed_data <- mtest::fillServer(input, output, session, recoded)
#' # })
#' @export


fillServer <- function(input, output, session, recoded) {

for_impute<-reactive({
  req(recoded())

  tmp <- recoded()

  tmp<-tmp%>%mutate(across(where(~n_distinct(.,na.rm =TRUE)<3),factor))
  # 规范化变量名称
  # tmp<-tmp%>%dplyr::rename_with(make.names)
  # labelled::var_label(tmp)<-names(res_filter$filtered())

  # 自定义一个函数用于检查Date类型
  is_date <- function(x) {
    inherits(x, "Date")
  }

  # 自定义一个函数用于检查POSIXct类型
  is_posixct <- function(x) {
    inherits(x, "POSIXct")
  }

  # 只保留numeric和factor两种属性
  tmp<-tmp%>%
    mutate(across(where(is.numeric), as.numeric)) %>%
    mutate(across(where(is.integer), as.numeric)) %>%
    mutate(across(where(is_date), as.numeric)) %>%
    mutate(across(where(is_posixct), as.numeric)) %>%
    mutate(across(where(is.factor), as.factor)) %>%
    mutate(across(where(is.character), as.factor))

  return (tmp)

})


output$fill_type <- renderUI({
  req(for_impute())
  radioButtons("fill_type", "选择填充类型", choices = c("普通填充", "模型填充"))
})



output$impute <- renderUI({
  req(for_impute())

  num_vars <- for_impute() %>% select_if(is.numeric) %>%
    select(where(~n_distinct(., na.rm = TRUE) > 2)) %>% variable.names()

  cat_vars <- for_impute() %>% select_if(purrr::negate(is.numeric)) %>% variable.names()

  tagList(


    conditionalPanel(
      condition = "input.fill_type == '普通填充'",
      selectInput(
        inputId = "num_vars",
        label = "请选择要填充的连续性变量:",
        choices = num_vars,
        multiple = TRUE
      ),
      selectInput(
        inputId = "cat_vars",
        label = "请选择要填充的分类变量:",
        choices = cat_vars,
        multiple = TRUE
      ),

      radioButtons("fill_method", "选择统计量填充还是规则填充", choices = c("统计量填充", "规则填充")),
      conditionalPanel(
        condition = "input.fill_method == '统计量填充'",
        selectInput("num_fill_method", "连续性变量填充方法", choices = c("均值填充", "中位数填充", "众数填充", "三倍标准差填充", "负三倍标准差填充")),
        selectInput("cat_fill_method", "分类变量填充方法", choices = c("众数填充", "将缺失值单独作为一个分类Unknown"))
      ),
      conditionalPanel(
        condition = "input.fill_method == '规则填充'",
        selectInput("num_fill_rule", "连续性变量填充方法", choices = c("纵向缺失值用上一个值替换", "纵向缺失值用下一个值替换", "固定值填充")),
        conditionalPanel(
          condition = "input.num_fill_rule == '固定值填充'",
          numericInput("fixed_value_num", "输入连续性变量固定值", value = 0, min = -Inf, max = Inf)
        ),
        selectInput("cat_fill_rule", "分类变量填充方法", choices = c("纵向缺失值用上一个值替换", "纵向缺失值用下一个值替换", "固定值填充")),

        conditionalPanel(
          condition = "input.cat_fill_rule == '固定值填充'",
          textInput("fixed_value_cat", "输入分类变量固定值", value = "")
        )
      )

    ),
    conditionalPanel(
      condition = "input.fill_type == '模型填充'",
      selectInput("model_fill", "选择填充模型种类", choices = c("KNN填充法", "随机森林填充法"))
    ),
    radioButtons("inplace", "选择在原变量上填充还是新生成一个变量填充",
                 choices = c("原变量上填充" = TRUE, "新生成变量填充" = FALSE),
                 selected = FALSE),
    conditionalPanel(
      condition = "input.inplace == 'FALSE'",
      textInput("suffix", "输入新变量后缀", value = "_imputed")
    ),

    actionButton("start_fill", "开始填充"),
    actionButton("reset_fill", "复位")

  )
})


imputed_data <- reactiveVal()
observeEvent(for_impute(),{
  imputed_data(for_impute())
})

observeEvent(input$start_fill,{

  req(imputed_data())
  isolate({

    withProgress(message = "开始填充，请耐心等待 ...", {
      incProgress(0.4)
      if (input$fill_type == '普通填充') {
        if (input$fill_method == '统计量填充') {
          tmp <-  mtest::impute_statistical(
            data = imputed_data(),
            cat_vars = input$cat_vars,
            num_vars = input$num_vars,
            cat_fill = input$cat_fill_method,
            num_fill = input$num_fill_method,
            inplace = input$inplace,
            suffix = input$suffix
          )
        } else if (input$fill_method == '规则填充') {
          tmp <- mtest::impute_rules(
            data = imputed_data(),
            cat_vars = input$cat_vars,
            num_vars = input$num_vars,
            cat_fill = input$cat_fill_rule,
            num_fill = input$num_fill_rule,
            fixed_value_num = input$fixed_value_num,
            fixed_value_cat = input$fixed_value_cat,
            inplace = input$inplace,
            suffix = input$suffix
          )
        }
      }
      else if (input$fill_type == '模型填充') {
        tmp <-mtest::impute_model(
          data = imputed_data(),
          model_fill = input$model_fill,
          inplace = input$inplace,
          suffix = input$suffix
        )
      }

      imputed_data (tmp)
    })
  })
})


# Reset dataset to the initial state
observeEvent(input$reset_fill, {
  req(for_impute()) # 确保文件已上传

  imputed_data(for_impute())
})



# 显示数据----
output$imputed_data <- DT::renderDataTable({
  req(imputed_data()) # 确保数据集已加载
  mtest::downloadDT(imputed_data(),"数据如下")},server = FALSE)

output$imputed_Summary <- renderText({
  req(imputed_data())
  res <- capture.output(summarytools::dfSummary(imputed_data(),graph.col=F))
  res <- paste(res, collapse = "\n")
  return(res)
})

return(imputed_data)
}
