#' 修改变量的UI
#'
#' @title Modify Variables UI
#' @description This function generates a Shiny UI module that allows the user to modify variables in a dataset.
#' @param id A string, the namespace of the module.
#' @return Returns a shiny UI object that can be used in UI assembly.
#' @export
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   ui <- fluidPage(
#'     mtest::modifyUI("vars")
#'   )
#'   server <- function(input, output, session) { }
#'   shinyApp(ui, server)
#' }
modifyUI <- function(id) {
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        width = 6,
        tags$b("此页面可以选择要分析的变量、修改变量名称、设置连续性变量和分类变量"),br(),br(),
        "1. 请勾选需要下一步做分析的变量，如不清楚请全选",br(),br(),
        "2. 变量可以改名，改成统计表中最后展现的名称，如字段名 age，
               修改成首字母大写 Age 等等。变量名支持中文，但不能包含英文冒号: 否则会出错，如果需要特殊
  符号，尽量用下划线_和英文句号.",br(),br(),
  "3. 最重要的事情：请设置连续性变量和分类变量，请把连续性变量设置为 numeric，分类变量设置为 factor；
  数值型变量系统默认设置为 numeric 或 integer，文本型的变量系统已经默认设置成 factor，就不用变了。这里唯一需要您手工调整的，是有一些数值型变量，其实代表的是分类
  变量，例如您原始数据用 1，2，3，4 代表“工人”，“农民”，“知识分子”，“干部”，则要手工改成 factor。这个
  是整个数据统计的核心思想，numeric 和 factor 在后面统计模型中的处理是不一样的，一定要非常重视。",br(),br(),
  tags$b("4. 改好后点击最下方的“应用更改”按钮，在这个页面，哪怕你没有作任何修改，也要点击此按钮才能下一步"),br(),br(),

  update_variables_ui(id),tags$br(), tags$br(),
      ),

  mainPanel(
    width = 6,
    tabBox(
      width = "100%",
      tabPanel("原始数据概览",
               verbatimTextOutput("orig_Summary")),
      tabPanel("修改后数据概览",
               verbatimTextOutput("updated_Summary"))
    )
  )
    )
  )

}

#' 修改变量的Server端
#'
#' @title Modify Variables Server
#' @description This function generates a Shiny server module that allows the user to modify variables in a dataset.
#' @param input, output, session The input, output, and session objects of the Shiny server.
#' @param id A string, the namespace of the module.
#' @param importData A dataframe, the data to be modified.
#' @return Returns a reactive expression that expresses the updated data.
#' @export
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   ui <- fluidPage(
#'     mtest::modifyUI("vars")
#'   )
#'   server <- function(input, output, session) {
#'     imported_data <- mtest::uploadServer(input, output, session, "uploadId")
#'     modified <- mtest::modifyServer(input, output, session, "vars", imported_data)
#'   }
#'   shinyApp(ui, server)
#' }
modifyServer <- function(input, output, session, id, importData){
  # 将所有缺失值写法替换成NA,把数据库中所有的冒号:替换成.以免出错
  NA_displace<-reactive({
    df<-importData()
    df[] <- lapply(df, function(x) {
      if(is.character(x) || is.factor(x)){
        x <- as.character(x)  # 确保数据是字符型的
        x <- gsub(":", ".", x)  # 用"."替换":"
      }
      x  # 返回结果
    })
    df[] <- lapply(df, function(x) {
      x[x %in% c("NA", "na", "n/a", "N/A", "null", "", "NaN",
                 "#N/A", "#NA", "#N/A N/A", "#NULL!",
                 # "inf", "-inf",
                 ".", "...", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_")] <- NA
      return(x)
    })

    colnames(df) <- gsub(":", ".", colnames(df))

    return(df)
  })


  # 进行变量修改----
  # 先把所有character转换成factor
  DF <- reactive({
    tmp<-data.frame(unclass(NA_displace()),stringsAsFactors=TRUE)
    setNames(tmp,names(NA_displace()))
  })


  updated_data <- update_variables_server(
    id = id,
    data = reactive(DF()),
    height = 600
  )

  output$orig_Summary <- renderText({
    req(DF())
    res <- capture.output(summarytools::dfSummary(DF()))
    res <- paste(res, collapse = "\n")
    return(res)
  })

  output$updated_Summary <- renderText({
    req(updated_data())
    res <- capture.output(summarytools::dfSummary(updated_data()))
    res <- paste(res, collapse = "\n")
    return(res)
  })

  return(updated_data)
}
