#' User interface for data filtering
#'
#' @return A fluidPage object that provides an interactive data filtering interface
#'
#' @examples
#' # Run the filterUI function to generate a data filtering interface
#' ui_missing <- filterUI()
#' @export


filterUI <- function() {
fluidPage(
  tags$b('如果您的数据还没有完全准备好，这里提供了一个工具让您进行患者的筛选，以排除一些异常数据的患者，或者只选一些亚组人群来进行整个分析。如果您不需要再筛选患者，则跳过这一步，直接进入下一页面。'),br(),br(),
  tags$hr(style="border-color: grey;"),


  fluidRow(
    column(
      width = 3,
      filter_data_ui("filtering", max_height = "500px")
    ),
    column(
      width = 9,
      progressBar(
        id = "pbar", value = 100,
        total = 100, display_pct = TRUE
      ),
      DTOutput("data_filtered"),
      tags$b("患者筛选的 R 代码:"),
      verbatimTextOutput(outputId = "code_dplyr"),
      tags$b("条件表达式:"),
      verbatimTextOutput(outputId = "code"),
      tags$b("筛选后的数据概览:"),
      verbatimTextOutput("filtered_summary")
    )
  )
)
}

#' Server-side function for data filtering
#'
#' @param input Input parameters from Shiny
#' @param output Output parameters to Shiny
#' @param session Shiny session information
#' @param imputed_data A reactive expression for the data that should be filtered
#'
#' @return data_filtered A reactive expression for the filtered data
#'
#' @examples
#' # Example usage within a Shiny server function
#' server <- function(input, output, session) {
#'   # The following are placeholders and should be replaced with your own data/variables
#'   imported_data <- mtest::uploadServer(input, output, session, "uploadId")
#'   modified <- mtest::modifyServer(input, output, session, "vars", imported_data)
#'   recoded <- mtest::recodeServer(input, output, session, "recodeId", modified)
#'   imputed_data <- mtest::fillServer(input, output, session, recoded)
#'   # Use the filterServer function with the imputed data
#'   filtered_data <- mtest::filterServer (input, output, session, imputed_data)
#' }
#' @export


  filterServer <- function(input, output, session, imputed_data) {

  res_filter <- filter_data_server(
    id = "filtering",
    data =reactive(imputed_data()),
    # name = reactive(input$dataset),
    # vars = vars,
    # defaults = defaults,
    widget_char ="select",
    widget_num = "slider",
    widget_date = "slider",
    label_na = "缺失值"
  )



  # 把字段名称更改规范,并用改名前的字段名作为新数据集的vairable label
  data_filtered<-reactive({
    tmp<-res_filter$filtered() %>%
      #   dplyr::rename_with(make.names)
      # labelled::var_label(tmp)<-names(res_filter$filtered())
      return (tmp)
  })


  observeEvent(data_filtered(), {
    updateProgressBar(
      session = session, id = "pbar",
      value = nrow(data_filtered()), total = nrow(imputed_data())
    )
  })

  # output$table <- reactable::renderReactable({
  #   reactable::reactable(data_filtered())
  # })


  output$data_filtered <- DT::renderDataTable({
    req(data_filtered()) # Ensure the dataset is loaded
    mtest::downloadDT(data_filtered(), "筛选后的数据如下")}, server = FALSE)


  output$code_dplyr <- renderPrint({
    res_filter$code()
  })
  output$code <- renderPrint({
    res_filter$expr()
  })


  output$filtered_summary <- renderText({
    req(data_filtered())
    res <- capture.output(summarytools::dfSummary(data_filtered(),graph.col=F))
    res <- paste(res, collapse = "\n")
    return(res)
  })

  return(data_filtered)

  }
