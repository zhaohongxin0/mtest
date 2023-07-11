#' @title Upload UI for Shiny
#'
#' @description A function to generate a Shiny UI for uploading files
#'
#' @param id A character string for the id of the UI elements
#'
#' @return A shiny UI definition for file upload
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   shinyApp(
#'     ui = fluidPage(
#'       uploadUI("uploadId","example.csv")
#'     ),
#'     server = function(input, output, session) {
#'       imported <- uploadServer("uploadId")
#'     }
#'   )
#' }
uploadUI<- function(id,example_data) {
  tagList(
    sidebarLayout(
      sidebarPanel(
        tags$b('请点击下方的浏览按钮上传您的数据，上传完成后点击最下方的“导入数据”按钮，可上传文件大小限制为5M，有更大需求请登录 VIP 版。'),br(),br(),
        a(href=example_data, "下载 Excel 样例数据（右击另存为）", download=NA, target="_blank"),br(),br(),
        "注意：请用微软的 Excel 软件来准备数据，不要上传 WPS 编辑过的数据，会有问题。如果数据有中文，请在
        Excel 中另存为 csv 时选择 UTF-8。",br(),br(),
        "缺失数据建议用空白表示，如果您的缺失数据是用文本表示的，例如 NA，则最好存为
        CSV 文件，而不是 XLSX 文件，以免导入出错。",br(),br(),
        "CSV 是在不同体系平台中的最佳传播文件，强烈推荐用
        Excel 将文件另存为 CSV（UTF-8）格式再导入",
        width = 4,
        tags$hr(style="border-color: grey;"),
        import_file_ui(id),tags$br(), tags$br(),
      ),
      mainPanel(
        width = 8,
        tags$b("是否上传成功："),
        verbatimTextOutput(outputId = paste0(id, "_status")),
        tags$b("是否导入成功："),
        verbatimTextOutput(outputId = paste0(id, "_name")),
        tags$b("浏览数据，看有没有问题："),
        tags$head(
          tags$style(HTML("
            .dataTables_scrollBody {
              max-height: 500px;
            }
            .dataTables_scrollBody tbody tr {
              height: 20px;
            }
            .dataTables_scrollBody tbody td {
              white-space: nowrap;
              overflow: hidden;
              text-overflow: ellipsis;
            }
          "))
        ),
        DT::dataTableOutput(paste0(id, "_import_data"))
      )
    )
  )
}


#' @title Upload Server for Shiny
#'
#' @description A function to generate a Shiny server for handling uploaded files
#'
#' @param id A character string for the id of the UI elements
#'
#' @return A list of shiny outputs and the imported data
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   shinyApp(
#'     ui = fluidPage(
#'       uploadUI("uploadId")
#'     ),
#'     server = function(input, output, session) {
#'       imported <- uploadServer("uploadId",output)
#'     }
#'   )
#' }
uploadServer<- function(input, output, session, id) {
  # 3.1 上传数据----
  imported <- import_file_server(id)

  output[[paste0(id, "_status")]] <- renderPrint({
    imported$status()
  })
  output[[paste0(id, "_name")]] <- renderPrint({
    imported$name()
  })

  output[[paste0(id, "_import_data")]] <- DT::renderDataTable({
    req(imported$data()) # Ensure the dataset is loaded
    mtest::downloadDT(imported$data(), "")}, server = T)


  return(imported$data)
}


