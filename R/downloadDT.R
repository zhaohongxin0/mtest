#' Render a DT table with download buttons
#'
#' This function renders a DT table with buttons to download the table in various formats, such as CSV and Excel.
#' It also provides options to download the current page or all data.
#'
#' @param dt A data frame or tibble to be rendered as a DT table
#' @param title A string representing the title of the table, which will be displayed as the table caption
#'
#' @return A DT table with download buttons
#' @examples
#' # In a Shiny app
#' # UI part:
#' DTOutput("orig_data")
#'
#' # Server part:
#' output$orig_data <- DT::renderDataTable({
#'   req(orig_data()) # Ensure the dataset is loaded
#'   mtest::downloadDT(orig_data(), "The original data is as follows")}, server = FALSE)
#'
#' @importFrom DT datatable
#' @export

downloadDT <- function(dt, title) {

  DT::datatable(dt,
                caption = title,
                # filter = 'top',
                extensions = 'Buttons',
                options = list(

                  scrollX = TRUE,
                  autoWidth = FALSE,
                  dom = 'Blrtip',
                  buttons = list(
                    I('colvis'), 'copy', 'print',
                    list(
                      extend = 'collection',
                      buttons = list(
                        list(extend = "csv", filename = "page", exportOptions = list(
                          columns = ":visible", modifier = list(page = "current"))
                        ),
                        list(extend = 'excel', filename = "page", title = NULL,
                             exportOptions = list(columns = ":visible", modifier = list(page = "current")))),
                      text = '下载当前页'),

                    list(
                      extend = 'collection',
                      buttons = list(
                        list(extend = "csv", filename = "data", exportOptions = list(
                          columns = ":visible", modifier = list(page = "all"))
                        ),
                        list(extend = 'excel', filename = "data", title = NULL,
                             exportOptions = list(columns = ":visible", modifier = list(page = "all")))),
                      text = '下载全部数据')

                  ),
                  language = list(url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Chinese.json")

                ))

}
