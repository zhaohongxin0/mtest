#' User Interface for Factor Level Reordering
#'
#' @return A fluidPage object that provides an interactive interface for reordering factor levels
#'
#' @examples
#' # Run the relevelUI function to generate an interface for reordering factor levels
#' ui_factor <- mtest::relevelUI()
#' @export

relevelUI <- function() {

fluidPage(tags$br(),
          "如果之前您已经把分类变量设置为了factor，下面会列出它们的亚组水平，请用鼠标拖拽的方式改变它们的顺序，从而决定各亚组在后面生成的统计图表中显示的顺序。例如最左边的亚组在统计表中会显示在最上方。排在最前面的亚组通常会被设定为参照组。如果没有列出您要的变量，请回到前面把它们设置为factor。",


          tags$br(),tags$br()

          ,uiOutput("factor")
)

}


#' Server-side Function for Factor Level Reordering
#'
#' @param input Input parameters from Shiny
#' @param output Output parameters to Shiny
#' @param session Shiny session information
#' @param data A reactive expression for the data that should be reordered
#'
#' @return ordered A reactive expression for the reordered data
#'
#' @examples
#' # Example usage within a Shiny server function
#' server <- function(input, output, session) {
#'   # The following are placeholders and should be replaced with your own data/variables
#'   imported_data <- mtest::uploadServer(input, output, session, "uploadId")
#'   modified <- mtest::modifyServer(input, output, session, "vars", imported_data)
#'   recoded <- mtest::recodeServer(input, output, session, "recodeId", modified)
#'   imputed_data <- mtest::fillServer(input, output, session, recoded)
#'   filtered_data <- mtest::filterServer (input, output, session, imputed_data)
#'   # Use the relevelServer function with the filtered data
#'   ordered <- mtest::relevelServer (input, output, session, filtered_data)
#' }
#' @export

relevelServer <- function(input, output, session, data) {

factorData<-reactive({
  data()%>%
    select_if(purrr::negate(is.numeric))%>%
    select(where(~n_distinct(.,na.rm =TRUE)<30))
})

var_factor<-reactive({
  names(factorData())
})


output$factor <- renderUI({
  if (length(var_factor())>0){


    factor_output_list <- lapply(1:length(var_factor()), function(i) {

      orderInput(inputId = paste0("_",var_factor()[i]), label = var_factor()[i], items = do::unique_no.NA(as.character(factorData()[[i]])))
    })
    do.call(tagList, factor_output_list)

  }

})


# 因子排序----
ordered<-reactive({
  tmp <- data()

  if (length(var_factor())>0 && isTruthy(input[[paste0("_",var_factor()[1])]])){
    # 此处 input[[var_factor()[1]]]为真，代表用户鼠标点开排序的tab时，
    # 才实施下面的level替换，否则用户不点开，直接执行下面代码会把空值设为level
    vec <- 1:length(var_factor())
    for(i in vec){
      tmp[[var_factor()[i]]] <- factor(tmp[[var_factor()[i]]], levels = c(input[[paste0("_",var_factor()[i])]]))
    }
  }

  tmp<-tmp%>%mutate(across(where(~n_distinct(.,na.rm =TRUE)<3),factor))
  # 规范化变量名称
  tmp<-tmp%>%dplyr::rename_with(make.names)
  labelled::var_label(tmp)<-names(data())

  return (tmp)


})

return (ordered)

}
