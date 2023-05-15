#' Render Plots with UI
#'
#' This function creates a Shiny UI element to display a list of plots along with their titles.
#' It also provides options to download the plots as images.
#'
#' @param plots A list of ggplot objects to be displayed.
#' @param ID A character string to identify the plots.
#' @param titlelist A list of character strings to be used as titles for each plot. Default is NULL.
#' @param x Width of the plots in pixels.
#' @param y Height of the plots in pixels.
#' @return A Shiny UI element containing the list of plots with titles and download buttons.
#' @export
#'
#' @examples
#' resPlots <- reactive({
#'   req(results()$models[[1]]$assump$resPlots)
#'   lapply(seq_along(results()$models[[1]]$assump$resPlots), function(i) {
#'     results()$models[[1]]$assump$resPlots[[i]]$plot$clone()$print() + theme_prism()
#'   })
#' })
#'
#' output$resPlots <- renderUI({
#'   req(input$OK)
#'   isolate({
#'     req(input$resPlots==TRUE)
#'     description_text <- glue("残差图是一种用于评估线性回归模型拟合效果的可视化工具。在线性回归分析中，残差是指实际观测值与模型预测值之间的差值。残差图可以帮助我们检查模型的假设，例如误差的正态性、误差的独立性以及误差的方差齐性等。<br><br>",
#'                                "解读残差图时，请关注以下几点：<br><br>",
#'                                "残差与预测值或自变量的关系：理想情况下，残差图中的点应该在横轴上均匀分布，没有明显的模式。如果残差图呈现出某种趋势或形状，可能表示模型存在某种系统误差。<br><br>",
#'                                "残差的正态性：观察残差的分布，理想情况下，它们应当呈现出对称的正态分布。如果残差分布明显偏离正态分布，可能需要对数据进行相应的变换，或考虑使用其他类型的回归模型。<br><br>",
#'                                "残差的方差齐性：观察残差的大小或波动范围是否在整个横轴上保持一致。如果残差的波动范围随着横轴的增大或减小而变化，可能表示存在异方差问题，需要采取措施进行处理。<br><br>",
#'                                "总之，残差图是评估线性回归模型有效性的重要工具。通过仔细观察残差图，我们可以更好地了解模型的性能，从而对模型进行调整和优化。")
#'
#'     tagList(
#'       tags$b("残差图"),
#'       mtest::render_plots_UI(
#'         plots = resPlots(),
#'         ID = "resPlots",
#'         titlelist = NULL,
#'         x = input$plot_x,
#'         y = input$plot_y
#'       ),br(),
#'       tags$span(HTML(description_text),style = "font-size: 10px;")
#'     )
#'   })
#' })
#'
#' @export
render_plots_UI <- function(plots,ID,titlelist=NULL,x,y) {
  req(length(plots) > 0)

  plot_list <- lapply(seq_along(plots), function(i) {
    mtest::downloadPlot_server(glue("{ID}{i}"), plots[[i]], x, y)
    tagList(
      br(),
      titlelist[[i]],
      renderPlot({
        plots[[i]]
      }, width = x, height = y), br(),
      mtest::downloadPlot_UI(glue("{ID}{i}")), br(), br()
    )
  })

  tagList(div(id = "myPlots",
              do.call(tagList, unlist(plot_list, recursive = FALSE))))
}






