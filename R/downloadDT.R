## 前置项目 创造dt下载的函数----
downloadDT<- function(dt,title){

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
                        list(extend = "csv", filename = "page",exportOptions = list(
                          columns = ":visible",modifier = list(page = "current"))
                        ),
                        list(extend = 'excel', filename = "page", title = NULL,
                             exportOptions = list(columns = ":visible",modifier = list(page = "current")))),
                      text = 'Download current page'),

                    list(
                      extend = 'collection',
                      buttons = list(
                        list(extend = "csv", filename = "data",exportOptions = list(
                          columns = ":visible",modifier = list(page = "all"))
                        ),
                        list(extend = 'excel', filename = "data", title = NULL,
                             exportOptions = list(columns = ":visible",modifier = list(page = "all")))),
                      text = 'Download all data')

                  ))

  )
}
