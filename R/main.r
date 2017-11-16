#' @import bnutil
#' @import shiny
#' @import dplyr
#'
#' @export
operatorProperties = function() {
  return (list(
    list('Interactive', list('No', 'Yes')))
  )
}

#' @export
shinyServerRun = function(input, output, session, context) {

  output$body = renderUI({
    mainPanel(
        checkboxInput("affine", "Affine normalization", value = TRUE),
        checkboxInput("refset", "Use a reference set", value = FALSE),
        conditionalPanel(condition = 'input.refset',
            fileInput("reffile", "Select file with reference set")
        ),
        actionButton("start", "Start")
      )
  })

  getPropertiesAsMapReactive = context$getPropertiesAsMap()
  getDataReactive = context$getData()
  go = reactiveVal(0)

  observe({

    getPropertiesAsMapReactive = context$getPropertiesAsMap()
    getPropertiesAsMap=getPropertiesAsMapReactive$value
    if (is.null(getPropertiesAsMap)) return()
    propertiesAsMap = getPropertiesAsMap()

    getData=getDataReactive$value
    if (is.null(getData)) return()

    bndata = getData()
    df = bndata$data

    observeEvent(go(), {
      showNotification(ui = "Running VSN ...", type = "message", closeButton = TRUE, duration = NULL)
      if (bndata$hasColors){
        grouping = droplevels(interaction(df[bndata$colorColumnNames]))
      } else {
        grouping = "none"
      }
      df = data.frame(df, grp = grouping)
      if(!input$refset){
        vsnResult = df %>% group_by(grp) %>% do(vsn0(., normalization = input$affine))
        hdf = vsnResult %>% group_by(grp) %>% do(vsnh(.))
      }
      hdf = hdf[,-1]
      hdf = hdf[!is.na(hdf$Hvsn),]
      meta.hdf = data.frame(labelDescription = c("rowSeq", "colSeq", "Hvsn"),
                            groupingType = c("rowSeq", "colSeq", "QuantitationType"))
      result = AnnotatedData$new(data = hdf, metadata = meta.hdf)
      context$setResult(result)

    }, ignoreInit = TRUE)

    observeEvent(input$start, {
      if(is.null(input$start)) return()
      if ( (propertiesAsMap$Interactive == "No") | (input$start > 0) ){
        val = go()
        go(val + 1)
      }
    }, ignoreNULL = FALSE)
  })
}
