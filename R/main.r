#' @import bnutil
#' @import shiny
#' @import dplyr
#' @import plyr
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
        actionButton("start", "Run VSN"),
        verbatimTextOutput("status")
      )
  })

  getPropertiesAsMapReactive = context$getPropertiesAsMap()
  getRunFolderReactive = context$getRunFolder()
  getDataReactive = context$getData()

  observe({

    getPropertiesAsMapReactive = context$getPropertiesAsMap()
    getPropertiesAsMap=getPropertiesAsMapReactive$value
    if (is.null(getPropertiesAsMap)) return()
    propertiesAsMap = getPropertiesAsMap()

    getData=getDataReactive$value
    if (is.null(getData)) return()

    getFolder = getRunFolderReactive$value
    if(is.null(getFolder)) return()

    bndata = getData()
    df = bndata$data

    output$status = renderText({
      if(input$start >0){
        showNotification(ui = "Running VSN ...", type = "message", closeButton = FALSE, duration = NULL)
        if (bndata$hasColors){
          grouping = droplevels(interaction(df[bndata$colorColumnNames]))
        } else {
          grouping = "none"
        }
        df = data.frame(df, grp = grouping)
        if(!input$refset){
          #vsnResult = df %>% group_by(grp) %>% do(vsn0(., normalization = input$affine))
          vsnResult = ddply(df, ~ grp, .fun = vsn0, normalization = input$affine)
          #hdf = vsnResult %>% group_by(grp) %>% do(vsnh(.))
          hdf = ddply(vsnResult, ~ grp, .fun = vsnh)
        } else{
          if(is.null(input$reffile)) return()
          refFile = input$reffile
          load(refFile$datapath)
          refdf = aCube
          vsnResult = df %>% group_by(grp) %>% do(vsnr(., refdf, normalization = input$affine))

          hdf = vsnResult %>% group_by(grp) %>% do(vsnh(.))
        }
        save(file = file.path(getFolder(),"runData.RData"), vsnResult)

        hdf = hdf[,-1]
        hdf = hdf[!is.na(hdf$Hvsn),]
        meta.hdf = data.frame(labelDescription = c("rowSeq", "colSeq", "Hvsn"),
                              groupingType = c("rowSeq", "colSeq", "QuantitationType"))
        result = AnnotatedData$new(data = hdf, metadata = meta.hdf)
        context$setResult(result)
        return("Done")
      } else {
        return(".")
      }

    })

    # observeEvent(input$start, {
    #   if(is.null(input$start)) return()
    #   if ( (propertiesAsMap$Interactive == "No") | (input$start > 0) ){
    #     val = go()
    #     go(val + 1)
    #   }
    # }, ignoreNULL = FALSE)
  })
}

shinyServerShowResults = function(input, output, session, context){
  getFolderReactive = context$getRunFolder()

  output$body = renderUI({
    mainPanel(
      selectInput("group", "Show meanSdPlot for", choices = ""),
      plotOutput("msplot")
    )
  })

  observe({
    getFolder = getFolderReactive$value
    if (is.null(getFolder)) return()
    load(file.path(getFolder(), "runData.RData"))
    updateSelectInput(session,"group", choices = vsnResult$grp)

    output$msplot = renderPlot({
      idx = which(input$group == vsnResult$grp)
      meanSdPlot(vsnResult$vsn[[idx[1]]])
    })
  })
}
