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
        checkboxInput("refset", "Use reference data", value = FALSE),
        conditionalPanel(condition = 'input.refset',
            fileInput("reffile", "Browse ...")
        ),
        actionButton("start", "Run"),
        verbatimTextOutput("status")
      )
  })

  getPropertiesAsMapReactive = context$getPropertiesAsMap()
  getRunFolderReactive = context$getRunFolder()
  getStepFolderReactive = context$getFolder()
  getDataReactive = context$getData()

  refFile = NULL

  observe({
    getPropertiesAsMap=getPropertiesAsMapReactive$value
    if (is.null(getPropertiesAsMap)) return()
    propertiesAsMap = getPropertiesAsMap()

    getData=getDataReactive$value
    if (is.null(getData)) return()

    getRunFolder = getRunFolderReactive$value
    if(is.null(getRunFolder)) return()

    getStepFolder = getStepFolderReactive$value
    if(is.null(getStepFolder)) return()

    bndata = getData()
    df = bndata$data

    output$status = renderText({
      isolate({
        bRef = input$refset
      })
      if(input$start >0){
        showNotification(ui = "Running VSN ...", type = "message", closeButton = FALSE, duration = NULL)
        if (bndata$hasColors){
          grouping = droplevels(interaction(df[bndata$colorColumnNames]))
        } else {
          grouping = "Main"
        }
        df = data.frame(df, grp = grouping)
        isolate({
          if(!bRef){
            vsnResult = df %>% group_by(grp) %>% do(vsn0(., normalization = input$affine))
            hdf = vsnResult %>% group_by(grp) %>% do(vsnh(.))
            reslist = list(vsnResult = vsnResult)
          }else{
            refdf = getRefData()
            if(!is.null(refdf)){
              vsnResult = df %>% group_by(grp) %>% do(vsnr(., refdf, normalization = input$affine))
              hdf = vsnResult %>% group_by(grp) %>% do(vsnh(.))
              reslist = list(vsnResult = vsnResult, refdf = refdf, refFileName = input$reffile$name)
            } else {
              stop("Please select a file with a reference set.")
            }
          }
        })
        save(file = file.path(getRunFolder(),"runData.RData"), reslist)
        hdf = hdf[,-1]
        hdf = hdf[!is.na(hdf$Hvsn),]
        hdf$rowSeq = as.double(hdf$rowSeq)
        hdf$colSeq = as.double(hdf$colSeq)
        meta.hdf = data.frame(labelDescription = c("rowSeq", "colSeq", "Hvsn"),
                              groupingType = c("rowSeq", "colSeq", "QuantitationType"))
        result = AnnotatedData$new(data = hdf, metadata = meta.hdf)
        context$setResult(result)
        return("Done")
      } else {
        return(".")
      }
    })

    getRefData = reactive({
      refFile = input$reffile
      if(is.null(refFile)){
        return(NULL)
      } else {
       load(refFile$datapath)
       refdf = aCube
       return(refdf)
      }
    })

  })
}

#' @export
shinyServerShowResults = function(input, output, session, context){
  getFolderReactive = context$getRunFolder()

  output$body = renderUI({
    mainPanel(
      selectInput("group", "Show meanSdPlot for", choices = ""),
      plotOutput("msplot"),
      verbatimTextOutput("ref")
    )
  })

  observe({
    getFolder = getFolderReactive$value
    if (is.null(getFolder)) return()
    load(file.path(getFolder(), "runData.RData"))
    vsnResult = reslist$vsnResult
    updateSelectInput(session,"group", choices = vsnResult$grp)

    output$msplot = renderPlot({
      idx = which(input$group == vsnResult$grp)
      meanSdPlot(vsnResult$vsn[[idx[1]]])
    })

    output$ref = renderText({
      if(!is.null(reslist$refFileName)){
        return(reslist$refFileName)
      } else {
        return("No reference file used.")
      }
    })
  })
}
