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
            selectInput("reffactor", "Reference annotation factor", choices = list(), selected = "nothing"),
            selectInput("reflevel",  "Reference annotation level", choices = list())
        ),
        actionButton("start", "Run"),
        verbatimTextOutput("status")
      )
  })

  getPropertiesAsMapReactive = context$getPropertiesAsMap()
  getRunFolderReactive = context$getRunFolder()
  getStepFolderReactive = context$getFolder()
  getDataReactive = context$getData()


  setStoredSettings = function(settingsFile, indat){
    return()
    # if(!file.exists(settingsFile)) return()
    #
    # load(settingsFile)
    # if (! settings$reffactor %in% indat$arrayLabels) return()
    # rf = indat$data[[settings$reffactor]]
    # if (! settings$reflevel  %in% levels(rf) ) return()
    #
    # updateSelectInput(session,  inputId = "reffactor", selected = settings$reffactor)
    # updateSelectInput(session,  inputId = "reflevel" , selected = settings$reflevel)
    # updateCheckboxInput(session, inputId = "affine", value = settings$affine)
    # updateCheckboxInput(session, inputId = "refset", value = settings$refset)
    #
    # return()
  }


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

    updateSelectInput(session,  inputId = "reffactor", choices = bndata$arrayColumnNames)
    rf = factor(df[[ bndata$arrayColumnNames[1] ]])
    updateSelectInput(session, inputId = "reflevel", choices = levels(rf))

    #settingsFile = file.path(getStepFolder(),"stepData.RData")
    #setStoredSettings(settingsFile, bndata)

    output$status = renderText({
      observeEvent(input$reffactor, {
        rf = factor(df[[input$reffactor]])
        updateSelectInput(session, inputId = "reflevel", choices = levels(rf))
      })

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
            df$RefFactor = factor(df[[input$reffactor]])
            df$RefFactor = relevel(df$RefFactor, ref = input$reflevel)
            edit(levels(df$RefFactor))
            vsnResult = df %>% group_by(grp) %>% do(vsnr(., normalization = input$affine))
            hdf = vsnResult %>% group_by(grp) %>% do(vsnh(.))
            reslist = list(vsnResult = vsnResult)
          }

          settings = list(affine = input$affine,
                          refset = input$refset,
                          reffactor = input$reffactor,
                          reflevel = input$reflevel)
        })
        reslist$settings = settings
        reslist$df = df
        save(file = file.path(getRunFolder(),"runData.RData"), reslist)
        #save(file = settingsFile , settings)
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
      if(reslist$settings$refset){
        str = paste("Data with ", reslist$settings$reffactor," @",levels(reslist$df$RefFactor)[1]," used as reference data.", sep = "")
        return(str)
      } else {
        return("No reference data used.")
      }
    })
  })
}

