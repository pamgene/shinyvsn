library(bnutil)
library(vsn)
library(shinyvsn)
library(dplyr)
library(reshape2)
library(data.table)

getdata = function() {
  aData = AnnotatedData$new(data = S100.df, metadata = S100.mdf)
}

setResult = function(annotatedResult){
  print(annotatedResult)
  result = annotatedResult$data
}

getProperties = function(){
  props = list(Interactive = "Yes")
}

bnMessageHandler = bnshiny::BNMessageHandler$new()
bnMessageHandler$getDataHandler = getdata
bnMessageHandler$setResultHandler = setResult
bnMessageHandler$getPropertiesAsMapHandler = getProperties


bnshiny::startBNTestShiny('shinyvsn', sessionType='show', bnMessageHandler=bnMessageHandler)
