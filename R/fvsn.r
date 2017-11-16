#' @import vsn
#' @import reshape2
#' @import data.table

vsn0 = function(df, normalization = TRUE){
  X = acast(df, rowSeq ~ colSeq)
  aVsn = vsn2(X, calib = ifelse(normalization, "affine", "none"))
  result = data.table(vsn = list(aVsn))
}

vsnh = function(dt){
  H = attr(dt$vsn[[1]], "hx")
  aResult = melt(H)
  colnames(aResult) = c("rowSeq", "colSeq", "Hvsn")
  return(aResult)
}
