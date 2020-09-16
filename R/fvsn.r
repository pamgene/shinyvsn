#' @import vsn
#' @import reshape2
#' @import data.table

vsn0 = function(df, normalization = TRUE){
  X = acast(df, rowSeq ~ colSeq)
  aVsn = vsn2(X, calib = ifelse(normalization, "affine", "none"))
  result = data.table(vsn = list(aVsn))
}

vsnr = function(df, normalization = TRUE){
  X = acast(df, rowSeq ~ colSeq)
  R = acast(df %>% filter(RefFactor == levels(RefFactor)[1]), rowSeq ~ colSeq)

  if(dim(X)[1] != dim(R)[1]){
    stop("Number of rows in data and reference do not match")
  }

  if(all(rownames(X) == rownames(R))){
    aRef = vsn2(R, calib = ifelse(normalization, "affine", "none"))
    aVsn = vsn2(X, reference = aRef, calib = ifelse(normalization, "affine", "none"))
    result = data.table(vsn = list(aVsn))
  } else {
    stop("IDs of data and reference do not match")
  }
  return(result)
}

vsnh = function(dt){
  H = attr(dt$vsn[[1]], "hx")
  rownames(H) = 1:dim(H)[1]
  aResult = melt(H)
  colnames(aResult) = c("rowSeq", "colSeq", "Hvsn")
  return(aResult)
}
