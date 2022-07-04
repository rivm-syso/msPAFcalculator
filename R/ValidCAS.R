#' @title ValidCAS
#' @name ValidCAS
#' @author jaap slootweg
#' @description Checks the validity of a CAS code
#' @param CAScode the CAS code or a vector of CAS codes
#' @return Vector of boolean
#' @export
ValidCAS <- function(CAScode){
  #remove or ignore -
  CascodeNodash <- gsub("-", "", as.character(CAScode))
  CasCodeSingleChar <- sapply(CascodeNodash, strsplit, "")
  suppressWarnings( CasCodeLastNumber <- sapply(CasCodeSingleChar, function(x){
    as.numeric(x[length(x)])
  })) #can generate NA's
  suppressWarnings( CasCodeChecksum <- sapply(CasCodeSingleChar, function(x){
    revx <- rev(x)
    sum(sapply(1:(length(x)-1), function(y) 
       as.numeric(y*as.numeric(revx[y+1]))))
  })) #dito
  !is.na(CasCodeChecksum) & !is.na(CasCodeLastNumber) & (CasCodeChecksum %% 10) == CasCodeLastNumber
}