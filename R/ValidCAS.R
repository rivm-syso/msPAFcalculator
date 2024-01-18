#CAS code related routines

#' @name CAShash2CAS
#' @author jaap slootweg
#' @description converts #CAS (a number) to a CAS code with 2* "-"
#' @param CAS number like aaaabbc
#' @return CAS code like aaaa-bb-c
CAShash2CAS <- function(x) {
  xchar <- as.character(x)
  lenCAS <- sapply(xchar, nchar)
  paste(substr(xchar,start = 1, stop = lenCAS - 3),
        substr(xchar,start = lenCAS - 2, stop = lenCAS - 1),
        substr(xchar,start = lenCAS, stop = lenCAS),
        sep="-")
}


#' @title ValidCAS
#' @name ValidCAS
#' @author jaap slootweg
#' @description Checks the validity of a CAS code
#' @param CAScode the CAS code or a vector of CAS codes
#' @return Vector of boolean
ValidCAS <- function(CAScode, checkDash = T){
  CAScode <- sapply(CAScode, trimws)

  hasCASstring <- grepl("CAS", CAScode)
  onlynumordash <- !grepl("[a-z,A-Z]", CAScode)
  hasCASpattern <- grepl("\\d+-\\d\\d-\\d", CAScode)
  
  #if non number characters are present it should contain "CAS"
  poss.CAS <- onlynumordash | hasCASstring
  
  if (checkDash) {#accept CAS code only with the -dd-d pattern; 
    poss.CAS <- poss.CAS & hasCASpattern
  }
  #only the numbers for the !not.CAS
  CascodeNodash <- stringr::str_extract(gsub("-", "", CAScode[poss.CAS]), "[[:digit:]]+")
  
  CasCodeSingleChar <- sapply(CascodeNodash, strsplit, "")
  poss.CAS[poss.CAS] <- sapply(CasCodeSingleChar, function(x) {
    length(x) > 4
  })
  
  if (!any(poss.CAS)) return(poss.CAS) #prevent error and faster

  
  CasCodeLastNumber <- sapply(CasCodeSingleChar, function(x){
    as.numeric(x[length(x)])
  })
  CasCodeChecksum <- sapply(CasCodeSingleChar, function(x){
    revx <- rev(x)
    sum(sapply(1:(length(x)-1), function(y) 
      y*as.numeric(revx[y+1])))
  })
 
  #combine the poss.CAS and checksummed
  poss.CAS[poss.CAS] <- (CasCodeChecksum %% 10) == CasCodeLastNumber
  poss.CAS
}
