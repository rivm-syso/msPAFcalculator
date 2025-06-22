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
ValidCAS <- function(CAScode, checkDash = TRUE) {
  # Ensure CAScode is a character vector
  CAScode <- as.character(CAScode)
  CAScode <- sapply(CAScode, trimws)  # Remove leading/trailing whitespace
  
  # Validate `checkDash` argument: ensure it is logical
  if (!is.logical(checkDash) || length(checkDash) != 1) {
    stop("`checkDash` must be a single logical value (TRUE or FALSE).")
  }
  
  hasCASstring <- grepl("CAS", CAScode)  # Check if string contains "CAS"
  onlynumordash <- !grepl("[a-zA-Z]", CAScode)  # Only numeric chars or dashes
  hasCASpattern <- grepl("^\\d+-\\d\\d-\\d$", CAScode)  # Matches valid CAS pattern
  
  # If non-number characters are present, it should contain "CAS"
  poss.CAS <- onlynumordash | hasCASstring
  
  if (checkDash) { # Accept CAS code only with -dd-d pattern
    poss.CAS <- poss.CAS & hasCASpattern
  }
  
  # Initialize result as FALSE for all inputs
  result <- rep(FALSE, length(CAScode))
  
  # Process possible CAS codes
  if (any(poss.CAS)) {
    CascodeNodash <- stringr::str_extract(gsub("-", "", CAScode[poss.CAS]), "[[:digit:]]+")
    
    # Ensure `CascodeNodash` is not empty or invalid
    if (!is.null(CascodeNodash) && length(CascodeNodash) > 0) {
      # Split into individual characters for processing checksum
      CasCodeSingleChar <- lapply(CascodeNodash, function(x) strsplit(x, "")[[1]])
      
      # Validate CAS codes that have more than 4 characters
      valid_length <- sapply(CasCodeSingleChar, function(x) length(x) > 4)
      temp_poss_CAS <- poss.CAS[poss.CAS]
      temp_poss_CAS <- temp_poss_CAS & valid_length
      
      # Recheck if there are valid candidates to process further
      if (any(temp_poss_CAS)) {
        CasCodeLastNumber <- sapply(CasCodeSingleChar, function(x) as.numeric(x[length(x)]))
        CasCodeChecksum <- sapply(CasCodeSingleChar, function(x) {
          revx <- rev(x)
          sum(sapply(1:(length(x) - 1), function(y) y * as.numeric(revx[y + 1])))
        })
        
        # Validate checksum for remaining candidates
        valid_checksum <- (CasCodeChecksum %% 10) == CasCodeLastNumber
        temp_poss_CAS[temp_poss_CAS] <- valid_checksum
      }
      
      # Assign back the final results to `poss.CAS`
      poss.CAS[poss.CAS] <- temp_poss_CAS
    }
  }
  
  # Assign final validation result back to the `result` vector
  result <- poss.CAS
  return(result)
}
