#' @title HU_Calc2
#' @name  HU_Calc2
#' @author jaap slootweg
#' @description calculate and order Hazard Units (HU) for samples of concentrations and optional bio availability modifyers
#' @param ToHU data.frame with concentrations with at least the columns CAS, SampleID, MeasuredValue, possibly Filtered | PreTreatment
#' @param ChemData data.frame like DefChemical
#' @param SSDreplace data.frame like Defreplacements, wth at least the columns
#' @param EnvData data.frame like defmodifyers
#' @param DefaultFiltered set Filtered to, only if column "PreTreatment" is missing
#' @param DevRange Dev values outside this range will be set to 0.7 (average dev all SSDs)
#' @param aggrFUN function to aggregate the measurements, for example in case of multiple measurements
#' @param aggrParam see ...
#'
#' @return data.frame with msPAF ready data
#' @export
HU_Calc2 <- function (ToHU, ChemData = Gross, ChemReplace = NULL, EnvData = NULL,
                      muNames = c(acute = "Acute2.0Avg10LogMassTox.ug.L", chronic = "Chronic2.0Avg10LogMassTox.ug.L"),
                      sigmaNames = c(acute = "Acute2.0Dev10LogMassTox.ug.L", chronic = "Chronic2.0Dev10LogMassTox.ug.L"),
                      DefaultFiltered = T, aggrFUN = max, DevRange = c(0.2,2), 
                      status_bioavailability=TRUE, TooLowLimit = 0.0001){

  Awarning <- list()
  
  if (!all(c(muNames,sigmaNames) %in% names(ChemData))) {
    Awarning[["muSigmaMissing"]] <- paste("Missing mu and/or sigma;", muNames, sigmaNames)
    emptyPAF <- data.frame()
    attr(emptyPAF, "warning") <- Awarning
    return(emptyPAF)
  }
  needDataColumns <- c("PreTreatment","CAS", "SampleID", "ChemCode", "MeasuredValue")
  if(!all(needDataColumns %in% names(ToHU))){
    missingColumns <- needDataColumns [!needDataColumns %in% names(ToHU)]
    Awarning[["missingDataColumn"]] <- paste("Missing data column;", missingColumns)
    emptyPAF <- data.frame()
    attr(emptyPAF, "warning") <- Awarning
    return(emptyPAF)
  }

  
  if ("PreTreatment" %in% names(ToHU)) {
    ToHU$Filtered <- endsWith(tolower(ToHU$PreTreatment), "nf")   #na filtratie
  } else {
    ToHU$Filtered <- DefaultFiltered
  }

  #if Sample as filtered AND not filtered tuples for otherwise same sample & substance: take filtered
  #by first aggregate for SampleID, substance, Filtered and take the aggrFUN (usually max) then
  #aggregate for SampleID, substance and then the minimum;
  #basically: the filtered sample of the maximum concentration
  aggToHU <- aggregate(MeasuredValue~CAS+SampleID+ChemCode+Filtered , data = ToHU, FUN = aggrFUN)
  if (nrow(aggToHU) < nrow(ToHU)) {
    Awarning[["multiMeasurements"]] <- "Substance has multiple samples; one taken (maximum)"
  }
  
  order_var <- with(aggToHU, order(SampleID, CAS, ChemCode, MeasuredValue))
  keep_rows <- logical(nrow(aggToHU)) #init booleans to keep, we keep the first one
  keep_rows[order_var[1]] <- TRUE
  current_group <- aggToHU[order_var[1], c("SampleID", "CAS", "ChemCode")]
  for (i in 2:length(order_var)) {
    # If the current group is not the same as the previous group
    if (any(aggToHU[order_var[i], c("SampleID", "CAS", "ChemCode")] != current_group)) {
      # Update the current group
      current_group <- aggToHU[order_var[i], c("SampleID", "CAS", "ChemCode")]
      # Mark the row to be kept
      keep_rows[order_var[i]] <- TRUE
    }
  }
  deletedRows <- length(order_var) - sum(keep_rows)
  if (deletedRows > 0){
    Awarning[["nonFiltered"]] <- "filtered a Non-Filtered sample, because filtered is also in"
  }
  PAF <- aggToHU[keep_rows,] #outside of if; init of PAF
  
  if (!is.null(ChemReplace)) { #update SSD for chemicals from Chemreplace with "leenSSD's"
    #this (in the default version) includes NH4 - alikes where NH3 is the toxicant; see also the recalculation of concentration for NH4-alikes
    stopifnot ("CAS" %in% names(ChemReplace),
               "CASReplace" %in% names(ChemReplace))
    ToChange <- match(ChemReplace$CAS, ChemData$CAS)
    MatchToChange <- match(ChemReplace$CASReplace, ChemData$CAS)
    ChemData[ToChange,muNames] <- ChemData[MatchToChange, muNames]
    ChemData[ToChange,sigmaNames] <- ChemData[MatchToChange, sigmaNames]
  }

  ChemMatch <- match(PAF$CAS, ChemData$CAS)
  if (anyNA(ChemMatch)){
    MissingChem <- as.list(unique(PAF$CAS[is.na(ChemMatch)]))
    Awarning[["NoSSD"]] <- paste("No qualified SSD for", do.call(paste,MissingChem))
    PAF <- PAF[!is.na(ChemMatch),]
    ChemMatch <- ChemMatch[!is.na(ChemMatch)]
  }
  #All all needed chemical data
  PAF$MOI <- ChemData$M.O.I[ChemMatch]
  PAF$KocNew <- ChemData$KocNew[ChemMatch]
  PAF$KocNew <- ifelse(is.na(PAF$KocNew), ChemData$logKpSuspendedMatter[ChemMatch], PAF$KocNew) #Add ED 3-1-22
  PAF$PrimaryMoA <- ChemData$PrimaryMoA[ChemMatch]
  PAF$Avg10Log_acute <- ChemData[ChemMatch,muNames["acute"]]
  PAF$Dev10Log_acute <- ChemData[ChemMatch,sigmaNames["acute"]]
  PAF$Dev10Log_acute[PAF$Dev10Log_acute < DevRange[1] | PAF$Dev10Log_acute > DevRange[2]] <- 0.7
  PAF$Avg10Log_chronic <- ChemData[ChemMatch,muNames["chronic"]]
  PAF$Dev10Log_chronic <- ChemData[ChemMatch,sigmaNames["chronic"]]
  PAF$Dev10Log_chronic[PAF$Dev10Log_chronic < DevRange[1] | PAF$Dev10Log_chronic > DevRange[2]] <- 0.7
  PAF$UseClass <- ChemData$UseClass[ChemMatch]
  PAF$groep.fotoNL <- ChemData$groep.fotoNL[ChemMatch]


# Use Default modifiers if none are specified -----------------------------
  if (is.null(EnvData)) EnvData <- ModifierDefaults
  if (nrow(EnvData)==1){
    names(EnvData) <- sub("\\(.+\\)", "", names(EnvData))
    SampleMatch <- rep(1,nrow(PAF))
  } else {
    if (nrow(PAF) == nrow(EnvData)) {
      SampleMatch <- 1:nrow(EnvData)
    } else {
      stopifnot("SampleID" %in% names(EnvData) & "SampleID" %in% names(PAF))
      SampleMatch <- match(PAF$SampleID, EnvData$SampleID)
    }
  }
  PAF$TSS <- EnvData$TSS[SampleMatch]

  #save for NHx, Inorganic, all dissolved, all bioav.
  PAF$DissConc <- PAF$MeasuredValue
  iToHUisNH4 <- which(PAF$ChemCode == "sNH3NH4") #from specific dutch aquocode; sum of NH3 and NH4
  #NH4 / sNH3NH4 are considered the actual measured NH4 concentration, possibly replacing the calculated C[NH3]
  if(length(iToHUisNH4) > 0){
    pKa <- 0.09018 + (2729.92 / (273.2 + EnvData$Tw[SampleMatch[iToHUisNH4]])) #NH3/NH4
    PAF$DissConc[iToHUisNH4] <- 1/(1+10^(pKa-EnvData$pH[SampleMatch[iToHUisNH4]])) * PAF$MeasuredValue[iToHUisNH4]
    #remove others in samples
    iToDel <- which(PAF$SampleID %in% PAF$SampleID[iToHUisNH4] & PAF$ChemCode %in% c("NH3", "NH4"))
    if (length(iToDel) > 0 ){
      PAF <- PAF[-iToDel,]
      SampleMatch <- SampleMatch[-iToDel]
      ChemMatch <- match(PAF$CAS, ChemData$CAS)
    }
  }
  iToHUisNH4 <- which(PAF$CAS == "14798-03-9") # == NH4
  # replaced by toxicant NH3, mind the "LeenSSD" for this!
  if(length(iToHUisNH4) > 0){
    pKa <- 0.09018 + (2729.92 / (273.2 + EnvData$Tw[SampleMatch[iToHUisNH4]])) #NH3/NH4
    PAF$DissConc[iToHUisNH4] <- 1/(1+10^(pKa-EnvData$pH[SampleMatch[iToHUisNH4]])) * PAF$MeasuredValue[iToHUisNH4]
    #remove others in samples
    iToDel <- which(PAF$SampleID %in% PAF$SampleID[iToHUisNH4] & PAF$CAS == "7664-41-7") # == NH3
    if (length(iToDel) > 0 ){
      PAF <- PAF[-iToDel,]
      SampleMatch <- SampleMatch[-iToDel]
      ChemMatch <- match(PAF$CAS, ChemData$CAS)
    }
  }
  

# Correction for bioavailability of metals/OC -----------------------------

  #metals / Organics differ
  metals <- !is.na(PAF$MOI) & PAF$MOI == "M"
  SpecialM <- !is.na(ChemData$CoefLogMeTot[ChemMatch]) & ChemData$CoefLogMeTot[ChemMatch] != 0
  #stopifnot(!any(is.na(SpecialM)))
  organic <- !is.na(PAF$MOI) & PAF$MOI == "O"

  #Correction of metal concentrations for bioavailability --only execute if status_bioavailability = True
  if (status_bioavailability) {
    PAF$DissConc[metals] <- ifelse(PAF$Filtered[metals], PAF$MeasuredValue[metals],
                                    PAF$MeasuredValue[metals] / (1 + PAF$TSS[metals] * 10 ^ -6 * 10 ^ PAF$KocNew[metals])
    )
  }

  #for Inorganic & metals, excluding SpecialM (overwritten in the next line)
  PAF$ActConc <- PAF$DissConc
  
  #Correction of special metal concentrations for bioavailability --only execute if status_bioavailability = True
  if (status_bioavailability) {
    PAF$ActConc[SpecialM] <- 10 ^ (
      ChemData$Intercept[ChemMatch[SpecialM]] +
        ChemData$CoefLogMeTot[ChemMatch[SpecialM]] * log10(PAF$DissConc[SpecialM]) +
        ChemData$CoefLogDOC[ChemMatch[SpecialM]] * log10(EnvData$DOC[SampleMatch[SpecialM]]) +
        ChemData$CoefDOC[ChemMatch[SpecialM]] * EnvData$DOC[SampleMatch[SpecialM]] +
        ChemData$CoefCa[ChemMatch[SpecialM]] * EnvData$Ca[SampleMatch[SpecialM]] +
        ChemData$CoefLogCa[ChemMatch[SpecialM]] * log10(EnvData$Ca[SampleMatch[SpecialM]]) +
        ChemData$CoefMg[ChemMatch[SpecialM]] * EnvData$Mg[SampleMatch[SpecialM]] +
        ChemData$CoefNa[ChemMatch[SpecialM]] * EnvData$Na[SampleMatch[SpecialM]] +
        ChemData$CoefLogCl[ChemMatch[SpecialM]] * log10(EnvData$Cl[SampleMatch[SpecialM]]) +
        ChemData$CoefpH[ChemMatch[SpecialM]] * EnvData$pH[SampleMatch[SpecialM]]
    )
  }
  
  #Correction of organic content concentrations for bioavailability --only execute if status_bioavailability = True
  if (status_bioavailability) {
    POC <- ModifierDefaults$`POC(mg/kg)`
    PAF$ActConc[organic] <- ifelse(PAF$Filtered[organic], PAF$MeasuredValue[organic],
                                    PAF$MeasuredValue[organic] /
                                      (1 + PAF$TSS[organic] * 10 ^ -6 * POC * 10 ^ -6 * 10 ^ PAF$KocNew[organic])
    )
    
    
  }

  if (nrow(PAF) == 0) return(data.frame())
  
  #acute
  PAF$HU_acute <- PAF$ActConc / (10 ^ PAF$Avg10Log_acute)
  PAF$PAFacute <- pnorm(log10(PAF$HU_acute),
                              mean = 0,
                              sd = PAF$Dev10Log_acute)
  #Avoid NA 
  PAF$HU_acute[is.na(PAF$HU_acute)] <- 0

  #chronic
  PAF$HU_Chronic <- PAF$ActConc / (10 ^ PAF$Avg10Log_chronic)
  PAF$PAFchronic <- pnorm(log10(PAF$HU_Chronic),
                                mean = 0,
                                sd = PAF$Dev10Log_chronic)

  #Avoid NA 
  PAF$HU_Chronic[is.na(PAF$HU_Chronic)] <- 0

  #Extend with columns "TooLowAcute" and "TooLowChronic"
  TooLowText <- paste("<", TooLowLimit)
  PAF$TooLowAcute <- ifelse(PAF$HU_acute < TooLowLimit, TooLowText, "")
  PAF$TooLowChronic <- ifelse(PAF$HU_Chronic < TooLowLimit, TooLowText, "")
  
  #keep all relevant attributes
  AllNames <- c("CAS","ChemCode","UseClass",
                "Meetobject.lokaalID", "THEdatum",
                "MeasuredValue", "ActConc",
                "HU_acute", "HU_Chronic", "PAFacute", "PAFchronic",
                "groep.fotoNL","PrimaryMoA","SampleID",
                "TooLowAcute", "TooLowChronic",
                "Avg10Log_acute","Dev10Log_acute",
                "Avg10Log_chronic","Dev10Log_chronic")
  
  PAF <- PAF[,AllNames[AllNames %in% names(PAF)]]
  attr(PAF, "warning") <- Awarning
  return(PAF)
}
