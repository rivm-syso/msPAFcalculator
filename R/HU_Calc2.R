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

  stopifnot (all(c(muNames,sigmaNames) %in% names(ChemData)))

  if ("PreTreatment" %in% names(ToHU)) {
    ToHU$Filtered <- endsWith(tolower(ToHU$PreTreatment), "nf")   #na filtratie
  } else {
    ToHU$Filtered <- DefaultFiltered
  }

  #if Sample as filtered AND not filtered tuples for otherwise same sample & substance: take filtered
  splitHU <- split(ToHU, ~ CAS + SampleID)
  hasFiltered <- sapply(splitHU, function(x) {
    any(x$Filtered)
  })
  TakeFilteredIf <- function(dfWithFiltered, hasFilteredVector){
    dfWithFiltered[dfWithFiltered$Filtered == hasFilteredVector,]
  }
  ToHU <- do.call(rbind,
      mapply(TakeFilteredIf, splitHU, hasFiltered, SIMPLIFY=FALSE)
  )
  
  if (!is.null(ChemReplace)) { #update SSD for chemicals from Chemreplace with "leenSSD's"
    #this (in the default version) includes NH4 - alikes where NH3 is the toxicant; see also the recalculation of concentration for NH4-alikes
    stopifnot ("CAS" %in% names(ChemReplace),
               "CASReplace" %in% names(ChemReplace),
               "AquoCode" %in% names(ChemReplace))
    ToChange <- match(ChemReplace$CAS, ChemData$CAS)
    MatchToChange <- match(ChemReplace$CASReplace, ChemData$CAS)
    ChemData[ToChange,muNames] <- ChemData[MatchToChange, muNames]
    ChemData[ToChange,sigmaNames] <- ChemData[MatchToChange, sigmaNames]
  }

  ChemMatch <- match(ToHU$CAS, ChemData$CAS)
  if (anyNA(ChemMatch)){
    MissingChem <- as.list(unique(ToHU$CAS[is.na(ChemMatch)]))
    Awarning <- paste("No qualified SSD for", do.call(paste,MissingChem))
    ToHU <- ToHU[!is.na(ChemMatch),]
    ChemMatch <- ChemMatch[!is.na(ChemMatch)]
  }
  #All all needed chemical data
  ToHU$MOI <- ChemData$M.O.I[ChemMatch]
  ToHU$KocNew <- ChemData$KocNew[ChemMatch]
  ToHU$KocNew <- ifelse(is.na(ToHU$KocNew), ChemData$logKpSuspendedMatter[ChemMatch], ToHU$KocNew) #Add ED 3-1-22
  ToHU$PrimaryMoA <- ChemData$PrimaryMoA[ChemMatch]
  ToHU$Avg10Log_acute <- ChemData[ChemMatch,muNames["acute"]]
  ToHU$Dev10Log_acute <- ChemData[ChemMatch,sigmaNames["acute"]]
  ToHU$Dev10Log_acute[ToHU$Dev10Log_acute < DevRange[1] | ToHU$Dev10Log_acute > DevRange[2]] <- 0.7
  ToHU$Avg10Log_chronic <- ChemData[ChemMatch,muNames["chronic"]]
  ToHU$Dev10Log_chronic <- ChemData[ChemMatch,sigmaNames["chronic"]]
  ToHU$Dev10Log_chronic[ToHU$Dev10Log_chronic < DevRange[1] | ToHU$Dev10Log_chronic > DevRange[2]] <- 0.7
  ToHU$UseClass <- ChemData$UseClass[ChemMatch]
  ToHU$groep.fotoNL <- ChemData$groep.fotoNL[ChemMatch]


# Use Default modifiers if none are specified -----------------------------
  if (is.null(EnvData)) EnvData <- ModifierDefaults
  if (nrow(EnvData)==1){
    names(EnvData) <- sub("\\(.+\\)", "", names(EnvData))
    SampleMatch <- rep(1,nrow(ToHU))
  } else {
    if (nrow(ToHU) == nrow(EnvData)) {
      SampleMatch <- 1:nrow(EnvData)
    } else {
      stopifnot("SampleID" %in% names(EnvData) & "SampleID" %in% names(ToHU))
      SampleMatch <- match(ToHU$SampleID, EnvData$SampleID)
    }
  }
  ToHU$TSS <- EnvData$TSS[SampleMatch]

  #NH4 / sNH3NH4 are considered the actual measured NH4 concentration, possibly replacing the calculated C[NH3]
  iToHUisNH4 <- which(ToHU$AquoCode == "sNH3NH4")
  if(length(iToHUisNH4) > 0){
    pKa <- 0.09018 + (2729.92 / (273.2 + EnvData$Tw[SampleMatch[iToHUisNH4]])) #NH3/NH4
    ToHU$MeasuredValue[iToHUisNH4] <- 1/(1+10^(pKa-EnvData$pH[SampleMatch[iToHUisNH4]])) * ToHU$MeasuredValue[iToHUisNH4]
    #remove others in samples
    iToDel <- which(ToHU$SampleID %in% ToHU$SampleID[iToHUisNH4] & ToHU$AquoCode %in% c("NH3", "NH4"))
    if (length(iToDel) > 0 ){
      ToHU <- ToHU[-iToDel,]
      SampleMatch <- SampleMatch[-iToDel]
      ChemMatch <- match(ToHU$CAS, ChemData$CAS)
    }
  }
  iToHUisNH4 <- which(ToHU$AquoCode == "NH4")
  if(length(iToHUisNH4) > 0){
    pKa <- 0.09018 + (2729.92 / (273.2 + EnvData$Tw[SampleMatch[iToHUisNH4]])) #NH3/NH4
    ToHU$MeasuredValue[iToHUisNH4] <- 1/(1+10^(pKa-EnvData$pH[SampleMatch[iToHUisNH4]])) * ToHU$MeasuredValue[iToHUisNH4]
    #remove others in samples
    iToDel <- which(ToHU$SampleID %in% ToHU$SampleID[iToHUisNH4] & ToHU$AquoCode == "NH3")
    if (length(iToDel) > 0 ){
      ToHU <- ToHU[-iToDel,]
      SampleMatch <- SampleMatch[-iToDel]
      ChemMatch <- match(ToHU$CAS, ChemData$CAS)
    }
  }
  

# Correction for bioavailability of metals/OC -----------------------------

  #metals / Organics differ
  metals <- !is.na(ToHU$MOI) & ToHU$MOI == "M"
  SpecialM <- !is.na(ChemData$CoefLogMeTot[ChemMatch]) & ChemData$CoefLogMeTot[ChemMatch] != 0
  #stopifnot(!any(is.na(SpecialM)))
  organic <- !is.na(ToHU$MOI) & ToHU$MOI == "O"
  #for Inorganic, all dissolved, all bioav.
  ToHU$DissConc <- ToHU$MeasuredValue

  #Correction of metal concentrations for bioavailability --only execute if status_bioavailability = True
  if (status_bioavailability) {
    ToHU$DissConc[metals] <- ifelse(ToHU$Filtered[metals], ToHU$MeasuredValue[metals],
                                    ToHU$MeasuredValue[metals] / (1 + ToHU$TSS[metals] * 10 ^ -6 * 10 ^ ToHU$KocNew[metals])
    )
  }

  #for Inorganic & metals, excluding SpecialM (overwritten in the next line)
  ToHU$ActConc <- ToHU$DissConc
  
  #Correction of special metal concentrations for bioavailability --only execute if status_bioavailability = True
  if (status_bioavailability) {
    ToHU$ActConc[SpecialM] <- 10 ^ (
      ChemData$Intercept[ChemMatch[SpecialM]] +
        ChemData$CoefLogMeTot[ChemMatch[SpecialM]] * log10(ToHU$DissConc[SpecialM]) +
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
  
  
  
  #Correction of oragnic content concentrations for bioavailability --only execute if status_bioavailability = True
  if (status_bioavailability) {
    POC <- ModifierDefaults$`POC(mg/kg)`
    ToHU$ActConc[organic] <- ifelse(ToHU$Filtered[organic], ToHU$MeasuredValue[organic],
                                    ToHU$MeasuredValue[organic] /
                                      (1 + ToHU$TSS[organic] * 10 ^ -6 * POC * 10 ^ -6 * 10 ^ ToHU$KocNew[organic])
    )
    
    
  }

  #?collect all attributes to aggregate == all attributes but used above future?
  AllNames <- names(ToHU)[names(ToHU) %in% c("CAS","ChemCode","UseClass","groep.fotoNL","PrimaryMoA","SampleID",
                                             "Meetobject.lokaalID", "THEdatum",
                                             "TMOA", "Avg10Log_acute","Dev10Log_acute",
                                             "Avg10Log_chronic","Dev10Log_chronic")]
  #  AggrNames <- AllNames[!AllNames %in% c("Filtered", "MOI", "KocNew", "Unit", "LimitIndicator",
#                                         "TSS", "MeasuredValue", "DissConc", "ActConc")]
  
  if (nrow(ToHU) == 0) return(data.frame())
  
  PAF <- aggregate(ToHU[,c("MeasuredValue", "ActConc")], by = ToHU[,AllNames,drop=F],
                         FUN = aggrFUN)
  #acute
  PAF$HU_acute <- PAF$ActConc / (10 ^ PAF$Avg10Log_acute)
  PAF$PAFacute <- pnorm(log10(PAF$HU_acute),
                              mean = 0,
                              sd = PAF$Dev10Log_acute)
  #Avoid NA before aggregation later with other substances
  PAF$HU_acute[is.na(PAF$HU_acute)] <- 0

  #chronic
  PAF$HU_Chronic <- PAF$ActConc / (10 ^ PAF$Avg10Log_chronic)
  PAF$PAFchronic <- pnorm(log10(PAF$HU_Chronic),
                                mean = 0,
                                sd = PAF$Dev10Log_chronic)

  #Avoid NA before aggregation later with other substances
  PAF$HU_Chronic[is.na(PAF$HU_Chronic)] <- 0

  #Extend with columns "TooLowAcute" and "TooLowChronic"
  TooLowText <- paste("<", TooLowLimit)
  PAF$TooLowAcute <- ifelse(PAF$HU_acute < TooLowLimit, TooLowText, "")
  PAF$TooLowChronic <- ifelse(PAF$HU_Chronic < TooLowLimit, TooLowText, "")
  
  if (exists("Awarning")){
    attr(PAF, "warning") <- Awarning
  }
  PAF
}
