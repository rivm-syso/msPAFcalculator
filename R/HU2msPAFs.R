#' @title HU2msPAFs
#' @name  HU2msPAFs
#' @author jaap slootweg
#' @description process Hazard Units (HU) for HU_Calc2() to msPAF values for each Sample; each UseClass ..?
#' @param HU data.frame with HU with at least the columns CAS/AquoCode, SampleID, ...
#' @param TMOAname name of column for toxic mode of action = "PrimaryMoA", NO 
#' switched to AquoCode to force Response Addition
#' @param LimitColumnNames allows a lower limit for PAF values, acute and chronic separately, 
#' if the column != "" the PAF value is set to 0. LimitColumnNames can be either NULL or two strings
#' @return data.frame with msPAF values
#' @export
HU2msPAFs <- function(HU, TMOAname = "CAS", 
                      groupName = "groep.fotoNL", 
                      LimitColumnNames = c("TooLowAcute", "TooLowChronic"),
                      National = "Nederlands"){


  if (nrow(HU) == 0) return(data.frame())
  AggMsPAF <- function(df, HUcolumName, DevColumName, l.AggrNames, LimitColumnName){
    stopifnot(all(c(HUcolumName, DevColumName, TMOAname, l.AggrNames, LimitColumnName) %in% names(df)))
    #also aggregate TMOA
    AggTMOAnames <- c(l.AggrNames, TMOAname)

    #Set PAF values that are too low to be relevant to 0; They remain in the data!
    if (!is.null(LimitColumnName)) {
      areTooLow <- HU[,LimitColumnName] != ""
      HU[areTooLow, HUcolumName] <- 0
    }
    
    #1 HU 2 msPAF, mixed model
    SomHUTMOA <- aggregate( df[!is.na(df[,DevColumName]), HUcolumName],
                            by = df[!is.na(df[,DevColumName]), AggTMOAnames, drop = F],
                            FUN = sum)
    AvgDevTMOA <- aggregate(df[!is.na(df[,DevColumName]),DevColumName],
                            by = df[!is.na(df[,DevColumName]), AggTMOAnames, drop = F],
                            FUN = mean)
    #conc addition from sum (above) + prep effect addition
    SomHUTMOA$RemainFrac <- 1 - pnorm(log10(SomHUTMOA$x),
                                      mean = 0,
                                      sd = AvgDevTMOA$x)
    
    #effect addition; loose TMOA
    AcumsPAF <- aggregate(SomHUTMOA$RemainFrac,
                          by = SomHUTMOA[,l.AggrNames,drop = F],
                          FUN = prod)
    #names(AcumsPAF)[names(AcumsPAF)=="x"] <- "RemainFrac"
    #YrGemMsPAF <- aggregate(RemainFrac~X+Y+DataSource, data = msPAF, FUN = mean)
    AcumsPAF$msPAF <- 1 - AcumsPAF$x
    
    return(AcumsPAF[,-which(names(AcumsPAF) == "x")])
  }

  AllNames <- names(HU)

  #first exclude groupName to obtain the total over all groups
  AcAll <- AggMsPAF(HU, HUcolumName = "HU_acute",
                  DevColumName = "Dev10Log_acute",
                  LimitColumnName = LimitColumnNames[1], 
                  l.AggrNames = c("SampleID") # not: groupName,
                          )
  AcAll[,groupName] <- "All"
  names(AcAll)[names(AcAll)=="msPAF"] <- "Acute" # ED
  
  if (!is.null(groupName)) { #might be set to NULL in the call of the outer function!
    #Including aggregation by groupName
    AcGroup <- AggMsPAF(HU, HUcolumName = "HU_acute",
                            DevColumName = "Dev10Log_acute",
                            LimitColumnName = LimitColumnNames[1], 
                            l.AggrNames = c("SampleID", groupName)
    )
    names(AcGroup)[names(AcGroup)=="msPAF"] <- "Acute" # ED

    #concat "All" and Chemical groups (from GroupName) and pivot to show both columns
    AcAll <- rbind(AcAll, AcGroup)
    AcAllWide <- reshape(AcAll, timevar = groupName,
                         direction = "wide", 
                         idvar = "SampleID")
  } else { #
    AcAllWide <- AcAll
  }

  #same for chronic
  #first exclude groupName to obtain the total over all groups
  CrAll <- AggMsPAF(HU, HUcolumName = "HU_Chronic",
                    DevColumName = "Dev10Log_chronic",
                    LimitColumnName = ifelse(length(LimitColumnNames)>1, LimitColumnNames[2], NULL), 
                    l.AggrNames = c("SampleID") # not: groupName,
  )
  CrAll[,groupName] <- "All"
  names(CrAll)[names(CrAll)=="msPAF"] <- "Chronic" # ED

  if (!is.null(groupName)) { #might be set to NULL in the call of the outer function!
    #Including aggregation by groupName
    CrGroup <- AggMsPAF(HU, HUcolumName = "HU_Chronic",
                        DevColumName = "Dev10Log_chronic",
                        LimitColumnName = ifelse(length(LimitColumnNames)>1, LimitColumnNames[2], NULL), 
                        l.AggrNames = c("SampleID", groupName)
    )
    names(CrGroup)[names(CrGroup)=="msPAF"] <- "Chronic"    
    #concat "All" and Chemical groups (from GroupName) and pivot to show both columns
    CrAll <- rbind(CrAll, CrGroup)
    CrAllWide <- reshape(CrAll, timevar = groupName,
                         direction = "wide",
                         idvar = "SampleID")
  } else {
    CrAllWide <- CrAll
  }

  ###### colourcodes #"Toxic presure classes"
  ClAll <- merge(AcAll, CrAll,
                 all = TRUE) #is in the long format
 
  if (National == "Nederlands") {
    ClAll$Class <- mapply(function(Acute, Chronic) #x en y kunnen NA hebben
      if (is.na(Acute)) { 
        if (Chronic > 0.05) {"Niet te bepalen (matig tot Zeer hoog)"} else
          if ((Chronic <= 0.05) & (Chronic > 0.005)) {"2-Gering"} else
            #          if (Chronic <= 0.005) 
          {"1-Geen"}
      } else
        if (Acute > 0.10){"5-Zeer hoog (Very high)"} else 
          if ((Acute <= 0.10) & (Acute > 0.005)){"4-Hoog (High)"} else
            if ((Acute <= 0.005) & (is.na(Chronic))){"Niet te bepalen (Matig tot gering)"} else
              if ((Acute <= 0.005) & (Chronic > 0.05)){"3-Matig (Moderate)"} else
                if ((Acute <= 0.005) & (Chronic <= 0.05) & (Chronic >0.005)){"2-Gering (Low)"} else
                  if ((Acute <= 0.005) & (Chronic <= 0.005)) {"1-Geen"} else "NA",
      ClAll$Acute, ClAll$Chronic)
    
  } else { #International 
    
    ClAll$Class <- mapply(function(Acute, Chronic) #x en y kunnen NA hebben
      if (is.na(Acute)) { 
        if (Chronic > 0.05) {"Undetermined (moderate to high)"} else
          if ((Chronic <= 0.05) & (Chronic > 0.005)) {"2-Low (Gering)"} else
            #          if (Chronic <= 0.005) 
          {"1-None (Geen)"}
      } else
        if (Acute > 0.10){"5-Very high (Zeer hoog)"} else 
          if ((Acute <= 0.10) & (Acute > 0.005)){"4-High (Hoog)"} else
            if ((Acute <= 0.005) & (is.na(Chronic))){"Undetermined (low to moderate)"} else
              if ((Acute <= 0.005) & (Chronic > 0.05)){"3-Moderate (Matig)"} else
                if ((Acute <= 0.005) & (Chronic <= 0.05) & (Chronic >0.005)){"2-Low (Gering)"} else
                  if ((Acute <= 0.005) & (Chronic <= 0.005)) {"1-None (Geen)"} else "NA",
      ClAll$Acute, ClAll$Chronic)
  }
  ClAllWide <- reshape(ClAll, timevar = groupName,
                       direction = "wide", drop = c( "Acute", "Chronic"),
                       idvar = "SampleID")
  
  return(list(
    acute = AcAllWide,
    chronic = CrAllWide,
    class = ClAllWide
  ))
}
