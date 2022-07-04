#' @title HU2msPAFs
#' @name  HU2msPAFs
#' @author jaap slootweg
#' @description process Hazard Units (HU) for HU_Calc2() to msPAF values for each Sample; each UseClass ..?
#' @param HU data.frame with HU with at least the columns CAS/AquoCode, SampleID, ...
#' @param TMOAname name of column for toxic mode of action = "PrimaryMoA", NO 
#' switched to AquoCode to force Response Addition
#' @return data.frame with msPAF values
#' @export
HU2msPAFs <- function(HU, TMOAname = "AquoCode", groupName = "groep.fotoNL"){

  if (nrow(HU) == 0) return(data.frame())
  

  AggMsPAF <- function(df, HUcolumName, DevColumName, l.AggrNames){

    stopifnot(c(HUcolumName, DevColumName, TMOAname, l.AggrNames) %in% names(df))
    #also aggregate TMOA
    AggTMOAnames <- c(l.AggrNames, TMOAname)

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
    
    #remove substances with a too low effect i.e. < 0.01%
    SomHUTMOA <- SomHUTMOA[SomHUTMOA$RemainFrac <= (1 - 0.01 / 100),]
      
    if (nrow(SomHUTMOA) == 0) return(data.frame())
    
    #effect addition; loose TMOA
    AcumsPAF <- aggregate(SomHUTMOA$RemainFrac,
                          by = SomHUTMOA[,l.AggrNames,drop = F],
                          FUN = prod)
    names(AcumsPAF)[names(AcumsPAF)=="x"] <- "RemainFrac"
    #YrGemMsPAF <- aggregate(RemainFrac~X+Y+DataSource, data = msPAF, FUN = mean)
    AcumsPAF$msPAF <- 1 - AcumsPAF$RemainFrac
    AcumsPAF
  }

  AllNames <- names(HU)

  #stopifnot()
  # AggrNames <- AllNames[!AllNames %in% c("CAS", "AquoCode", groupName,
  #                                        "ActConc", "TMOA", "Dev10Log_acute",
  #                                        "HU_acute", "PAFacute",
  #                                        "Dev10Log_chronic","HU_Chronic","PAFchronic",
  #                                        "Avg10Log_acute","Avg10Log_chronic" )]

  #Including aggregation by UseClass
  AcUseClasses <- AggMsPAF(HU, HUcolumName = "HU_acute",
                           DevColumName = "Dev10Log_acute",
                           l.AggrNames = c("SampleID", groupName))
  #and exclude to obtain all
  AcAll <- AggMsPAF(HU, HUcolumName = "HU_acute",
                     DevColumName = "Dev10Log_acute",
                     l.AggrNames = c("SampleID") # not: groupName,
                    )
  
  if (nrow(AcAll) == 0) return(data.frame())
  
  AcAll[,groupName] <- "All"
  #concat and pivot
  AcAll <- rbind(AcAll, AcUseClasses)
  names(AcAll)[names(AcAll)=="msPAF"] <- "Acute" # ED
  AcAllWide <- reshape(AcAll, timevar = groupName,
                       direction = "wide", drop = c("RemainFrac"),
                       idvar = "SampleID")

  #same for chronic
  CrUseClasses <- AggMsPAF(HU, HUcolumName = "HU_Chronic",
                           DevColumName = "Dev10Log_chronic",
                           l.AggrNames = c("SampleID", groupName))
  CrAll <- AggMsPAF(HU, HUcolumName = "HU_Chronic",
                    DevColumName = "Dev10Log_chronic",
                    l.AggrNames = c("SampleID") # not: groupName,
  )
  CrAll[,groupName] <- "All"
  CrAll <- rbind(CrAll, CrUseClasses)
  names(CrAll)[names(CrAll)=="msPAF"] <- "Chronic" #ED
  CrAllWide <- reshape(CrAll, timevar = groupName,
                       direction = "wide", drop = c("RemainFrac"),
                       idvar = "SampleID")
  
  # kleurcodes #"Toxic presure classes"
  ClAll <- merge(AcAll, CrAll,
                 by = c("SampleID", "groep.fotoNL"),
                 all = TRUE) #is in het long format
  
  ClAll$Class <- mapply(function(Acute, Chronic) #x en y kunnen NA hebben
    if (is.na(Acute)) { 
      if (Chronic > 0.05) {"Niet te bepalen (matig tot Zeer hoog)"} else
        if ((Chronic <= 0.05) & (Chronic > 0.005)) {"2-Low (Gering)"} else
#          if (Chronic <= 0.005) 
            {"1-None (Geen)"}
    } else
      if (Acute > 0.10){"5-Very high (Zeer hoog)"} else 
        if ((Acute <= 0.10) & (Acute > 0.005)){"4-High (Hoog)"} else
          if ((Acute <= 0.005) & (is.na(Chronic))){"Niet te bepalen (Matig tot gering)"} else
            if ((Acute <= 0.005) & (Chronic > 0.05)){"3-Moderate (Matig)"} else
              if ((Acute <= 0.005) & (Chronic <= 0.05) & (Chronic >0.005)){"2-Low (Gering)"} else
                if ((Acute <= 0.005) & (Chronic <= 0.005)) {"1-None (Geen)"} else "NA",
    ClAll$Acute, ClAll$Chronic)
  
  ClAllWide <- reshape(ClAll, timevar = groupName,
                       direction = "wide", drop = c("RemainFrac.x", "RemainFrac.y", "Acute", "Chronic"),
                       idvar = "SampleID")
  
  return(list(
    acute = AcAllWide,
    chronic = CrAllWide,
    class = ClAllWide 
  ))
}
