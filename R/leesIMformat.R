#' @title leesIMformat
#' @name leesIMformat
#' @author jaap slootweg
#' @description inlezen IM data, het format voor waterschappen en het informatiehuis water:
# https://www.waterkwaliteitsportaal.nl/WKP.WebApplication/Beheer/Data/Bulkdata
#' to test: source("R/ValidCAS.R"); source("R/leesIMformat.R"); data(Gross, Modifyers, UnitConversions, ModifierDefaults, OtherChar); 
#' leesIMformat("tests/testthat/testdata/SampleDataMetInt.csv", National = "English")
#' @param filename the input filename
#' @param SSDbron source of SSD / chemical data
#' @param muNames column names in SSDbron of SSD for acute = "Acute2.0Avg10LogMassTox.ug.L", chronic = "Chronic2.0Avg10LogMassTox.ug.L"
#' @param sigmaNames  names in SSDbron of SSD for acute = "Acute2.0Dev10LogMassTox.ug.L" chronic = "Chronic2.0Dev10LogMassTox.ug.L"
#' @param MolMassName default MW.g.Mol
#' @param sep field seperator character in csv file; default  (dutch) ";"
#' @param dec decimal character in csv file; default (dutch) ","
#' @param verbose on warning messages (= T)
#' @return list of
#' 1) dataframe WaterConcentrations = input concentrations, 2) DataSamples, 3) inputwarnings
#' @export
leesIMformat <- function(filename,
                         SSDbron = "Gross",
                         MolMassName = "MW.g.Mol",
                         National, verbose = T) {
  #1 preparations #####
  #warnings/messages are collected in data.frame inputwarnings; code will be used for interface
  updateDate <- format(file.info("server.R")$mtime, "%Y-%m-%d")
  inputwarnings <-
    data.frame(#making sure it exists; first line is empty
      code = "Versionnumber",
      warningText = paste("Version ", updateDate),
      stringsAsFactors = F)
  if (!("data.frame" %in% class(SSDbron))){
    SSDbron <- try(get(SSDbron))
    if (!("data.frame" %in% class(SSDbron))) {
      if (National == "Nederlands") {
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("NoSSDbron","De SSD gegevens missen als bestand")
      } else {
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("NoSSDbron","datasource for SSD's not found")
      }
      return(  list(
        inputData = data.frame(),
        DataSamples = data.frame(),
        inputwarnings = inputwarnings
      ))
    }
  }
  # read inputfile and split grootheden and parameters #####
  if ("data.frame" %in% class(filename)) {
    inputData <- filename
  } else {
    if (endsWith(filename, ".xlsx")) {
      inputData <- openxlsx::read.xlsx(filename)
    } else {
      if (National == "Nederlands") {
        sep = ";"
        dec = ","
      } else {
        sep = ","
        dec = "."
      }
      inputData <- read.csv2(file = filename, sep = sep, dec = dec, stringsAsFactors = F)
    }
  }
  
  #names(inputData)
  UsedParameters <-
    c(
      "Parameter.code",
      "Parameter.CASnummer",
      # "Parameter.groep", "Typering.code",
      "Eenheid.code",
      "Grootheid.code",
      "Limietsymbool",
      "Begindatum",
      "Resultaatdatum",
      "Meetobject.lokaalID",
      "Numeriekewaarde"
    )
  OptionalParameters <- c(
    "Alfanumeriekewaarde",
    "Hoedanigheid.code"
  )
  Eng2UsedTranslate <- list(
    Parameter.code = "Parameter.code",
    CAS = "Parameter.CASnummer",
    Unit = "Eenheid.code",
    H2OParameter = "Grootheid.code",
    Quantification = "Hoedanigheid.code",
    Limit = "Limietsymbool",
    SampleDate = "Begindatum",
    Location = "Meetobject.lokaalID",
    MeasuredValue = "Numeriekewaarde"
  )
 
  if (National != "Nederlands" && any(names(Eng2UsedTranslate) %in% names(inputData))) {
    #conform column names to Dutch IMformat
    #remember to pass the conformation
    conforms <- names(inputData)[names(inputData) %in% names(Eng2UsedTranslate)]
    #check minimal list of columns (CAS, Location, SampleDate, MeasuredValue)
    NeededColumns <- c("CAS", "Location", "SampleDate", "MeasuredValue", "Unit")
    if (!all(NeededColumns %in% names(inputData))){
      missingcolumns <- NeededColumns[!(NeededColumns %in% names(inputData))]
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("MissingColumns",warningText = do.call(paste,as.list(c("needed columns:", missingcolumns))))
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("Language Setting", "The English setting requiers a , separator and a . decimal")
      return(  list(
        inputData = data.frame(),
        DataSamples = data.frame(),
        inputwarnings = inputwarnings
      ))
    }
    names(inputData)[names(inputData) %in% names(Eng2UsedTranslate)] <- Eng2UsedTranslate[conforms]
    # only to comply to IMformat
    inputData$Resultaatdatum <- inputData$Begindatum 
    # minimally columns
    if (!"Parameter.code" %in% names(inputData)){
      inputData$Parameter.code <- ""
    }
    inputData$Parameter.code[is.na(inputData$Parameter.code)] <- ""
    
    if (!"Limietsymbool" %in% names(inputData)){
      inputData$Limietsymbool <- ""
    }
    if (!"Grootheid.code" %in% names(inputData)){
      inputData$Grootheid.code <- ""
    }
  } else conforms <- NA
  
  if(!all(UsedParameters %in% names(inputData))){
    missingcolumns <- UsedParameters[!(UsedParameters %in% names(inputData))]
    
    inputwarnings[1 + nrow(inputwarnings), ] <-
      c("Missende kolommen",warningText = do.call(paste,as.list(c("nodig zijn:", missingcolumns))))
#      ifelse (National == "Nederlands",
#      c("Missende kolommen",warningText = do.call(paste,as.list(c("nodig zijn:", missingcolumns)))),
#      c("MissingColumns",warningText = do.call(paste,as.list(c("needed columns:", missingcolumns)))))
      if(National == "Nederlands"){
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("Taal/Language","Het Nederlandse formal gebruikt ; als scheiding, en , als decimaal")
      } else {
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("Taal/Language","The international format uses , as separator and . as decimal")
      }
    return(  list(
      inputData = data.frame(),
      DataSamples = data.frame(),
      inputwarnings = inputwarnings
    ))
  }
  #Alfanumeriekewaarde is optional; avoid errors if it's missing
  if (!"Alfanumeriekewaarde" %in% names(inputData)) {
    inputData$Alfanumeriekewaarde <- ""
  }
  #correct where Alfanumeriekewaarde contains "<" and or a numeric value & Numeriekewaarde is missing
  HasBT.STAlfa <- (
    !is.na(inputData$Alfanumeriekewaarde) &
      startsWith(
        ifelse(is.na(inputData$Alfanumeriekewaarde),"",inputData$Alfanumeriekewaarde),
        prefix = "<") &
      inputData$Limietsymbool != "<"
  ) |
    (
      !is.na(inputData$Alfanumeriekewaarde) &
        startsWith(
          ifelse(is.na(inputData$Alfanumeriekewaarde),"",inputData$Alfanumeriekewaarde),
          prefix = ">") &
        inputData$Limietsymbool != ">"
    )
  if (any(HasBT.STAlfa))
    inputwarnings[1 + nrow(inputwarnings), ] <-
    c("InconsistentLimiet", "< and/or > symbol in alfanum; missing as Limietsymbool")
  AlsoNumeric <- as.numeric(inputData$Alfanumeriekewaarde)
  if (any(!is.na(AlsoNumeric) & is.na(inputData$Numeriekewaarde)))
    inputwarnings[1 + nrow(inputwarnings), ] <-
    c("InconsistentAlfaNum", "value(s) in alfanum that seems missing as Numeriekewaarde")
  
  #test numeric
  if (!is.numeric(inputData$Numeriekewaarde)) {
    #column read as character; dutch decimal comma should be "."
    inputData$Numeriekewaarde <- gsub(",", ".",inputData$Numeriekewaarde)
    inputData$Numeriekewaarde <- as.numeric(inputData$Numeriekewaarde)
  }
  
  if(anyNA(inputData$Numeriekewaarde)){
    NumNA <- length(which(is.na(inputData$Numeriekewaarde)))
    inputData <- inputData[!is.na(inputData$Numeriekewaarde),]
    
    if(National == "Nederlands") {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("Niet-Numeriek",paste("Geen Numerieke waarde, aantal rijen overgeslagen:", NumNA))
    } else {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("NonNumeric",paste("Non Numeric Numeriekewaarde, rows deleted:", NumNA))
    }
  }
  
  #"Hoedanigheid.code" is optional; avoid errors if it's missing
  if (!"Hoedanigheid.code" %in% names(inputData)) {
    inputData$Hoedanigheid.code <- ""
  } else {
    inputData$Hoedanigheid.code <- ifelse(is.na(inputData$Hoedanigheid.code),
                                          "",
                                          as.character(inputData$Hoedanigheid.code))
  }
  
  inputData$Limietsymbool <- as.character(inputData$Limietsymbool)
  outLimit <- length(which(inputData$Limietsymbool %in% c("<", ">")))
  if (outLimit > 0) {
    if (verbose)
      if (National == "Nederlands") {
        inputwarnings[1 + nrow(inputwarnings), ] <-
                  c("Buiten limiet indicatie",
                    paste(outLimit, "rijen verwijderd met Limietsymbool < or >")) 
      } else {
        inputwarnings[1 + nrow(inputwarnings), ] <-
        c("Out of limit",
          paste(outLimit, "number of rows removed with Limietsymbool < or >"))
      }
    inputData <-
      inputData[!inputData$Limietsymbool %in% c("<", ">"), c(UsedParameters, OptionalParameters)]
  }
  
  grootheden <-
    unique(inputData[, c("Grootheid.code", "Parameter.code")])
  
  #MODgrootheden <- grootheden[grootheden$Grootheid.code %in% Modifyers$ModifMODname,]
  #pecularities: T = Tw; Corg = DOC (nf-na filtering);
  if ("T" %in% grootheden$Grootheid.code &
      (!"Tw" %in% grootheden$Grootheid.code)) {
    if (National == "Nederlands"){
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("AquoCode.Tw",
          "T gevonden, geen Tw; alle T is gelezen als watertemperature (Tw)")
    } else {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("AquoCode.Tw",
          "T found but no Tw; assuming all T is temperature of water (Tw)")
    }
    inputData$Grootheid.code[inputData$Grootheid.code == "T"] <- "Tw"
  }
  if ("Corg" %in% grootheden$Parameter.code) {
    inputData$Parameter.code[inputData$Parameter.code == "Corg" &
                               endsWith(tolower(inputData$Hoedanigheid.code), suffix = "nf")] <-
      "DOC"
    if (verbose)
      if (National == "Nederlands"){
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("Corg2DOC",
            "Waarden van Corg, na filtering worden als DOC gebruikt")
      } else {
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("Corg2DOC",
            "Corg values 'na filtering' are set as modifyer DOC")
      }
  }
  if ("OS" %in% grootheden$Parameter.code) {
    inputData$Parameter.code[inputData$Parameter.code == "OS"] <- "TSS"
    if (verbose)
      if (National == "Nederlands"){
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("OS2TSS", "Waarden van OS worden als TSS gebruikt")
      }  else {
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("OS2TSS", "OS values are set as modifyer TSS")
      }
  }
  
  #yes, again, to make sure changes in inputData are in?
  #grootheden <- unique(inputData[,
  
  # date and create SampleID from location/date ##### optional in future function?
  inputData$THEdate <-
    tryCatch(
      #      as.Date(
      as.character(
        inputData$Begindatum
        #        ,tryFormats = c("%d-%m-%Y", "%Y-%m-%d", "%Y/%m/%d", "%d-%m-%y", "%m-%d-%Y", "%m/%d/%Y")
      ),
      error = function(cond)
        return(NA),
      silent = T
    )
  inputData$THEdate[is.na(inputData$THEdate)] <-
    tryCatch(
      as.character(
        #    as.Date(
        inputData$Resultaatdatum[is.na(inputData$THEdate)]
        #      ,tryFormats = c("%d-%m-%Y", "%Y-%m-%d", "%Y/%m/%d", "%d-%m-%y", "%m-%d-%Y", "%m/%d/%Y")
      ),
      error = function(cond)
        return(NA),
      silent = T
    )
  if (any(is.na(inputData$THEdate)))
    if (National == "Nederlands"){
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("NoDateData", "rijen zonder datum zijn verwijderd")
    } else {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("NoDateData", "data with a missing date are removed")
    }
  inputData <- inputData[!is.na(inputData$THEdate), ]
  #back to string to prevent mis-interpretation
  inputData$THEdate <- format(inputData$THEdate)
  #setup Samples: unique date-locations
  ToSample <- inputData[, c("Meetobject.lokaalID", "THEdate")]
  Astmp <-
    paste0(ToSample$Meetobject.lokaalID,
           ":",ToSample$THEdate)
  inputData$SampleID <- unlist(Astmp) #
  
  #modifyers & substance matching
  MODgrootheden <-
    inputData[inputData$Grootheid.code %in% Modifyers$ModifMODname, ]
  MODIons <-
    inputData[inputData$Parameter.code %in% Modifyers$ModifMODname,]
  #remove from inputData, Ions as MODifying factors are not considered as toxicant!
  inputData <-
    inputData[!(
      inputData$Grootheid.code %in% Modifyers$ModifMODname |
        inputData$Parameter.code %in% Modifyers$ModifMODname
    ), ]
  
  # prep UnitConversions ##### (loaded in Globals.R)
  
  #add the no-conversion needed for vectorisation
  AddUnitConversions <- data.frame(
    Unit_in = unique(UnitConversions$Unit_out),
    Unit_out = unique(UnitConversions$Unit_out),
    Multiply_by = 1,
    Mul_molmass = F
  )
  AllUnitConversions <- rbind (UnitConversions, AddUnitConversions)
  
  #combine the defaults, MOD & MOD  #####
  DataSamples <- data.frame(SampleID = unique(inputData$SampleID),
                            stringsAsFactors = F)
  
  # unit conversions #####
  
  #next sections to separate function?
  #possibly values in inputData #loop modifying variables for grootheid and Ions separate
  for (VarName in Modifyers$ModifMODname[Modifyers$ModifMODname %in% MODgrootheden$Grootheid.code]) {
    #VarName = Modifyers$ModifMODname[Modifyers$ModifMODname %in% MODgrootheden$Grootheid.code][1]
    RightRows <-
      which(MODgrootheden$Grootheid.code == VarName, arr.ind = T) # VarName rows in MODgrootheden (relevant part of input)
    TranslateUnits <-
      AllUnitConversions[AllUnitConversions$Unit_out == Modifyers$MODnameUnit[match(VarName, Modifyers$ModifMODname)], ]
    GivenUnits <- MODgrootheden$Eenheid.code[RightRows]
    Factors <-
      TranslateUnits[match(GivenUnits, TranslateUnits$Unit_in) , ] #order in subset of MODgrootheden  = data source
    if (anyNA(Factors$Unit_in)) {
      UnknownUnits <- unique(GivenUnits)
      UnknownUnits <-
        UnknownUnits[!UnknownUnits %in% TranslateUnits$Unit_in]
      #quote the units to make the message clear
      UnknownUnits <- paste("'", paste(UnknownUnits, sep = "','"), "'", sep = "")
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("Unknown",
          do.call(paste, as.list(c(UnknownUnits, "Unit not found for", VarName))))
      Factors$Multiply_by[is.na(Factors$Unit_in)] <- 1
      Factors$Div_molmass[is.na(Factors$Unit_in)] <- F
    }
    MatchSample <-
      match(DataSamples$SampleID, MODgrootheden$SampleID[RightRows]) #order in DataSamples = data target
    DataSamples[,VarName] <- Factors$Multiply_by[MatchSample] *
      MODgrootheden$Numeriekewaarde[RightRows][MatchSample]
    #Molmass not checked
  }
  for (VarName in Modifyers$ModifMODname[Modifyers$ModifMODname %in% MODIons$Parameter.code]) { #VarName = "Ca"
    #VarName = Modifyers$ModifMODname[Modifyers$ModifMODname %in% MODIons$Parameter.code][5]
    RightRows <- which(MODIons$Parameter.code == VarName, arr.ind = T)
    #unit conversion for this VarName
    TranslateUnits <-
      AllUnitConversions[AllUnitConversions$Unit_out == Modifyers$MODnameUnit[match(VarName, Modifyers$ModifMODname)], ]
    GivenUnits <- MODIons$Eenheid.code[RightRows]
    Factors <-
      TranslateUnits[match(GivenUnits, TranslateUnits$Unit_in) , ]  #order in MODIons  = data source
    if (anyNA(Factors$Unit_in)) {
      UnknownUnits <- unique(GivenUnits)
      UnknownUnits <-
        UnknownUnits[!UnknownUnits %in% TranslateUnits$Unit_in]
      #quote them, to make a clear message and a single string
      UnknownUnits <- paste("'",
                            do.call(paste,c(as.list(UnknownUnits), list(sep = "','"))),
                            "'")
      if (National == "Nederlands"){
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("Unknown",
            paste(
              UnknownUnits,
              "Onbekende eenheid voor",
              VarName,
              ", NIET omgerekend"
            ))
      } else {
        inputwarnings[1 + nrow(inputwarnings), ] <-
          c("Unknown",
            paste(
              UnknownUnits,
              "Unit(s) not found for",
              VarName,
              "NOT converted"
            ))
      }
      #assuming unit is correct, append
      Factors$Multiply_by[is.na(Factors$Unit_in)] <- 1
      Factors$Div_molmass[is.na(Factors$Unit_in)] <- F
    }
    MatchSample <-
      match(DataSamples$SampleID, MODIons$SampleID[RightRows])  #order in DataSamples = data target
    DataSamples[,VarName] <- Factors$Multiply_by[MatchSample] *
      MODIons$Numeriekewaarde[RightRows][MatchSample]
    if (!is.na(Modifyers$MW.g.Mol[match(VarName, Modifyers$ModifMODname)])) {
      #possibly multiply by molar mass [g/mol]
      toMul <- which(Factors$Mul_molmass[MatchSample])
      if (length(toMul) > 0) {
        DataSamples[toMul, VarName] <- DataSamples[toMul, VarName] *
          Modifyers$MW.g.Mol[match(VarName, Modifyers$ModifMODname)]
      }
    }
  }
  
  #Units of substances and  hoedanigheid N/C  nf  NF and prepare for msPAF#####
  #match substances; register with CAS but not matched
  
  #force valid CAS, but keep the original CAS code
  #NB the CAS variable will be the ultimate match to the SSD data
  MatchChar2CAS <- match(inputData$Parameter.code, OtherChar$ChemCode)
  MatchChar2CAS[is.na(MatchChar2CAS)] <- match(inputData$Parameter.CASnummer[is.na(MatchChar2CAS)], OtherChar$ChemCode)
  inputData$CAS <- OtherChar$CAS[MatchChar2CAS] #init. with OtherChar, if present. Might be NA, might be update by:
  UniqCas <- unique(inputData$Parameter.CASnummer)
  CheckCASvalidUniq <- ValidCAS(UniqCas)
  CheckCASvalid <- inputData$Parameter.CASnummer %in% UniqCas[CheckCASvalidUniq]
  inputData$CAS[CheckCASvalid & is.na(MatchChar2CAS)] <- inputData$Parameter.CASnummer[CheckCASvalid& is.na(MatchChar2CAS)]
  unused <-
    inputData[is.na(inputData$CAS), c("Parameter.code", "Parameter.CASnummer")]
  NonSSDbronCAS <- unique(inputData$CAS[!is.na(inputData$CAS) & !inputData$CAS %in% SSDbron$CAS])
  if (length(NonSSDbronCAS) > 0) {
    CASnotChemlist <- do.call(paste,as.list(unique(NonSSDbronCAS)))
    if (National == "Nederlands") {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("NAmatchedCode",
          paste (nrow(NonSSDbronCAS), paste("stoffen komen niet voor in de stoffenlijst", CASnotChemlist)))
    } else {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("NAmatchedCode",
          paste (nrow(NonSSDbronCAS), paste("substances with CAS not in chemistry list", CASnotChemlist)))
    }
  }

  #remove all non substance measurements
  inputData <- inputData[!is.na(inputData$CAS), ]
  inputData <- inputData[!inputData$CAS %in% NonSSDbronCAS, ]

  #unit conversion for the toxic substances
  TranslateUnits <-
    AllUnitConversions[AllUnitConversions$Unit_out == "ug/l", ] #the unit in SSD's
  GivenUnits <- inputData$Eenheid.code
  Factors <-
    TranslateUnits[match(GivenUnits, TranslateUnits$Unit_in) , ]
  if (anyNA(Factors$Unit_in)) {
    UnknownUnits <- unique(GivenUnits)
    UnknownUnits <-
      UnknownUnits[!UnknownUnits %in% TranslateUnits$Unit_in]
    UnknownUnits <- paste("'",
                          do.call(paste,c(as.list(UnknownUnits), list(sep = "','"))),
                          "'")
    if (National == "Nederlands") {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("Unknown",
          paste(
            UnknownUnits,
            "onbekende concentratie eenheid; De rijen zijn verwijderd"
          ))
    } else {
      inputwarnings[1 + nrow(inputwarnings), ] <-
        c("Unknown",
          paste(
            UnknownUnits,
            " unknown concentration unit(s); row(s) deleted"
          ))
    }
    inputData <- inputData[!is.na(Factors$Unit_in),]
    Factors <- Factors[!is.na(Factors$Unit_in),]
  }
  
  #Match to SSD data source
  MatchChem <- match(inputData$CAS, SSDbron$CAS)
  inputData$Concentration <- inputData$Numeriekewaarde * Factors$Multiply_by
  toMul <- which(Factors$Mul_molmass)
  if (length(toMul) > 0) {
    inputData$Concentration[toMul] <- inputData$Concentration[toMul] *
      SSDbron[MatchChem[toMul],MolMassName]
  }
  inputData$NaFiltering <- endsWith(inputData$Hoedanigheid.code, "nf")
  AsN <- which(inputData$Hoedanigheid.code %in% c("N", "Nnf")) # correct to molmass of whole molecule
  if (length(AsN)>0) {
    ATOMMASSN <- 14.0067
    inputData$Concentration[AsN] <- inputData$Concentration[AsN] / ATOMMASSN * SSDbron[MatchChem[AsN],MolMassName]
  }
  AsP <- which(inputData$Hoedanigheid.code %in% c("P", "Pnf")) # correct to molmass of whole molecule
  if (length(AsP)>0) {
    ATOMMASSP <- 30.973762
    inputData$Concentration[AsP] <- inputData$Concentration[AsP] / ATOMMASSP * SSDbron[MatchChem[AsP],MolMassName]
  }
  
  #with defaults #from the one row in ModifierDefaults
  if ("TSS" %in% names(DataSamples))
    DataSamples$TSS[is.na(DataSamples$TSS)] <-
    ModifierDefaults$`TSS(mg/L)`[1] else #new column
      DataSamples$TSS <- ModifierDefaults$`TSS(mg/L)`[1]
  if ("POC" %in% names(DataSamples))
    DataSamples$POC[is.na(DataSamples$POC)] <-
    ModifierDefaults$`POC(mg/kg)`[1] else #new column
      DataSamples$POC <- ModifierDefaults$`POC(mg/kg)`[1]
  if ("DOC" %in% names(DataSamples))
    DataSamples$DOC[is.na(DataSamples$DOC)] <-
    ModifierDefaults$`DOC(mg/L)`[1] else #new column
      DataSamples$DOC <- ModifierDefaults$`DOC(mg/L)`[1]
  if ("pH" %in% names(DataSamples))
    DataSamples$pH[is.na(DataSamples$pH)] <-
    ModifierDefaults$`pH(units)`[1] else #new column
      DataSamples$pH <- ModifierDefaults$`pH(units)`[1]
  if ("Tw" %in% names(DataSamples))
    DataSamples$Tw[is.na(DataSamples$Tw)] <-
    ModifierDefaults$`Tw(oC)`[1] else #new column
      DataSamples$Tw <- ModifierDefaults$`Tw(oC)`[1]
  if ("Ca" %in% names(DataSamples))
    DataSamples$Ca[is.na(DataSamples$Ca)] <-
    ModifierDefaults$`Ca(ug/L)`[1] else #new column
      DataSamples$Ca <- ModifierDefaults$`Ca(ug/L)`[1]
  if ("Mg" %in% names(DataSamples))
    DataSamples$Mg[is.na(DataSamples$Mg)] <-
    ModifierDefaults$`Mg(ug/L)`[1] else #new column
      DataSamples$Mg <- ModifierDefaults$`Mg(ug/L)`[1]
  if ("Na" %in% names(DataSamples))
    DataSamples$Na[is.na(DataSamples$Na)] <-
    ModifierDefaults$`Na(ug/L)`[1] else #new column
      DataSamples$Na <- ModifierDefaults$`Na(ug/L)`[1]
  if ("Cl" %in% names(DataSamples))
    DataSamples$Cl[is.na(DataSamples$Cl)] <-
    ModifierDefaults$`Cl(ug/L)`[1] else #new column
      DataSamples$Cl <- ModifierDefaults$`Cl(ug/L)`[1]
  
  #for easier name handling in package
  names(inputData)[names(inputData)=="Parameter.code"] <- "ChemCode"
  if ("MeasuredValue" %in% names(inputData)) {
    inputData$MeasuredValue <- NULL #free name for Concentration column
  }
  names(inputData)[names(inputData)=="Concentration"] <- "MeasuredValue"
  inputData$PreTreatment <- ifelse(inputData$NaFiltering,"nf","")
  attr(inputData, "conforms") <- conforms
  
  #return
  list(
    inputData = inputData,
    DataSamples = DataSamples,
    inputwarnings = inputwarnings
  )
}


