# serve the ui 
# more documentation needed; reactives dependance?

library(shiny)
library(openxlsx)

#unfortunately, not on CRAN (no guaranteed maintainance)
#library(msPAFnoch2) #contains all major routines, calculations etc.

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  # load in the selected gross data set reactively from the global envirnment
  selectedGross <- reactive({
    get(input$gross_choice, envir = .GlobalEnv)
  })
  
  #DefChemFoto <- Gross[Gross$ABCquality %in% c("A","B") &
  #                       Gross$groep.fotoNL != "Niet meenemen",]
  #FotoReplace <- DefChemFoto[!is.na(DefChemFoto$Replace.fotoNL),c("CAS", "AquoCode", "Replace.fotoNL")]
  #FotoReplace$CASReplace <- DefChemFoto$CAS[match(FotoReplace$Replace.fotoNL,DefChemFoto$AquoCode)]
  DefChemFoto <- reactive({
    df <- selectedGross()
    df[df$ABCquality %in% c("A","B") & df$groep.fotoNL != "Niet meenemen", ]
  })
  FotoReplace <- reactive({
    df <- DefChemFoto()
    fr <- df[!is.na(df$Replace.fotoNL), c("CAS", "AquoCode", "Replace.fotoNL")]
    fr$CASReplace <- df$CAS[match(fr$Replace.fotoNL, df$AquoCode)]
    fr
  })
  #add ED. Text based on selected language, not pretty but it works. Idea: this should be output of a list. In ui selection of which part of the list
  output$Text_toolname <- renderText({
    paste0(
      Textdata[Textdata$logical_name == "manual", input$languageMenu],
      "\n \n ", get_package_version_string("msPAFcalculator",  National = input$languageMenu) #, # On a new line
    )
  })
  
  output$Text_substances <- renderText({
    Textdata[Textdata$logical_name == "substances", input$languageMenu]
  })
  
  #output$Text_manual <- renderText({
  #  Textdata[Textdata$logical_name == "manual", input$languageMenu]
  #})
  output$Text_choosefile <- renderText({
    Textdata[Textdata$logical_name == "choosefile", input$languageMenu]
  })
  output$Text_results <- renderText({
    Textdata[Textdata$logical_name == "results", input$languageMenu]
  })
  output$Text_bioAvailability <- renderText({
    Textdata[Textdata$logical_name == "BioAvailability", input$languageMenu]
  })
  output$BioAvailExplain <- renderText({
    Textdata[Textdata$logical_name == "BioAvailExplain", input$languageMenu]
  })
  
  InputList <- reactive({
    req(input$file1)
    tryCatch(
      {
        Status <- "Warnings"
        data <- leesIMformat(input$file1$datapath, National = input$languageMenu,
                     SSDbron = selectedGross(),gross_name = input$gross_choice
                     )
        # Add the additional info in warnings
        data$inputwarnings$add( if(input$languageMenu == "Nederlands"){ "Git versie" }else{ "Git version" }, 
                               nl_text = paste0("Git head van de SSDs: ", git_head), 
                               en_text =  paste0("Git head of the SSDs: ", git_head), 
                               National = input$languageMenu
                               )
        data$inputwarnings$add(if(input$languageMenu == "Nederlands"){ "Versienummer" }else{ "Versionumber" }, 
                               nl_text = get_package_version_string("msPAFcalculator",  National = input$languageMenu), 
                               en_text =  get_package_version_string("msPAFcalculator",  National = input$languageMenu), 
                               National = input$languageMenu
                               )
        updateDate <- format(file.info("server.R")$mtime, "%Y-%m-%d")
        data$inputwarnings$add("Update", 
                               nl_text = paste0("Laatste app update: ", updateDate), 
                               en_text = paste0("Last app update: ", updateDate), 
                               National = input$languageMenu
                               )
        return(data)
      },
      error = function(e) {
        # error probably (should be) in the warnings
        errorframe <- data.frame()
        return(errorframe)
      }
    )
  })
  
  #reactive, because Status
  inputwarnings <- reactive({
    req(InputList())
    
    # Safely extract with NULL checks
    warnings_df <- tryCatch(InputList()$inputwarnings$warnings, error = function(e) data.frame())
    inputData <- tryCatch(InputList()$inputData, error = function(e) data.frame())
    
    if (!is.null(warnings_df) && nrow(warnings_df) > 0 |
        !is.null(inputData) && nrow(inputData) > 0) {
      updateSelectizeInput(session, "ViewSelect", choices = {
        c("Warnings" = "Input Warnings",
          "PAFtable" = "PAF values",
          "msPAFacute" = "msPAF acute",
          "msPAFchronic" = "msPAF chronic",
          "msPAFqualitative" = "msPAF qualitative")
      },
      selected = "Input Warnings"
      )
      #this triggers PAFvalues, but fast enough for now
      ExtraWarning <- PAFvalues()
      HUWarning <- attr(ExtraWarning, "warning")
      if (length(HUWarning)==0) {
        warnings_df
      } else {
        rbind(warnings_df,
              data.frame(code = names(HUWarning), warningText = unlist(HUWarning)), 
              data.frame(code = "Bio availability", warningText = input$state_bioavailability)
        )
      }
    } else NULL
  })
  

# Bioavailability ---------------------------------------------------------

#  output$select_bioavailability <- renderUI({
#    checkboxInput('state_bioavailability', 'Bio availability?', value=TRUE)
#  })
  
  # observeEvent(input$state_bioavailability, {
  #   
  #   print(paste0("State bioavailability: ", input$state_bioavailability))
  #   
  # })
  
  
  
# PAF values --------------------------------------------------------------

  PAFvalues <- reactive({
    req(inputwarnings)
    ret <- tryCatch(
      {
        ret <- HU_Calc2(
          ToHU = InputList()$inputData,
          ChemData = DefChemFoto(),
          ChemReplace = FotoReplace(),
          muNames = c(acute = "Acute2.0Avg10LogMassTox.ug.L", chronic = "Chronic2.0Avg10LogMassTox.ug.L"),
          sigmaNames = c(acute = "Acute2.0Dev10LogMassTox.ug.L", chronic = "Chronic2.0Dev10LogMassTox.ug.L"),
          EnvData = InputList()$DataSamples,
          #aggrFUN = max,
          TooLowLimit = NULL,
          status_bioavailability=input$state_bioavailability,
        )
        return(ret)
      },
      error = function(e) {
        # error probably (should be) in the warnings
        errorframe <- data.frame()
        attr(errorframe, "warning") <- paste("No calculation possible;", e$message)
        return(errorframe)
      }
    )
    req(inputwarnings)

    return(ret)
  })
  
  msPAFvalues <- reactive({
    req(inputwarnings)
    paf_result <- PAFvalues()
    agg_result <- aggre_HU_Calc2(paf_result$PAF, aggrFUN = max, TooLowLimit = 0.0001) 
    result <- HU2msPAFs(agg_result$PAF, National = input$languageMenu)
    return(result)
  })
  
  msPAFvaluesAcute <- reactive({
    req(inputwarnings)
    msPAFvalues()$acute
  })
  
  msPAFvaluesChronic <- reactive({
    req(inputwarnings)
    msPAFvalues()$chronic
  })
  
  msPAFqualitative <- reactive({
    req(inputwarnings)
    msPAFvalues()$class
  })

  
  output$TableHeader <- renderText({
    req(inputwarnings)
    ifelse(nrow(inputwarnings()) > 0,
           input$ViewSelect,
           "")
  })
  
  output$oneTable <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, ... will be shown.
    paf_result <- PAFvalues()
    PAF <- paf_result$PAF
    # Add the excluded rows during HUcalc
    excluded_PAF <- paf_result$excluded_rows
    excluded_PAF_overlap <- excluded_PAF[,colnames(excluded_PAF)[colnames(excluded_PAF)%in%colnames(PAF)]]
    # And excluded rows during leesIMformat
    #excluded_leesIM <- InputList()$excludedData
    #excluded_leesIM_overlap <- excluded_leesIM[,colnames(excluded_leesIM)[colnames(excluded_leesIM)%in%colnames(PAF)]]
    library(dplyr)
    PAF_full <- bind_rows(PAF, excluded_PAF_overlap)
    #PAF_full <- bind_rows(PAF_full, excluded_leesIM_overlap)
    
    if(input$ViewSelect == "PAF values"){
      #PAFvalues()$PAF
      PAF_full
    }else
      if(input$ViewSelect == "msPAF acute")
        msPAFvaluesAcute() else {
          if(input$ViewSelect == "msPAF chronic") msPAFvaluesChronic() else
            if (input$ViewSelect == "msPAF qualitative") msPAFqualitative() else
            inputwarnings()            
        }
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("msPAF", gsub("\\D+", "", Sys.time()), ".xlsx", sep = "")
    },
    content = function(file) {
      wb <- createWorkbook()
      
      addWorksheet(wb=wb, sheetName = "warnings")
      writeData(wb, sheet = "warnings", inputwarnings())
      
      #export list of substances in inputdata with SSD data including leen-SSD
      leenSSD <- FotoReplace()$Replace.fotoNL[FotoReplace()$AquoCode  %in% unique(InputList()$inputData$AquoCode) |
                                              FotoReplace()$CAS %in% unique(InputList()$inputData$CAS)]
      leenSSD <- leenSSD[!is.na(leenSSD)]
      
      SSDinfo <- DefChemFot(o)[DefChemFoto()$AquoCode %in% unique(InputList()$inputData$AquoCode) |
                               DefChemFoto()$CAS %in% unique(InputList()$inputData$CAS) | 
                             DefChemFoto()$AquoCode %in% leenSSD,
                       c("AquoCode",	"CAS", "Replace.fotoNL","ABCquality","groep.fotoNL",
                         "Acute2.0Avg10LogMassTox.ug.L","Chronic2.0Avg10LogMassTox.ug.L",
                         "Acute2.0Dev10LogMassTox.ug.L","Chronic2.0Dev10LogMassTox.ug.L")]
      names(SSDinfo) <- c("AquoCode",	"CAS", "LeenSSD","SSDquality","stofgroep",
                          "log10AvgAcute","log10AvgChronic","Devlog10Acute","Devlog10Chronic")
      addWorksheet(wb=wb, sheetName = "SSDinfo")
      writeData(wb, sheet = "SSDinfo", SSDinfo)

      addWorksheet(wb=wb, sheetName = "input data")
      writeData(wb, sheet = "input data", InputList()$inputData)
      
      openxlsx::addWorksheet(wb=wb, sheetName = "ModFactors")
      OutputDataSamples <- InputList()$DataSamples
      headersMatch <- match(names(OutputDataSamples), Modifyers$ModifMODname)
      headers <- paste(names(OutputDataSamples), Modifyers$MODnameUnit[headersMatch])
      names(OutputDataSamples) <- headers
      
      openxlsx::writeData(wb, sheet = "ModFactors", OutputDataSamples)
      
      addWorksheet(wb=wb, sheetName = "PAF values")
      writeData(wb, sheet = "PAF values", PAFvalues()$PAF)

      addWorksheet(wb=wb, sheetName = "msPAF chronic")
      writeData(wb, sheet = "msPAF chronic", msPAFvaluesChronic())
      
      addWorksheet(wb=wb, sheetName = "msPAF acute")
      writeData(wb, sheet = "msPAF acute", msPAFvaluesAcute())
      
      addWorksheet(wb=wb, sheetName = "msPAF qualitative")
      writeData(wb, sheet = "msPAF qualitative", msPAFqualitative())      

      saveWorkbook(wb, file, overwrite = TRUE)
      
    }
  )
  
}