#unfortunately, not on CRAN (no guaranteed maintainance)
#library(msPAFnoch2) #contains all major routines, calculations etc.

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  # load in the selected gross data set reactively from the global envirnment
  selectedGross <- reactive({
    SSDbron <- get(input$gross_choice, envir = .GlobalEnv)
    # Handle case where SSDbron is a list of dataframes
    if ("list" %in% class(SSDbron) && !"data.frame" %in% class(SSDbron)) {
      SSDbron <- SSDbron[[input$gross_choice]]
    }
    SSDbron
  })
  
  #DefChemFoto <- Gross[Gross$ABCquality %in% c("A","B") &
  #                       Gross$groep.fotoNL != "Niet meenemen",]
  DefChemFoto <- reactive({
    df <- selectedGross()
    idx <- which(df$ABCquality %in% c("A","B") & df$groep.fotoNL != "Niet meenemen")
    result <- df[idx, , drop = FALSE]
    # Add substance_key if not present (required by CleanFase2)
    if (!"substance_key" %in% names(result)) {
      result$substance_key <- paste0("CAS:", result$CAS)
    }
    result
  })
  
  # Track language changes to invalidate old data
  dataLanguage <- reactiveVal(NULL)
  
  # Reset file input when language changes to prevent mismatches
  observeEvent(input$languageMenu, {
    if (!is.null(input$file1)) {
      showNotification(
        if(input$languageMenu == "Nederlands") {
          "Taal gewijzigd. Upload uw bestand opnieuw."
        } else {
          "Language changed. Please re-upload your file."
        },
        type = "warning",
        duration = 10
      )
      # Reset the file input
      shinyjs::reset("file1")
      dataLanguage(NULL)  # Invalidate data language
    }
  }, ignoreInit = TRUE)
  
  # Track the language when file is uploaded
  observeEvent(input$file1, {
    if (!is.null(input$file1)) {
      dataLanguage(input$languageMenu)
    }
  })
  
  #add ED. Text based on selected language, not pretty but it works. Idea: this should be output of a list. In ui selection of which part of the list
  output$Text_toolname <- renderText({
    paste0(
      Textdata[Textdata$logical_name == "manual", input$languageMenu],
      #Git version
      "\n \n ", if("msPAFcalculator"%in%rownames(installed.packages())){
        get_package_version_string("msPAFcalculator",  National = input$languageMenu)
        }else{
          if(!is.null(app_git_head)){
          paste0("msPAFcalculator app git commit: ",app_git_head)
          }else{
            if(file.exists("/app/DESCRIPTION")){
              desc <- read.dcf("/app/DESCRIPTION")
              package_version <- desc[1, "Version"]
              paste0("msPAFcalculator version: ", package_version)
            }else{
              ""
            }
          }
        } #, # On a new line
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
        file_to_read <- input$file1$datapath
        if (grepl("\\.zip$", tolower(file_to_read))) { # if it is a zip, it can be 1 or multiple files
          zipfile <- unzip(file_to_read, list = TRUE)$Name
          if (length(zipfile)>1){
            # If there are multiple files, we need IMformatMultiple
            data <- IMformatMultiple(zipFile = file_to_read, National = input$languageMenu,
                                 SSDbron = selectedGross()
            )
          } else { #if it is 1 file, use the default leesIMformat
            data <- leesIMformat(file_to_read, National = input$languageMenu,
                                 SSDbron = selectedGross(),gross_name = input$gross_choice
            )
          }
        } else{
          data <- leesIMformat(file_to_read, National = input$languageMenu,
                               SSDbron = selectedGross(),gross_name = input$gross_choice
          )
        }


        # Convert inputwarnings dataframe back to R6 object so we can add warnings
        data$inputwarnings <- InputWarnings$new(data$inputwarnings)
        
        # Add the additional info in warnings
        data$inputwarnings$add( if(input$languageMenu == "Nederlands"){ "Git versie" }else{ "Git version" }, 
                               nl_text = paste0("Git head van de SSDs: ", git_head), 
                               en_text =  paste0("Git head of the SSDs: ", git_head), 
                               National = input$languageMenu
                               )
        data$inputwarnings$add(if(input$languageMenu == "Nederlands"){ "Versienummer" }else{ "Versionumber" }, 
                               nl_text = ifelse(!is.null(app_git_head),
                                                    paste0("msPAFcalculator app git commit: ", app_git_head),
                                                    ifelse(file.exists("/app/DESCRIPTION"),
                                                           paste0("msPAFcalculator versie: ", read.dcf("/app/DESCRIPTION")[1, "Version"]),
                                                           get_package_version_string("msPAFcalculator",  National = input$languageMenu)
                                                           )),
                               en_text =  ifelse(!is.null(app_git_head),
                                                 paste0("msPAFcalculator app git commit: ", app_git_head),
                                                 ifelse(file.exists("/app/DESCRIPTION"),
                                                        paste0("msPAFcalculator version: ", read.dcf("/app/DESCRIPTION")[1, "Version"]),
                                                        get_package_version_string("msPAFcalculator",  National = input$languageMenu)
                                                 )),
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
    req(dataLanguage() == input$languageMenu)  # Ensure data matches current language
    
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
      
      # Accumulate warnings from CleanFase2
      cleanWarnings <- tryCatch(CleanedData()$inputwarnings, error = function(e) data.frame())
      if (!is.null(cleanWarnings) && nrow(cleanWarnings) > 0) {
        warnings_df <- rbind(warnings_df, cleanWarnings)
      }
      
      # Accumulate warnings from PAFvalues
      ExtraWarning <- PAFvalues()
      HUWarning <- attr(ExtraWarning, "warning")
      if (length(HUWarning) > 0) {
        warnings_df <- rbind(warnings_df,
                             data.frame(code = names(HUWarning), warningText = unlist(HUWarning)), 
                             data.frame(code = "Bio availability", warningText = input$state_bioavailability))
      }
      
      # Remove duplicate warnings
      unique(warnings_df)
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
  
  
  # Step 2: Clean data ------------------------------------------------------

  CleanedData <- reactive({
    req(InputList())
    req(dataLanguage() == input$languageMenu)  # Ensure data matches current language
    CleanFase2(
      inputData = InputList()$inputData,
      SSDsubstanceData = DefChemFoto(),
      init_inputwarnings = InputList()$inputwarnings$warnings,
      National = input$languageMenu
    )
  })
  
  
# Step 3: Calculate PAF values --------------------------------------------

  PAFvalues <- reactive({
    req(CleanedData())
    ret <- tryCatch(
      {
        ret <- HU_Calc2(
          ToHU = CleanedData()$inputData,
          ChemData = DefChemFoto(),
          muNames = c(acute = "Acute2.0Avg10LogMassTox.ug.L", chronic = "Chronic2.0Avg10LogMassTox.ug.L"),
          sigmaNames = c(acute = "Acute2.0Dev10LogMassTox.ug.L", chronic = "Chronic2.0Dev10LogMassTox.ug.L"),
          EnvData = InputList()$DataSamples,
          National = input$languageMenu,
          status_bioavailability = input$state_bioavailability,
          TooLowLimit = NULL
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

    return(ret)
  })
  
  
# Step 4: Calculate zero PAFs ---------------------------------------------

  ZeroPAFs <- reactive({
    req(PAFvalues())
    zeroPAFs(
      asIM = InputList(),
      ChemData = DefChemFoto(),
      HU_results = PAFvalues()
    )
  })
  
  
# Step 5: Aggregate to msPAF ----------------------------------------------

  msPAFvalues <- reactive({
    req(PAFvalues(), ZeroPAFs())
    
    agg_result <- aggre_HU_Calc2(
      CalcedHU = PAFvalues()$PAF,
      ChemData = DefChemFoto(),
      zeros_PAF = ZeroPAFs(),
      aggrFUN = max,
      agg_jaar = FALSE,
      National = input$languageMenu
    )
    
    #result <- HU2msPAFs(agg_result$PAF, National = input$languageMenu)
    #return(result)
    return(agg_result)
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
  
  output$filterControl <- renderUI({
    req(inputwarnings)
    if(input$ViewSelect == "PAF values") {
      input_text <- ifelse(input$languageMenu == "Nederlands", "Toon concentratie waardes:", "Show concentration values:")
      input_choices1 <- ifelse(input$languageMenu == "Nederlands", "Alle waardes", "All values")
      input_choices2 <- ifelse(input$languageMenu == "Nederlands", "Sluit nul-waardes uit", "Exclude zeros")
      
      # Create named vector dynamically
      choices_vec <- c("all", "nonzero")
      names(choices_vec) <- c(input_choices1, input_choices2)
      
      radioButtons("filterZeros", input_text,
                   choices = choices_vec,
                   selected = "all",
                   inline = TRUE)
    }
  })
  
  output$oneTable <- renderDT({
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
      #PAF_full
      
      # Filter based on radio button selection
      if(!is.null(input$filterZeros) && input$filterZeros == "nonzero") {
        PAF_full <- PAF_full[PAF_full$Concentration != 0, ]
      }
      
      PAF_full$substance_key <- as.factor(PAF_full$substance_key)
      PAF_full$UseClass <- as.factor(PAF_full$UseClass)
      PAF_full$Meetobject.lokaalID <- as.factor(PAF_full$Meetobject.lokaalID)
      PAF_full$groep.fotoNL <- as.factor(PAF_full$groep.fotoNL)
      #PAF_full$PrimaryMoA <- as.factor(PAF_full$PrimaryMoA)
      PAF_full$SampleID <- as.factor(PAF_full$SampleID)
      PAF_full$ExclusionReason <- as.factor(PAF_full$ExclusionReason)
      #PAF_full$THEdate<-as.Date(as.numeric(PAF_full$THEdate), origin = "1899-12-30")
      if("PrimaryMoA" %in% colnames(PAF_full)){
        PAF_full <- PAF_full[,-which(colnames(PAF_full) == "PrimaryMoA")]
      }
      #fix dates
      if("THEdate"%in%colnames(PAF_full)){
        PAF_full$THEdate<-as.Date(as.numeric(PAF_full$THEdate), origin = "1899-12-30")
      }
      datatable(
        PAF_full, 
        rownames = FALSE,
        filter = "top",
        extensions = 'Scroller',
        options = list(dom = 't',
                       pageLength =-1,
                       order = list(list(2, 'asc')), # = Meetobject.lokaal.ID #DT counts starting from 0
                       deferRender = TRUE,
                       scrollY = 700,
                       scrollX = TRUE,
                       scroller = TRUE
                       )
        )
      
    }else{
      if(input$ViewSelect == "msPAF acute"){
        datatable(
          msPAFvaluesAcute(),
          rownames = FALSE,
          #filter = "top",
          extensions = 'Scroller',
          options = list(dom = 't',
                         pageLength =-1,
                         deferRender = TRUE,
                         scrollY = 700,
                         scrollX = TRUE,
                         scroller = TRUE
          )
        )
        }else {
          if(input$ViewSelect == "msPAF chronic"){
            datatable(
              msPAFvaluesChronic(),
              rownames = FALSE,
              extensions = 'Scroller',
              #filter = "top",
              options = list(dom = 't',
                             pageLength =-1,
                             deferRender = TRUE,
                             scrollY = 700,
                             scrollX = TRUE,
                             scroller = TRUE
              )
            )
            }else{
            if (input$ViewSelect == "msPAF qualitative"){
              datatable(
                msPAFqualitative(),
                rownames = FALSE,
                #filter = "top",
                extensions = 'Scroller',
                options = list(dom = 't',
                               pageLength =-1,
                               deferRender = TRUE,
                               scrollY = 700,
                               scrollX = TRUE,
                               scroller = TRUE
                )
              )
              }else{
                datatable(
                  inputwarnings(),
                  rownames = FALSE,
                  #filter = "top",
                  options = list(dom = 't',
                                 pageLength =-1
                  )
                )
              }
            }
        }
    }
  }, server = TRUE)
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("msPAF", gsub("\\D+", "", Sys.time()), ".xlsx", sep = "")
    },
    content = function(file) {
      wb <- createWorkbook()
      
      addWorksheet(wb=wb, sheetName = "warnings")
      writeData(wb, sheet = "warnings", inputwarnings())
      
      # Export list of substances in inputdata with SSD data
      xx<<-DefChemFoto()
      yy<<-InputList()
      SSDinfo <- DefChemFoto()[DefChemFoto()$substance_key %in% unique(InputList()$inputData$substance_key),
                       c("AquoCode",	"CAS", "Replace.fotoNL","ABCquality","groep.fotoNL",
                         "Acute2.0Avg10LogMassTox.ug.L","Chronic2.0Avg10LogMassTox.ug.L",
                         "Acute2.0Dev10LogMassTox.ug.L","Chronic2.0Dev10LogMassTox.ug.L")]
      names(SSDinfo) <- c("AquoCode",	"CAS", "Replace.fotoNL","SSDquality","stofgroep",
                          "log10AvgAcute","log10AvgChronic","Devlog10Acute","Devlog10Chronic")
      addWorksheet(wb=wb, sheetName = "SSDinfo")
      writeData(wb, sheet = "SSDinfo", SSDinfo)

      addWorksheet(wb=wb, sheetName = "input data")
      input_data<-InputList()$inputData
      #fix dates
      if("Resultaatdatum"%in%colnames(input_data)){
        input_data$Resultaatdatum<-as.Date(as.numeric(input_data$Resultaatdatum), origin = "1899-12-30")
      }
      if("Begindatum"%in%colnames(input_data)){
        input_data$Begindatum<-as.Date(as.numeric(input_data$Begindatum), origin = "1899-12-30")
      }
      if("THEdate"%in%colnames(input_data)){
        input_data$THEdate<-as.Date(as.numeric(input_data$THEdate), origin = "1899-12-30")
      }
      writeData(wb, sheet = "input data", input_data)
      
      openxlsx::addWorksheet(wb=wb, sheetName = "ModFactors")
      OutputDataSamples <- InputList()$DataSamples
      headersMatch <- match(names(OutputDataSamples), Modifyers$ModifMODname)
      headers <- paste(names(OutputDataSamples), Modifyers$MODnameUnit[headersMatch])
      names(OutputDataSamples) <- headers
      
      openxlsx::writeData(wb, sheet = "ModFactors", OutputDataSamples)
      
      addWorksheet(wb=wb, sheetName = "PAF values")
      PAF_full <- PAFvalues()$PAF
      if("PrimaryMoA" %in% colnames(PAF_full)){
        PAF_full <- PAF_full[,-which(colnames(PAF_full) == "PrimaryMoA")]
      }
      #fix dates
      if("THEdate"%in%colnames(PAF_full)){
        PAF_full$THEdate<-as.Date(as.numeric(PAF_full$THEdate), origin = "1899-12-30")
      }
      writeData(wb, sheet = "PAF values", PAF_full)

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