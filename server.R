# serve the ui 
# more documentation needed; reactives dependance?

library(shiny)
library(openxlsx)

#unfortunately, not on CRAN (no guaranteed maintainance)
#library(msPAFnoch2) #contains all major routines, calculations etc.

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  DefChemFoto <- Gross[Gross$ABCquality %in% c("A","B") &
                         Gross$groep.fotoNL != "Niet meenemen",]
  FotoReplace <- DefChemFoto[!is.na(DefChemFoto$Replace.fotoNL),c("CAS", "AquoCode", "Replace.fotoNL")]
  FotoReplace$CASReplace <- DefChemFoto$CAS[match(FotoReplace$Replace.fotoNL,DefChemFoto$AquoCode)]

  #add ED. Text based on selected language, not pretty but it works. Idea: this should be output of a list. In ui selection of which part of the list
  output$Text_toolname <- renderText({
    Textdata[Textdata$logical_name == "toolname", input$languageMenu]
  })
  output$Text_manual <- renderText({
    Textdata[Textdata$logical_name == "manual", input$languageMenu]
  })
  output$Text_choosefile <- renderText({
    Textdata[Textdata$logical_name == "choosefile", input$languageMenu]
  })
  output$Text_results <- renderText({
    Textdata[Textdata$logical_name == "results", input$languageMenu]
  })
  output$Text_bioAvailability <- renderText({
    Textdata[Textdata$logical_name == "BioAvailability", input$languageMenu]
  })
  
  InputList <- reactive({
    req(input$file1)
    tryCatch(
      {
        Status <- "Warnings"
        leesIMformat(input$file1$datapath, National = input$languageMenu)
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
    if (nrow(InputList()$inputwarnings) > 0 |
        nrow(InputList()$inputData) > 0) {
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
      if (is.null(HUWarning)) {
        InputList()$inputwarnings
      } else {
        rbind(InputList()$inputwarnings,
              data.frame(code = "CalculationWarnings", warningText = HUWarning), 
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
        HU_Calc2(
          ToHU = InputList()$inputData,
          ChemData = DefChemFoto,
          ChemReplace = FotoReplace,
          muNames = c(acute = "Acute2.0Avg10LogMassTox.ug.L", chronic = "Chronic2.0Avg10LogMassTox.ug.L"),
          sigmaNames = c(acute = "Acute2.0Dev10LogMassTox.ug.L", chronic = "Chronic2.0Dev10LogMassTox.ug.L"),
          EnvData = InputList()$DataSamples,
          aggrFUN = max,
          status_bioavailability=input$state_bioavailability,
        )
      },
      error = function(e) {
        # error probably (should be) in the warnings
        errorframe <- data.frame()
        attr(errorframe, "warning") <- "No calculation possible"
        return(errorframe)
      }
    )
    ret
    
  })
  
  msPAFvalues <- reactive({
    req(inputwarnings)
    HU2msPAFs(PAFvalues())
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
    
    if(input$ViewSelect == "PAF values")
      PAFvalues() else
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
      leenSSD <- FotoReplace$Replace.fotoNL[FotoReplace$AquoCode  %in% unique(InputList()$inputData$AquoCode) |
                                              FotoReplace$CAS %in% unique(InputList()$inputData$CAS)]
      leenSSD <- leenSSD[!is.na(leenSSD)]
      
      SSDinfo <- DefChemFoto[DefChemFoto$AquoCode %in% unique(InputList()$inputData$AquoCode) |
                               DefChemFoto$CAS %in% unique(InputList()$inputData$CAS) | 
                             DefChemFoto$AquoCode %in% leenSSD,
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
      openxlsx::writeData(wb, sheet = "ModFactors", InputList()$DataSamples)
      
      addWorksheet(wb=wb, sheetName = "PAF values")
      writeData(wb, sheet = "PAF values", PAFvalues())
      
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