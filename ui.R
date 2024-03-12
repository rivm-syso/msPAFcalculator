# Define UI for data upload app ----
ui <- fluidPage(

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput(
        "languageMenu", "Taal/Language", c("Nederlands", "English")
      ),

      h1(textOutput("Text_toolname"), style = "font-size:16px;"), #ED
      p(textOutput("Text_manual"), style = "font-size:12px;"), #ED

      # Input: Select a file ----
      fileInput("file1", textOutput("Text_choosefile"), #ED
                multiple = FALSE,
                accept = c(".csv", ".xlsx")),
      
      p(textOutput("BioAvailExplain"), style = "font-size:12px;"),
      checkboxInput("state_bioavailability", textOutput("Text_bioAvailability"), value = TRUE), #select_bioavailability
      
      selectizeInput("ViewSelect", textOutput("Text_results"), c("select an inputfile")), #ED
      
      downloadButton("downloadData", "Download"),

      #max width, standaard is 4/12, nu 3/12
      width = 3
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      textOutput("TableHeader"),
      # Output: Data file ----
      tableOutput("oneTable")
      
    )
    
  )
)
