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
      tags$style(HTML("#Text_manual { white-space: pre-line; }")), #Otherwise it ignores the /n in the renderText
      p(textOutput("Text_manual"), style = "font-size:12px;"), #ED
      
      # Choose a Gross dataset
      selectInput(
        "gross_choice",
        label = textOutput("Text_substances"),
        choices = gross_choices 
      ),

      # Input: Select a file ----
      fileInput("file1", textOutput("Text_choosefile"), #ED
                multiple = FALSE,
                accept = c(".csv", ".xlsx",".zip")),
      
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
    
  ),
  
  # Fix for iframe scroll issue - hide file input to prevent focus scroll
  tags$script(
    HTML(
      'setTimeout(() => $(".shiny-bound-input[type=\'file\']").css("all","unset").css("display", "none"), 750);'
    )
  )
)
