# Define UI for data upload app ----
ui <- fluidPage(
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
#      style = 
#      "position:fixed;
#      overflow-y: scroll;
#      width:inherit;",
      h1(Textdata[1], style = "font-size:16px;"),
      p(Textdata[2], style = "font-size:12px;"),

      # Input: Select a file ----
      fileInput("file1", Textdata[3],
                multiple = FALSE,
                accept = c(
#                  "text/csv",
#                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      selectizeInput("ViewSelect", Textdata[4], c("select an inputfile")),
      
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
