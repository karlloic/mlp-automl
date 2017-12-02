ui <- fluidPage(
  
  # App title ----
  titlePanel("Download PDFs"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Choose dataset ----
      selectInput("dataset", "Choose a PDF:",
                  choices = c("EDA", "Random Forest", "Regression")),
      
      # Button
      downloadButton("downloadData", "Download")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      tableOutput("table")
      
    )
    
  )
)

server <- function(input, output) {
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "EDA" = EDA,
           "Random Forest" = Random_Forest,
           "Regression" = Regression)
  })
  
  # Table of selected dataset ----
  output$table <- renderTable({
    datasetInput()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".pdf", sep = "")
    },
    content = function(file) {
      file.copy(datasetInput(), file)
    }
  )
  
}

# Create Shiny app ----
shinyApp(ui, server)