# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Welcome to AutoML!"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      h2("Make your Choice: "),

      br(),
      br(),

      checkboxInput('eda', 'Show EDA', TRUE),
      checkboxInput('build', 'Build Model', TRUE),
      checkboxInput('display', 'Show Model', TRUE),

      # Main panel for displaying outputs ----
      mainPanel(

        # Output: Data file ----
        tableOutput("contents"),

        tags$a(href="www.google.com", "Proceed to the Processing")

      )

    )
  )
  
