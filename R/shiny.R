
rm(list = ls())

shinyApp(
  shinyUI(
    fluidPage(
      mainPanel(
        
        tabsetPanel(
          tabPanel("Upload File", 
                   fileInput("file1", "Choose CSV File",
                             multiple = TRUE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")),
                           tableOutput("contents")
                   ),
          
          tabPanel("EDA",
                   # fluidRow(...)
                   tableOutput("UnStats"),
                   plotOutput("SctrPlot"),
                   plotOutput("BoxPlot"),
                   plotOutput("CorrPlot")
                ),
          
          tabPanel("ModelOutput",
                   # fluidRow(...)
                   tableOutput("ModelStats")
          )
        )
      )
    )
  ),
  
  shinyServer(function(input, output) {
    
    output$contents <- renderTable({
      
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      
      req(input$file1)
      
      df <- read.csv(input$file1$datapath)
      
      return (df)
    })
    
    
    print(head(df))
    
    output$UnStats <- renderTable({
      
      
      df <- read.csv(input$file1$datapath)
      
      source("C:/Users/rmadala/Documents/Code/Shiny/CalcDscrStatsInt.R")
      
      print("calculating stats")
      UnStats <-  CalcDscrStatsInt(df)
      
      print("calculated stats")
      
      print(UnStats)
      
      return (UnStats)
      
    })
    
    
    
    library(dplyr)
    
    
    output$SctrPlot <- renderPlot({
      
      print("Inside Pair Plots")
      
      df <- read.csv(input$file1$datapath)
      
      dataNmr <- select_if(df, is.numeric)
      pairs(dataNmr)
       
    })
    
    
    output$BoxPlot <- renderPlot({
      
      print("Inside Box Plots")
      
      df <- read.csv(input$file1$datapath)
      
      dataNmr <- select_if(df, is.numeric)
      boxplot(dataNmr)
      
    })
    
    output$CorrPlot <- renderPlot({
      
      library(corrplot)
      
      print("Inside Corr Plots")
      
      df <- read.csv(input$file1$datapath)
      
      dataNmr <- select_if(df, is.numeric)
      corVal <- cor(dataNmr)
      corrplot(corVal, method = "number")
      
    })
    
    
    output$ModelStats <- renderTable({
      
      print("RUnning Model'")
      
      source("C:/Users/rmadala/Documents/Code/Shiny/SplitData.R")
      source("C:/Users/rmadala/Documents/Code/Shiny/trainModel.R")
      source("C:/Users/rmadala/Documents/Code/Shiny/predictModel.R")
      source("C:/Users/rmadala/Documents/Code/Shiny/modelEval.R")
    
      df <- read.csv(input$file1$datapath)

      colnames(df)[ncol(df)] <- "Y"
      
      print(head(df))
      
      TrTsLst <- SplitData(df,0.8)
      
      print(TrTsLst)
      
      train <- TrTsLst$train
      test <- TrTsLst$test
      
      print(dim(train))
      print(dim(test))
      

      mfit <- LrModel(train)
      
      print(mfit)
      
      Pdf <- lrPredict(mfit,test)
      
      
      print(head(Pdf))
      
      ModelStats <- modelEval(Pdf,"LinearRegression")
      
      return(ModelStats)
      
    })
    
    
    
  })  

)



