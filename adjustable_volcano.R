library(shiny)
library(dplyr)
# Define UI for dataset viewer app
ui <- fluidPage(
  
  # App title
  titlePanel("Microarray Analysis: Adjustable Volcano Plot"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      
      #                                                   INPUT VARIABLES
      
      # Input: Select a file                                      file1
      fileInput("file1", "Choose CSV File:",
                multiple = F,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      # Input: Range of logFC values                              range
      sliderInput("range", "Select range of log2 FC values:",
                  min = -6, max = 6,
                  value = c(-1,1),
                  step = 0.1),
      
      # Input: Decimal P-Value with step value                    pval       
      sliderInput("pval", "Select adjusted p-value",
                  min = 0, max = 0.5,
                  value = 0.05, step = 0.001),   
      
      # Input: Select number of rows to display                   rownum
      numericInput("rownum", "Select number of rows in table: ", 
                   value = 10 ,min = 1, max = , step = 1),
            
      # Input: Select separator                                   sep
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = "\t"),
      
      # Input: Checkbox if file has header                        header
      checkboxInput("header", "Header", TRUE)
      
    ),
    
    # Main panel for displaying outputs                    volcano, summary, table
    mainPanel(
      plotOutput("volcano"),
      verbatimTextOutput("summary"),
      tableOutput("table")
    )
  ))


# Define server logic to summarize and view selected dataset
server <- function(input, output) {
  
  # Read in data as a reactive function
  csvInput <- reactive({
    req(input$file1)
    csv = read.csv(input$file1$datapath, sep = input$sep)
  }) 
  
  # Make Volcano Plot
  output$volcano <- renderPlot({
    # Calculate significant subset with smaller p-value and larger fold change than specified
    sig = csvInput()[(csvInput()$adj.P.Val <= input$pval & (csvInput()$logFC <= min(input$range) | csvInput()$logFC >= max(input$range))), ]
    # Calculate size of significant subset for display in title
    size = length(sig[,1])
    # Create title based on above
    volctitle = paste("logFC from ", min(input$range), " to ", max(input$range), "; Adj. P.Val = ", input$pval, " ; Size: ", size, sep = "")
    # Plot volcano plot 
    plot(csvInput()$logFC, 
         -log10(csvInput()$adj.P.Val), 
         main = volctitle,
         pch="*", 
         xlab="log2 Fold Change", 
         ylab="-10log(adjusted p-value)")
    # Create dashed lines according to p-value and FC choices
    abline(h=-log10(input$pval), v=input$range, col="red", lty=2)
    # Colour significant datapoints
    points(sig$logFC, -log10(sig$adj.P.Val), col="red", pch="*") 
  })
  
  
  # Generate a summary of the significant dataset (with very ugly code)
  datasetInput <- reactive({input$file1})
  output$summary <- renderPrint({
    summary(csvInput()[(csvInput()$adj.P.Val <= input$pval & (csvInput()$logFC <= min(input$range) | csvInput()$logFC >= max(input$range))), 3:5])
  })  
  
  # Display table of genes with selected number of rows
  output$table <- renderTable({                              
    req(input$file1)
    csvInput()[(csvInput()$adj.P.Val <= input$pval & (csvInput()$logFC <= min(input$range) | csvInput()$logFC >= max(input$range))), ] %>% arrange(adj.P.Val) %>% top_n(input$rownum)
  })
}

# Create Shiny app
shinyApp(ui = ui, server = server)


