library(shiny)
library(rsconnect)

ui <- fluidPage(
  
  
 titlePanel("Simple Linear Regression"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selecttype",label = "Select Type:",choices = c('Simple Linear Regression','None'),selected = 'Simple Linear Regression'),
      fileInput('file',label = 'Upload File',placeholder = 'Upload file from here',accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv")),
      uiOutput(outputId = 'depvar'),
      conditionalPanel(condition = "input.selecttype == 'Simple Linear Regression'",
      uiOutput(outputId = 'indvar'))
  ),
    
    mainPanel(
      conditionalPanel(condition = "input.selecttype == 'Simple Linear Regression'",
                       titlePanel("Data from CSV"),
                       verbatimTextOutput("data"),
                       titlePanel("Plot between dependent and independent variables"),
                       plotOutput("plt"),
                       titlePanel("Correlation Coefficient and Test"),
                       verbatimTextOutput("correlationcoeff"),
                       verbatimTextOutput("kptest"),
                       titlePanel("Linear Model and summarisation"),
                       verbatimTextOutput("linearmodel"),
                       verbatimTextOutput("summarymodel"),
                       titlePanel("Regression Equation"),
                       verbatimTextOutput("regeq"))
    )
  )
)

server <- function(input, output) {
  inputdata <- reactive({read.csv(input$file$datapath)})
  
  removeNAvar <- function(f){
    
    return(f[!is.na(rowMeans(f)),])
  }
  output$depvar = renderUI(selectInput(inputId = 'dep',label = 'Dependent Variable',choices = names(inputdata())) )
  output$indvar = renderUI(selectInput(inputId = 'ind',label = 'Independent Variable',choices = names(inputdata()[!names(inputdata())==input$dep])))
  output$plt = renderPlot({
    x<-inputdata()[,input$ind]
    y<-inputdata()[,input$dep]
    plot(x,y,main="Scatter Plot")
  })
  
  output$data = renderPrint({
    x<-inputdata()[,input$ind]
    y<-inputdata()[,input$dep]
    df <- data.frame(x,y)
    df
  })
  
  output$correlationcoeff = renderPrint({
    x<-inputdata()[,input$ind]
    y<-inputdata()[,input$dep]
    df <- data.frame(x,y)
    df <- removeNAvar(df)
    cor(df[,1],df[,2])
  })
  
  output$kptest = renderPrint({
    x <- inputdata()[,input$ind]
    y <- inputdata()[,input$dep]
    df <- data.frame(x,y)
    df <- removeNAvar(df)
    cor.test(df[,1],df[,2])
  })
  
  output$linearmodel = renderPrint({
    x<-inputdata()[,input$ind]
    y<-inputdata()[,input$dep]
    lm(y~x)
  })
  
  output$summarymodel = renderPrint({
    x<-inputdata()[,input$ind]
    y<-inputdata()[,input$dep]
    summary(lm(y~x))
  })
  
  output$regeq = renderPrint({
    x<-inputdata()[,input$ind]
    y<-inputdata()[,input$dep]
    eq <-(lm(y~x))
    a <- eq$coefficients[1]
    b <- eq$coefficients[2]
    cat(sprintf("Y=%f + %f*X",a,b))
  })
  
}

shinyApp(ui = ui, server = server)

