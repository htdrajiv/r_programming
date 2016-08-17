server <- shinyServer(function(input, output){
  observeEvent(input$dataFinal,{
    inputData <- input$dataFinal
    dataTable <- read.csv(inputData$datapath)
    query <- sprintf("select distinct Country as countries from dataTable")
    countries <- sqldf(query)
    output$country <- renderUI({
      selectInput("countryInput","Countries",choices = countries)
    })
    
    output$attr_x <- renderUI({
      selectInput("attr_x","x-attribute",choices = colnames(dataTable))
    })
    
    output$attr_x <- renderUI({
      selectInput("attr_y","y-attribute",choices = colnames(dataTable))
    })
    
  })

   output$main_table <- renderTable({
     inputData <- input$dataFinal
     if (is.null(inputData))
       return(NULL)
     dataTable <- read.csv(inputData$datapath)
     trimmedInputCountry <- trimws(input$countryInput, which = "both")
  
     query <- sprintf("select * from dataTable where country = '%s' ",trimmedInputCountry)
     dataByInput <- sqldf(query)
     dataByInput
   })
  
   output$plots <- renderPlot({
     inputData <- input$dataFinal
     if (is.null(inputData))
       return(NULL)
     dataTable <- read.csv(inputData$datapath)
     trimmedInputCountry <- trimws(input$countryInput, which = "both")
     attribx <- input$attr_x
     attriby <- input$attr_y
     query <- sprintf("select * from dataTable where country = '%s' ",trimmedInputCountry)
     dataByInput <- sqldf(query)
     df <- data.frame(dataByInput)
     plot(df[,eval(attribx)],df[,eval(attriby)], type = "l", xlab = attribx,
          ylab = attriby)
   })

})