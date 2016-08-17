server <- shinyServer(function(input, output){
  
  observeEvent(input$dataFinal,{
    inputData <- input$dataFinal
    dataTable <- read.csv(inputData$datapath)
    queryCountries <- sprintf("select distinct Country as countries from dataTable")
    queryMinMaxYear <- sprintf("select max(year) as maxVal, min(year) as minVal from dataTable")
    countries <- sqldf(queryCountries)
    maxMin <- sqldf(queryMinMaxYear)
    output$country <- renderUI({
      selectInput("countryInput","Countries",choices = countries)
    })
    
  })
  
  observeEvent(input$plotType,{
    output$target <- renderUI({})
    output$inputs <- renderUI({})
    inputData <- input$dataFinal
    if (is.null(inputData))
      return(NULL)
    dataTable <- read.csv(inputData$datapath)
    if(input$plotType=="regression"){
      output$target <- renderUI({
        radioButtons("target","Target",choices = colnames(dataTable))
      })
      
      output$inputs <- renderUI({
        checkboxGroupInput("inputs","Inputs", choices = colnames(dataTable))
      })
      output$attr_x <- renderUI({})
      output$attr_y <- renderUI({})
      output$attr_multiple <- renderUI({})
      output$plotSingleDouble <- renderUI({})
    }else{
      output$attr_x <- renderUI({
        selectInput("attr_x","x-attribute",choices = colnames(dataTable),
                    selected = input$attr_x)
      })
      
      output$attr_y <- renderUI({
        selectInput("attr_y","y-attribute",choices = colnames(dataTable),
                    selected = input$attr_y)
      })
      
      output$annova <- renderUI({})
      output$fitted <- renderUI({})
    }
  })
  
  observeEvent(input$plotSingleDouble,{
    inputData <- input$dataFinal
    if (is.null(inputData))
      return(NULL)
    dataTable <- read.csv(inputData$datapath)
    if(input$plotSingleDouble == FALSE){
      output$attr_multiple <- renderUI({})
      output$attr_y <- renderUI({
        selectInput("attr_y","y-attribute",choices = colnames(dataTable))
      })
    }else{
      output$attr_y <- renderUI({})
      output$attr_multiple <- renderUI({
        checkboxGroupInput("attr_multiple","Choose",
                           choices = colnames(dataTable))
      })
    }
  })
  
  observeEvent(input$countryOption, {
    if(input$countryOption == "countryWise"){
      inputData <- input$dataFinal
      if (is.null(inputData))
        return(NULL)
      dataTable <- read.csv(inputData$datapath)
      output$target <- renderUI({})
      output$inputs <- renderUI({})
      output$annova <- renderUI({})
      output$fitted <- renderUI({})
      
      output$attr_x <- renderUI({
        selectInput("attr_x","x-attribute",choices = colnames(dataTable))
      })
      
      output$attr_y <- renderUI({
        selectInput("attr_y","y-attribute",choices = colnames(dataTable))
      })
      
      output$plotType <- renderUI({
        selectInput("plotType","Plot Type", 
                    choices = c("boxplot","line",histogram="h","density","smooth",
                                "jitter",points="p",PointsAndLines="o",
                                StairSteps="s","regression"))
      })
      
      output$main_table <- renderTable({
        trimmedInputCountry <- trimws(input$countryInput, which = "both")
        query <- sprintf("select * from dataTable where country = '%s' ",trimmedInputCountry)
        dataByInput <- sqldf(query)
        dataByInput
      })
      
      output$plots <- renderPlot({
        trimmedInputCountry <- trimws(input$countryInput, which = "both")
        query <- sprintf("select * from dataTable where country = '%s' ",trimmedInputCountry)
        dataByInput <- sqldf(query)
        df <- data.frame(dataByInput)
        attribx <- input$attr_x
        attriby <- input$attr_y
        
        if(input$plotSingleDouble == FALSE){
            if(input$plotType=="bar" || input$plotType=="density"){
              qplot(df[,eval(attribx)],
                    data = df,
                    geom = input$plotType, 
                    shape=attribx, color=attribx,
                    xlab = attribx)
            }else if(input$plotType=="h" || input$plotType=="p"  ||
                     input$plotType=="o" ||
                     input$plotType=="s" ){
              plot(df[,eval(attribx)], df[,eval(attriby)], border = "red",
                   type = input$plotType, col = "red",
                   main = input$plotType, xlab = attribx, ylab = attriby)
            }else if(input$plotType=="regression"){
              output$attr_x <- renderUI({})
              output$attr_y <- renderUI({})
              
              b <- paste("",input$inputs, collapse = '+')
              formula <- paste("",input$target," ~ ",b)
              print(formula)
              fit <- lm(formula, data = dataByInput)
              plot(fit)
              
              
              output$annova <- renderTable({
                anova(fit)
              })
              
              output$fitted <- renderTable({
                vcov(fit)
              })
            }
            else{
              qplot(df[,eval(attribx)], df[,eval(attriby)],
                    data = df, 
                    geom = input$plotType, 
                    shape=attribx, color=attribx,
                    xlab = attribx, ylab = attriby)
              
            }
        }else{
          aaaaa <- c(input$attr_multiple)
          
          g <- ggplot(df, aes(df[,eval(attribx)]))
          for(yc in aaaaa){
            
            g = g + geom_smooth(aes_string(y=yc), 
                                color = "green") + scale_y_continuous(limits = c(0.0,10000.0))
            g = g + geom_point(aes_string(y=yc), color = "red")
          }
          g
        }
      })  
    }
    else if(input$countryOption == "allCountry"){
      output$attr_x <- renderUI({})
      output$attr_y <- renderUI({})
      
      output$plotType <- renderUI({
        selectInput("plotTypeAll","Plot Type", 
                    choices = c("regression","cluster"))
      })
      
      inputData <- input$dataFinal
      if (is.null(inputData))
        return(NULL)
      dataTable <- read.csv(inputData$datapath)
      if(input$plotTypeAll=="regression"){
            output$target <- renderUI({
              radioButtons("target","Target",choices = colnames(dataTable))
            })
            
            output$inputs <- renderUI({
              checkboxGroupInput("inputs","Inputs", choices = colnames(dataTable))
            }) 
          
          
          output$main_table <- renderTable({
            query <- sprintf("select * from dataTable")
            dataByInput <- sqldf(query)
            dataByInput
          })
          
          output$plots <- renderPlot({
            b <- paste("",input$inputs, collapse = '+')
            formula <- paste("",input$target," ~ ",b)
            print(formula)
            fit <- lm(formula, data = dataTable)
            plot(fit)
             
                # summary(fit)
                # coefficients(fit) # model coefficients
                # confint(fit, level=0.95) # CIs for model parameters 
                # fitted(fit) # predicted values
                # residuals(fit) # residuals
                # anova(fit) # anova table 
                # vcov(fit) # covariance matrix for model parameters 
                # influence(fit) # regression diagnostics
          })
              
          output$annova <- renderTable({
            b <- paste("",input$inputs, collapse = '+')
            formula <- paste("",input$target," ~ ",b)
            print(formula)
            fit <- lm(formula, data = dataTable)
            anova(fit)
          })
          
          output$fitted <- renderTable({
            b <- paste("",input$inputs, collapse = '+')
            formula <- paste("",input$target," ~ ",b)
            print(formula)
            fit <- lm(formula, data = dataTable)
            vcov(fit)
          })
      }else if(input$plotTypeAll=="cluster"){
        output$target <- renderUI({})

        output$inputs <- renderUI({})

        output$plots <- renderPlot({
          myIris <- scale(c(input$kmeansScaling))
          irisKM <- kmeans(myIris,input$clusterNumbers)
          plot(db$plotOption, col = irisKM$cluster)
          clusplot(df, irisKM$cluster, color = TRUE, lines = 0)
        })
      }
    }
     
  })
  
})


shinyApp(ui,server)
