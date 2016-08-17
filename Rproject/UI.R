library(shiny)
library(sqldf)
library(ggplot2)
library(fpc)
library(cluster)

ui <- shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        fileInput(
          'dataFinal','choose csv file',
          accept = c('text/csv', 
                     'text/comma-separated-values,text/plain', 
                     '.csv')
        ),
          selectInput("countryOption","Choose",
                  choices = c(AllCountries="allCountry", CountryWise = "countryWise")),
          
          uiOutput("country"),
          uiOutput("plotType"),
          checkboxInput("plotSingleDouble","Multiple Parameters"),
          uiOutput("attr_x"),
          uiOutput("attr_multiple"),
          uiOutput("attr_y"),
          uiOutput("target"),
          uiOutput("inputs"),
          uiOutput("kmeansScaling")
          
      ),
      mainPanel(
        tableOutput("main_table"),
        tags$style(type="text/css", "#main_table{height:280px; overflow-y:scroll}"),
        plotOutput("plots"),
        uiOutput("annova"),
        uiOutput("fitted"),
        uiOutput("clusterNumbers")
      )
    )
    
  )
)

