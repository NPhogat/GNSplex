library(shiny)

shinyUI(fluidPage(

  titlePanel("Analysis of the data of lateral flow assay"),

  sidebarLayout(

    sidebarPanel(

      fileInput("file","Choose .csv or .txt tab separated file to upload"),

      tags$hr(),

      checkboxInput("header","Header", TRUE),

      selectInput("filetype", "Select the type of file", choices = c(".csv",".txt")),

      br(),

      numericInput("yin", "Intensity value to compute concentration","1"),

      br(),

      numericInput("mslp", "Slope value to compute concentration","1"),

      br(),

      numericInput("cint", "Slope value to compute concentration","1"),

      br(),

      actionButton("compute","COMPUTE the analysis results!"),

      p("Click on COMPUTE! button to compute the results"),

      br(),

      downloadButton("download", "Download Results")

    ),

    mainPanel(

      uiOutput("tabset")

    )

  )

))
