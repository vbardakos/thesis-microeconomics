#install.packages('wavelets')
library(shiny)
# install.packages("wavelets", destdir = "D:/OneDrive/DEREE/RStudio/shiny regression/packages")
library(wavelets)
# library(rsconnect)
# install.packages('rsconnect')
shinyUI(
  fluidPage(
  titlePanel(h3("Maximum Overlap Discrete Wavelets Calculator")),
  sidebarLayout(
     sidebarPanel(tags$head(
      tags$style(type="text/css", "select { max-width: 200px; }"),
      tags$style(type="text/css", ".span4 { max-width: 250px; }"),
      tags$style(type="text/css", ".well { max-width: 514px; }")
    ),
      fileInput("file","Upload the Stocks"),
      checkboxInput(inputId = "week", "Transform Daily to Weekly", value = FALSE),
      fileInput("bench","Upload the Benchmark"),
      fileInput("free","Upload the Risk Free"),
      helpText("Accepts only '.csv' file type."),
      tags$hr(),
      h5(helpText("Select the table parameters")),
      checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
      checkboxInput(inputId = "stringAsFactors", "string As Factors", FALSE),
      #br(),

      tags$hr(),

          
### {WAVELET INPUTS} ###

      selectInput("filter", label = h5(helpText("Select the M.O. Wavelet filter")), 
                  choices = list("Haar" = "haar", "Least Assymetric 8" = "la8",
                                 "Daubechies 4" = "d4", "Coiflet 6" = "c6"), selected = "la8"),
      selectInput("boundary", label = h5(helpText("Select the M.O. Wavelet boundary")), 
                  choices = list("Periodic" = "periodic", "Reflection" = "reflection"), selected = "periodic"),
      numericInput("levels", label = h5(helpText("Select levels")), value = 3, min = 1, max = 6),
      actionButton("calc", "Calculate M.O.D.W.T", width = '100%'),
    
      tags$hr(),
      h5(helpText("Wavelet & Scaling Coefficient Matrices")),
      downloadLink('downloadw', 'Download Wavelet Coefficients'),
      downloadButton('downloadv', 'Download Scaling Coefficients'),
      downloadLink('downloadr', 'Download Change Rate'),

      tags$hr(),
      textInput(inputId = "model1", label = "LS Regression", width = NULL, placeholder = NULL),
      helpText("Use the Column Headers to call a variable"),
      helpText("Formula General Format:"),
      helpText("x1 ~ x2 + x3 + ... + xn")
    ),

    mainPanel(
      uiOutput("tb")
      
    )
    
  )
))
