library(shiny)
Sys.setenv(R_CMDZIP = 'C:/Rtools/bin/zip')

shinyServer(function(input,output){
  source("./Functions.R", local = TRUE)
  
  ### {INPUT TABLE TAB} ###
  output$table <- renderTable({
    if(is.null(data())){return ()}
    shortInput(data())
  }, rownames = TRUE)
  
  ### {RATE TABLE} ###
  output$rate <- renderTable({
    shortInput(rateInput())
  }, rownames = TRUE)
  
  ### {INPUT SUMMARY TAB} ###
  output$sum <- renderTable({
    do.call(cbind, lapply(data(), summary))
  }, rownames = TRUE)
  
  
  ### {REGRESSION TAB} ###
  
  output$beta1 <- renderTable({
    if(is.null(data()) | is.null(bench()) | is.null(free())){return ()}
    shortInput(regrInput())
  }, digits = 4, rownames = TRUE)
  
  
  ### {REGRESSION TAB 2} ###
  
  output$beta2 <- renderTable({
    if(is.null(data()) | is.null(bench()) | is.null(free())){return ()}
    shortInput(regrInput2())
  }, digits = 4, rownames = TRUE)
  
  output$beta3 <- renderTable({
    if(is.null(data()) | is.null(bench()) | is.null(free())){return ()}
    shortInput(regrInput3())
  }, digits = 4, rownames = TRUE)
  
  ### {MODWT COEFS TAB} ###
  
  # Title 1
  output$tmod1 <- renderUI({
    t1coefsInput()
  })
  
  # Title 2
  output$tmod2 <- renderUI({
    t2coefsInput()
  })
  
  # Table WSF
  output$wsf <- renderTable({
    if(is.null(coefs$wsf)){return()}
    shortInput(coefs$wsf)
  }, digits = 4, rownames = TRUE)
  
  # Table VSF
  output$vsf <- renderTable({
    if(is.null(coefs$vsf)){return()}
    shortInput(coefs$vsf)
  }, digits = 4, rownames = TRUE)
  
  # Table WBF
  output$wbf <- renderTable({
    if(is.null(coefs$wbf)){return()}
    shortInput(coefs$wbf)
  }, digits = 4, rownames = TRUE)

  # Table VBF
  output$vbf <- renderTable({
    if(is.null(coefs$vbf)){return()}
    shortInput(coefs$vbf)
  }, digits = 4, rownames = TRUE)
  
  
  ### {MODWT PLOT TAB} ###
  
  output$plot <- renderPlot({
    if(is.null(data())){return ()}
    # input$calc
    # waveInput()
    plot(waveInput(), col.plot = "blueviolet")
    
  })
  #height = 100%, width = 100%
    


  ### {DOWNLOADS} ###

  output$downloadr <- downloadHandler(
    filename = function() {
      paste("ChangeRate", Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(rateInput(), file)
    }
  )
  
  output$downloadw <- downloadHandler(
    filename = function() {
      paste(input$filter, "L", input$levels, "WavCoef", Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(dwcoefsInput(), file)
    }
  )
  
  output$downloadv <- downloadHandler(
    filename = function() {
      paste(input$filter, "L", input$levels, "ScalCoef", Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(dvcoefsInput(), file)
    }
  )
  
  output$downloadbeta <- downloadHandler(
    filename = function() {
      paste("BetaCoefs", Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(regrInput(), file)
    }
  )
  output$downloadwav <- downloadHandler(
    filename = function() {
      paste("WavBetaCoefs", Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(regrInput2(), file)
    }
  )
 
  ### {PATH TAB} ###
  
  output$filedf <- renderTable({
    if(is.null(data())){return ()}
    rbind(input$file, input$bench, input$free)
    
  })
  
  ### {TAB GENERATOR} ###
  
  output$tb <- renderUI({
    if(is.null(data()))
      HTML('<center><img src="deree_logo.jpg"></center>')
    else
      tabsetPanel(tabPanel("Data", tableOutput("table")),
                  tabPanel("Change Rate", tableOutput("rate")),
                  tabPanel("Summary", tableOutput("sum")),
                  tabPanel("Beta Coefficients", tableOutput("beta1")),
                  tabPanel("Beta Coefficients 2", tableOutput("beta2")),
                  tabPanel("Wavelets Coefs", fluidRow(htmlOutput("tmod1")),
                           fluidRow(
                             column(width = 7, tableOutput("wsf")),
                             column(width = 5, tableOutput("wbf"))),
                           fluidRow(htmlOutput("tmod2")),
                             column(width = 7, tableOutput("vsf")),
                             column(width = 5, tableOutput("vbf"))),
                  tabPanel("Wavelet Plot", plotOutput("plot", width = 1280, height = 720)),
                  tabPanel("About file", tableOutput("filedf"),
                          htmlOutput("dcenter"))
                  )
  })
})
