library(shiny)

# file input limit
# options(shiny.maxRequestSize = 9*1024^2)

# file$datapath -> gives the path of the file

shinyServer(function(input,output){
  source("./Functions.R", local = TRUE)
  
  ### {INPUT TABLE TAB} ###
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
  }, rownames = TRUE)
  
  ### {RATE TABLE} ###
  output$rate <- renderTable({
    if(is.null(data())){return ()}
    rateInput()
  }, rownames = TRUE)
  
  ### {INPUT SUMMARY TAB} ###
  output$sum <- renderTable({
    if(is.null(data())){return ()}
    #summary(data())
    do.call(cbind, lapply(data(), summary))
  }, rownames = TRUE)
  
  
  ### {REGRESSION TAB} ###
  
  output$reg <- renderPrint({
    if(is.null(data())){return ()}
    regrInput()
  })
  
  
  ### {MODWT COEFS TAB} ###
  
  # Title 1
  output$tmod1 <- renderUI({
    t1coefsInput()
  })
  
  # Title 2
  output$tmod2 <- renderUI({
    t2coefsInput()
  })
  
  # Table W
  output$mod1 <- renderTable({
    coefs$w
  }, rownames = TRUE)
  
  # Table V
  output$mod2 <- renderTable({
    coefs$v
  }, rownames = TRUE)
  
  
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
      write.csv(ddata(), file)
      
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
 
  ### {PATH TAB} ###
  
  output$filedf <- renderTable({
    if(is.null(data())){return ()}
    input$file
  })
  
  output$dcenter <- renderTable({
    No. <- c(1:12)
    Tables <- c('Stocks: Change Rate', 'Stocks: Wavelet Coefficients', 'Stocks: Scaling Coefficients',
                'Stocks: Beta Coefficients','Benchmark: Change Rate', 'Benchmark: Wavelet Coefficients',
                'Benchmark: Scaling Coefficients', 'Benchmark: Beta Coefficients', 'Free Risk: Change Rate',
                'Free Risk: Wavelet Coefficients', 'Free Risk: Scaling Coefficients','Free Risk: Beta Coefficients')
    Data <- rep("Download", 12)
    data.frame(No., Tables, Data)
  })
  
  
  ### {TAB GENERATOR} ###
  
  output$tb <- renderUI({
    if(is.null(data()))
      HTML('<center><img src="deree_logo.jpg"></center>')
    else
      tabsetPanel(tabPanel("Data", tableOutput("table")),
                  tabPanel("Change Rate", tableOutput("rate")),
                  tabPanel("Summary", tableOutput("sum")),
                  tabPanel("Beta Coefficients", verbatimTextOutput("beta1")),
                  tabPanel("Wavelet Coefficients", htmlOutput("tmod1"),
                           tableOutput("mod1"), htmlOutput("tmod2"),
                           tableOutput("mod2")),
                  tabPanel("Wavelet Plot", plotOutput("plot", width = 1280, height = 720)),
                  tabPanel("About file", tableOutput("filedf"),
                           tableOutput("dcenter"))
                  )
  })
}) 
