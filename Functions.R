### {IMPORTED DATA} ###
  data <- reactive({
      file1 <- input$file
      if(is.null(file1)){return()}
      data <- read.table(file=file1$datapath, sep= ',', header = input$header, stringsAsFactors = input$stringAsFactors)
      if(nrow(data) <= 30){
        data
      } else {
        rbind(head(data), tail(data))
      }
  })

### {BENCHMARK} ###  
  bench <- reactive({
    file1 <- input$bench
    if(is.null(file1)){return()}
    data <- read.table(file=file1$datapath, sep= ',', header = input$header, stringsAsFactors = input$stringAsFactors)
    for (i in 1:nrow(data)) {
      for (j in 1:ncol(data)) {
        data[i,j] <- (data[i+1,j] - data[i,j])/data[i,j]
      }
    }
  })

### {FREE RISK} ###  
  free <- reactive({
    file1 <- input$free
    if(is.null(file1)){return()}
    data <- read.table(file=file1$datapath, sep= ',', header = input$header, stringsAsFactors = input$stringAsFactors)
    for (i in 1:nrow(data)) {
      for (j in 1:ncol(data)) {
        data[i,j] <- (data[i+1,j] - data[i,j])/data[i,j]
      }
    }
  })

### {FULL DATA} ###
  ddata <- reactive({
      if(input$week == FALSE) {
        file1 <- input$file
        if(is.null(file1)){return()}
        data <- read.table(file=file1$datapath, sep= ',', header = input$header, stringsAsFactors = input$stringAsFactors)
        for (i in 1:nrow(data)) {
          for (j in 1:ncol(data)) {
            data[i,j] <- (data[i+1,j] - data[i,j])/data[i,j]
          }
        }
      } else {
            week <- function(x)format(x, '%Y.%W')
          mydataw <- aggregate(mydata, by = week, FUN = mean)
          mydataw <- data.frame(mydataw)
      }
      data <- data[-nrow(data),]
      as.data.frame(data)
  })

### {RATE TABLE} ###
  rateInput <- reactive({
      data <- data()
      for (i in 1:nrow(data)) {
        for (j in 1:ncol(data)) {
          data[i,j] <- (data[i+1,j] - data[i,j])/data[i,j]
        }
      }
      data <- data[-nrow(data),]
      as.data.frame(data)
  })

### {WAVELET HEADER 1} ###
  t1coefsInput <- reactive({
      HTML(paste
           (tags$br(), h5("W: Wavelet Coefficients"),
           tags$hr()
           )
      )
  })

  t2coefsInput <- reactive({
      HTML(paste(h5("V: Scaling Coefficients"),
               tags$hr())
    )
  })
  
### {MY WAVE} ###
  waveInput <- reactive({
       data <- ddata()
       wave <- wavelets::modwt(ddata(), filter = input$filter, n.levels = input$levels, boundary = input$boundary)
  })

### {W TABLE} ###
  coefs <- reactiveValues(w = NULL, v = NULL)
  wcoefsInput <- observeEvent(input$calc, {
      data <- data()
      wave <- waveInput()
      name_list <- expand.grid(W=names(wave@W),N=names(data))
      name_list <- name_list[order(name_list$W),]
      name_list <- unlist(apply(name_list,1,paste0,collapse=""))
      output <- data.frame(do.call(cbind,wave@W))
      names(output) <- name_list
      if(nrow(output) <= 30){
        coefs$w <- output
      } else {
        coefs$w <- rbind(head(output), tail(output))
      }
  })

### {V TABLE} ###
  vcoefsInput <- observeEvent(input$calc, {
      data <- data()
      wave <- waveInput()
      name_list <- expand.grid(V=names(wave@V),N=names(data))
      name_list <- name_list[order(name_list$V),]
      name_list <- unlist(apply(name_list,1,paste0,collapse=""))
      output <- data.frame(do.call(cbind,wave@V))
      names(output) <- name_list
      if(nrow(output) <= 30){
        coefs$v <- output
      } else {
        coefs$v <- rbind(head(output), tail(output))
      }
  })

### {REGRESSION} ###
  regrInput <- reactive({
      rdf <- apply(ddata(),2,'-',free()) # Stock minus Risk Free Table
      rdf <- data.frame(rdf)
      rbf <- apply(bench(),2,'-',free()) # Benchmark minus Risk Free Table
      rbf <- data.frame(rbf)
      
      for (i in 1:ncol(ddata())) {
        lm(rdf[,i] ~ rbf)
      }
  })

### {DOWNLOADS} ###
  dwcoefsInput <- reactive({
      data <- ddata()
      wave <- wavelets::modwt(ddata(), filter = input$filter, n.levels = input$levels, boundary = input$boundary)
      name_list <- expand.grid(W=names(wave@W),N=names(data))
      name_list <- name_list[order(name_list$W),]
      name_list <- unlist(apply(name_list,1,paste0,collapse=""))
      output <- data.frame(do.call(cbind,wave@W))
      names(output) <- name_list
      output
  })
  dvcoefsInput <- reactive({
      data <- ddata()
      wave <- wavelets::modwt(ddata(), filter = input$filter, n.levels = input$levels, boundary = input$boundary)
      name_list <- expand.grid(V=names(wave@V),N=names(data))
      name_list <- name_list[order(name_list$V),]
      name_list <- unlist(apply(name_list,1,paste0,collapse=""))
      output <- data.frame(do.call(cbind,wave@V))
      names(output) <- name_list
      output
})
