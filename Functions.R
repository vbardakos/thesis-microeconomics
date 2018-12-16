### {IMPORTED DATA} ###
  data <- reactive({
      file1 <- input$file
      if(is.null(file1)){return()}
      data <- read.table(file=file1$datapath, sep= ',', header = input$header, stringsAsFactors = input$stringAsFactors)
  })

### {BENCHMARK} ###  
  bench <- reactive({
    file1 <- input$bench
    if(is.null(file1)){return()}
    data <- read.table(file=file1$datapath, sep= ',', header = input$header, stringsAsFactors = input$stringAsFactors)
    for (i in 1:nrow(data)) {
      data[i,1] <- (data[i+1,1] - data[i,1])/data[i,1]
    }
    data <- data[-nrow(data),]
  })

### {FREE RISK} ###  
  free <- reactive({
    file1 <- input$free
    if(is.null(file1)){return()}
    data <- read.table(file=file1$datapath, sep= ',', header = input$header, stringsAsFactors = input$stringAsFactors)
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
  })

### {SHORT OUTPUT} ###
  shortInput <- function(x) { 
    if(nrow(x) <= 30 & ncol(x) <= 8){
      x
    } else {
      if(ncol(x) > 8) {
        x <- cbind(x[,1:3], x[,(ncol(x)-2):ncol(x)])
      } else {}
      x <- rbind(head(x), tail(x))
    }
  }

### {WAVELET HEADERS} ###
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
  
### {RSF/RBF} ###
  rsfbf <- function(x) {
    data <- rateInput(); free <- free()
    free <- data.frame(free)
    if(x == 'rsf') {
      x <- data.frame(apply(data[,],2,'-',free))
    } else if(x == 'rbf') {
      bench <- bench()
      bench <- data.frame(bench)
      x <- data.frame(apply(bench[],2,'-',free))
    } else {return()}
  }

  wav <- function(x, y) {
    # wave <- waveInput()
    data <- rsfbf(x)
    wave <- wavelets::modwt(data, filter = input$filter, n.levels = input$levels, boundary = input$boundary)
    if(y == 'w') {
      name_list <- expand.grid(W=names(wave@W),N=names(data))
      name_list <- name_list[order(name_list$W),]
      name_list <- unlist(apply(name_list,1,paste0,collapse=""))
      output <- data.frame(do.call(cbind,wave@W))
      names(output) <- name_list
      x <- output
    } else if(y == 'v') {
      name_list <- expand.grid(V=names(wave@V),N=names(data))
      name_list <- name_list[order(name_list$V),]
      name_list <- unlist(apply(name_list,1,paste0,collapse=""))
      output <- data.frame(do.call(cbind,wave@V))
      names(output) <- name_list
      x <- output
    } else {return()}
  }
  
### {WAVELET TABLES} ###
  coefs <- reactiveValues(wsf = NULL, wbf = NULL, vsf = NULL, vbf = NULL)

  # WSF
  wsfcoefsInput <- observeEvent(input$calc, {
      coefs$wsf <- wav('rsf', 'w')
      write.table(coefs$wsf, file = "./tmp/wsf.csv", col.names = NA, qmethod = "double", sep = ",")
  })
  
  # VSF
  vsfcoefsInput <- observeEvent(input$calc, {
      coefs$vsf <- wav('rsf', 'v')
      write.table(coefs$vsf, file = "./tmp/vsf.csv", col.names = NA, qmethod = "double", sep = ",")
  })

  # WBF
  wbfcoefsInput <- observeEvent(input$calc, {
    coefs$wbf <- wav('rbf', 'w')
    write.table(coefs$wbf, file = "./tmp/wbf.csv", col.names = NA, qmethod = "double", sep = ",")
  })

  # VBF
  vbfcoefsInput <- observeEvent(input$calc, {
    coefs$vbf <- wav('rbf', 'v')
    write.table(coefs$vbf, file = "./tmp/vbf.csv", col.names = NA, qmethod = "double", sep = ",")
  })

### {REGRESSION} ###
  regrInput <- reactive({
      data <- rateInput(); bench <- bench(); free <- free()
      a1 <- vector(); b1 <- vector(); rs <- vector()

      bench <- data.frame(bench)
      free <- data.frame(free)

      rsf <- data.frame(apply(data[,],2,'-',free))
      rbf <- data.frame(apply(bench[],2,'-',free))

      for (i in 1:ncol(rsf)) {
        mod <- lm(rsf[,i] ~ rbf[,1])
        a1[i] <- mod$coefficients[1]
        b1[i] <- mod$coefficients[2]
        rs[i] <- summary(mod)$adj.r.squared
      }
      table <- data.frame('Company_Symbol' = names(data), 'Alpha_Coef' = a1, 'Beta_Coef' = b1, 'Adj.R_Squared' = rs)
  })
  
  ### REG 3
  regrInput3 <- reactive({

  })
  
  ### {REGRESSION 2} ###
  regrInput2 <- reactive({
    wreghandle <- function(x, y){
      mod <- lm(wsf[[x]] ~ wbf[[y]])
      
      return(list(Beta_coef = unname(mod$coefficients[2]),
                  Adj.R_Squared = unname(summary(mod)$adj.r.squared)))
    }
    
    a1 <- vector(); b1 <- vector(); rs <- vector()
    
    vsf <- data.frame(coefs$vsf)
    vlevel <- (ncol(vsf)/input$levels)*(input$levels-1) + 1
    vsf <- vsf[,vlevel:ncol(vsf)]
    vbf <- data.frame(coefs$vbf)
    
    for (i in 1:ncol(vsf)) {
      mod <- lm(vsf[,i] ~ vbf[,input$levels])
      b1[i] <- mod$coefficients[2]
      rs[i] <- summary(mod)$adj.r.squared
    }
    vtable <- data.frame( 'Beta_Coef' = b1, 'Adj.R_Squared' = rs)
    
    k <- ncol(wsf)/input$levels
    wbenchmarknames <- sort(rep(names(wbf), k))
    wtablelist <- Map(wreghandle, names(wsf), wbenchmarknames)
    table <- do.call(rbind, lapply(wtablelist, data.frame))
    
    count <- sort(rep(1:input$levels, nrow(table)/input$levels))
    names <- array(rep(names(data()), input$levels))
    table <- cbind(names, count, table)
    table <- reshape(table, idvar = "names", timevar = "count", direction = "wide")
    table <- cbind(table, vtable)
    row.names(table) <- names(data()); table <- table[,-1]
    table
  })

  
### {DOWNLOADS} ###
  dwcoefsInput <- reactive({
      coefs$w
  })
  dvcoefsInput <- reactive({
      coefs$v
})
