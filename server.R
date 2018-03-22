library(tidyverse)
library(shiny)
library(gridExtra)
set.seed(21102017)

regression_line <- function(x, beta0, beta1){
  return(x * beta1 + beta0)
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  
  # DISTRIBUTIONS --------------------------------------------
  output$distPlot <- renderPlot({
    N <- input$nObs
    x <- seq(0,(N - 1))
    
    
    if(input$distName == "normal"){
      x1 <- rnorm(N^2, mean = input$normalM, sd = input$normalSd)
      
      ggplot(data_frame(x1), aes(x1)) +
        stat_function(fun = dnorm, n = N ,args = list(mean = input$normalM, sd = input$normalSd), colour = "blue")  +
        scale_x_continuous(name = '') +
        scale_y_continuous(name = '', labels = scales::percent)
    }else if(input$distName == "binom"){
      x1 <- dbinom(x,N,input$binomProb)
      
      ggplot(data_frame(x,x1), aes(x,x1)) +
        geom_bar(aes(y = x1), stat = "identity") +
        stat_function(fun = dbinom, n = N ,args = list(size = N, prob = input$binomProb), colour = "blue") +
        scale_x_continuous(name = '') +
        scale_y_continuous(name = '', labels = scales::percent)
    }else if(input$distName == "poisson"){
      x1 <- dpois(x, lambda = input$poisLambda)
      
      ggplot(data_frame(x,x1), aes(x,x1)) +
        geom_bar(aes(y = x1), stat = "identity") +
        stat_function(fun = dpois, n = N ,args = list(lambda = input$poisLambda), colour = "blue") +
        scale_x_continuous(name = '') +
        scale_y_continuous(name = '', labels = scales::percent)
      
    }else if(input$distName == "uniform"){
      x1 <- dunif(x, min = min(x), max(x))
      
      ggplot(data_frame(x,x1), aes(x,x1)) +
        stat_function(fun = dunif, n = N ,args = list(min = min(x), max = max(x)), colour = "blue")  +
        scale_x_continuous(name = '') +
        scale_y_continuous(name = '', labels = scales::percent)
      
    }else if(input$distName == "exponential"){
      x1 <- dexp(x, rate = input$expRate)
      
      ggplot(data_frame(x,x1), aes(x,x1)) +
        stat_function(fun = dexp, n = N ,args = list(rate = input$expRate), colour = "blue")  +
        scale_x_continuous(name = '') +
        scale_y_continuous(name = '', labels = scales::percent)
    }else if(input$distName == "geometric"){
      x1 <- dgeom(x,input$geomProb)
      
      ggplot(data_frame(x,x1), aes(x,x1)) +
        geom_bar(aes(y = x1), stat = "identity") +
        stat_function(fun = dgeom, n = N ,args = list(prob = input$geomProb), colour = "blue") +
        scale_x_continuous(name = '') +
        scale_y_continuous(name = '', labels = scales::percent)
    }else if(input$distName == "nbinom"){
      x1 <- dnbinom(x,N,input$nbinomProb)
      
      ggplot(data_frame(x,x1), aes(x,x1)) +
        geom_bar(aes(y = x1), stat = "identity") +
        stat_function(fun = dnbinom, n = N ,args = list(size = N, prob = input$nbinomProb), colour = "blue") +
        scale_x_continuous(name = '') +
        scale_y_continuous(name = '', labels = scales::percent)
    }else{
      warning("There is a problem!")
    }
    
  })
  
  # CENTRAL LIMIT THEOREM -----------------------------------------------
  
  
  # sample means matrix
  sample.means <- function(samps, r, n) {
    rowMeans(matrix(samps,nrow=r,ncol=n))
  }
  
  # Generate Plots
  generate.plots <- function(samps, samp.means, r, n, distName) {
    p1 <- qplot(samps, geom="histogram", bins=25, main="Sampling Distribution") +
      labs(x = "", y = "Frequency")
    p2 <- qplot(samp.means, geom="histogram", bins=25, main="Sampling Distribution of Mean ")+
      labs(x = "", y = "Frequency") 
    grid.arrange(p1,p2)
  }
  
  
  output$cltPlot <- renderPlot({
    n <- input$samplesize # Sample Size
    r <- n ^ 2 # number of replications
    
    samps <- switch(input$cltDistName,
                    "normal" = rnorm(r*n,input$cltNormalM,input$cltNormalSd),
                    "binom" = rbinom(r*n,n,input$cltBinomP),
                    "exponential" = rexp(r*n,input$cltExpRate),
                    "poisson" = rpois(r*n, input$cltPoisLambda),
                    "geometric" = rgeom(r*n, input$cltGeomProb),
                    "nbinom" = rnbinom(r*n,n,input$cltNbinomProb),
                    "uniform" = runif(r*n, min = input$cltUnifMin, max = input$cltUnifMax))
    
    samp.means <- sample.means(samps,r,n)
     
    
    generate.plots(samps,samp.means,r,n)
  })
  
  
  # SIMPLE LINEAR REGRESSION -----------------------------------------
  
  regN <- reactive({
    regN <- input$regressionN
  })
  
  summary_data <- reactive({
    summary_data <- summary(data()) %>%
      as_tibble() %>%
      select(-Var1)
    
    new_summary <- tibble(x = summary_data$n[1:6],
                          y = summary_data$n[7:12])
  })
  
  data <- reactive({
    beta <- runif(1,-1,1)*rnorm(1,3,1)
    x <- runif(regN(),10,100)
    y <- runif(1,-10,10) + x * beta + rnorm(regN(),0,10)
    data <- tibble(x, y)
  })
  
  
  
 
  
  output$regressionPlot <- renderPlot({
    p <- ggplot(data(), mapping = aes(x, y)) + 
      geom_point() +
      geom_abline(slope = input$regressionB1, 
                  intercept = input$regressionB0,
                  colour = 'blue',
                  size = 1)
    
    if(input$regressionShowModel){
     p <-  p + geom_smooth(method = "lm", se = FALSE, colour = "red")
    }
    
    p
      
  })
  
  output$regressionTable <- renderTable({
    head(data(),10)
  })
  
  output$regressionSummaryDataset <- renderTable({
    summary_data()
  })
  
  observeEvent({input$regressionB0 | input$regressionB1},{
    showNotification(paste("Notification message"), duration = 3)
  })
  
  y_predicted <- reactive({
    y_predicted <- input$regressionB0 + input$regressionB1 * data()$x
  })
  
  regressionTable <- reactive({
    sse <- sum((data()$y - y_predicted()) ^ 2)
    sst <- sum((data()$y - mean(data()$y)) ^ 2)
    ssr <- sst - sse
    regressionTable <- list(
      mse = (sse / (regN() - 2)),
      msr = (ssr / 1),
      b0 = input$regressionB0,
      b1 = input$regressionB1,
      r2 = (1 - sse / sst),
      f_value = (ssr / 1) / (sse / (regN() - 2))
    )
  })
  
  
  regressionOLS <- reactive({
    x_mean <- mean(data()$x)
    y_mean <- mean(data()$y)
    
    beta1 <- sum((data()$x-x_mean)*(data()$y-y_mean))/sum((data()$x-x_mean)^2)
    beta0 <- y_mean - x_mean * beta1
    y_hat <- beta1 * data()$x + beta0
    error <- data()$y - y_hat
    
    se_beta0 <- sqrt(var(error) * ((1/regN())+(x_mean^2)/ sum((data()$x-x_mean)^2)))
    se_beta1 <- sqrt(var(error) /  sum((data()$x-x_mean)^2))
    p_beta0 <- 2 * pt(-abs((beta1-0)/se_beta1), df=regN()-2)
    p_beta1 <- 2 * pt(-abs((beta0-0)/se_beta0), df=regN()-2)   
    
    r_square <- 1 - (sum(error ^ 2) / sum((data()$y - y_mean) ^ 2))
    mse <- sum(error ^ 2) / (regN() - 2)
    f_value <- (sum((data()$y - y_mean) ^ 2) - sum(error ^ 2)) / mse
    
    
    return(
      list(
        beta1 = beta1,
        beta0 = beta0,
        se_beta0 = se_beta0,
        se_beta1 = se_beta1,
        p_beta0 = p_beta0,
        p_beta1 = p_beta1,
        r_square = r_square,
        f_value = f_value,
        mse = mse
      )
    )
  })
  
  
  
  
  output$regressionInformationModel <- renderText({
    HTML("<div>
        B0: ", regressionTable()$b0, "<br>",
        "B1: ", regressionTable()$b1, "<br>",
        "MSE: ", regressionTable()$mse, "<br>",
        "R^2: ", regressionTable()$r2, "<br>",
        "F: ", regressionTable()$f_value, "<br>",
        
         "</div>")
    
  })
  
  
  
  output$regressionRealInformation <- renderText({
    HTML("<div>
        B0: ", regressionOLS()$beta0, "<br>",
         "B1: ", regressionOLS()$beta1, "<br>",
         "Standart Error of B0: ", regressionOLS()$se_beta0, "<br>",
         "Standart Error of B1: ", regressionOLS()$se_beta1, "<br>",
         "p-value of B0: ", regressionOLS()$p_beta0, "<br>",
         "p-value of B1: ", regressionOLS()$p_beta1, "<br>",
         "R^2: ", regressionOLS()$r_square, "<br>",
         "F: ", regressionOLS()$f_value, "<br>",
         "MSE: ", regressionOLS()$mse, "<br>",
         "</div>")
    
  })
  
  
  
  
  
})
