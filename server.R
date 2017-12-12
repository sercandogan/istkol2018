library(tidyverse)
library(shiny)
library(gridExtra)
set.seed(21102017)

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
  
  
  
  
  
  
})
