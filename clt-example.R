library(tidyverse)
library(gridExtra)
set.seed(123) ## set the seed for reproducibility
r <- 30 ^ 2      # number of replication
n <- 30      ## I use 200 instead of 20 to enhance convergence to Gaussian

## this function computes the r samples of the sample mean from the 
## r*n original samples
sample.means <- function(samps, r, n) {
  rowMeans(matrix(samps,nrow=r,ncol=n))
}



generate.plots <- function(samps, samp.means,r,n) {
  p1 <- qplot(samps, geom="histogram", bins=30, main="Sample Histogram")
  p2 <- qplot(samp.means, geom="histogram", bins=30, main="Sample Mean Histogram") +
    stat_function(fun = dnorm, n = (r*n), args = list(mean = mean(samps), sd = (sd(samps) /sqrt(r))))
  grid.arrange(p1,p2)
}


samps <- rpois(r*n, lambda = 3)  ## poison distribution [0,1]
# compute sample means
samp.means <- sample.means(samps, r, n)
# generate plots
generate.plots(samps, samp.means,r,n)


# Make density line
x3 <- rnorm(r*n,mean = mean(samps), sd = sd(samps) /  sqrt(r * n))

ggplot(data_frame(x3),aes(x3)) +
  stat_function(fun = dnorm, n = r*n, args = list(mean = mean(samps), sd = (sd(samps) / sqrt(r*n))))





