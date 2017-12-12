library(tidyverse)

set.seed(123) ## set the seed for reproducibility
r <- 30 ^ 2      # number of replication
n <- 30      ## I use 200 instead of 20 to enhance convergence to Gaussian

## this function computes the r samples of the sample mean from the 
## r*n original samples
sample.means <- function(samps, r, n) {
  rowMeans(matrix(samps,nrow=r,ncol=n))
}



generate.plots <- function(samps, samp.means) {
  p1 <- qplot(samps, geom="histogram", bins=30, main="Sample Histogram")
  p2 <- qplot(samp.means, geom="histogram", bins=30, main="Sample Mean Histogram")
  grid.arrange(p1,p2)
}


samps <- rpois(r*n, lambda = 3)  ## uniform distribution [0,1]
# compute sample means
samp.means <- sample.means(samps, r, n)
# generate plots
generate.plots(samps, samp.means)



