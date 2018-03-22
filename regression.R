library(tidyverse)


# Generate Data for Regression
# variance for errors: 10
N <- 100
x <- runif(N, 10,100)
beta <- runif(1,-1,1)*rnorm(1,3,1)
y    <- runif(1,-10,10) + x * beta + rnorm(N,0,10)

y_mean <- mean(y)

data <- tibble(x,y)

ggplot(data, mapping = aes(x, y)) +
  geom_point()

# beta 0: -8 / beta 1: -0.59
ypredicted <- 3 + (1.3) * x 


summary(lm(y ~ x, data = data)) 

SSE <- sum((y - ypredicted) ^ 2)
SST <- sum((y - y_mean) ^ 2)
SSR <- sum((ypredicted - y_mean) ^ 2) 

1 - SSE / SST
SSR / SST


foo <- summary(data)


foo <- as_tibble(foo) %>%
  select(-Var1)

foo

tibble(x = foo$n[1:6],
       y = foo$n[7:12])


