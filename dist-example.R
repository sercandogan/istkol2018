library(tidyverse)
set.seed(21102017)
N <- 20
x <- seq(0,(N-1))

data <- data.frame(x = x, y = x1)

ggplot2::fortify(data)

# Binomial Distribution
x1 <- dnorm(x, mean(x), sd(x))

ggplot(data_frame(x,x1), aes(x,x1)) +
  geom_bar(aes(y = x1), stat = "identity") +
  stat_function(fun = dbinom, n = N ,args = list(size = N, prob = 0.5), colour = "black")

# Normal Distribution
ggplot(data_frame(x,x1), aes(x,x1)) +
  geom_bar(aes(y = x1), stat = "identity") +
  stat_function(fun = dnorm, n = N ,args = list(mean = mean(x), sd = sd(x)), colour = "black") +
  scale_x_continuous(name = '') +
  scale_y_continuous(name = '', labels = scales::percent)



