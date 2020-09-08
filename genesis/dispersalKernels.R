
library(tidyverse)

dispRange         <- 100

dispersal_range = c(0, 100)
disp_factor <- 1

# weibull distribution
scale      <- ((dispersal_range[2] - dispersal_range[1]) * disp_factor ) + dispersal_range[1]
wei_values <- rweibull(100000, shape = 1, scale = scale) # 2.5
wei_data   <- data.frame(wei_values)

ggplot(wei_data, aes(x = wei_values)) +
  geom_histogram(bins = 300) +
  ggtitle("weibull")


# exponential
exp_values <- rexp(100000, rate = 1/dispRange)
exp_data   <- data.frame(exp_values)

ggplot(exp_data, aes(x = exp_values)) +
  geom_histogram(bins = 300) +
  ggtitle("exponential")
