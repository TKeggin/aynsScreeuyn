traits[,"neutral"]  <- c(traits[,"neutral"]) + rnorm(length(c(traits[,"neutral"])), mean=0,sd=0.05)

test <- rep(0.5, 1000)
rate <- seq(from=0.01, to=0.15, by = 0.01)

result.df <- data.frame(test)
for(r in 1:length(rate)){
  x <- test + rnorm(length(test), mean = 0, sd = rate[r])
  result.df <- cbind(result.df,x)
}

rate <- c(0,rate)
colnames(result.df) <- as.character(rate)

result.df <- pivot_longer(result.df,as.character(rate))
colnames(result.df) <- c("sd","niche")


ggplot(result.df, aes(x = niche)) +
  #geom_histogram(bins = 100, aes(fill = sd), colour = "black", alpha = 0.3, position = "identity" ) +
  geom_density( aes(fill = sd), colour = "black", alpha = 0.3) +
  #xlim(c(0,1)) +
  theme_bw()
