##############################################################################
########## Test p-value selon la taille de l'échantillon et l'effet ##########
##############################################################################

# D'après Arman Oganisian, Stable Markets, https://stablemarkets.wordpress.com/2016/05/21/exploring-p-values-with-simulations-in-r/


# load packages ----

library(ggplot2)
library(reshape2)
library(dplyr)

options(scipen = 10000)


# create simulation function ----

SimulPvalue <-function(nsamp, b0, b1, x1, ressd){
  y <- b0 + b1 * x1 + rnorm(n = nsamp, mean = 0, sd = ressd)
  linMod <- summary(lm(y ~ x1))$coefficients
  pValue <- linMod[2, 4]
  return(pValue)
}


# set global parameters ----

nsimul = 1000
sampSize <- seq(5, 30, 1)
effectSize <- seq(0.05, .25, 0.01)


# run simulation ----

allPvalues <- expand.grid(SAMPSIZE = sampSize, EFFECTSIZE = effectSize, PVALUE = NA) %>% arrange(EFFECTSIZE, SAMPSIZE)

i <- 1
for(b1 in effectSize){
  for(nsamp in sampSize){
    set.seed(1)
    x1 <- rnorm(n = nsamp, mean = 100, sd = 20)
    pTemp <- replicate(n = nsimul, expr = SimulPvalue(nsamp = nsamp, b0 = 10, b1 = b1, x1 = x1, ressd = 1))
    allPvalues[i, 3] <- mean(pTemp)
    i <- i + 1
  }
}


# plot results ----

ggplot(allPvalues) +
  geom_line(aes(x = SAMPSIZE, y = PVALUE, group = EFFECTSIZE, color = EFFECTSIZE))+
  scale_x_continuous("Sample size", breaks = seq(5, 25, 5)) + 
  scale_y_continuous("Average p-value", breaks = seq(0, 0.25, 0.05)) +
  geom_hline(aes(yintercept = .10), col = "red", linetype = 2) +
  geom_hline(aes(yintercept = .05), col = "red", linetype = 2) +
  geom_hline(aes(yintercept = .01), col = "red", linetype = 2) +
  theme_bw()


