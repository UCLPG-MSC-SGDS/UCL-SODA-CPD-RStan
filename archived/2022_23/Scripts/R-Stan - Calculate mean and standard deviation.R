

library("rstan")
library("bayesplot")
library("ggplot2")
library("tidyverse")
library("tidybayes")

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

exercise_data <- list(N = 7, 
	d = c(9.02, 5.98, 9.53, 4.53, 5.55, 4.32, 10.46),
	r = c(52, 50, 51, 53, 55, 57, 58))

# N is sample size
# d is distance in km
# r is resting heart rate 

setwd("/Volumes/Anwar-HHD/Model Catalogue/Stan Scripts")

SummaryMeasures <- stan("Basic Stan script - Mean and SD.stan",
	data = exercise_data, iter = 2000, chains = 4, warmup = floor(2000/2))

# full summary 
SummaryMeasures

# print selected estimates only i.e., mu, sigma and 95% credibility intervals
print(SummaryMeasures, pars = c("mu_d", "mu_r","sigma_d","sigma_r"), probs = c(0.025, 0.975))

# show traceplots a diagnostics for convergence
traceplot(SummaryMeasures, pars = c("mu_d", "sigma_d"))
traceplot(SummaryMeasures, pars = c("mu_r", "sigma_r"))

# extract the posterior estimates and form a data frame
PosteriorSamples <- as.data.frame(SummaryMeasures)

mcmc_hist(PosteriorSamples, pars = c("mu_d", "mu_r"))