
library("rstan")
options(mc.cores = 4)

# for the best "rstan" experience - always use the following settings coded in lines 5 and 6
# options(mc.cores = parallel::detectCores())
# rstan_options(auto_write = TRUE)

# suppose we want to simulate data from a normal distribution to recover the following parameters:
# --- true mean (mu) of 5;
# --- true standard deviation (sigma) of 1;
# --- sample size (N) of 1000
x <- rnorm(1000, mean = 5, sd = 1)

# note that r-stan only accept the data in a list format
data <- list(N = 1000, X = x)

# run stan model
setwd("/Users/anwarmusah/Desktop/Model Catalogue/Stan Scripts/")
results = stan(file = "Estimate summary measures Mean and SD.stan", data = data)
print(results)

# graph the posterior samples

params <- extract(results)
hist(params$mu)
hist(params$sigma)


# The mean BMI value is 23 with SD of 1.2. Simulate sample of 1000 with BMI values based on this 
# distribution N(23, 2.6) and perform Bayesian inference. What is the posterior mean BMI 
# and its 95% Credibility intervals?

# **Hints:**

# 1. In the RScript, use the function `rnorm()` to generate your sample of 1000 BMI points
# 2. In the RScript, create a list() with `N` and `bmi` defined
# 3. In Stan script, define the data block in accordance to the list() object
# 4. In Stan script, the `bmi` values are measured outcome. Code this in the model block as a likelihood function using 
# the `norm(mu, sigma)` notation
# 5. In Stan script, use the parameter block, and make sure to code your `mu` (mean) and `sigma` 
# (standard deviation) as real (non-negative) numbers 