# Task 2
# set-up
library("rstan")

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# create dataset
bmi <- rnorm(1000, mean=23, 8.3)

# remove negative values generated from rnorm()
bmi <- bmi[bmi>=0]

# use length() to count the length of the bmi array and set to N instead of 'hardcoding' the value.
task2.dataset <- list(N=length(bmi), bmi=bmi)

# run stan code 
predictions.bmi = stan("Task 2 Stan script.stan", data=task2.dataset, seed = 123456780, iter=3000, chains=3, verbose = FALSE)

# show summary table
print(predictions.bmi, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))

# extracting the BMI samples
extracted_samples <- rstan::extract(predictions.bmi)

# generate density plot
plot(density(extracted_samples$mu), 
	main = "Posterior samples", 
	xlab = "Posterior Means prediction for BMI", 
	ylab = "Posterior Probability Density (Plausibility)")