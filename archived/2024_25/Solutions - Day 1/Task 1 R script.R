# Task 1
# set-up
library("rstan")

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# create dataset
task1.dataset <- list(Total=50, Poisoned=19)

# run stan code 
predictions.poisoning = stan("Task 1 Stan script.stan", data=task1.dataset, seed=19870526, iter=3000, chains=3, verbose = FALSE)
# random generation number has been set to 19870526 for reproducibility

# show summary table
print(predictions.poisoning, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))
# Rhat's okay
# check for divergent samples in the trace plot - if they overlap - it is fine
traceplot(predictions.poisoning)
# they are okay

# extracting the samples (it should be 4500)
theta.draws <- rstan::extract(predictions.poisoning, 'theta')[[1]] 

# you can create a graph of a histogram
hist(theta.draws, xlim = c(0,1), 
	main= "Posterior samples", ylab = "Posterior Density", xlab = expression(paste("Predicted Prevalence: ", theta, " [%]")))

# alternatively, you can create a graph of a density plot
plot(density(theta.draws), 
	main = "Posterior samples", 
	xlab = expression(paste("Predicted Prevalence: ", theta, " [%]")), 
	ylab = "Posterior Probability Density (Plausibility)")

# Calculating posterior mean (estimator of the prevalence of metallic poisoning)
mean(theta.draws)
# Calculating posterior intervals for prevalence
quantile(theta.draws, probs=c(0.025, 0.975))

# The predicted prevalence is 0.3797 (37.97%) with 95% credibility of 0.259 and 0.510 
# We can formally write this as θ = 37.97% (95% CrI: 25.90-51.00%).