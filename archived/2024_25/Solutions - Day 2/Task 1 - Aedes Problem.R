
# Load the packages with library()
library('rstan')

# set up for stan
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# create list() object with N, Infested
stan_dataset <- list(N = 967, Infested = 428)

# compile stan code
aedes_prediction <- stan("Task 1 - Aedes Problem.stan", data = stan_dataset, seed = 123456780, iter = 3000, chains = 3, verbose = FALSE)

# print result with posterior mean prevalence and 95% CrI
print(aedes_prediction, probs = c(0.025, 0.5, 0.975))

# extract posterior results
posterior_estimates <- extract(aedes_prediction)

# generate probability density plot of the posterior samples for prevalence
plot(density(posterior_estimates$prevalence), 
	main = "Posterior samples", 
	xlab = "Predicted Prevalence of Aedes Infestation", 
	ylab = "Posterior Probability Density (Plausibility)")