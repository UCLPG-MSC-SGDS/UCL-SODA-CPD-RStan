
# generate data for Bti larvicide

Sample_size <- 120
mu_true <- 25.3
sigma_true <- 5.2
Bti_mgl <- rnorm(Sample_size, mean = mu_true, sd = sigma_true)

standing_water_id <- 1:120

Bti_data <- data.frame(standing_water_id, Bti_mgl)
write.csv(Bti_data, file = "archived/2024_25/Bti_Larvicide_dataset.csv", row.names = FALSE)


# Load the packages with library()
library('rstan')
library('tidybayes')
library('tidyverse')

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


View(Bti_data)

stan_dataset <- list(N = 120, bti = Bti_data$Bti_mgl)

bti_prediction <- stan("archived/2024_25/Predicting_larvicide_concentrations.stan", data=stan_dataset, iter=3000, chains=3, verbose = FALSE, model_name = "My model")
print(bti_prediction, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))


extracted_samples <- rstan::extract(bti_prediction)

# mean posterior for mu
mean(extracted_samples$mu)
# calculate 95% Credibility limits (95% CrI) for mu
quantile(extracted_samples$mu, probs = c(0.025, 0.975))

# generate probability density plot of the posterior samples
plot(density(extracted_samples$mu), 
	main = "Posterior samples", 
	xlab = "Posterior Means for Bti concentrations (mg/L)", 
	ylab = "Posterior Probability Density (Plausibility)")

# Add vertical dashed line at the mean
abline(v = 25.24285, lty = "dashed", col = "darkgrey", lwd = 2)
# add extract lines for the 95% CrI
abline(v = 24.34943, lty = "dashed", col = "darkgrey", lwd = 2)
abline(v = 26.12595, lty = "dashed", col = "darkgrey", lwd = 2)


# posterior mean for sigma 
mean(extracted_samples$sigma)
# calculate 95% Credibility limits (95% CrI) for sigma
quantile(extracted_samples$sigma, probs = c(0.025, 0.975))

# generate probability density plot of the posterior samples for sigma
plot(density(extracted_samples$sigma), 
	main = "Posterior samples", 
	xlab = "Posterior Means for Standard Deviation", 
	ylab = "Posterior Probability Density (Plausibility)")
abline(v = 4.998751, lty = "dashed", col = "darkgrey", lwd = 2)
abline(v = 4.411431, lty = "dashed", col = "darkgrey", lwd = 2)
abline(v = 5.666914, lty = "dashed", col = "darkgrey", lwd = 2)

