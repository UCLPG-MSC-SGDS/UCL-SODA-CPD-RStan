
setwd("/Users/anwarmusah/Documents/Websites/UCL-SODA-CPD-RStan/archived/2024_25")

# Load the packages with library()
library('rstan')

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Simulated data
day <- 0:14
observed_cases <- c(12, 9, 19, 30, 27, 45, 67, 71, 103, 119, 161, 213, 288, 340, 431)

# Data list for Stan
stan_data <- list(
	N = length(day),
	t = as.vector(day),
	y = as.integer(observed_cases)
)

fit <- stan(
	file = "Incidence_rates.stan",
	data = stan_data,
	iter = 3000,
	chains = 3,
	verbose = FALSE
)

print(fit, pars = c("I0", "eta", "r"), probs = c(0.025, 0.5, 0.975))

# Extract and plot posterior predictions
posterior <- extract(fit)
y_pred_mean <- apply(posterior$y_pred, 2, mean)

plot(day, observed_cases, pch = 16, col = "red", xlab = "Day", ylab = "Cases",
	main = "Observed vs. Posterior Predicted Cases")
lines(day, y_pred_mean, col = "blue", lwd = 2)
legend("topleft", legend = c("Observed", "Predicted Mean"),
	col = c("red", "blue"), pch = c(16, NA), lty = c(NA, 1))


posterior_df <- data.frame(
	I0 = posterior$I0,
	r = posterior$r,
	Predictions = posterior$y_pred
)


# Load packages
library(ggplot2)
library(dplyr)

# Define grid of values for I0 and r
I0_vals <- seq(1, 50, length.out = 200)
r_vals <- seq(-1, 1, length.out = 200)

# Create grid
grid <- expand.grid(I0 = I0_vals, r = r_vals)

# Evaluate prior densities
grid$prior_I0 <- dgamma(grid$I0, shape = 2, rate = 0.1)  # Stan-style rate
grid$prior_r <- dnorm(grid$r, mean = 0, sd = 1)

# Joint prior = product of marginals (independent priors)
grid$joint_prior <- grid$prior_I0 * grid$prior_r

# Plot as heat map
ggplot(grid, aes(x = I0, y = r, fill = joint_prior)) +
	geom_tile() +
	scale_fill_viridis_c(option = "C") +
	labs(title = "Joint Prior Distribution of I0 and r",
		x = expression(I[0]), y = "r",
		fill = "Density") +
	theme_minimal()

# Load libraries
library(ggplot2)
library(dplyr)

# Define grid
I0_vals <- seq(1, 50, length.out = 200)
r_vals <- seq(-3, 3, length.out = 200)
grid <- expand.grid(I0 = I0_vals, r = r_vals)

# Compute densities
grid$prior_I0 <- dgamma(grid$I0, shape = 2, rate = 0.1)
grid$prior_r <- dnorm(grid$r, mean = 0, sd = 1)
grid$joint_density <- grid$prior_I0 * grid$prior_r

# Plot contour (ellipse-like) levels of joint density
ggplot(grid, aes(x = I0, y = r, z = joint_density)) +
	geom_contour(bins = 15, color = "blue") +
	labs(title = "Joint Prior Distribution of I₀ and r",
		x = expression(I[0]), y = "r") +
	theme_minimal()

library(ggplot2)
library(viridis)

# Define grid of values for I0 and r
I0_vals <- seq(1, 50, length.out = 200)
r_vals <- seq(-1, 1, length.out = 200)

# Create grid
grid <- expand.grid(I0 = I0_vals, r = r_vals)

# Evaluate prior densities
grid$prior_I0 <- dgamma(grid$I0, shape = 2, rate = 0.1)  # Stan-style rate
grid$prior_r <- dnorm(grid$r, mean = 0, sd = 1)

# Joint prior = product of marginals (independent priors)
grid$joint_prior <- grid$prior_I0 * grid$prior_r

# Plot as heat map with contour lines
ggplot(grid, aes(x = I0, y = r)) +
	geom_tile(aes(fill = joint_prior)) +
	geom_contour(aes(z = joint_prior), color = "white", bins = 10, size = 0.5) +
	scale_fill_gradientn(
		colors = c("blue", "yellow", "red"),
		name = "Density"
	) +
	labs(
		title = "Joint Prior Distribution of I0 & r",
		x = expression(I[0]), y = "r"
	) +
	theme_minimal()


# Define lognormal parameters
meanlog <- 0
sdlog <- 0.5

# Sequence of eta values
eta_vals <- seq(0.01, 5, length.out = 1000)

# Compute density values
density_vals <- dlnorm(eta_vals, meanlog = meanlog, sdlog = sdlog)

# Plot
plot(eta_vals, density_vals,
	type = "l", lwd = 2, col = "blue",
	xlab = expression(eta), ylab = "Density",
	main = expression("LogNormal(0, 0.5) Prior for " * eta))
grid()


# Parameters
mean_r <- 0
sd_r <- 1

# Range of r values
r_vals <- seq(-4, 4, length.out = 1000)

# Density values
density_vals <- dnorm(r_vals, mean = mean_r, sd = sd_r)

# Plot the normal distribution
plot(r_vals, density_vals,
	type = "l", lwd = 2, col = "blue",
	xlab = "r (growth rate)", ylab = "Density",
	main = expression("Normal(0, 1) Prior for Growth Rate " * r))
grid()


# Parameters
shape <- 2
rate <- 0.1  # Stan-style (rate, not scale)

# Sequence of I0 values
I0_vals <- seq(0, 100, length.out = 1000)

# Density values
density_vals <- dgamma(I0_vals, shape = shape, rate = rate)

# Plot
plot(I0_vals, density_vals,
	type = "l", lwd = 2, col = "blue",
	xlab = expression(I[0]), ylab = "Density",
	main = expression("Gamma(2, 0.1) Prior for " * I[0]))
grid()
