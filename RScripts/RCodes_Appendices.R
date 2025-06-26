# Load the packages with library()
library('rstan')
library('plot3D')
library('ggplot2')
library('MASS')
library('tidybayes')
library('posterior')

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Simulated data
day <- 0:14
observed_cases <- c(12, 9, 19, 30, 27, 45, 67, 71, 103, 119, 161, 213, 288, 340, 431)

# Data list for Stan
stan_dataset <- list(
	N = length(day),
	t = as.vector(day),
	y = as.integer(observed_cases)
)

fit <- stan(
	file = "Incidence_rates.stan",
	data = stan_dataset,
	iter = 5000,
	warmup = 20,
	chains = 2,
	seed = 20231221,
	verbose = FALSE
)

print(fit, pars = c("D0", "r"), probs = c(0.025, 0.5, 0.975))

# extract (near) full posterior which include warm up/burnin samples (i.e., non-discarded)
full_posterior <- spread_draws(fit, D0, r)

# Create a grid of values for the 3D plot
D0_seq <- seq(min(full_posterior$D0), max(full_posterior$D0), length.out = 500)
r_seq <- seq(min(full_posterior$r), max(full_posterior$r), length.out = 500)
grid <- expand.grid(D0 = D0_seq, R = r_seq)

# Estimate the density on the grid
density_vals <- kde2d(full_posterior$D0, full_posterior$r, n = 500)

# 3D surface plot using persp3D
persp3D(
	x = density_vals$x, y = density_vals$y, z = density_vals$z,
	theta = 30, phi = 30,          # Adjust the view angles
	expand = 0.6,                  # Scale the plot
	colvar = density_vals$z,       # Use density values for color mapping
	col = colorRampPalette(c("darkblue", "lightblue", "yellow", "orange", "red"))(100),  # Gradient colors
	xlab = "D0",                   # X-axis label
	ylab = "r",                    # Y-axis label
	zlab = "Plausibility",         # Z-axis label (Density)
	main = "3D Joint Posterior Distribution of D0 and r",
	ticktype = "detailed",         # Detailed tick marks on axes
	facets = TRUE                  # Add grid lines on the surface
)

# Create a 2D heatmap using ggplot2
heatmap_data <- expand.grid(D0 = density_vals$x, r = density_vals$y)
heatmap_data$Density <- as.vector(density_vals$z)

# Create the heatmap plot using ggplot2
ggplot(heatmap_data, aes(x = D0, y = r, fill = Density)) +
	geom_tile() +
	scale_fill_gradientn(colors = c("darkblue", "lightblue", "yellow", "orange", "red")) +
	geom_vline(xintercept = 12.18, linetype = "dashed", color = "black", size = 1, linewidth = 0.5) +
	geom_hline(yintercept = 0.26, linetype = "dashed", color = "black", size = 1, linewidth = 0.5) +
	labs(
		title = "2D Heatmap of Posterior Distribution for D0 and r",
		x = "Estimated D0", y = "Estimated r", fill = "Density"
	) +
	theme_minimal() +
	theme(
		axis.text = element_text(size = 12), 
		axis.title = element_text(size = 14),
		plot.title = element_text(size = 16, face = "bold")
	)



library(plot3D)

# 1. Create the surface
persp3D(
	x = density_vals$x, y = density_vals$y, z = density_vals$z,
	theta = 30, phi = 30,
	expand = 0.6,
	colvar = density_vals$z,
	col = colorRampPalette(c("darkblue", "lightblue", "yellow", "orange", "red"))(100),
	xlab = "D0", ylab = "r", zlab = "Plausibility",
	main = "3D Joint Posterior Distribution of D0 and r",
	ticktype = "detailed",
	facets = TRUE
)

# 2. Get Z values for D0 = 12.18 and r = 0.26
# Find nearest x and y index
ix <- which.min(abs(density_vals$x - 12.18))
iy <- which.min(abs(density_vals$y - 0.26))

# Extract z at intersection
z_val <- density_vals$z[ix, iy]

# 3. Add vertical line at D0 = 12.18 (x fixed)
lines3D(
	x = rep(density_vals$x[ix], 2),
	y = rep(density_vals$y[iy], 2),
	z = c(0, z_val),
	col = "black", lwd = 2, add = TRUE
)

# Optional: simulate dashed effect by breaking into segments
# Repeat with small gaps if needed for a "dashed" appearance

# You can draw the projection lines separately for a grid effect too






