library(ggplot2)
library(viridis)

# Define parameter ranges
I0_vals <- seq(0, 12, length.out = 300)    # Initial infections (I0)
r_vals  <- seq(0, 1, length.out = 300)     # Growth rate (r)

# Build grid
grid <- expand.grid(I0 = I0_vals, r = r_vals)

# Define priors
alpha <- 2       # shape for I0
beta <- 0.1        # rate for I0 (mean = alpha / beta = 3)

mu_r <- 0      # mean of r
sigma_r <- 1   # sd of r

# Compute densities
grid$gamma_density <- dgamma(grid$I0, shape = alpha, rate = beta)
grid$normal_density <- dnorm(grid$r, mean = mu_r, sd = sigma_r)

# Joint density (assumed independent)
grid$joint_density <- grid$gamma_density * grid$normal_density

# Plot
ggplot(grid, aes(x = I0, y = r, z = joint_density)) +
	geom_raster(aes(fill = joint_density), interpolate = TRUE) +
	geom_contour(color = "white", bins = 10, size = 0.4) +
	scale_fill_viridis(option = "plasma", name = "Density") +
	geom_point(aes(x = 5, y = 0.5), color = "red", size = 3) +
	labs(
		x = expression(I[0]),
		y = "r",
		title = "Joint Prior Distribution: Gamma(I₀), Normal(r)"
	) +
	theme_minimal(base_size = 14) +
	theme(
		panel.background = element_rect(fill = "black"),
		plot.background = element_rect(fill = "black"),
		panel.grid = element_blank(),
		axis.text = element_text(color = "white"),
		axis.title = element_text(color = "white"),
		plot.title = element_text(color = "white"),
		legend.text = element_text(color = "white"),
		legend.title = element_text(color = "white"),
		legend.background = element_rect(fill = "black")
	)


